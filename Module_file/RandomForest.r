#!/bin/bash
# Content: A function can run random forest with simultaneous selection method on provided arguments, which at least should include data, knowledge-driven predictors and data-driven predictors.
# Note: 
#		The data should have response_variable (just one) be either continuous or categorical, .
# Author: Yang Yang
# Date:"Tue Jul  1 08:57:26 2014"
# Usage: To be loaded by main program:ModelComputation_workbench_setup.r 
#######################################################################################
#--------------------#
# load some prerequisite libraries
suppressPackageStartupMessages( require(randomForestSRC) )	# the essence
suppressPackageStartupMessages( require(survival) )	# the essence
#-----#
# a subfunction to settup the parallel scheme within rfsrc package.
# called later in RSForest, however, it will be setup in .GlobalEnv.
setRSForest_cores <- function(rf.cores_openMP = detectCores()-1, mc.cores_fork = detectCores()-1) {
	options(rf.cores=rf.cores_openMP, mc.cores=mc.cores_fork )	# setting up OpenMP cores.
	invisible()
}
#--------------------#
# args(rfsrc)
# function (formula, data, ntree = 1000, bootstrap = c("by.root",
    # "by.node", "none"), mtry = NULL, nodesize = NULL, nodedepth = NULL,
    # splitrule = NULL, nsplit = 0, split.null = FALSE, importance = c("permute",
        # "random", "permute.ensemble", "random.ensemble", "none"),
    # na.action = c("na.omit", "na.impute", "na.random"), nimpute = 1,
    # ntime, cause, xvar.wt = NULL, proximity = FALSE, forest = TRUE,
    # var.used = c(FALSE, "all.trees", "by.tree"), split.depth = c(FALSE,
        # "all.trees", "by.tree"), seed = NULL, do.trace = FALSE,
    # membership = TRUE, statistics = FALSE, ...)


#--------------------#
RandomForest <- function(data = data_input,response_variables = response_variables_vec, knowledge_driven_factors = knowledge_driven_factors_vec, data_driven_factors = data_driven_factors_vec, factors_annotation_table = factors_all,.seed=123,plotNameEssence = sprintf("RandomForest_plot_at_%s",nowTimeToString()), add_varMarginalEffectPartialPlot = FALSE ){
	# browser()
	# response_variables <- response_variables_vec <- c("LOS.x","died")	# debugging usage
	# data = data[1:1000]	# reduced set, debugging usage.
	#
	
	#----match call for 'setRSForest_cores'------#
	function_index_in_calls <- grep(sprintf(":%s$","setRSForest_cores"), names(calls_pm),perl=T )	# hard match
	stopifnot(length(function_index_in_calls) <= 1) # must only have one match or no match for a function within module.
	if (length(function_index_in_calls)) {
		eval.quoted(calls_pm[[function_index_in_calls]])	# need plyr.
		# # or use
		# eval(calls_pm[[function_index_in_calls]][[1]])	# due to named calls
	} else {	# if no configuration found, use this:
		setRSForest_cores(rf.cores_openMP = detectCores()-1, mc.cores_fork = detectCores()-1)
	}
	
	#-----------#
	originalN <- nrow(data)	# record the data raw size
	data <- na.omit( data[,c(response_variables,knowledge_driven_factors,data_driven_factors),with=F] )
										# get a perfect data in sense, otherwise stepAIC will report error due to different sample size each time when it deals with a variable.
										# alternative is to impute for those
										# variables having NAs.
	message("The final perfect sample size (records) before 'predictive model' module running is:",nrow(data))
	message("sample size (records) loss is:",originalN - nrow(data))
	stopifnot( length(response_variables) == 1 )	# limit for 1 response variable.
	invisible( na.fail(data) )	# check for data integrity.
	#----------#
	#----------#
	# need to remove those variables having only one level due to a few samples removal above.
	flag_uniform_info_variable <- data[,sapply(.SD,function(x)length(table(x))),.SDcols=colnames(data) ] == 1
	stopifnot( length(flag_uniform_info_variable) == ncol(data) )
	data <- data[,colnames(data)[!flag_uniform_info_variable],with=F]	# remove those factors only have one level.
	data_driven_factors <- data_driven_factors[!is.na( match(data_driven_factors, colnames(data) ) )]
	knowledge_driven_factors <- knowledge_driven_factors[!is.na( match(knowledge_driven_factors, colnames(data) ) )]
	# cat(data_driven_factors)	# for debugging.
	message("We have ", length(data_driven_factors), " data-driven variables left after QC and before 'predictive model'")
	message("We have ", length(knowledge_driven_factors), " knowledge-driven variables left after QC and before 'predictive model'")

	
	# result <- enet(x = data.matrix(data[,c(knowledge_driven_factors,data_driven_factors),with=F] ), y = data.matrix(data[,response_variables,with=F]),lambda=0.5,trace=T)	# 1 for lasso, 0 for ridge.
	#
	#---a subfunc to manipulate the covariate format---#
	# to correspond to continuous or categorical variables definition.	
	# assignCovariateFormat <- function(variableName, dataType=factors_annotation_table[variableName,dataType][,dataType]){
		# switch(dataType,
				# categorical = sprintf("as.factor(%s)",variableName),
				# continuous = variableName
		# )
	# }
	#---#
	# this variant version change the data itself in place.
	assignCovariateFormat_onData_version <- function(variableName){
		data_type = factors_annotation_table[variableName,dataType] %>% unlist	# involve the data.table and pipeR syntax.		
		switch(data_type,
				categorical = { 
					temp <- unlist(data[,variableName,with=F]);
					data[,variableName:=as.factor(temp),with=F]
				},
				continuous = NULL	# do nothing
		)
		invisible(NULL)
	}
	# set the newkey to use in subfunc "assignCovariateFormat"
	setkey(factors_annotation_table,colname)	
	sapply(knowledge_driven_factors,assignCovariateFormat_onData_version )	# no need to return.
	sapply(data_driven_factors,assignCovariateFormat_onData_version )
	sapply(response_variables,assignCovariateFormat_onData_version )
	#-------#
	## not useful here, so commented.
	# formula_knowledgeOnly <- as.formula(sprintf("~%s",paste(knowledge_driven_factors,collapse="+") ) )
	# formula_dataOnly <- as.formula(sprintf("~%s",paste(data_driven_factors,collapse="+") ) )
	# formula_unionAll <- as.formula(sprintf("~ %s + %s",paste(knowledge_driven_factors,collapse="+"), paste(data_driven_factors,collapse="+")  ) )
	#--------#
	formula_unionALLnY <- as.formula( sprintf("%s ~ %s + %s", response_variables ,paste(knowledge_driven_factors,collapse="+"), paste(data_driven_factors,collapse="+")  ) )
	
	#----match call for 'rfsrc'------#
	function_index_in_calls <- grep(sprintf(":%s$","rfsrc"), names(calls_pm),perl=T )	# hard match
	stopifnot(length(function_index_in_calls) <= 1) # must only have one match or no match for a function within module.
	# browser()
	if (length(function_index_in_calls)) {
		result <- eval.quoted(calls_pm[[function_index_in_calls]])[[1]]	# need plyr,return a list, extract 1st element.
	} else {	# if no configuration found, use this:
		result <- rfsrc(
		formula = formula_unionALLnY, 
		data = data, 
		ntree=1000, 	# the default value
		split.null = TRUE, # test null hypothesis y has no assocaition with x.
		na.action = "na.omit",
		nsplit = 5,	# a maximum of ‘nsplit’ split points are chosen randomly for each of the ‘mtry’ variables within a node
		importance = "none",	# save time, we will do it later using 'vimp' function.
		# xvar.wt = 	# optional the weight of each variable; a vector.
		proximity = FALSE,	# Should the proximity between observations be calculated? very large matrix nxn to store
		forest = TRUE,	# store the forest for future prediction of new obs.
		var.used = "all.trees",
		split.depth = "all.trees",
		seed = -(.seed), # Negative integer specifying seed for the random number generator
		do.trace = TRUE	# trace info,
		# ntime = ,	# Integer value which for survival families constrains ensemble calculations to a grid of time values of no more than ntime time points.
		)

	}

	################################################################
	#-----plot in batch------#
	pdf(sprintf("%s_rfsrcObjplot.pdf",plotNameEssence) )
	par(ps=12,cex=1)
	plot.rfsrc(result)
	dev.off()
	#
	# #------plot some individuals---------#
	# pdf(sprintf("%s_rfsrcSurvObjplot_individualPlot.pdf",plotNameEssence) )
	# par(ps=12,cex=1)
	# # plot.rfsrc(result)
	# plot.survival(result, subset = c(3,6,9,12,66),haz.model="spline")
	# plot.survival(result, subset = c(3,6,9,12,66),haz.model="ggamma")
	# # plot.variable(result)
	# dev.off()
	
	#------plot marginal Partial Effect of variables---------# TOO SLOW, can SKIP
	if (add_varMarginalEffectPartialPlot) {
		pdf(sprintf("%s_rfsrcObjplot_varMarginalEffectPartialPlot.pdf",plotNameEssence) )
		par(ps=12,cex=1)
		plot.variable(result,partial=T, npts=5)
		dev.off()
	}
	
	#------plot various marginal Effect of variables differing in response_variables---------#
	pdf(sprintf("%s_rfsrcObjplot_varMarginalEffectPlot.pdf",plotNameEssence) )
	par(ps=12,cex=1)
	plot.variable(result)
	dev.off()
	#
	# 
	#----------------------------------------------------------#
	############################################################
	
	###########################################################
	#--------------------var.select---------------------------#
	#----match call for 'rfsrc'------#
	function_index_in_calls <- grep(sprintf(":%s$","var.select"), names(calls_pm),perl=T )	# hard match
	stopifnot(length(function_index_in_calls) <= 1) # must only have one match or no match for a function within module.
	if (length(function_index_in_calls)) {
		result_var_select <- eval.quoted(calls_pm[[function_index_in_calls]])[[1]]	# need plyr.
	} else {	# if no configuration found, use this:
		result_var_select <- var.select(
					object = result, 
					method = "md", 
					conservative = "medium",
					nsplit = 5, # same as above 'rfsrc' function.
					na.action = "na.omit",
					always.use = knowledge_driven_factors,	# fix these knowledge_driven_factors
					do.trace = TRUE,	# trace info,
					verbose = TRUE,	# verbose info,
		)
	}
	# result_var_select$topvars will be printed automatically by function.
	#---------------------get the VIMP-----------------------------#
	# variable importance.
	function_index_in_calls <- grep(sprintf(":%s$","vimp"), names(calls_pm),perl=T )	# hard match
	stopifnot(length(function_index_in_calls) <= 1) # must only have one match or no match for a function within module.
	if (length(function_index_in_calls)) {
		result_vimp <- eval.quoted(calls_pm[[function_index_in_calls]])[[1]]	# need plyr.
	} else {	# if no configuration found, use this:
		result_vimp <- vimp(
					object = result, 
					importance = "permute",
					xvar.names = result_var_select$topvars,	# need this one to use only a subset of features, otherwise too slow to bear. STILL memory/time consuming, e.g. 60G mem
					joint = FALSE,
					seed = -(.seed),
					do.trace = TRUE,				
		)
	}
	print(result_vimp)
	#------plot vimp object and side-effect print---------#
	pdf(sprintf("%s_rfsrcObjplot_vimp.pdf",plotNameEssence) )
	par(ps=12,cex=1)
	plot.rfsrc(result_vimp)
	dev.off()
	
	return( structure(list(result = result, coefs_selected = result_var_select$topvars, result_var_select = result_var_select, result_vimp = result_vimp ),class= c("rfsrcObj","list")) )
	# result and coefs_selected are necessary
}	


# Browse[2]> result
                         # Sample size: 63093
                    # Number of deaths: 2774
                     # Number of trees: 1000
          # Minimum terminal node size: 3
       # Average no. of terminal nodes: 4106.309
# No. of variables tried at each split: 22
              # Total no. of variables: 455
                            # Analysis: RSF
                              # Family: surv
                      # Splitting rule: logrank *random*
       # Number of random split points: 5
                          # Error rate: 51.13%

						  
						  
#======================================================================#
# some evidence about model specification regarding factors.
# data(pbc, package = "randomForestSRC")
# pbc$random_category <- sample(10,nrow(pbc),replace=T)
# # pbc$random_category  <- as.factor(pbc$random_category )
# sapply(pbc,class)
# pbc$stage <- as.factor(pbc$stage)
# pbc.obj <- rfsrc(Surv(days, status) ~ ., pbc, nsplit = 10)
# # pbc.obj <- rfsrc(Surv(days, status) ~ days + status + age + treatment + sex + ascites + hepatom + spiders + edema + bili + as.factor(random_category), pbc, nsplit = 10)

# pdf()
# plot.variable(pbc.obj)
# dev.off()		
# > pdf("marginalEffect.pdf")
# > plot.variable(pbc.obj)
# Warning message:
# In bxp(list(stats = c(0.616921280550937, 5.57784999729378, 19.3937990099956,  :
  # some notches went outside hinges ('box'): maybe set notch=FALSE
# > dev.off()
# plot.variable(pbc.obj,partial=T,smooth=T)
# dev.off()
# > dev.off()
				  