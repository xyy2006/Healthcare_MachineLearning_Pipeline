#!/bin/bash
# Content: A function can run random survival forest with simultaneous selection method on provided arguments, which at least should include data, knowledge-driven predictors and data-driven predictors.
# Note: now only deal with right censored, no competing risk survival data.
#		The data should have response_variables including time and event.
# Author: Yang Yang
# Date:"Tue Jul  1 08:57:26 2014"
# Usage: To be loaded by main program:ModelComputation_workbench_setup.r 
#######################################################################################
#--------------------#
# load some prerequisite libraries
suppressPackageStartupMessages( require(glmnet)	)# the essence
suppressPackageStartupMessages( require(survival)	)# the essence
#-----#


#--------------------#
Survival_glmnet <- function(data = data_input,response_variables = response_variables_vec, knowledge_driven_factors = knowledge_driven_factors_vec, data_driven_factors = data_driven_factors_vec, factors_annotation_table = factors_all, timeResponseVar = "LOS.x", eventResponseVar = "died",.seed=123,plotNameEssence = sprintf("Survival_glmnet_plot_at_%s",nowTimeToString()) ){
	# browser()
	# response_variables <- response_variables_vec <- c("LOS.x","died")	# debugging usage
	# data = data[1:1000]	# reduced set, debugging usage.
	originalN <- nrow(data)	# record the data raw size
	#
	data <- na.omit( data[,c(response_variables,knowledge_driven_factors,data_driven_factors),with=F] )
										# get a perfect data in sense, otherwise stepAIC will report error due to different sample size each time when it deals with a variable.
										# alternative is to impute for those
										# variables having NAs.
	data <- data[data[[timeResponseVar]] > 0,]	# cleaning time to include only those >0. Under debate, whether to include 0 or not. Though here, include 0 will not work for package glmnet with cox. So only limit T > 0 to be included. 
	#
	message("The final perfect sample size (records) before 'predictive model' module running is:",nrow(data))
	message("sample size (records) loss is:",originalN - nrow(data))
	stopifnot( length(response_variables) == 2 )	# limit for glmnetCox, only two trait allowed.
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
	assignCovariateFormat <- function(variableName ){
		data_type = factors_annotation_table[variableName,dataType] %>% unlist	# involve the data.table and pipeR syntax.
		switch(data_type,
				categorical = sprintf("as.factor(%s)",variableName),
				continuous = variableName
		)
	}
	# set the newkey to use in subfunc "assignCovariateFormat"
	setkey(factors_annotation_table,colname)
	knowledge_driven_factors <- sapply(knowledge_driven_factors,assignCovariateFormat )
	data_driven_factors <- sapply(data_driven_factors,assignCovariateFormat )
	#-------#

	formula_knowledgeOnly <- as.formula(sprintf("~%s",paste(knowledge_driven_factors,collapse="+") ) )
	formula_dataOnly <- as.formula(sprintf("~%s",paste(data_driven_factors,collapse="+") ) )
	formula_unionAll <- as.formula(sprintf("~ %s + %s",paste(knowledge_driven_factors,collapse="+"), paste(data_driven_factors,collapse="+")  ) )
	#--------#
	# need expand the design matrix to fit the glmnet or equivalent input format.
	new_X_modelExpanded_knowledgeDriven <- model.matrix(formula_knowledgeOnly, data)[,-1]	# exclude intercept column. 
	new_X_modelExpanded_dataDriven <- model.matrix(formula_dataOnly, data)[,-1] 
	new_X_modelExpanded_overall <- cbind(new_X_modelExpanded_knowledgeDriven,new_X_modelExpanded_dataDriven)	# now the format is suitable for glmnet package, glmnet(x=,y=)
	#
	# browser()
	message("After model formula (design matrix) expanded accouting for categorical variables, we have ",ncol(new_X_modelExpanded_overall)," features columnwisely.")
	message("ncol increased by ",ncol(new_X_modelExpanded_overall) - length(c(knowledge_driven_factors,data_driven_factors)), " from original.")
	# Browse[2]> colnames(new_X_modelExpanded) %>>% head
	# [1] "AGE"                "as.factor(FEMALE)1" "as.factor(RACE)2"
	# [4] "as.factor(RACE)3"   "as.factor(RACE)4"   "as.factor(RACE)5"
	# Browse[2]> knowledge_driven_factors
					# AGE              FEMALE                RACE
				  # "AGE" "as.factor(FEMALE)"   "as.factor(RACE)"
	# knowVar_name <- foreach (knowVar = knowledge_driven_factors,.combine=c) %do% {
		# grep(sprintf("%s",knowVar),colnames(new_X_modelExpanded),fixed=T,value=T)
	# }	# obsolete
	#---#
	# manipulate the keepKnowledgeFactor_seq as argument to glmnet or equivalent functions.
	keepKnowledgeFactor_seq <- c( rep(0,length.out = ncol(new_X_modelExpanded_knowledgeDriven) ), rep(1, length.out = ncol(new_X_modelExpanded_dataDriven) ) )
	stopifnot( length(keepKnowledgeFactor_seq) == ncol(new_X_modelExpanded_overall) )
	#
	#--------------------------------#
	# start computation.
	set.seed(.seed)	# because of CV.
	#
	#---prepare the Surv obj---#
	Surv_obj = as.matrix(data[,c(timeResponseVar,eventResponseVar),with=F])
	colnames(Surv_obj) <- c("time","status")	# fixed names, The latter is a binary variable, with ’1’ indicating death, and ’0’ indicating right censored.
	#
	#----match call for 'cv.glmnet'------#
	function_index_in_calls <- grep(sprintf(":%s$","cv.glmnet"), names(calls_pm),perl=T )	# hard match
	stopifnot(length(function_index_in_calls) <= 1) # must only have one match or no match for a function within module.
	# browser()
	if (length(function_index_in_calls)) {
		result <- eval.quoted(calls_pm[[function_index_in_calls]])[[1]]	# need plyr,return a list, extract 1st element.
	} else {	# if no configuration found, use this:
		result <- cv.glmnet(
							x = new_X_modelExpanded_overall, 
							y = Surv_obj, 
							family="cox",
							alpha=0.5, 
							lambda = NULL, 
							nfolds=10, 
							# keep=T,	# not used for 'cox' 
							penalty.factor = keepKnowledgeFactor_seq,
							parallel=T,
							standardize = F)	# 1 for lasso, 0 for ridge.
												# since we have many x in 0/1 value, we don't want to standardize. Usually it need standardize.
	}
	
	coefs_lambda.min <- coef(result,s="lambda.min")
	pdf(sprintf("%s_glmnetCVplot.pdf",plotNameEssence) )
	par(ps=12,cex=1)
	plot(result)
	dev.off()
	# result <- cv.glmnet(x = new_X_modelExpanded_overall, y = unlist(data[,response_variables,with=F]), family="gaussian",alpha=0.5, lambda=seq(12,1,by=-0.05),penalty.factor = keepKnowledgeFactor_seq, nfolds=10, keep=T, parallel=T)	# 1 for lasso, 0 for ridge.
	pdf(sprintf("%s_glmnetObjplot.pdf",plotNameEssence) )
	par(ps=12,cex=1)
	plot(result$glmnet.fit, xvar="lambda",label=F)
	abline(v=log(result$lambda.min))
	dev.off()

	return( structure(list(result = result, coefs_selected = coefs_lambda.min, result_fitted = result$glmnet.fit ),class= c("RegularizedLinearObj","list")) )
	# result and coefs_selected are necessary

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
				  
