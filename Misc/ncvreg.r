#!/bin/bash
# Content: A function can run non-convex regularized regression (i.e., Regularization paths for SCAD- and MCP-penalized regression models) on provided arguments, which at least should include data, knowledge-driven predictors and data-driven predictors.
# Note: penaly = penalty=c("MCP", "SCAD", "lasso"), choose one, recommed first two, since lasso is executed by another function elsewhere.
# Author: Yang Yang
# Date:"Tue Jul  1 08:57:26 2014"
# Usage: To be loaded by main program:ModelComputation_workbench_setup.r
#######################################################################################--------------------#
# load some prerequisite libraries
require(ncvreg)	# the essence
#-----#

# potential cmd args:pvalue_cutoff
ncvreg <- function(data = data_input,response_variables = response_variables_vec, knowledge_driven_factors = knowledge_driven_factors_vec, data_driven_factors = data_driven_factors_vec, factors_annotation_table = factors_all,.seed=123,plotNameEssence = sprintf("Ncvreg_plot_at_%s",nowTimeToString()) ){
	# browser()
	recover()
	# response_variables <- "LOS.x"	# for debugging purpose.
	originalN <- nrow(data)	# record the data raw size	
	data <- na.omit( data[,c(response_variables,knowledge_driven_factors,data_driven_factors),with=F] )
										# get a perfect data in sense, otherwise stepAIC will report error due to different sample size each time when it deals with a variable.
										# alternative is to impute for those
										# variables having NAs.
	message("The final perfect sample size (records) before 'predictive model' module running is:",nrow(data))
	message("sample size (records) loss is:",originalN - nrow(data))
	stopifnot( length(response_variables) == 1 )	# limit for regularized regression model.
	invisible( na.fail(data) )	# check for data integrity.
	#----------#
	# need to remove those variables having only one level due to a few samples removal above.
	flag_uniform_info_variable <- data[,sapply(.SD,function(x)length(table(x))),.SDcols=colnames(data) ] == 1
	stopifnot( length(flag_uniform_info_variable) == ncol(data) )
	data <- data[,!flag_uniform_info_variable,with=F]	# remove those factors only have one level.
	data_driven_factors <- data_driven_factors[!is.na( match(data_driven_factors, colnames(data) ) )]
	message("We have ", length(data_driven_factors), " data-driven variables left after 'pre-feature selection' module and QC before 'predictive model'")
	
	# result <- enet(x = data.matrix(data[,c(knowledge_driven_factors,data_driven_factors),with=F] ), y = data.matrix(data[,response_variables,with=F]),lambda=0.5,trace=T)	# 1 for lasso, 0 for ridge.
	#
	#---a subfunc to manipulate the covariate format---#
	# to correspond to continuous or categorical variables definition.	
	assignCovariateFormat <- function(variableName, dataType=factors_annotation_table[variableName,dataType][,dataType]){
		switch(dataType,
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
	new_X_modelExpanded_overall <- cbind(new_X_modelExpanded_knowledgeDriven,new_X_modelExpanded_dataDriven)
	#
	message("After model formula (design matrix) expanded accouting for categorical variables, we have ",ncol(new_X_modelExpanded_overall)," features columnwisely.")
	message("ncol increased by ",ncol(new_X_modelExpanded_overall) - length(c(knowledge_driven_factors,data_driven_factors)))
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
	# result <- glmnet(x = data.matrix(data[,c(knowledge_driven_factors,data_driven_factors),with=F] ), y = data.matrix(data[,response_variables,with=F]), family="gaussian",alpha=0.5,penalty.factor = keepKnowledgeFactor_seq)	# 1 for lasso, 0 for ridge.
	# result <- cv.glmnet(x = new_X_modelExpanded_overall, y = unlist(data[,response_variables,with=F]), family="gaussian",alpha=0.5, lambda=seq(12,3,by=-0.05),penalty.factor = keepKnowledgeFactor_seq, nfolds=10, keep=T, parallel=T)	# 1 for lasso, 0 for ridge.
	# result <- cv.glmnet(x = new_X_modelExpanded_overall, y = unlist(data[,response_variables,with=F]), family="gaussian",alpha=0.5, lambda=seq(12,3,by=-0.05), nfolds=10, keep=T, parallel=T)	# 1 for lasso, 0 for ridge.
	# result <- cv.glmnet(x = new_X_modelExpanded_overall, y = unlist(data[,response_variables,with=F]), family="gaussian",alpha=0.5, lambda=seq(12,3,by=-0.05), nfolds=10, keep=T, parallel=T,standardize = F)	# 1 for lasso, 0 for ridge.
	# result <- cv.glmnet(x = new_X_modelExpanded_overall, y = unlist(data[,response_variables,with=F]), family="gaussian",alpha=0.5, lambda=seq(12,0,by=-0.05), nfolds=10, keep=T, parallel=T,standardize = F)	# 1 for lasso, 0 for ridge.
	# result <- cv.glmnet(x = new_X_modelExpanded_overall, y = unlist(data[,response_variables,with=F]), family="gaussian",alpha=0.5, lambda=seq(8,0,by=-0.1), nfolds=10, keep=T, penalty.factor = keepKnowledgeFactor_seq,parallel=T,standardize = F)	# 1 for lasso, 0 for ridge.
	recover()
	result <- cv.ncvreg(X = new_X_modelExpanded_overall, y = unlist(data[,response_variables,with=F]), family="gaussian",  penalty = "MCP", alpha=1, nfolds=10, penalty.factor = keepKnowledgeFactor_seq,seed = .seed, trace = T)				# no parallel scheme in this ncvreg package. need manually setup if you want to use.
					# for standardize the X part.
	
	coefs_lambda.min <- coef(result,s="lambda.min")
	#
	pdf(sprintf("%s_CVplot.pdf",plotNameEssence) )
	par(ps=12,cex=1)
	plot(result)
	dev.off()
	# result <- cv.glmnet(x = new_X_modelExpanded_overall, y = unlist(data[,response_variables,with=F]), family="gaussian",alpha=0.5, lambda=seq(12,1,by=-0.05),penalty.factor = keepKnowledgeFactor_seq, nfolds=10, keep=T, parallel=T)	# 1 for lasso, 0 for ridge.
	pdf(sprintf("%s_ncvregObjplot.pdf",plotNameEssence) )
	par(ps=12,cex=1)
	plot(result$fit)
	abline(v=result$lambda.min)
	dev.off()

	return( structure(list(result = result, coefs_selected = coefs_lambda.min ),class= "RegularizedLinearObj") )
	# result and coefs_selected are necessary
}
