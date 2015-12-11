#!/bin/bash
# Content: A function can run random survival forest with simultaneous selection method on provided arguments, which at least should include data, knowledge-driven predictors and data-driven predictors.
# Note: now only deal with right censored, no competing risk survival data.
#		The data should have response_variables including time and event.
# Author: Yang Yang
# Date:"Tue Jul  1 08:57:26 2014"
# Usage: To be loaded by main program:ModelComputation_workbench_setup.r 
#######################################################################################
###
##
# for debugging
setkeyv(NY_SID_2009_HFdata_refined,"died")
data_input <- NY_SID_2009_HFdata_refined
data_input_testing <- dplyr::sample_frac(data_input,size=0.3)
data_input <- dplyr::setdiff(data_input, data_input_testing)	# get the left part as new version of data_input.

load("demo_output/RSForest_predictiveResult_by_RSForest.Rdata")
#
##
###

#---------------------------#
# we dont need to require specific library again, as it was done before.
# they are in memory.

#===========================#
# test module for rfsrcObj.
test.rfsrcObj <- function(data = data_input_testing, pR = predictiveResult,response_variables = response_variables_vec, knowledge_driven_factors = knowledge_driven_factors_vec, data_driven_factors = data_driven_factors_vec, factors_annotation_table = factors_all, timeResponseVar = "LOS.x", eventResponseVar = "died",.seed=123,plotNameEssence = sprintf("RSForest_test_plot_at_%s",nowTimeToString()) )
{	# for RSForest.r generated output
	browser()
	# this variant version change the data itself in place.
	assignCovariateFormat_onData_version <- function(variableName, dataType=factors_annotation_table[variableName,dataType][,dataType]){
		switch(dataType,
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
	
	testResult <- predict(pR$result, newdata = data_input, importance = "permute", na.action = "na.omit", seed = .seed, do.trace = TRUE, outcome = "train")	 	
	
}

