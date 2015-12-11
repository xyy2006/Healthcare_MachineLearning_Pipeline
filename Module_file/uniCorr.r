#!/bin/bash
# Content: A function can run uniCorr feature selection on provided arguments, which at least should include data, knowledge-driven predictors and data-driven predictors.
# Author: Yang Yang
# Date:"Tue Jul  1 08:57:26 2014"
# Usage: To be loaded by main program:ModelComputation_workbench_setup.r
#######################################################################################
# potential cmd args:pvalue_cutoff
uniCorr <- function(data = data_input,response_variables = response_variables_vec, knowledge_driven_factors = knowledge_driven_factors_vec, data_driven_factors = data_driven_factors_vec, pvalue_cutoff = 0.05){
	# browser()
	res <- foreach(factor = data_driven_factors,.errorhandling="pass",.combine=c) %dopar% {
		res <- suppressWarnings(cor.test(data[[factor]],data[[response_variables]],alternative = "two.sided",method="spearman",exact=T, continuity=T,na.action=na.omit) )
		res$p.value
	}
	names(res) <- data_driven_factors
	res_pass_cutoff <-  res[res <= pvalue_cutoff]
	res_pass_cutoff <- sort(res_pass_cutoff)
	return(list(features = names(res_pass_cutoff), rank_list = res_pass_cutoff  ))
}
