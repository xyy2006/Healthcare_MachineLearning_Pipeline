#!/bin/bash
# Content: A function can run linear model with stepwise variable selection method on provided arguments, which at least should include data, knowledge-driven predictors and data-driven predictors.
# Author: Yang Yang
# Date:"Tue Jul  1 08:57:26 2014"
# Usage: To be loaded by main program:ModelComputation_workbench_setup.r
#######################################################################################
#--------------------#
# load some prerequisite libraries
suppressPackageStartupMessages( require(MASS) )	# for stepAIC

#-----------------#
linearModelStepwise <- function(data = data_input,response_variables = response_variables_vec, knowledge_driven_factors = knowledge_driven_factors_vec, data_driven_factors = data_driven_factors_vec, factors_annotation_table = factors_all,plotNameEssence = sprintf("linearModelStepwise_plot_at_%s",nowTimeToString() ) ){
	originalN <- nrow(data)	# record the data raw size
	data <- na.omit( data[,c(response_variables,knowledge_driven_factors,data_driven_factors),with=F] )
										# get a perfect data in sense, otherwise stepAIC will report error due to different sample size each time when it deals with a variable.
										# alternative is to impute for those
										# variables having NAs.
	message("The final perfect sample size (records) before 'predictive model' module running is:",nrow(data))
	message("sample size (records) loss is:",originalN - nrow(data))
	stopifnot( length(response_variables) == 1 )	# limit for linearModel, only one trait allowed.
	invisible( na.fail(data) )	# check for data integrity.
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
	formula_knowledgeOnly <- as.formula(sprintf("%s ~ %s", response_variables,paste(knowledge_driven_factors,collapse="+") ) )
	formula_unionAll <- as.formula(sprintf("%s ~ %s + %s", response_variables,paste(knowledge_driven_factors,collapse="+"), paste(data_driven_factors,collapse="+")  ) )
	#-------#
	# prefit <- glm(formula = formula_unionAll, family = gaussian, data = data,na.action=na.omit)
	prefit <- glm(formula = formula_knowledgeOnly, family = gaussian, data = data,na.action=na.omit)
	#	
	result <- stepAIC(prefit, scope = list(upper = formula_unionAll,lower=formula_knowledgeOnly) , trace = T )
	#---do the residual vs. fitted plot---#
	pdf(sprintf("%s_lm_residualVsFitted.pdf",plotNameEssence) )
	par(ps=12,cex=1)
	plot(result)	# call the plot.lm* which is invisible.
	dev.off()

	return(structure( list(stepAIC_result = result, stepAIC_anova_table = result$anova, stepAIC_final_coefs = result$coefficients),class=c("glm","lm") ) )	
}
# 205hours