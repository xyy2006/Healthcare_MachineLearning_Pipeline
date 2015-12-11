#!/bin/bash
# Content: A function can run Normal + Probit Bivariate method on provided arguments, which at least should include data, knowledge-driven predictors and data-driven predictors.
# Note: this method jointly model the timeResponseVar and eventResponseVar.
# Author: Yang Yang
# Date:"Mon Aug  4 14:17:45 2014"
# Usage: To be loaded by main program:ModelComputation_workbench_setup.r
#######################################################################################
#--------------------#
# load some prerequisite libraries
require(MASS)	# for stepAIC
require(geepack)
#-----------------#
normalProbitBivariate <- function(data = data_input,response_variables = response_variables_vec, knowledge_driven_factors = knowledge_driven_factors_vec, data_driven_factors = data_driven_factors_vec, factors_annotation_table = factors_all, timeResponseVar = "LOS.x", eventResponseVar = "died",.seed=123,plotNameEssence = sprintf("RSForest_plot_at_%s",nowTimeToString()) ){
	# browser()
	originalN <- nrow(data)	# record the data raw size
	#
	data <- na.omit( data[,c(response_variables,knowledge_driven_factors,data_driven_factors),with=F] )
										# get a perfect data in sense, otherwise stepAIC will report error due to different sample size each time when it deals with a variable.
										# alternative is to impute for those
										# variables having NAs.
	message("The final perfect sample size (records) before 'predictive model' module running is:",nrow(data))
	message("sample size (records) loss is:",originalN - nrow(data))
	#
	# browser()
	# stopifnot( length(response_variables) == 1 )	# limit for linearModel, only one trait allowed.
	invisible( na.fail(data) )	# check for data integrity.
	#----------#
	# need to remove those variables having only one level due to a few samples removal above.
	flag_uniform_info_variable <- data[,sapply(.SD,function(x)length(table(x))),.SDcols=colnames(data) ] == 1
	stopifnot( length(flag_uniform_info_variable) == ncol(data) )
	data <- data[,!flag_uniform_info_variable,with=F]	# remove those factors only have one level.
	data_driven_factors <- data_driven_factors[!is.na( match(data_driven_factors, colnames(data) ) )]
	message("We have ", length(data_driven_factors), " data-driven variables left after 'pre-feature selection' module and QC before 'predictive model'")
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
	formula_knowledgeOnly <- as.formula(sprintf("%s~%s",timeResponseVar,paste(knowledge_driven_factors,collapse="+") ) )
	formula_unionAll <- as.formula(sprintf("%s ~ %s + %s",timeResponseVar,paste(knowledge_driven_factors,collapse="+"), paste(data_driven_factors,collapse="+")  ) )
	#-------#
	# browser()
	#================================================================#
	# prefit <- glm(formula = formula_unionAll, family = gaussian, data = data,na.action=na.omit)
	# model_on_time_continuous <- glm(formula = formula_unionAll, family = gaussian, data = data,na.action=na.omit)
	# 1st, the lm to model time as a continuous variable.
	model_on_time_continuous <- lm(formula = formula_unionAll, data = data,na.action=na.omit)	# lm is sufficient enough.
	# model_on_time_continuous <- lm(formula = formula_knowledgeOnly, data = data,na.action=na.omit)	# lm is sufficient enough.	# for debugging
	#--------------------------------------------------------------------#
	formula_unionAll_on_event_binary <- as.formula(sprintf("%s ~ %s + %s + %s",eventResponseVar,paste(knowledge_driven_factors,collapse="+"), paste(data_driven_factors,collapse="+"), timeResponseVar  ) )
	# formula_unionAll_on_event_binary <- as.formula(sprintf("%s ~ %s + %s",eventResponseVar,paste(knowledge_driven_factors,collapse="+"),  timeResponseVar  ) )	# for debugging
	model_on_event_binary <- glm(formula_unionAll_on_event_binary, family = binomial(link="probit"), data = data, na.action = na.omit)
	# browser()

	#---------------------------------------------------------------------#
	summy_model_on_time_continuous <- summary(model_on_time_continuous)
	summy_model_on_event_binary <- summary(model_on_event_binary)
	#---getting the beta coeffs from model_on_time_continuous and model_on_event_binary---#
	alpha_0 <- coef(model_on_time_continuous)['(Intercept)']
	alpha_1 <- coef(model_on_time_continuous)[-grep("Intercept",names(model_on_time_continuous$coef),perl=T)] %>>% na.omit
	lambda_0 <- coef(model_on_event_binary)['(Intercept)']
	lambda_Y <- coef(model_on_event_binary)[timeResponseVar]
	if( is.na(lambda_Y) ) stop("parameter estimate for timeResponseVar cannot be estimated due to reasons like sigularity, etc.")
	lambda_X <- coef(model_on_event_binary)[ -match(c('(Intercept)',timeResponseVar),names(model_on_event_binary$coef)) ] %>>% na.omit
	sigma <- summy_model_on_time_continuous$sigma
	sigmaSQ <- sigma^2	# the value
	N_coefs_noIntercept <- length(alpha_1)
	stopifnot( N_coefs_noIntercept == length(lambda_X) )
	#---then---#
	# we can have beta
	beta_0 <- lambda_0 + lambda_Y * alpha_0
	beta_1 <- lambda_X + lambda_Y * alpha_1
	rho <- unname( sqrt( lambda_Y^2 * sigma^2 / ( 1 + lambda_Y^2 * sigma^2) ) )	
	names(rho) <- "rho"
	#---getting each cov---#	
	cov_on_time_continuous <- summy_model_on_time_continuous$cov.unscaled
	cov_on_event_binary <- summy_model_on_event_binary$cov.unscaled
	N = nrow(model_on_time_continuous$model)	# the exact model.frame used in lm.
	cov_of_sigmaSQ <- 2 * sigma^4 / N
	#---merging the covariance matrix from each model and the var of sigma^2.---#
	###
	##
	# THE ORDER IS IMPORTANT!!!
	# the diagnol order in the big merged matrix should be cov_on_time_continuous, cov_of_sigmaSQ and cov_on_event_binary.
	##
	###
	length_temp <- nrow(cov_on_time_continuous) + 1 + nrow(cov_on_event_binary)
	cov_big_merged <- matrix(0,nrow=length_temp,ncol=length_temp)
	indexToFill <- nrow(cov_on_time_continuous)
	#
	cov_big_merged[1:indexToFill,1:indexToFill] <- cov_on_time_continuous
	indexToFill <- indexToFill + 1	# the room for cov_of_sigmaSQ
	#
	cov_big_merged[indexToFill,indexToFill] <- cov_of_sigmaSQ
	#
	cov_big_merged[ (indexToFill + 1) : (indexToFill + nrow(cov_on_event_binary) ),(indexToFill + 1) : (indexToFill + nrow(cov_on_event_binary) ) ] <- cov_on_event_binary
	#=======get derivatives of (14.7) through (14.9) in book Longitudinal Data Analysis edited by Garrett Fitzmaurice, Marie Davidian, Geert Verbeke, Geert Molenberghs======#
	# h_1 <- 1/(2 * rho)  *  lambda_Y^2 / ( 1 + lambda_Y^2 * sigma^2)^2
	# h_2 <- 1/(2 * rho)  *  (2 * lambda_Y * sigma^2) / ( 1 + lambda_Y^2 * sigma^2)^2
	# # delta_betaVec <- cbind( c(lambda_Y, lambda_Y, h_1)*diag(3),  diag(3) + matrix(c(0,0,alpha_0,0,0,alpha_1,0,0,h_2 - 1),nrow=3,ncol=3,byrow=T)
	#---first need to get each derivatives---#
	deriv_denominators <- c("alpha_0",sprintf("alpha_1_%d",seq(N_coefs_noIntercept) ),"sigmaSQ","lambda_0",sprintf("lambda_X_%d",seq(N_coefs_noIntercept) ), "lambda_Y")	# the symbols in denominators of derivatives.
	# deriv_denominators_env <- new.env()
	# mapply(assign,x=deriv_denominators,value = c(alpha_0,alpha_1,sigmaSQ,lambda_0,lambda_X,lambda_Y),MoreArgs = list(envir = deriv_denominators_env) )	# multiple assign
	value_list <- as.list(c(alpha_0,alpha_1,sigmaSQ,lambda_0,lambda_X,lambda_Y))
	names(value_list) <- deriv_denominators
	deriv_denominators_env <- list2env(value_list)	# another method of multiple assign
	#---#
	# building expr.
	beta_0_expr <- expression( lambda_0 + lambda_Y * alpha_0)	# scalar expression
	beta_1_expr <- sprintf( "lambda_X_%d + lambda_Y * alpha_1_%d",seq(N_coefs_noIntercept),seq(N_coefs_noIntercept)) %:>% parse(text=.)	# a vector of expressions
	rho_expr <- expression( sqrt( lambda_Y^2 * sigmaSQ / ( 1 + lambda_Y^2 * sigmaSQ) ) )	# sigmaSQ represents sigma^2 in values, but here it is a symbol needed in expression. # a scalar expression
	#---#
	# deriv the exprs.
	dx2x_beta_0 <- deriv(beta_0_expr, deriv_denominators)
	dx2x_beta_1 <- sapply(beta_1_expr, deriv, deriv_denominators)	# a vector output
	dx2x_rho <- deriv(rho_expr, deriv_denominators)
	#---#
	# eval the derivs.
	dx2x_beta_0_gradient <- eval(dx2x_beta_0,env=deriv_denominators_env)
	dx2x_beta_0_gradient <- attr(dx2x_beta_0_gradient, "gradient")
	dx2x_beta_1_gradient <- Vectorize(eval,"expr",SIMPLIFY=F)(dx2x_beta_1,env=deriv_denominators_env)	# SIMPLIFY=T will give only the .value.
	# dx2x_beta_1_gradient <- lapply(dx2x_beta_1,eval,env=deriv_denominators_env)	# also works.
	dx2x_beta_1_gradient <- lapply(dx2x_beta_1_gradient,function(x )attr(x, "gradient")) %:>% do.call(rbind,.)
	dx2x_rho_gradient <- eval(dx2x_rho)
	dx2x_rho_gradient <- attr(dx2x_rho_gradient, "gradient")
	# # ---rbind them to constitute the whole derivatives.---#
	# ###
	derivatives_betaVec_binded <- rbind(dx2x_beta_0_gradient, dx2x_beta_1_gradient,dx2x_rho_gradient)
	rownames( derivatives_betaVec_binded ) <- c("beta_0",sprintf("beta_1_%s",names(beta_1)),"rho")
	#============now we can calculate the delta method variance of beta Vector===#
	sigma_over_n <- (derivatives_betaVec_binded) %*% cov_big_merged %*% t(derivatives_betaVec_binded)	# cov_big_merged is already the sigma_over_n for B, which is the original parameter estimate vector.
	var_of_beta_vec <- diag(sigma_over_n)
	statistics <- c(beta_0,beta_1,rho) / sqrt(var_of_beta_vec)	# can deemed as a normal distrubition
	pvalues <- pnorm(abs(statistics),lower=F)*2
	# pnorm(1.96,lower=F)*2
	# [1] 0.04999579	
	result <- data.frame(Estimate = c(beta_0,beta_1,rho), `Std. Error` = sqrt(var_of_beta_vec), `z value` = statistics, `Pr(>|z|)` = pvalues, check.names=F,stringsAsFactors=F )	# this is the coef table for beta_vec
	result_timeResponseVar <- coef(summy_model_on_time_continuous)
	result_eventResponseVar <- coef(summy_model_on_event_binary)
	# browser()
	return( structure(list(result =  result, result_timeResponseVar = result_timeResponseVar, result_eventResponseVar= result_eventResponseVar), class=c("normalProbitBivariateObj","list") ) )	
}
# Browse[1]> coef(summy_model_on_time_continuous)
                       # Estimate  Std. Error     t value     Pr(>|t|)
# (Intercept)         6.603788921 0.677700496  9.74440621 2.021492e-22
# AGE                -0.002639892 0.008648593 -0.30523950 7.601848e-01
# as.factor(FEMALE)1 -0.066119448 0.226363587 -0.29209401 7.702157e-01
# as.factor(RACE)2    0.003432088 0.300382559  0.01142572 9.908838e-01
# as.factor(RACE)3   -0.025298001 0.384940350 -0.06571927 9.476015e-01
# as.factor(RACE)4    0.537980358 0.879612772  0.61161044 5.407977e-01
# as.factor(RACE)5    0.477139981 2.361035939  0.20208925 8.398476e-01
# as.factor(RACE)6    0.094219830 0.513645787  0.18343347 8.544585e-01
# Browse[1]> result
                      # Estimate   Std..Error    z.value      Pr...z..
# (Intercept)        -3.06743238 0.0687322397 -44.628727  0.000000e+00
# AGE                 0.01826752 0.0008494239  21.505777 1.374648e-102
# as.factor(FEMALE)1 -0.07648559 0.0187770816  -4.073348  4.634211e-05
# as.factor(RACE)2   -0.19172298 0.0282976543  -6.775225  1.242125e-11
# as.factor(RACE)3   -0.13906991 0.0348900924  -3.985943  6.721274e-05
# as.factor(RACE)4   -0.10128875 0.0778038397  -1.301848  1.929685e-01
# as.factor(RACE)5   -0.35105067 0.2708527968  -1.296094  1.949431e-01
# as.factor(RACE)6   -0.13762162 0.0476779914  -2.886481  3.895763e-03
                    # 0.51746258 0.0156771690  33.007399 6.361023e-239
