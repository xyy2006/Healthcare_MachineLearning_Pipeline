#!/bin/bash
# Content: A function can run random survival forest with simultaneous selection method on provided arguments, which at least should include data, knowledge-driven predictors and data-driven predictors.
# Note: if encounter singular matrix:
# run below cmd to check which pairs are highly correlated.
# pairs(data[,list(AGE,NDX,NPR,acutePhysiologyScore,apacheScore,N_CM,predictedICUMortality,predictedICULOS,actualICULOS,predictedHospitalMortality,predictedHospitalLOS,actualHospitalLOS),with=T])

# Author: Yang Yang
# Date:"Tue Jul  1 08:57:26 2014"
# Usage: To be loaded by main program:ModelComputation_workbench_setup.r 
#######################################################################################
#--------------------#
# load some prerequisite libraries
require(quantreg)	# the essence
require(Brq)	# the essence
require(survival)	# the essence
#-----#


#--------------------#
quantileReg <- function(data = data_input,response_variables = response_variables_vec, knowledge_driven_factors = knowledge_driven_factors_vec, data_driven_factors = data_driven_factors_vec, factors_annotation_table = factors_all,.seed=123,plotNameEssence = sprintf("quantileReg_plot_at_%s",nowTimeToString()) ){
	# browser()
	# response_variables <- response_variables_vec <- c("LOS.x","died")	# debugging usage
	# data = data[1:1000]	# reduced set, debugging usage.
	# response_variables <- "TOTCHG"
	originalN <- nrow(data)	# record the data raw size
	#
	data <- na.omit( data[,c(response_variables,knowledge_driven_factors,data_driven_factors),with=F] )
										# get a perfect data in sense, otherwise stepAIC will report error due to different sample size each time when it deals with a variable.
										# alternative is to impute for those
										# variables having NAs.
	#
	message("The final perfect sample size (records) before 'predictive model' module running is:",nrow(data))
	message("sample size (records) loss is:",originalN - nrow(data))
	stopifnot( length(response_variables) == 1 )	# limit one trait allowed.
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
	# construct the formula
	formula_knowledgeOnly <- as.formula(sprintf("%s ~ %s", response_variables,paste(knowledge_driven_factors,collapse="+") ) )	
	formula_unionALLnY <- as.formula( sprintf("%s ~ %s + %s", response_variables ,paste(knowledge_driven_factors,collapse="+"), paste(data_driven_factors,collapse="+")  ) )
	# in the case that there are collinearity, we need to remove one of them.
	formula_unionALLnY_excludingCollinearVar <- as.formula( sprintf("%s ~ %s + %s", response_variables ,paste(knowledge_driven_factors,collapse="+"), paste( data_driven_factors[-match(c('acutePhysiologyScore','actualHospitalMortality','actualHospitalLOS','actualICULOS','actualICUMortality', 'ASOURCE','ATYPE','MEDINCSTQ','predictedICUMortality','predictedICULOS','HCUP_ED','HCUP_OS'
	),names(data_driven_factors) )] ,collapse="+")  ) )	# I found acutePhysiologyScore is highly correlated with apcheScore, so delete any one should be fine. Do this for a few others in this logic.
	#
	# names(data_driven_factors) [-match(c('acutePhysiologyScore','actualHospitalMortality','actualHospitalLOS','actualICULOS','actualICUMortality'),names(data_driven_factors) ) ]
	#---#
	# temp <- (data_driven_factors) [-match(c('acutePhysiologyScore','actualHospitalMortality','actualHospitalLOS','actualICULOS','actualICUMortality'),(data_driven_factors) ) ]
	 GGally::ggpairs(data_input[,temp,with=F])
	splom(~data_input[,temp,with=F])
	pairs(data_input[,temp,with=F],pch=19)
	#-------#
	# browser()
	result <- dynrq(formula_knowledgeOnly,data = data,na.action = na.omit)	
	result <- nlrq(formula_knowledgeOnly,data = data)	
	result <- dynrq(formula_unionALLnY,data = data,na.action = na.omit)	
	result <- rq(formula_unionALLnY,data = data,na.action = na.omit,method="lasso")	
	# result <- rqss(formula_unionALLnY,data = data,na.action = na.omit,method="br")	# need extra package.
	result <- rq(formula_unionALLnY,data = data,na.action = na.omit)	
	result_rq <- rq(formula_unionALLnY_excludingCollinearVar,data = data,na.action = na.omit)	
	result_rq_summy <- summary(result_rq)
	# Browse[1]> result_rq_summy %>>% (coefficients) %>>% print(digit=3)
							   # coefficients   lower bd   upper bd
	# (Intercept)                    -23801.1  -4.51e+04   1.94e+04
	# AGE                              -439.0  -5.85e+02  -1.92e+02
	# as.factor(FEMALE)1              -3248.1  -7.84e+03   1.75e+03
	# as.factor(RACE)2                  -91.9  -6.04e+03   7.91e+03
	# as.factor(RACE)4                -7774.7  -2.13e+04   4.94e+03
	# as.factor(RACE)5               -23965.6 -1.80e+308  1.80e+308
	# as.factor(DISPUNIFORM)2         37727.9   2.61e+04   4.49e+04
	# as.factor(DISPUNIFORM)5         44243.3   3.17e+04   4.83e+04
	# as.factor(DISPUNIFORM)6         23998.3   1.27e+04   3.11e+04
	# as.factor(DISPUNIFORM)7        -28592.6  -1.08e+16   4.07e+03
	# as.factor(DISPUNIFORM)20        12878.8  -1.96e+03   1.70e+04
	# NDX                              2470.1   1.31e+03   3.29e+03
	# NPR                             12351.4   1.15e+04   1.38e+04
	# as.factor(PAY1)2                 8870.0  -2.30e+02   1.87e+04
	# as.factor(PAY1)3                 3405.6  -6.18e+03   1.78e+04
	# as.factor(PAY1)4                45337.8  -2.66e+04   6.37e+05
	# as.factor(PAY1)6                 5072.4  -1.16e+04   2.54e+04
	# as.factor(PAY2)2                -8039.6  -3.05e+04   2.58e+03
	# as.factor(PAY2)3                -4948.5  -2.82e+04   7.57e+03
	# as.factor(PAY2)4                -9957.6  -3.47e+04   3.76e+03
	# as.factor(PAY2)5               -12196.1  -4.48e+04   2.31e+04
	# as.factor(PAY2)6                15393.1  -7.62e+03   2.29e+04
	# as.factor(PAY2)-8               11871.6  -6.16e+04   1.99e+16
	# as.factor(PAY2)NULL             -7127.9  -3.01e+04   1.93e+03
	# as.factor(PL_UR_CAT4)2           6041.6  -1.85e+03   1.52e+04
	# as.factor(PL_UR_CAT4)3          -6623.5  -2.31e+04   1.63e+04
	# as.factor(PL_UR_CAT4)4           6496.8 -1.80e+308  1.80e+308
	# as.factor(ZIPINC_QRTL)2          -515.6  -1.70e+04   7.23e+03
	# as.factor(ZIPINC_QRTL)3          8751.4  -1.07e+04   1.63e+04
	# as.factor(ZIPINC_QRTL)4          1435.0  -1.52e+04   1.10e+04
	# as.factor(ZIPINC_QRTL)NULL       4290.4  -4.53e+04   1.58e+04
	# apacheScore                       152.0  -9.25e+00   3.02e+02
	# predictedHospitalMortality     -37227.2  -4.91e+04  -2.71e+04
	# predictedHospitalLOS             1799.1   1.23e+03   2.18e+03
	# as.factor(year)2009             -7322.2  -1.52e+04   2.75e+03
	# as.factor(year)2010              -862.3  -8.86e+03   8.25e+03
	# as.factor(year)2011             -5384.6  -1.65e+04   2.86e+03
	# N_CM                            -1589.1  -3.55e+03  -2.22e+01
	
	save(result_rq, file = "QuantileReg.Rdata")
	attach(data)
	result <- brq(formula_knowledgeOnly)	
	result <- brq(formula_knowledgeOnly, runs=2000)	
	result <- brq(TOTCHG ~ AGE + FEMALE + RACE,runs=2000)	# wrong, need as.factor() correct one.
	result <- brq(formula_unionALLnY_excludingCollinearVar)	# it works.
	print(summary.brq(result),digits=2
	result_summy <- summary.brq(result)
	# > result_summy
						 # Variable        Mean         Sd      Median   2.5% C.I.  97.5% C.I.
	# 1                 (Intercept) -23794.4315 25.4405629 -23801.1242 -23801.1243 -23692.1779
	# 2                         AGE   -439.1193  0.3002596   -439.0436   -440.2892   -439.0414
	# 3          as.factor(FEMALE)1      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 4            as.factor(RACE)2      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 5            as.factor(RACE)4      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 6            as.factor(RACE)5      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 7     as.factor(DISPUNIFORM)2      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 8     as.factor(DISPUNIFORM)5  44241.8840  5.6869218  44243.3478  44221.1944  44243.3478
	# 9     as.factor(DISPUNIFORM)6      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 10    as.factor(DISPUNIFORM)7      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 11   as.factor(DISPUNIFORM)20      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 12                        NDX   2470.2588  0.6376824   2470.1104   2470.0923   2472.7665
	# 13                        NPR  12351.6976  1.2456633  12351.3994  12351.3966  12355.9032
	# 14           as.factor(PAY1)2      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 15           as.factor(PAY1)3      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 16           as.factor(PAY1)4  51243.4856 23.7759829  51253.3928  51183.1160  51264.3250
	# 17           as.factor(PAY1)6      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 18           as.factor(PAY2)2      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 19           as.factor(PAY2)3      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 20           as.factor(PAY2)4      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 21           as.factor(PAY2)5      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 22           as.factor(PAY2)6  15399.6676 26.4913117  15393.0796  15392.8012  15501.9981
	# 23          as.factor(PAY2)-8      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 24        as.factor(PAY2)NULL      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 25     as.factor(PL_UR_CAT4)2   6040.9716  2.9883327   6041.6286   6029.8176   6041.7228
	# 26     as.factor(PL_UR_CAT4)3      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 27     as.factor(PL_UR_CAT4)4      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 28    as.factor(ZIPINC_QRTL)2      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 29    as.factor(ZIPINC_QRTL)3      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 30    as.factor(ZIPINC_QRTL)4   1433.4759  6.2824109   1435.0235   1408.1155   1435.1200
	# 31 as.factor(ZIPINC_QRTL)NULL      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 32                apacheScore    151.9214  0.2772765    151.9911    150.7778    151.9946
	# 33 predictedHospitalMortality      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 34       predictedHospitalLOS   1798.7602  1.4397168   1799.1159   1793.3669   1799.1292
	# 35        as.factor(year)2009      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 36        as.factor(year)2010      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 37        as.factor(year)2011      0.0000  0.0000000      0.0000      0.0000      0.0000
	# 38                       N_CM  -1588.0403  4.4732185  -1589.1319  -1589.1803  -1571.4312

	result <- brq(formula_unionALLnY,runs=3000)	# it works.
	save(result, file = "bayesianQuantileReg.Rdata")
# Error in chol.default(t(x) %*% V %*% x + invLamb) :
  # the leading minor of order 43 is not positive definite
	
	# usually not converge or encounter singular matrix.
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	


	return( structure(list(result = result, coefs_selected = coefs_lambda.min ),class= c("RegularizedLinearObj","list")) )
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
				  