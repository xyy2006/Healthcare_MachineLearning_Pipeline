# Healthcare_MachineLearning_Pipeline
Introduction:

Modelling, measurement and assessment of hospital performance have gained an increased importance over recent years due to their value towards (a) better self-evaluation by hospitals, (b) more informed hospital selection decisions by patients, and (c) development of performance-based payment structures by health-insurance providers such as the Center for Medicare and Medicaid Services (CMS). Due to data heterogeneity and complexity, there is no single best model working for all the scenarios, e.g. modelling in-hospital mortality as an outcome on administrative data is a scenario different than modelling in-hospital length of stay as an outcome on patient clinical data. There is an urgent need to build an automated pipeline which gives the user opportunities to test a few different types of models on the same dataset in a neat and fast manner. We developed such a pipeline wrapped up in a Linux command-line operated program. The pipeline has additional properties like flexible parallel/serial scheme, flexible model parameter tuning, robust to different datasets with mixed types of explanatory and response variables, complete logging and error collecting system, and the ease to add more models in the future. We named it “PhilipsHealthcareBDS”, where “BDS” represents “Big Data Solution”.

---------------------------------------------------------------------------------------------------------------------------------

Quick Manual:

Open R console under Linux, install R package “packrat” from CRAN by the command “install.packages(“packrat”)”; then type “packrat::unbundle(bundle, where)” to unbundle the compressed ‘bundle’d project to your designated path. 

install.packages(“packrat”)

packrat::unbundle(bundle, where)

The main executable “ModelComputation_main” (working on Linux HPC) is put in the program root folder. The bash script wrapper for "ModelComputation_main" is "main.sh", which is suggested to call instead of directly calling “ModelComputation_main”. SparkR version executable (working on Amazon EC2 with pre-installed hadoop and spark) is "ModelComputation_main_SparkR_version" associated with the wrapper "main_sparkR_version.sh". There are also several folders under root folder: Main_file, packrat, Factors_details_file, Misc, Module_file and Model_file. Each folder has several files/scripts within it. “Module_file” contains module algorithm file; “Model_file” contains algorithm configuration template file (make a copy before you want to modify); “Main_file” contains major functions called within main executable and an optional parallel configuration file template (user system specific, make a copy before you want to modify); “Factors_details_file” contains templates for specifying the variables in your provided input dataset; “Misc” contains miscellaneous files such as untested algorithms, working notes, etc, and they will not affect program run; “packrat” contains project related library files and source file backup, usually you are supposed not to change anything here. Please check each folder for details, all the files are well commented. When running the program, we need to ensure the most important data, which is the healthcare patient data extracted from database and preprocessed already, is put under program root folder, i.e. same path as main executable. Then we run the command:

./main.sh -i NY_SID_2009_HFdata_refined_subsetDemo.Rdata -F factor_details_forLM.txt -f uniCorr -p Survival_glmnet -c uniCorr_model.txt -e Survival_glmnet_model.txt -o Survival_glmnet -P SNOW -j T -m MultipleNodes_profile.txt

-i: prepared patient data

-F: prepared covariates information file, corresponding to patient data

-f: the pre-feature selection module we want to use

-p: the prediction module we want to use

-c: model template for pre-feature selection module

-e: model template for prediction module

-o: the prefix for filename used in output files

-P: parallel scheme used

-j: will the parallel scheme run on multiple nodes?

-m: the multiple nodes template

For detailed help page about command argument usage, type:

./main.sh 

Or

./main.sh –h

Or

./main.sh --help

When program starts to run, information will be print on screen and a more verbose version will be logged. Whenever an error encountered, not only the error info will be saved in the log file (Note: not printed on screen due to R limitation for now), but also the run environment upon error occurs will be save to a “.rda” file and can be examined further in R console by 

loadFileName <- load(“xxxx.rda”); 

debugger( get(loadFileName) );

This code snippet will examine the runtime environment upon error occurred and greatly helps debugging.

