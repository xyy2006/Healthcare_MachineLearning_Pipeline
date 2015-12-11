#!/bin/bash
# Content: A function can extract the module list from Modules_Catalog.txt put under './Module_file'.
# Note: 
# Author: Yang Yang
# Date:"Thu Jul 24 15:12:22 EDT 2014"
# Usage: To be loaded by main program:ModelComputation_workbench_setup.r
#######################################################################################
#--------------------#
# load some prerequisite libraries
suppressPackageStartupMessages (require(stringr) )	# the string manipulation package

# use formals & alist, substitute, eval, match.call, parse, etc.
#--------a example-------------#
# write.csv <- function(...) {
# Call <- match.call(expand.dots = TRUE)
# for (arg in c("append", "col.names", "sep", "dec", "qmethod")) {
# if (!is.null(Call[[arg]])) {
# warning(gettextf("attempt to set '%s' ignored", arg))
# }
# }
# rn <- eval.parent(Call$row.names)
# Call$append <- NULL
# Call$col.names <- if (is.logical(rn) && !rn) TRUE else NA
# Call$sep <- ","
# Call$dec <- "."
# Call$qmethod <- "double"
# Call[[1L]] <- as.name("write.table")
# eval.parent(Call)
# }
#-------#
# Modules_Catalog_file <- "Modules_Catalog.txt"
extract_moduleList_from_ModulesCatalog <- function(Modules_Catalog_file){
	model_raw_text <- readLines(Modules_Catalog_file)  
	sectionIndex <- grep("^\\w*\\:",model_raw_text,perl=T)
	sectionIndex_value <- grep("^\\w*\\:",model_raw_text,perl=T,value=T)
	sectionIndex_value <- sub( "#.*$","",sectionIndex_value,perl=T)	# remove if any comment put after ':'
	sectionIndex_value <- str_trim(sectionIndex_value)
	#
	moduleName <- c()	# we will use it later to give names to calls
	algorithmsUnderModule <- c()
	for (i in 1:length(sectionIndex))  {
		# ---get ready the function name---#
		# functionName[i] <- gsub(".*?:","",model_raw_text[ functionIndex[i] ])	
		moduleName[i] <- gsub(":.*?$","",sectionIndex_value[i] )	
		# ---get ready the arguments for that function---#		
		if (i != length(sectionIndex)) {
			argument_chunk <- model_raw_text[ (sectionIndex[i]+1) : (sectionIndex[i+1]-1) ]
		} else 	argument_chunk <- model_raw_text[ (sectionIndex[i]+1) : length(model_raw_text) ]
		#---#
		#cleansing
		argument_chunk <- str_trim(argument_chunk)
		# argument_chunk <- str_replace_all(argument_chunk,pattern=",","")	# remove any , in the arguments, we will knit them together using , later.
		argument_chunk <- grep( "^[^#].+?",argument_chunk,perl=T,value=T)	# not start from comment.
		argument_chunk <- sub( "#.*$","",argument_chunk,perl=T)	# remove line end comments if any.
		argument_chunk <- str_trim(argument_chunk)	# trim both white space again.
		argument_chunk <- str_replace_all(argument_chunk,pattern="(^,)|(,$)","")	# remove any , in the arguments, we will knit them together using , later.
		
		#---#
		algorithmsUnderModule[i] <- sprintf("
%s:
#----------------------------------------------------#
%s	
											", moduleName[i], paste(argument_chunk,collapse='\n')
		)
		# message("#====================================================#")
		message(algorithmsUnderModule[i])
	}
	message("#====================================================#")
	names(algorithmsUnderModule) <- sectionIndex_value
	# [1] "RSForest"
	return(algorithmsUnderModule=algorithmsUnderModule)

}	# returned are a bunch of named calls in that model file.


