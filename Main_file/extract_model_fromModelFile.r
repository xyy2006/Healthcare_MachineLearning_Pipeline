#!/bin/bash
# Content: A function can extract the model details from specified model file, e.g. RSForest_model.txt.
# Note: 
# Author: Yang Yang
# Date:"Thu Jul 24 15:12:22 EDT 2014"
# Usage: To be loaded by main program:ModelComputation_workbench_setup.r
#######################################################################################
#--------------------#
# load some prerequisite libraries
suppressPackageStartupMessages( require(stringr) )	# the string manipulation package

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
# model_file <- "RSForest_model.txt"
extract_model_fromModelFile <- function(model_file){
	model_raw_text <- readLines(model_file)  
	functionIndex <- grep("^\\s*\\:.+$",model_raw_text,perl=T)
	functionIndex_value <- grep("^\\s*\\:.+$",model_raw_text,perl=T,value=T)
	functionIndex_value <- sub( "#.*$","",functionIndex_value,perl=T)	# remove if any comment put after ':'
	functionIndex_value <- str_trim(functionIndex_value)
	#
	functionName <- c()	# we will use it later to give names to calls
	calls <- foreach (i = 1:length(functionIndex)) %do% {
		# ---get ready the function name---#
		# functionName[i] <- gsub(".*?:","",model_raw_text[ functionIndex[i] ])	
		functionName[i] <- gsub(".*?:","",functionIndex_value[i] )	
		# ---get ready the arguments for that function---#		
		if (i != length(functionIndex)) {
			argument_chunk <- model_raw_text[ (functionIndex[i]+1) : (functionIndex[i+1]-1) ]
		} else 	argument_chunk <- model_raw_text[ (functionIndex[i]+1) : length(model_raw_text) ]
		#---#
		#cleansing
		argument_chunk <- str_trim(argument_chunk)
		# argument_chunk <- str_replace_all(argument_chunk,pattern=",","")	# remove any , in the arguments, we will knit them together using , later.
		argument_chunk <- grep( "^[^#].+?=.+?",argument_chunk,perl=T,value=T)	# not start from comment.
		argument_chunk <- sub( "#.*$","",argument_chunk,perl=T)	# remove line end comments if any.
		argument_chunk <- str_trim(argument_chunk)	# trim both white space again.
		argument_chunk <- str_replace_all(argument_chunk,pattern="(^,)|(,$)","")	# remove any , in the arguments, we will knit them together using , later.

		#---#
		# making up the call
		call <- as.call( parse(text= sprintf("%s(%s)",functionName[i], paste(argument_chunk,collapse=", ") ) ) )
		names(call) <- functionName[i]	# a call with name.
		call <- matchCall_model_fromExtract(call)
		# call <- make_call(functionName[i], (argument_chunk) )	# doesnt work.
		message("#====================================================#")
		message("The returned call from ",model_file," is:\n",call)
		return(call )
		# return(call = call )
	}
	message("#====================================================#")
	names(calls) <- functionIndex_value
	# [1] "RSForest"
	return(calls=calls)

}	# returned are a bunch of named calls in that model file.


#----subfunc---#
# neat the call by using match.call
matchCall_model_fromExtract <- function(modelCall) {
	call = match.call(eval(parse(text=names(modelCall))),modelCall)
	names(call) <- names(modelCall)
	# class(call) <-c("named_call","call")
	return(call)
}	# return the call in complete argument names version.