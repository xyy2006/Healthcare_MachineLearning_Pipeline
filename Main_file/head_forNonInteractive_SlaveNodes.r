## This file is for head_forNonInteractive_SlaveNodes, differ with head.r in 
## it doesn't force slave to install the missing packages if any.
################################################################################
# options(show.error.messages=FALSE)
temp <- try(library(MASS))	# standard, no need to install

temp <- try(library(class))	# standard, no need to install

temp <- try(library(cluster))	

#temp <- try(library(impute))# install it for imputing missing value
#
#temp <- try(library(WGCNA))
#
temp <- try(library(doMC))	# parallele

temp <- try(library(parallel))	# parallele

#temp <- try(library(multicore))# parallele

temp <- try(library(stringr)) # string process

temp <- try(library(plyr)) # data split and merge

temp <- try(library(microbenchmark)) # test speed

temp <- try(library(gplots)) # enforced plot functions

temp <- try(library(gdata)) # enforced plot functions

temp <- try(library(hash)) # hash table

temp <- try(library(rbenchmark) )# another benchmark package

temp <- try( library(Rcpp) )	##enable R + c/cpp integration

temp <- try( library(snow) )	# load snow package

temp <- try( library(doSNOW) )	# load doSNOW parallel backend

temp <- try( library(doRNG) )	# load doSNOW parallel backend

temp <- try( library(compiler) )	# enable byte compile                                        #

temp <- try( library(lattice) )		# lattice plot package

temp <- try( library(geepack) )		# lattice plot package

temp <- try( library(data.table) )		# lattice plot package

temp <- try( library(xtable) )		# lattice plot package

temp <- try( library(getopt) )		# lattice plot package

temp <- try( library(dplyr) )		# lattice plot package

temp <- try( library(pipeR) )		# lattice plot package

temp <- try( library(bigmemory) )		# lattice plot package

temp <- try( library(bit64) )		# lattice plot package

#--------------------------------------------#
options(stringsAsFactors=F)

collect_garbage <-function(){while (gc()[2,4] != gc()[2,4] | gc()[1,4] != gc()[1,4]){}}
# 2.
catt <- function(...)
{
 cat(...,'\n' )
}
# 3.
naPlus <- function(x,y) {
    x[is.na(x)] <- 0;
    y[is.na(x)] <- 0;
    x + y 	# this allow pairwise +, not sum to a scalar.
}
# 4.
iblkcol <- function(a, chunks) {
  n <- ncol(a)
  i <- 1

  nextElem <- function() {
    if (chunks <= 0 || n <= 0) stop('StopIteration')
    m <- ceiling(n / chunks)
    r <- seq(i, length=m)
    i <<- i + m
    n <<- n - m
    chunks <<- chunks - 1
    a[,r, drop=FALSE]
  }

  structure(list(nextElem=nextElem), class=c('iblkcol', 'iter'))
}

nextElem.iblkcol <- function(obj) obj$nextElem()
#-------------------#
obj_size <- function(x) print(utils::object.size(x),units='Mb')
save_Rdata <- function(string_obj) save(list=string_obj,file=sprintf('%s.Rdata',string_obj))
read_table <- function(con,...) read.table(con,header=T,check.names=F,sep='\t',fill=T,skip=0,quote='',comment='',strip.white=T,...)
write_matrix <- function(dataframe,filename,...) write.table(dataframe, file = filename,quote=F,row.names=T, col.names=NA,sep='\t',...)
# check history of some pattern for previous cmds.
hishow <- function(pattern) history(pattern=pattern)
# unit is MB here.
purge_bigOBJ <- function(big_cutoff = 100)	# list the objects in memory with corresponding size.
{
	all_obj_strings <- ls(pos=1)
	a<-c()
	for (i in 1:length(all_obj_strings)) {
		a[[i]] <- cbind(obj = all_obj_strings[i],size = round(object.size(eval(parse(text=all_obj_strings[i])))/1024/1024,3) )
	}
	obj_list <- do.call(rbind,a)
	obj_list_sorted <- obj_list[order(as.numeric(obj_list[,2]),decreasing=T),]
	colnames(obj_list_sorted) <- c('obj','size(mb)')
	print(obj_list_sorted,quote=F)
	invisible(obj_list_sorted)
	# # prompt whether to delete.
	# anwser = readline(cat('Do you want to delete those unused (old) obj bigger than',big_cutoff,'mb? (yes or no, then press \'enter\')\n') )
	# if (substr(anwser,1,1)=='y') {
		
		# for )
		
	# }
}
#---------------#
# try to capitalize 1st letter of each word.
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}


nowTimeToString <- function(){
 paste(unlist(str_split(Sys.time(),pattern=' ')),collapse='_')
}

head6 <-function(x,...,ncol=6){
  head(x=x,...)[,1:ncol]
}


head10 <- function(x,...,ncol=10){
    head(x=x,...)[,1:ncol]
  }
 ##================================================================##
 ###  In longer simulations, aka computer experiments,            ###
 ###  you may want to                                             ###
 ###  1) catch all errors and warnings (and continue)             ###
 ###  2) store the error or warning messages                      ###
 ###                                                              ###
 ###  Here's a solution  (see R-help mailing list, Dec 9, 2010):  ###
 ##================================================================##
 tryCatch.W.E <- function(expr)	# for 
{
    W <- NULL
    w.handler <- function(w){ # warning handler
      W <<- w
      invokeRestart("muffleWarning")
    }
    list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler),
       warning = W)
}


pchShow <-	# demonstrate all the symbols for plot convenience.
       function(extras = c("*",".", "o","O","0","+","-","|","%","#"),
                cex = 3, ## good for both .Device=="postscript" and "x11"
                col = "red3", bg = "gold", coltext = "brown", cextext = 1.2,
                main = paste("plot symbols :  points (...  pch = *, cex =",
                             cex,")"))
       {
         nex <- length(extras)
         np  <- 26 + nex
         ipch <- 0:(np-1)
         k <- floor(sqrt(np))
         dd <- c(-1,1)/2
         rx <- dd + range(ix <- ipch %/% k)
         ry <- dd + range(iy <- 3 + (k-1)- ipch %% k)
         pch <- as.list(ipch) # list with integers & strings
         if(nex > 0) pch[26+ 1:nex] <- as.list(extras)
         plot(rx, ry, type="n", axes = FALSE, xlab = "", ylab = "",
              main = main)
         abline(v = ix, h = iy, col = "lightgray", lty = "dotted")
         for(i in 1:np) {
           pc <- pch[[i]]
           ## 'col' symbols with a 'bg'-colored interior (where available) :
           points(ix[i], iy[i], pch = pc, col = col, bg = bg, cex = cex)
           if(cextext > 0)
               text(ix[i] - 0.3, iy[i], pc, col = coltext, cex = cextext)
         }
       }

# > dim(datExp_list_numeric_only8cancer_transposedGeneAsColumns$Breast_datExp)
# [1]   529 17813
# try(allowWGCNAThreads(8))
# Allowing multi-threading with up to 8 threads.

# for loop
options(show.error.messages=TRUE)
