library(SparkR)
#---setting the env for Rstudio to find Spark----#
# Sys.setenv(SPARK_HOME=”/home/ec2-user/spark-1.5.0/”)
# .libPaths(new = c(file.path(Sys.getenv(“SPARK_HOME”), “R”, “lib”), .libPaths()))
# sc <- sparkR.init(master = "local",sparkHome = "/home/ec2-user/spark-1.5.0")
# sqlContext <- sparkRSQL.init(sc)
# SparkDF <- createDataFrame(sqlContext, faithful)
# head(SparkDF)

#---with settup put in ./.Rprofile, we will have simply----#
sc <- sparkR.init(master = "local")
sqlContext <- sparkRSQL.init(sc)
SparkDF <- createDataFrame(sqlContext, faithful)
head(SparkDF)
colnames(SparkDF)
names(SparkDF)
dim(SparkDF)
