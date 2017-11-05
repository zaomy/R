
#################################################
# step 1: import data
dataname = "./etch1.dat"
data <- read.delim(dataname, header = FALSE, sep="", skip=0, as.is=TRUE)
#data <-read.table(dataname, header=FALSE)
colnames(data) <- c("power","etch")

boxplot(etch ~ power, data=data)

lmod = lm(etch ~ power, data=data)
summary(lmod)
