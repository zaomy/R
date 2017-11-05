
#################################################
# step 1: import data
dataname = "./lister.dat"
data <- read.delim(dataname, header = FALSE, sep="", skip=0, as.is=TRUE)
#data <-read.table(dataname, header=FALSE)
colnames(data) <- c("ID","Y", "A", "L","Outcom")

temp <- data[data$A==0 & data$L==1, "Outcom"]
mean(temp)

temp2 <- data[data$A==1 & data$L==1, "Outcom"]
mean(temp2)

temp3 <- data[data$A==0 & data$L==2, "Outcom"]
mean(temp3)

temp4 <- data[data$A==1 & data$L==2, "Outcom"]
mean(temp4)


