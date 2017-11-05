
#################################################
# step 1: import data
dataname = "./pricequal.dat"
data <- read.delim(dataname, header = FALSE, sep="", skip=0, as.is=TRUE)
#data <-read.table(dataname, header=FALSE)
colnames(data) <- c("Price","Quality", "Count")

data1 <- data[data$Price==1,]
data2 <- data[data$Price==2,]
data3 <- data[data$Price==3,]

R1 <- sum(data1$Quality * data1$Count)/sum(data1$Count)
R2 <- sum(data2$Quality * data2$Count)/sum(data2$Count)
R3 <- sum(data3$Quality * data3$Count)/sum(data3$Count)

plot(c(1, 2, 3), c(R1, R2, R3))

#############################################################

Mtemp <- matrix(  , nrow=0, ncol=2)

for (m in 1:nrow(data)) {
  
  nc <- data[m,3]
  
  Mtemp <- rbind(Mtemp,matrix(rep(as.matrix(data[m,c(1,2)]),nc),nc,2,byrow=TRUE))
}

colnames(Mtemp) <- c("price", "quality")

boxplot(quality ~ price, data=Mtemp)




######################################

Mtemp2 = matrix(,3,3)

uniPrice <- unique(Mtemp[,1])
Mtemp2[,1] <- uniPrice

for (mm in 1:length(uniPrice)) {
  
  
  Ntemp <- Mtemp[Mtemp[,1] == uniPrice[mm]]
  avg <- mean(Ntemp)
  std <- sd(Ntemp)
  Mtemp2[mm,2] <- avg
  Mtemp2[mm,3] <- std
  
}

colnames(Mtemp2) <- c("x", "avg", "sdev")
Mtemp3 <-as.data.frame(Mtemp2)



plot(Mtemp3$x, Mtemp3$avg,
     ylim=range(c(Mtemp3$avg-Mtemp3$sdev, Mtemp3$avg+Mtemp3$sdev)),
     pch=19, xlab="Price", ylab="Rating (Mean +/- SD)",
     main="Scatter plot with std.dev error bars"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(Mtemp3$x, Mtemp3$avg-Mtemp3$sdev, Mtemp3$x, Mtemp3$avg+Mtemp3$sdev, length=0.05, angle=90, code=3)

