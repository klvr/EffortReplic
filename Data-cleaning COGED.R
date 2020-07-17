## COGED/N-back data-cleaning
## From raw igdat-file, to cleaned/summarized CSV-file

#(local) File-structure: 
##getwd()/Data/COGED/file


#Read in the raw data
files <- list.files(paste(getwd(), "/Data/COGED", sep = ""))
files <- files[grepl("*raw.iqdat", files)]
if (length(files) > 1) {print("IT'S MORE THAN ONE FILE! SCRIPT MUST BE ADAPTED FIRST!")}
for (i in files) {
data <- read.delim2(paste(getwd(), "/Data/COGED/", i, sep =""))
}

#Set up
COGED <- as.numeric()

#D-prime blocks
dpblock <- c("block2", "block3", "block4")
#IP blocks
ipblock <- c("block5_12", "block5_13", "block5_14")

#D-prime
subj <- unique(data$subject)
for (i in subj) {
  subjdata <- (data$subject == i)
  subjdata <- data[subjdata,]
  subjdp2 <- subjdata[subjdata$blockcode == dpblock[1],]
  subjdp3 <- subjdata[subjdata$blockcode == dpblock[2],]
  subjdp4 <- subjdata[subjdata$blockcode == dpblock[3],]
  subjdp2 <- subjdp2[rowSums(subjdp2[c(20,21,22,23)]) == 1,]
  subjdp3 <- subjdp3[rowSums(subjdp3[c(20,21,22,23)]) == 1,]
  subjdp4 <- subjdp4[rowSums(subjdp4[c(20,21,22,23)]) == 1,]
  
  subjdp2hitrate <- (sum(subjdp2[,20]) / (sum(subjdp2[,22])+sum(subjdp2[,20])))
  subjdp2FArate <- (sum(subjdp2[,21]) / (sum(subjdp2[,21])+sum(subjdp2[,23])))
  if (subjdp2hitrate == 1) { subjdp2hitrate <- (1-(1/(2*80))) }
  if (subjdp2FArate == 0) { subjdp2FArate <- (1/(2*80)) }
  if (subjdp2hitrate == 0) { subjdp2hitrate <- (1/(2*80)) }
  if (subjdp2FArate == 1) { subjdp2FArate <- (1-(1/(2*80))) }
  subjdp2 <- qnorm(subjdp2hitrate) - qnorm(subjdp2FArate)
  
  subjdp3hitrate <- (sum(subjdp3[,20]) / (sum(subjdp3[,22])+sum(subjdp3[,20])))
  subjdp3FArate <- (sum(subjdp3[,21]) / (sum(subjdp3[,21])+sum(subjdp3[,23])))
  if (subjdp3hitrate == 1) { subjdp3hitrate <- (1-(1/(2*80))) }
  if (subjdp3FArate == 0) { subjdp3FArate <- (1/(2*80)) }
  if (subjdp3hitrate == 0) { subjdp3hitrate <- (1/(2*80)) }
  if (subjdp3FArate == 1) { subjdp3FArate <- (1-(1/(2*80))) }
  subjdp3 <- qnorm(subjdp3hitrate) - qnorm(subjdp3FArate)

  subjdp4hitrate <- (sum(subjdp4[,20]) / (sum(subjdp4[,22])+sum(subjdp4[,20])))
  subjdp4FArate <- (sum(subjdp4[,21]) / (sum(subjdp4[,21])+sum(subjdp4[,23])))
  if (subjdp4hitrate == 1) { subjdp4hitrate <- (1-(1/(2*80))) }
  if (subjdp4FArate == 0) { subjdp4FArate <- (1/(2*80)) }
  if (subjdp4hitrate == 0) { subjdp4hitrate <- (1/(2*80)) }
  if (subjdp4FArate == 1) { subjdp4FArate <- (1-(1/(2*80))) }
  subjdp4 <- qnorm(subjdp4hitrate) - qnorm(subjdp4FArate)
  COGED <- rbind(COGED, cbind(i, subjdp2, subjdp3, subjdp4, subjdp2hitrate, subjdp2FArate,subjdp3hitrate, subjdp3FArate,subjdp4hitrate, subjdp4FArate))
}

#IP


rownames(COGED) <- COGED[,1]
COGED <- COGED[,-1]
names <- c("DP2", "DP3", "DP4", "HR2", "FA2", "HR3", "FA3", "HR4", "FA4")
colnames(COGED) <- paste("Coged", names, sep="")
