## Data cleaning Effort Replic (in progress - contains leftovers from RQ2-set piloting)
# Loading and labeling
rawdata <- read.csv("Data/qualt.csv", colClasses = "character") #Export from Qualtrics into csv-file, rename the qualtrics-file "qualt.csv", and place it into your project/working directory
rawdata <- rawdata[-c(1:8),] #Replace the 10 with the number of piloting data points (and other rows that are not participants)
rawdata <- rawdata[,-c(1,2,3,4,6,7,8,9)] #Remove the uninteresting data (e.g., start time, end time, language)
cnames <- c("Duration","Lang","ID","DSTNTLXMental", "DSTNTLXPhysical","DSTNTLXTime", "DSTNTLXPerformance", "DSTNTLXFrustration", "DSTNTLXEffort","RQ1Disjunctive","RQ2Covariance","RQ3Toy","RQ4Lily","RQ5Water","RQ6Class","RQ7Stocks","RQ8Wason","RQ9Cancer","RQ10ProbMatch1","RQ10ProbMatch2","RQ10ProbMatch3","RQ10ProbMatch4","RQ10ProbMatch5","RQ10ProbMatch6","RQ10ProbMatch7","RQ10ProbMatch8","RQ10ProbMatch9","RQ10ProbMatch10","RQ11Knave","RQ12Bus","RQ13Pig","RQ14Bayesian1","RQ14Bayesian2","RQNTLXMental", "RQNTLXPhysical","RQNTLXTime", "RQNTLXPerformance", "RQNTLXFrustration", "RQNTLXEffort","RQDebrief1","RQDebrief2", "NFC1", "NFC2","NFC3","NFC4","NFC5","NFC6","NFC7","NFC8","NFC9","NFC10","NFC11","NFC12","NFC13","NFC14","NFC15","NFC16","NFC17","NFC18")
colnames(rawdata) <- cnames #Does not include curiosity and randomizer sequence as of now
rq <- rawdata[,10:33] #input first and last RQ-item
dstntlx <- rawdata[,4:9] #input first and last DST-NTLX-item
rqntlx <- rawdata[,34:39] #input first and last RQ-NTLX-item
nfc <- rawdata[,42:59] #input first and last NFC-item
rqdebrief <- rawdata[,40:41]
rest <- rawdata[,1:3] #Duration, ID and language of survey

#Make "rest" analysis friendly
rest["Lang"] <-replace(rest["Lang"], (rest["Lang"]=="EN"), 1) #English as 1
rest["Lang"] <-replace(rest["Lang"], (rest["Lang"]=="NO"), 2) #Manually switched to Norwegian as 2
rest["Lang"] <-as.numeric(rest$Lang)

#Make RQ-Debrief analysis friendly
rqdebrief$RQDebrief1 <- replace(rqdebrief$RQDebrief1, rqdebrief$RQDebrief1=="No", 0)
rqdebrief$RQDebrief2 <- replace(rqdebrief$RQDebrief2, rqdebrief$RQDebrief2=="Less than 3", 1)
rqdebrief$RQDebrief2 <- replace(rqdebrief$RQDebrief2, rqdebrief$RQDebrief2=="Between 3 and 6", 2)
rqdebrief$RQDebrief1 <- replace(rqdebrief$RQDebrief1, rqdebrief$RQDebrief1=="Yes", 0)
rqdebrief$RQDebrief1 <- as.numeric(rqdebrief$RQDebrief1)
rqdebrief$RQDebrief2 <- as.numeric(rqdebrief$RQDebrief2)
rqdebrief$RQDebrief1 <- rowSums(rqdebrief, na.rm=TRUE)


# Calculate NFC-score
nfc <- replace(nfc, nfc=="very strong disagreement", 1)
nfc <- replace(nfc, nfc=="very strong agreement", 5)
nfc[,"NFC1"] <- as.numeric(nfc[,"NFC1"])
nfc[,"NFC2"] <- as.numeric(nfc[,"NFC2"])
nfc[,"NFC3"] <- as.numeric(nfc[,"NFC3"])
nfc[,"NFC4"] <- as.numeric(nfc[,"NFC4"])
nfc[,"NFC5"] <- as.numeric(nfc[,"NFC5"])
nfc[,"NFC6"] <- as.numeric(nfc[,"NFC6"])
nfc[,"NFC7"] <- as.numeric(nfc[,"NFC7"])
nfc[,"NFC8"] <- as.numeric(nfc[,"NFC8"])
nfc[,"NFC9"] <- as.numeric(nfc[,"NFC9"])
nfc[,"NFC10"] <- as.numeric(nfc[,"NFC10"])
nfc[,"NFC11"] <- as.numeric(nfc[,"NFC11"])
nfc[,"NFC12"] <- as.numeric(nfc[,"NFC12"])
nfc[,"NFC13"] <- as.numeric(nfc[,"NFC13"])
nfc[,"NFC14"] <- as.numeric(nfc[,"NFC14"])
nfc[,"NFC15"] <- as.numeric(nfc[,"NFC15"])
nfc[,"NFC16"] <- as.numeric(nfc[,"NFC16"])
nfc[,"NFC17"] <- as.numeric(nfc[,"NFC17"])
nfc[,"NFC18"] <- as.numeric(nfc[,"NFC18"])
# Reverse NFC item 3,4,5,7,8,9,12,16 & 17
nfc[,"NFC3"] <- 6 - nfc[,"NFC3"]
nfc[,"NFC4"] <- 6 - nfc[,"NFC4"]
nfc[,"NFC5"] <- 6 - nfc[,"NFC5"]
nfc[,"NFC7"] <- 6 - nfc[,"NFC7"]
nfc[,"NFC8"] <- 6 - nfc[,"NFC8"]
nfc[,"NFC9"] <- 6 - nfc[,"NFC9"]
nfc[,"NFC12"] <- 6 - nfc[,"NFC12"]
nfc[,"NFC16"] <- 6 - nfc[,"NFC16"]
nfc[,"NFC17"] <- 6 - nfc[,"NFC17"]
#Quick(bad) fix for missing values - Replace by 3, would suggest a weighted adjustment, e.g., dividing their score on the number of items answered times 18, must be discussed.
nfc <- replace(nfc, is.na(nfc), 3)
#Add up NFC-scores
nfc <- cbind(nfc, rowSums(nfc))
colnames(nfc)[19] <- "NFCSum"


# Calculate RQ-scores
rq$RQ1Disjunctive <- replace(rq$RQ1Disjunctive, !rq$RQ1Disjunctive=="Yes",0)
rq$RQ1Disjunctive <- replace(rq$RQ1Disjunctive, rq$RQ1Disjunctive=="Yes",1)
rq$RQ1Disjunctive <- as.numeric(rq$RQ1Disjunctive)

rq$RQ2Covariance <- replace(rq$RQ2Covariance, rq$RQ2Covariance>0, 0)
rq$RQ2Covariance <- replace(rq$RQ2Covariance, rq$RQ2Covariance<0, 1)
rq$RQ2Covariance <- as.numeric(rq$RQ2Covariance)

rq$RQ3Toy <- replace(rq$RQ3Toy, !rq$RQ3Toy == "5", 0)
rq$RQ3Toy <- replace(rq$RQ3Toy, rq$RQ3Toy == "5", 1)
rq$RQ3Toy <- as.numeric(rq$RQ3Toy)

rq$RQ4Lily <- replace(rq$RQ4Lily, !rq$RQ4Lily == "47", 0)
rq$RQ4Lily <- replace(rq$RQ4Lily, rq$RQ4Lily == "47", 1)
rq$RQ4Lily <- as.numeric(rq$RQ4Lily)

rq$RQ5Water <- replace(rq$RQ5Water, !rq$RQ5Water == "4", 0)
rq$RQ5Water <- replace(rq$RQ5Water, rq$RQ5Water == "4", 1)
rq$RQ5Water <- as.numeric(rq$RQ5Water)

rq$RQ6Class <- replace(rq$RQ6Class, !rq$RQ6Class == "29", 0)
rq$RQ6Class <- replace(rq$RQ6Class, rq$RQ6Class == "29", 1)
rq$RQ6Class <- as.numeric(rq$RQ6Class)

rq$RQ7Stocks <- replace(rq$RQ7Stocks, !rq$RQ7Stocks == "has lost money", 0)
rq$RQ7Stocks <- replace(rq$RQ7Stocks, rq$RQ7Stocks == "has lost money", 1)
rq$RQ7Stocks <- as.numeric(rq$RQ7Stocks)

rq$RQ8Wason <- replace(rq$RQ8Wason, !rq$RQ8Wason == "Card 2) Entering,Card 4) Vaccinated against: typhoid fever", 0)
rq$RQ8Wason <- replace(rq$RQ8Wason, rq$RQ8Wason == "Card 2) Entering,Card 4) Vaccinated against: typhoid fever", 1)
rq$RQ8Wason <- as.numeric(rq$RQ8Wason)

rq$RQ9Cancer <- replace(rq$RQ9Cancer, !rq$RQ9Cancer == "1 in 10", 0)
rq$RQ9Cancer <- replace(rq$RQ9Cancer, rq$RQ9Cancer == "1 in 10", 1)
rq$RQ9Cancer <- as.numeric(rq$RQ9Cancer)

rq$RQ10ProbMatch1 <- replace(rq$RQ10ProbMatch1, !rq$RQ10ProbMatch1 == "Blue cup", 0)
rq$RQ10ProbMatch1 <- replace(rq$RQ10ProbMatch1, rq$RQ10ProbMatch1 == "Blue cup", 1)
rq$RQ10ProbMatch1 <- as.numeric(rq$RQ10ProbMatch1)

rq$RQ10ProbMatch2 <- replace(rq$RQ10ProbMatch2, !rq$RQ10ProbMatch2 == "Blue cup", 0)
rq$RQ10ProbMatch2 <- replace(rq$RQ10ProbMatch2, rq$RQ10ProbMatch2 == "Blue cup", 1)
rq$RQ10ProbMatch2 <- as.numeric(rq$RQ10ProbMatch2)

rq$RQ10ProbMatch3 <- replace(rq$RQ10ProbMatch3, !rq$RQ10ProbMatch3 == "Blue cup", 0)
rq$RQ10ProbMatch3 <- replace(rq$RQ10ProbMatch3, rq$RQ10ProbMatch3 == "Blue cup", 1)
rq$RQ10ProbMatch3 <- as.numeric(rq$RQ10ProbMatch3)

rq$RQ10ProbMatch4 <- replace(rq$RQ10ProbMatch4, !rq$RQ10ProbMatch4 == "Blue cup", 0)
rq$RQ10ProbMatch4 <- replace(rq$RQ10ProbMatch4, rq$RQ10ProbMatch4 == "Blue cup", 1)
rq$RQ10ProbMatch4 <- as.numeric(rq$RQ10ProbMatch4)

rq$RQ10ProbMatch5 <- replace(rq$RQ10ProbMatch5, !rq$RQ10ProbMatch5 == "Blue cup", 0)
rq$RQ10ProbMatch5 <- replace(rq$RQ10ProbMatch5, rq$RQ10ProbMatch5 == "Blue cup", 1)
rq$RQ10ProbMatch5 <- as.numeric(rq$RQ10ProbMatch5)

rq$RQ10ProbMatch6 <- replace(rq$RQ10ProbMatch6, !rq$RQ10ProbMatch6 == "Blue cup", 0)
rq$RQ10ProbMatch6 <- replace(rq$RQ10ProbMatch6, rq$RQ10ProbMatch6 == "Blue cup", 1)
rq$RQ10ProbMatch6 <- as.numeric(rq$RQ10ProbMatch6)

rq$RQ10ProbMatch7 <- replace(rq$RQ10ProbMatch7, !rq$RQ10ProbMatch7 == "Blue cup", 0)
rq$RQ10ProbMatch7 <- replace(rq$RQ10ProbMatch7, rq$RQ10ProbMatch7 == "Blue cup", 1)
rq$RQ10ProbMatch7 <- as.numeric(rq$RQ10ProbMatch7)

rq$RQ10ProbMatch8 <- replace(rq$RQ10ProbMatch8, !rq$RQ10ProbMatch8 == "Blue cup", 0)
rq$RQ10ProbMatch8 <- replace(rq$RQ10ProbMatch8, rq$RQ10ProbMatch8 == "Blue cup", 1)
rq$RQ10ProbMatch8 <- as.numeric(rq$RQ10ProbMatch8)

rq$RQ10ProbMatch9 <- replace(rq$RQ10ProbMatch9, !rq$RQ10ProbMatch9 == "Blue cup", 0)
rq$RQ10ProbMatch9 <- replace(rq$RQ10ProbMatch9, rq$RQ10ProbMatch9 == "Blue cup", 1)
rq$RQ10ProbMatch9 <- as.numeric(rq$RQ10ProbMatch9)

rq$RQ10ProbMatch10 <- replace(rq$RQ10ProbMatch10, !rq$RQ10ProbMatch10 == "Blue cup", 0)
rq$RQ10ProbMatch10 <- replace(rq$RQ10ProbMatch10, rq$RQ10ProbMatch10 == "Blue cup", 1)
rq$RQ10ProbMatch10 <- as.numeric(rq$RQ10ProbMatch10)

#Should be made separate and not replace the first value
rq$RQ10ProbMatch1 <- rowSums(rq[,10:19])
rq$RQ10ProbMatch1 <- replace(rq$RQ10ProbMatch1, !rq$RQ10ProbMatch1 == "10", 0)
rq$RQ10ProbMatch1 <- replace(rq$RQ10ProbMatch1, rq$RQ10ProbMatch1 == "10", 1)

rq$RQ11Knave <- replace(rq$RQ11Knave, !rq$RQ11Knave == "Knave", 0)
rq$RQ11Knave <- replace(rq$RQ11Knave, rq$RQ11Knave == "Knave", 1)
rq$RQ11Knave <- as.numeric(rq$RQ11Knave)

rq$RQ12Bus <- replace(rq$RQ12Bus, !rq$RQ12Bus == "Both options are equally likely?", 0)
rq$RQ12Bus <- replace(rq$RQ12Bus, rq$RQ12Bus == "Both options are equally likely?", 1)
rq$RQ12Bus <- as.numeric(rq$RQ12Bus)

rq$RQ13Pig <- replace(rq$RQ13Pig, !rq$RQ13Pig == "20", 0)
rq$RQ13Pig <- replace(rq$RQ13Pig, rq$RQ13Pig == "20", 1)
rq$RQ13Pig <- as.numeric(rq$RQ13Pig)

rq$RQ14Bayesian1 <- replace(rq$RQ14Bayesian1, !rq$RQ14Bayesian1>rq$RQ14Bayesian2, 0)
rq$RQ14Bayesian1 <- replace(rq$RQ14Bayesian1, !rq$RQ14Bayesian1==0,1)
rq$RQ14Bayesian1 <- as.numeric(rq$RQ14Bayesian1)

rq <- rq[c(1,2,3,4,5,6,7,8,9,10,20,21,22,23)]

# Add up the RQ-score
rq <- cbind(rq, rowSums(rq))
colnames(rq)[15] <- "RQSum"

# Seperate heuristic and non-heuristic items
rqh<-rq[c(3,4,5,6,7,13)]
rqnh<-rq[c(1,8,9,11,12,14)]

# Add up the sub-scores seperatly
rqh <- cbind(rqh, rowSums(rqh))
colnames(rqh)[7] <- "RQHSum"
rqnh <- cbind(rqnh, rowSums(rqnh))
colnames(rqnh)[7] <- "RQnHSum"

# Make and export a csv-file
summary <- cbind(rest["ID"], rq, rqh["RQHSum"], rqnh["RQnHSum"], rest["Duration"], rest["Lang"], nfc["NFCSum"], rqdebrief[1], dstntlx, rqntlx)
RQID <- summary[1]
summary <- summary[-1]
rownames(summary) <- RQID$ID

write.csv(summary, file = "Data/summary.csv")


