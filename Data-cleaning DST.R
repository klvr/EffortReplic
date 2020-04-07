#Data-cleaning DST-Main and SwitchCost

#Get the R.matlab package to read Excel-documents (if not installed), and load the package
pakke <- library()
if (sum(grepl("R.matlab", pakke$results)) < 1) {
  install.packages("R.matlab", dependencies = TRUE)
  library(R.matlab)
} else {
  library(R.matlab)
}

#Main task
ParticipantSummary <- row.names(c("ID", "LDPOverall", "LDPBlock1", "LDPBlock2", "LDPBlock3", "LDPBlock4", "LDPBlock5", "LDPBlock6", "LDPBlock7", "LDPBlock8", "Accuracy", "MeanRT", "MedianRT", "MeanCRT", "MedianCRT"))
ParticipantSummary <- as.data.frame(ParticipantSummary)
path <- "/Users/klevjer/R Projects/EffortReplic/Data/DST-Data/MainTask/"
pattern <- "*.mat"
MainFiles <- list.files(path = path, pattern = pattern)
FilesPath <- paste(path, MainFiles, sep = "")
for (i in FilesPath) {
  matfil <- readMat(i)
  ID <- matfil$dataHeader
  ID <- unlist(ID[4])
  matdata <- matfil$data
  BlockNumber <- unlist(matdata[1])
  TrialNumber <- unlist(matdata[2])
  ChoiceRT <- unlist(matdata[6])
  Choice <- unlist(matdata[7])
  TargetColor <- unlist(matdata[8])
  TargetDigit <- unlist(matdata[9])
  TargetRT <- unlist(matdata[11])
  TargetAcc <- unlist(matdata[13])
  AllTrials <- cbind(BlockNumber, TrialNumber, ChoiceRT, Choice, TargetColor, TargetDigit, TargetRT, TargetAcc)
  AllTrials <- as.data.frame(AllTrials)
  nTrials <- length(AllTrials[1])
  LDPOverall <- (mean(Choice - 1))
  LDPBlock1 <- (mean(Choice[1:(max(TrialNumber))] -1))
  LDPBlock2 <- (mean(Choice[(max(TrialNumber)*1):(max(TrialNumber)*2)] -1))
  LDPBlock3 <- (mean(Choice[(max(TrialNumber)*2):(max(TrialNumber)*3)] -1))
  LDPBlock4 <- (mean(Choice[(max(TrialNumber)*3):(max(TrialNumber)*4)] -1))
  LDPBlock5 <- (mean(Choice[(max(TrialNumber)*4):(max(TrialNumber)*5)] -1))
  LDPBlock6 <- (mean(Choice[(max(TrialNumber)*5):(max(TrialNumber)*6)] -1))
  LDPBlock7 <- (mean(Choice[(max(TrialNumber)*6):(max(TrialNumber)*7)] -1))
  LDPBlock8 <- (mean(Choice[(max(TrialNumber)*7):(max(TrialNumber)*8)] -1))
  Accuracy <- mean(TargetAcc)
  MeanRT <- mean(TargetRT)
  MedianRT <- median(TargetRT)
  MeanCRT <- mean(ChoiceRT)
  MedianCRT <- median(ChoiceRT)
  Participant <- cbind(ID, LDPOverall, LDPBlock1, LDPBlock2, LDPBlock3, LDPBlock4, LDPBlock5, LDPBlock6, LDPBlock7, LDPBlock8, Accuracy, MeanRT, MedianRT, MeanCRT, MedianCRT)
  ParticipantSummary <- rbind(ParticipantSummary, Participant)
}

pathsc <- "/Users/klevjer/R Projects/EffortReplic/Data/DST-Data/SwitchCost/"
patternsc <- "*.mat"
SCFiles <- list.files(path = pathsc, pattern = patternsc)
SCFilesPath <- paste(pathsc, SCFiles, sep = "")
SwitchCost <- row.names(c("tom"))
SwitchCost <- as.data.frame(SwitchCost)
RepTime <- row.names(c("tom"))
RepTime <- as.data.frame(RepTime)
SwitchTime <- row.names(c("tom"))
SwitchTime <- as.data.frame(SwitchTime)
for (i in SCFilesPath) {
  scmatfil <- readMat(i)
  scmatdata <- scmatfil$data
  SCBlockNumber <- unlist(scmatdata[1])
  SCTrialNumber <- unlist(scmatdata[2])
  SCTargetColor <- unlist(scmatdata[3])
  SCTargetDigit <- unlist(scmatdata[4])
  SCTargetRT <- unlist(scmatdata[7])
  SCTargetAcc <- unlist(scmatdata[9])
  SCAllTrials <- cbind(SCBlockNumber, SCTrialNumber, SCTargetColor, SCTargetDigit, SCTargetRT, SCTargetAcc)
  SCAllTrials <- as.data.frame(SCAllTrials)
  SwOrRp <- seq(from = 2, to=length(SCTargetColor), by=1)
  logical <- row.names(c("tom"))
  logical <- as.data.frame(logical)
  for (v in SwOrRp) {
    latest <- (SCTargetColor[v-1] == SCTargetColor[v])
    logical <- rbind(logical, latest)
  }
  Repeat <- rbind(FALSE, logical)
  Repname <- "Repeat"
  colnames(Repeat) <- Repname
  SCAllTrials <- cbind(SCAllTrials, Repeat)
  SCRepeatRT <- median(SCTargetRT[SCAllTrials$Repeat == TRUE])
  SCSwitchRT <- median(SCTargetRT[SCAllTrials$Repeat == FALSE])
  SwitchCostParticipant <- (SCSwitchRT - SCRepeatRT)
  SwitchCost <- rbind(SwitchCost, SwitchCostParticipant)
  RepTime <- rbind(RepTime, SCRepeatRT)
  SwitchTime <- rbind(SwitchTime, SCSwitchRT)
  Costs <- cbind(SwitchCost, RepTime, SwitchTime)
  costname <- c("SwitchCost", "RepeatTime", "SwitchTime")
  colnames(Costs) <- costname
}
IDTemp <- list.files(path = pathsc, pattern = patternsc)
IDT <- sapply(strsplit(IDTemp, split='.mat', fixed=TRUE), function(x) (x[1]))
IDSC <- sapply(strsplit(IDT, split='Cost_', fixed=TRUE), function(x) (x[2]))
row.names(Costs) <- IDSC

#Some ID's were typed in wrong (2), and thus the ID as collected in the Matlab file is not correct in two instances, the filenames however are correct, so extracting from those
ParticipantSummary <- ParticipantSummary[-1]
IDTemp <- list.files(path = path, pattern = pattern)
IDT <- sapply(strsplit(IDTemp, split='_1.mat', fixed=TRUE), function(x) (x[1]))
IDFixed <- sapply(strsplit(IDT, split='data_s', fixed=TRUE), function(x) (x[2]))
IDFixed <- sapply(strsplit(IDFixed, split="_", fixed=TRUE), function(x) (x[1]))
row.names(ParticipantSummary) <- IDFixed

CompleteDST <- merge(ParticipantSummary, Costs, all = TRUE, by = 'row.names')
CompleteDSTID <- CompleteDST[1]
CompleteDST <- CompleteDST[-1]
row.names(CompleteDST) <- CompleteDSTID$Row.names

write.csv(CompleteDST, file = "Data/DSTsummary.csv")

