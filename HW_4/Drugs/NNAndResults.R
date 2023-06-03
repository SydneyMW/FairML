library(fairml)
load("C:/Users/Sydney/Desktop/189G/FairML/drug.consumption.rda")
data <- drug.consumption

# Study only considers heroin, cocaine, crack, and ecstasy use
data <- data[,!names(data) %in% c("Amphet","Amyl","Benzos","Caff","Cannabis","Choc", "Ketamine", "Legalh","LSD","Meth","Mushrooms","Nicotine","Semer","VSA")]


# Neural net used "NoAlcohol" and other features to predict "NoDrugUse"
data$NoDrugUse <- as.factor(data$Heroin == "Never Used" & data$Crack == "Never Used" & data$Coke == "Never Used" & data$Ecstasy == "Never Used")
data$NoAlcohol <- as.factor(data$Alcohol == "Never Used")


# Same as paper; 55% of participants use drugs (can add graph for this)
percentDrugs <- nrow(data[which(data$NoDrugUse == FALSE),])/nrow(data)
percentNoDrugs <- nrow(data[which(data$NoDrugUse == TRUE),])/nrow(data)


# Data for neural net
dataNN <- data[,!names(data) %in% c("Ecstasy","Heroin","Coke","Crack","Alcohol")]
model <- qeNeural(dataNN, yName="NoDrugUse")


# We get 0.71 acc vs. 0.77 in paper
# Graph shows 0.8 acc and 0.74 validation acc?
testAcc <- 1-model$testAcc

# We get 0.55 base acc? Seems wrong?
baseAcc <- 1-model$baseAcc
