library(fairml)
library(corrplot)
library(vcd)
library(mltools)
library(tidyverse)
library(fastDummies)
load("C:/Users/Sydney/Desktop/189G/FairML/drug.consumption.rda")
data <- drug.consumption

# Study only considers heroin, cocaine, crack, and ecstasy use
data <- data[,!names(data) %in% c("Amphet","Amyl","Benzos","Caff","Cannabis","Choc", "Ketamine", "Legalh","LSD","Meth","Mushrooms","Nicotine","Semer","VSA")]

# Neural net used "NoAlcohol" and other features to predict "NoDrugUse"
data$NoDrugUse <- as.numeric(data$Heroin == "Never Used" & data$Crack == "Never Used" & data$Coke == "Never Used" & data$Ecstasy == "Never Used")
data$NoAlcohol <- as.numeric(data$Alcohol == "Never Used")
dataCorr <- data[,!names(data) %in% c("Ecstasy","Heroin","Coke","Crack","Alcohol")]

corrMatrix <- matrix(ncol = length(dataCorr), nrow=length(dataCorr), dimnames=list(names(dataCorr), names(dataCorr)))
for (row in seq(nrow(corrMatrix))) {
  for (col in seq(ncol(corrMatrix))) {
    rowName <- names(dataCorr)[row]
    colName <- names(dataCorr)[col]
    bothNumeric <- is.numeric(data[[rowName]]) && is.numeric(data[[colName]])
    if (bothNumeric) {
      # If both numeric, use correlation
      corrMatrix[[row,col]] <- cor(data[[rowName]], data[[colName]])
    } else {
      # If one or more non-numeric factors, use Cramer's V Associativity
      corrMatrix[[row, col]] <- assocstats(table(data[[row]], data[[col]]))$cramer
    }
  }
}

# HEATMAP - Look at correlation or association of all variables
corrplot(corrMatrix, method="circle", type='lower', diag=FALSE, tl.col="black")
corrplot(corrMatrix, method="pie", type='lower', diag=FALSE, tl.col="black")
corrplot(corrMatrix, method="color", type='lower', diag=FALSE, tl.col="black")
corrplot(corrMatrix, method="color", type='lower', outline=TRUE, tl.col="black")
corrplot(corrMatrix, order = 'hclust', tl.col="black", outline=TRUE, type="lower")
corrplot(corrMatrix, order = 'hclust', tl.col="black")
corrplot.mixed(corrMatrix, upper="pie", number.cex = .8, tl.cex=0.8, tl.col = "black", tl.pos ="lt")
corrplot.mixed(corrMatrix, lower="shade", upper="pie", number.cex = .8, tl.cex=0.8, tl.col = "black", tl.pos ="lt")

# One hot encoding
#corrplot.mixed(corrMatrix, lower="color", upper="pie")
#corrplot.mixed(corrMatrix, lower='color', upper='pie', order='hclust')
#oneHot <- one_hot(as.data.table(data))
#corrplot(cor(oneHot))

# BARPLOT - Looking at associativity of all variables with NoDrugUse
VarEffects <- data.frame(matrix(ncol = 1, nrow=length(dataCorr), dimnames=list(names(dataCorr), "DrugUseCorr")))
for (row in 1:nrow(VarEffects)) {
  rowName <- names(dataCorr)[row]
  VarEffects$DrugUseCorr[row] <- assocstats(table(dataCorr[[row]], data$NoDrugUse))$cramer
}
VarEffects$Var <- row.names(VarEffects)
colnames(VarEffects) <- c("Corr","Var")
VarEffects <- VarEffects[order(VarEffects$Corr),]
VarEffects <- VarEffects[which(VarEffects$Var != "NoDrugUse"),]
ggplot(VarEffects, aes(x=fct_inorder(Var),y=Corr,fill=Corr)) + geom_bar(stat="identity") + ggtitle("Carson's V Association of Variables with No Drug Use") + xlab("Variable") + ylab("Associativity") + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 65, vjust = 1, hjust=1))

ggsave("CarsonVNoDummies.png", width=7, height=5)

# BARPLOT - Looking at correlation for dummy variables with NoDrugUse
dataDummy <- dummy_cols(dataCorr, remove_selected_columns = TRUE)

VarEffects <- data.frame(matrix(ncol = 1, nrow=length(dataDummy), dimnames=list(names(dataDummy), "DrugUseCorr")))
for (row in 1:nrow(VarEffects)) {
  rowName <- names(dataDummy)[row]
  VarEffects$DrugUseCorr[row] <- assocstats(table(dataDummy[[row]], data$NoDrugUse))$cramer
}
VarEffects$Var <- row.names(VarEffects)
colnames(VarEffects) <- c("Corr","Var")

# Sort by corr values
VarEffects <- VarEffects[order(VarEffects$Corr),]
VarEffects <- VarEffects[which(VarEffects$Var != "NoDrugUse"),]
ggplot(VarEffects, aes(x=fct_inorder(Var),y=Corr,fill=Corr)) + geom_bar(stat="identity") + ggtitle("Carson's V Association of Variables with No Drug Use") + xlab("Variable") + ylab("Associativity") + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 65, vjust = 1, hjust=1))

ggsave("CarsonVInOrder.png", width=10, height=8)

# Group dummy vars by original categories
VarEffects <- VarEffects[order(VarEffects$Var, decreasing=FALSE),]
ggplot(VarEffects, aes(x=fct_inorder(Var),y=Corr,fill=Corr)) + geom_bar(stat="identity") + ggtitle("Carson's V Association of Variables with No Drug Use") + xlab("Variable") + ylab("Associativity") + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 65, vjust = 1, hjust=1))

ggsave("CarsonVGrouped.png", width=10, height=8)
