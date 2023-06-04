library(patchwork)
library(fairml)
library(corrplot)
load("C:/Users/Sydney/Desktop/189G/FairML/drug.consumption.rda")
data <- drug.consumption

# Study only considers heroin, cocaine, crack, and ecstasy use
data <- data[,!names(data) %in% c("Amphet","Amyl","Benzos","Caff","Cannabis","Choc", "Ketamine", "Legalh","LSD","Meth","Mushrooms","Nicotine","Semer","VSA")]

# Neural net used "NoAlcohol" and other features to predict "NoDrugUse"
data$NoDrugUse <- as.factor(data$Heroin == "Never Used" & data$Crack == "Never Used" & data$Coke == "Never Used" & data$Ecstasy == "Never Used")
data$NoAlcohol <- as.factor(data$Alcohol == "Never Used")

plotColumnFreq <- function(data) {
  isFactor <- sapply(data, is.factor)
  df <- data[,isFactor]
  cont <- data[,!isFactor]
  
  # Add bar plots for categorical variables to evaluate spread
  if (length(colnames(df)) > 0) {
    for (i in 1:length(colnames(df))) {
      col <- colnames(df)[i]
      eval(parse(text=paste("summary",col," <- as.data.frame(summary(df$",col,"))", sep="")))
      eval(parse(text=paste("summary",col,"$x <- row.names(summary",col,")", sep="")))
      eval(parse(text=paste("colnames(summary",col,") <- c('freq','x')", sep="")))
      eval(parse(text=paste("plot",col," <- ggplot(summary",col,", aes(x=x,y=freq,fill=x)) + geom_bar(stat='identity')+ theme(legend.title= element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ggtitle('",col,"')", sep="")))
    } 
  }
  
  # Add histograms for continuous variables to evaluate spread
  if (length(colnames(cont)) > 0) {
    for (i in 1:length(colnames(cont))) {
      col <- colnames(cont)[i]
      eval(parse(text=paste("plot",col," <- ggplot() + aes(cont$",col,") + geom_histogram(binwidth=1, colour='black', fill='salmon') + ggtitle('",col,"')", sep="")))
    }  
  }

  # Call plots in sequence using patchwork to stitch together
  cols <- c(colnames(df), colnames(cont))
  exec <- ""
  for (i in 1:length(cols)) {
    col <- cols[i]
    if (i < length(cols)) {
      exec <- paste(exec, "plot",col," + ",sep="")
    }
    else {
      exec <- paste(exec, "plot",col,sep="")
    }
  }
  eval(parse(text=exec))
}

isFactor <- sapply(data, is.factor)
dataFactor <- data[,isFactor]
dataContinuous <- data[,!isFactor]

# See all categorical variable frequencies
plotColumnFreq(data)
ggsave("AllDataBalance.png", width=30, height=20)

# See categorical only
plotColumnFreq(dataFactor)
ggsave("CatDataBalance.png", width=30, height=20)

# See continuous only
plotColumnFreq(dataContinuous)
ggsave("ContDataBalance.png", width=30, height=20)
