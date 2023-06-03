library(qeML)
library(fairml)
library(dplyr)
library(ggplot2)


conditDisparity <- function(data,yName,sName,xName,condits,qeFtn,
                minS=50,yLim=NULL,useLoess=TRUE) {
  ### Apply conditions to data -------------------------------------------------
  for (i in 1:length(condits)) { 
    condit <- paste("data[which(data$",condits[i],"),]",sep="")
    expr <- parse(text=condit)
    data <- eval(expr)
  } # IT WORKS
  
  ### Train qeFtn on data ------------------------------------------------------
  model <- qeFtn(data,yName)
  
  ### Add minS condition, eliminating sCats with < minS ppl --------------------
  data$xCats <- data[[xName]]
  data$sCats <- data[[sName]]
  data <- data[order(data$xCats),]   # order by xCat column
  
  allSCats <- unique(data[,sName])
  sCats <- factor()
  for (i in 1:length(allSCats)) {    # find sCats with sufficient numbers
    if (nrow(data[which(data$sCats == allSCats[i]),]) > minS) {
      sCats <- c(sCats,allSCats[i])
    }
  }
  
  ### Collect mean predictions for each sCat/xCat combination
  n <- length(sCats)
  allPreds <- vector("list",n)
  allCats <- vector("list",n)
  
  # Subset for each race
  for (i in 1:length(sCats)) {
    s <- sCats[i]
    sSubset <- data[which(data$sCats == s),]
    predList <- c() # collect predictions for race
    catsList <- c() # collect ages corresponding to predictions for race
    
    # Find all ages which appear in a particular race
    xCats <- unique(sSubset$xCats)
    for (j in 1:length(xCats)) {
      xCat <- xCats[j]
      xsSubset <- sSubset[which(sSubset$xCats==xCat),]
      xsSubset <- xsSubset[,!(names(xsSubset) %in% c('sCats','xCats',yName))]
      
      # Collect recidivism prediction for each age in the race group
      if (nrow(xsSubset) > 0) {
        sPreds <- predict(model, xsSubset)
        sPredsList <- sPreds[1,]
        avgSPreds <- mean(sPredsList)
        predList <- c(predList,avgSPreds)
        catsList <- c(catsList,xCat)
      }
    }
    allPreds[[i]] <- predList
    allCats[[i]] <- catsList
  }
  
  ### Plot probabilities -------------------------------------------------------
  # For each s category
  for (i in 1:length(sCats)) {
    eval(parse(text=paste("plotdf",i," <- data.frame(allCats[[",i,"]],allPreds[[",i,"]])",sep="")))
    eval(parse(text=paste("colnames(plotdf",i,") <- c('cats','preds')",sep="")))
  }
  
  # Get list of unique, distinct colors for plot
  colorList <- rainbow(length(sCats))
  
  exec <- "ggplot() + \n"
  for (i in 1:length(sCats)) {
    col <- colorList[i]
    if (i < length(sCats)) {
      exec <- paste(exec,"geom_smooth(data=plotdf",i,",color='",col,
                    "',mapping=aes(x=cats, y=preds),se=F) + \n",sep="")
    } else {
      exec <- paste(exec,"geom_smooth(data=plotdf",i,",color='",col,
                    "',mapping=aes(x=cats, y=preds),se=F) \n",sep="")
    }
  }
  
  #cat(exec)
  eval(parse(text=exec))
}


### Age/Recidivism Example -----------------------------------------------------
data(compas)
compas$two_year_recid <- as.numeric(compas$two_year_recid == 'Yes')

data <- compas
yName <- 'two_year_recid'
sName <- 'race'
xName <- 'age'
condits <- c('priors_count <= 4','decile_score >= 6')
qeFtn <- qeKNN
minS <- 50
yLim <- NULL
useLoess <- TRUE

conditDisparity(data,yName,sName,xName,condits,qeFtn)
