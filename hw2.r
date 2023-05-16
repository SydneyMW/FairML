library(qeML)
library(fairml)
library(regtools)
data(law.school.admissions)


#Usage for
Ncut <- function(data, Name, numCuts=2, max = NULL, min = NULL){
  if (!is.numeric(data[,Name])){
    stop("data must be numeric, call form: cutN(data, categoryName, numCuts (MIN 2), max (OPT), min (OPT)")
  }
  df <- data[order(data[,Name]),,drop=FALSE]  # order by wkswrkd, keep original row indices to sort later
  size <- length(df[,Name])                   # numrows = 20090
  top <- df[size, Name]
                                              #Highest possible value of the category
  if (is.null(max)){
    intervalSize <- floor(top/ numCuts)       # interval size 13 for splitting min:max 0:52 wkswrkd into 4 categories
  } else{
    intervalSize <- (max-min)/numCuts
  }
  iVal <- intervalSize
  
  #Incase a user wants to specify categories on categorical variables, that aren't labeled as categorcial
  #i.e. inc on the ucla data set, this flag portion allows for that with correct intervals
  val <- which.min(abs(df[,Name] - iVal))
  flag <- FALSE
  if (val != 1){
    cutPoints <- c(1)
  } else{
    flag <- TRUE
    cutPoints <- c()                          #Prevents the first interval being from the first occurence of 1 to the first occurance of 1
  }
  
  for (i in 1:numCuts){                       # 1:4 groups of wkswrkd
    val <- match(iVal, df[,Name])             # find group cutoff (i.e. 13) index
    if (is.na(val)){                          # if no match, find closest neighbor index (min)
      val <- which.min(abs(df[,Name] - iVal)) 
    }
    if (i == numCuts){
      if (flag){                               #If the flag is true, this means the data is categorical in nature, but not in practice
        cutPoints <- append(cutPoints, val)    #Without this, the list would contain intervals 1-2, 2-3, 3-4, 4-end (we want 4-5, 5-end)
      }                                        # for last group: add last row index to index list, exit
      cutPoints <- append(cutPoints, size)
      break
    }
    cutPoints <- append(cutPoints, val)       # add last row index to index list
    iVal <- iVal + intervalSize               # i.e. 13+13 -> 26+13 -> 39+13 -> 52
  }                                           # print indices of cutoffs
  df$newFactors <- ""
  retPoints <- c()
  for (i in 1:numCuts) {
    if (is.null(max)){
      first <- df[cutPoints[i], Name]         # min of range (i.e. 13 wkswrkd)
      if (top != numCuts){
        second <-df[cutPoints[i+1], Name]
      } else{
        second <- df[cutPoints[i], Name]
      }
    }
    else{
      first <- min+(intervalSize*i-1)
      second <- min+(intervalSize*i)
    }
    
    # max of range (i.e. 26 wkswrkd)
    label <- paste("(", first, ", ", second, "]", sep = "") # i.e. "(13, 26]"
    indexMin <- cutPoints[i]                  # index of min (i.e. 1643)
    indexMax <- cutPoints[i+1]
    retPoints <- append(retPoints, df[cutPoints[i], Name])
                                               # index of max (i.e. 2310)
    df$newFactors[indexMin:indexMax] <- label # add range label to new column
  }
  if (!flag){
    retPoints <- append(retPoints, df[cutPoints[i+1], Name])
  }
  df$newFactors <- as.factor(df$newFactors)   # convert to factor
  #print(unique(df$newFactors))
  df <- df[order(as.numeric(row.names(df))),] # sort using original order
  attr(df$newFactors, "CutPoints") <- retPoints #attach cut points attribute
  return(df$newFactors)
}


relativeProxy <- function(data,proxyName,catName,nQuantls,nCut=NULL) {
  if (is.null(nCut)){
    data$catCategory <- data[,catName]
  } else{
    data$catCategory <- Ncut(data,catName, numCuts=nCut)
  }
  # Create S3 list of dataframes split by category levels
  catLevels <- levels(data$catCategory)
  quantls <- split(data, data$catCategory,drop = FALSE)
  class(quantls) <- 'relProx'
  subset <- quantls[[1]]
  
  #Attribute to help keep track of the different quantiles
  quantPoints <- data.frame()
    
  # In each category, find relative percentiles and quantiles for proxy variable
  for (i in 1:length(quantls)) {
    # Isolate dataframe for a particular level
    subset <- quantls[[i]]
    
    # Calculate percentiles for each proxy value
    if (is.factor(subset[,proxyName])){
    
      subset$proxyCategory <- levels(subset[,proxyName])
    }
    else{
      subset$proxyPercentile <- rank(subset[,proxyName])/nrow(subset)*100
      # Split proxy into quantiles by percentile
      
      subset$proxyCategory <- Ncut(subset,"proxyPercentile", numCuts=nQuantls)
    }
    temp <- c(attr(subset$proxyCategory, "CutPoints"))
    quantPoints <- rbind(quantPoints, temp)
    # Re-assign dataframe with new columns
    quantls[[i]] <- subset
  }
  quantls$cutPoints <- attr(data$catCategory, "CutPoints")
  quantls$quantPoints <- quantPoints
  return(quantls)
}


#Finds where a new variable will fit into a given category
findCategory <- function(relProxObj, newCase, cuts, category = NULL){
  cutPoints <- relProxObj[[cuts]]
  
  if (!is.null(category)){
    cutPoints <- cutPoints[category,]
  }
  size <- length(cutPoints)

  #Smaller than any of the cut points, so belongs to the first category
  if (newCase < cutPoints[1]){
    return(0)
  }
  #larger than any of the cut points, so belongs to last category
  if (newCase > cutPoints[size]){
    return(size)
  }
  for (i in 1:(size-1)){
    if (newCase == cutPoints[i]){
      return(i)
    }
    if (newCase == cutPoints[i+1]){
      return(i+1)
    }
    if (newCase > cutPoints[i] && newCase < cutPoints[i+1]){
      return(i)
    }
  }
}

#returns a factor of the range of quantiles
relativeVal <- function(relProxObj, category, proxyName, newCase){
  catSize <- nrow(relProxObj[[category]])
  quantPoints <- relProxObj$quantPoints
  size <- length(quantPoints)
  
  proxyCat <- findCategory(relProxObj, newCase, length(relProxObj), category)
  if (category != size){
    proxyCat <- proxyCat + 1
  }
  min <- quantPoints[category, proxyCat-1]
  max <- quantPoints[category, proxyCat]
  label <- paste("(", min, ", ", max, "]", sep="")
  ret <- as.factor(label)
  return(ret)
}


predict.relProx <- function(relProxObj, newCases, proxyName, catName){
  #Last two items are cutpoints, don't include in the merge
  size <- length(relProxObj)
  quantls <- relProxObj[-(size-1): -size]
  mergedData <- quantls[[1]]
  for (i in 2:length(quantls)){
    mergedData <- rbind(mergedData, quantls[[i]])
  }
  
  # Training data! 
  # This section will need to be edited for testing of other datasets/variables)
  ##############################################################################
  yName <- 'bar'
  adjustedData <- mergedData
  adjustedData$lsat <- adjustedData$proxyCategory
  
  # We include only these variables, will have to adjust for other data sets?
  adjustedData <- adjustedData[,c('lsat', 'ugpa', 'cluster', 'bar')]
  ##############################################################################
  
  model <- qeKNN(data = adjustedData, yName = yName)
  
  #Run new case
  newCaseCats <- c()
  for (i in 1:nrow(newCases)){
    case <- newCases[i,]
    caseCat <- case[,catName]
    caseProxy <- case[,proxyName]
    
    #get the category for which this data belongs (e.g. on income)
    category <- findCategory(relProxObj, caseCat, size-1)
    df <- relProxObj[[category]]
    
    #Calculate the percentile of new data
    proxyList <- c(df[,proxyName], caseProxy)
    proxyListLen <- length(proxyList)
    newCasePercentile <- (rank(proxyList)/proxyListLen*100)[proxyListLen]
    
    #Find relative value
    relVal <- relativeVal(relProxObj, category, proxyName, newCasePercentile)
    case$lsat <- relVal
    
    #Remove fam_inc before prediction
    case <- case[,-1]
    
    caseCat <- predict(model, case)$predClasses[1,1]
    newCaseCats <- c(caseCat, newCaseCats)
    
  }
 return(newCaseCats)
}

charToNum <- function(array){
  ret <- c()
  for (i in 1:length(array)){
    if (array[i] == "TRUE"){
      ret <- c(as.factor('TRUE'), ret)
    }else{
      ret <- c(as.factor('FALSE'), ret)
    }
  }
  return(ret)
}


# Test on lsat/fam_inc example
data <- law.school.admissions               # data
proxyName <- 'lsat'                         # proxy (i.e. wkswrkd, SAT score)
catName <- 'fam_inc'                        # category (i.e. age, income)
nQuantls <- 5                               # split 'wkswrkd' into two high/low groups
nCut <- 5                                   # split 'age' into four groups

yName <- 'bar'
data1 <- data[,c('lsat', 'ugpa', 'cluster', 'bar')] # use to predict with regular lsat
data2 <- data[,c('lsat', 'ugpa', 'cluster', 'bar')] # 'bar' to be replaced with relprox predictions in newCases subset
data3 <- data[,c('ugpa', 'cluster', 'bar')]         # 

### generate new cases
newCases <- data[sample(nrow(data), size = 1800), !names(data) %in% c(yName)]
newCases <- newCases[,c('fam_inc', 'lsat', 'ugpa', 'cluster')]

### get quantiles
quantls <- relativeProxy(data,proxyName,catName,nQuantls,nCut)

### Run relprox's predict, which will train qeKNN on quantls and predict bar for newCases
barVals <- predict.relProx(quantls, newCases, proxyName, catName)
newCases$bar <- barVals
corrTests <- charToNum(barVals)
data2[rownames(newCases), 'bar'] <- corrTests
###


# Test Accuracy - running qeKNN 100 times to predict "bar"
wLSATAcc <- replicMeans(100, "qeKNN(data1, 'bar')$testAcc")
qLSATAcc <- replicMeans(100, "qeKNN(data2, 'bar')$testAcc")
nLSATAcc <- replicMeans(100, "qeKNN(data2, 'bar')$testAcc")

#Predict 1 - run qeKNN directly on data sample, original lsat
###
newCase1und <- data[sample(nrow(data), size = 1800), !names(data) %in% c(yName)]
newCase1 <- newCase1und[,c('lsat', 'ugpa', 'cluster')]
wLSATmodel <- qeKNN(data1, 'bar')
wLSATpreds <- c()
for (i in 1:nrow(newCase1)){
  wLSATpred <- predict(wLSATmodel,newCase1[i,])$predClasses[1,1]
  wLSATpreds <- c(wLSATpred, wLSATpreds)
}
fam_inc1 <- newCase1und$fam_inc
bar1 <- charToNum(wLSATpreds)

#Predict 3 - run qeKNN on data sample without lsat
###
newCase3und <- data[sample(nrow(data), size = 1800), !names(data) %in% c(yName)]
newCase3 <- newCase3und[,c('ugpa', 'cluster')]
nLSATmodel <- qeKNN(data3, 'bar')
nLSATpreds <- c()
for (i in 1:nrow(newCase3)){
  nLSATpred <- predict(nLSATmodel,newCase3[i,])$predClasses[1,1]
  nLSATpreds <- c(nLSATpred, nLSATpreds)
}
print(nLSATpreds)
fam_inc3 <- newCase3und$fam_inc
bar3[length(bar3)] <- 'FALSE'
bar3 <- charToNum(nLSATpreds)


#Test Fairness
wLSATCorr <- cor(fam_inc1, as.numeric(bar1), method = 'kendall')

quantCorr <- cor(newCases$fam_inc,as.numeric(corrTests), method = 'kendall')

#Note that with qeKNN, this is an inaccurate measure. SD is 0.
nLSATCorr <- cor(fam_inc3, as.numeric(bar3), method = 'kendall')



#########OutPut#############
cat(
  paste(
    "=====Utility=====\nWith LSAT: ",
    wLSATAcc,
    "\nUsing Quantiles: ",
    qLSATAcc,
    "\nWithout LSAT: ",
    nLSATAcc,
    "\n=====Fairness=====\nWith LSAT: ",
    wLSATCorr,
    "\nUsing Quantiles: ",
    quantCorr,
    "\nWithout LSAT: ",
    nLSATCorr
  )
)
