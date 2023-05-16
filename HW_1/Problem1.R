takeALookAround <- function(data, yName, sName, maxFeatureSetSize) {
  library(qeML)
  
  # Select columns excluding yName and sName
  featureSet <- colnames(data[,!names(data) %in% c(yName, sName)])
  results <- data.frame(matrix(nrow=0, ncol=4))
  
  # counter for feature set comparisons
  count <- 0
  
  # iterate through unique combinations of features
  for (i in 1:length(featureSet)) {
    for (j in i:length(featureSet)) {
      if (i != j) {
        feature1 <- featureSet[i] # i.e. age
        feature2 <- featureSet[j] # i.e. educ
        
        # a: predicting y from feature set
        data_a <- data[,c(feature1, feature2, yName)]
        # b: predicting y from feature set plus S
        data_b <- data[,c(feature1, feature2, yName, sName)]
        # c: predicting S from feature set
        data_c <- data[,c(feature1, feature2, sName)]
        
        # use qeLin on numeric data to predict y
        # use qeLogit on factor data to predict y
        if (is.numeric(data[1,yName])) {
          acc_a <- qeLin(data_a,yName)$testAcc # without S
          acc_b <- qeLin(data_b,yName)$testAcc # with S
        } else if (is.factor(data[1,yName])) {
          acc_a <- qeLogit(data_a,yName)$testAcc # without S
          acc_b <- qeLogit(data_b,yName)$testAcc # with S
        } else {
          acc_a <- NA # invalid data
          acc_b <- NA # invalid data
        }
        # use qeLin on numeric data to predict S
        # use qeLogit on factor data to predict S
        if (is.numeric(data[1,sName])) {
          acc_c <- qeLin(data_c,sName)$testAcc # predict S
        } else if (is.factor(data[1,sName])) {
          acc_c <- qeLogit(data_c,sName)$testAcc # predict S
        } else {
          acc_c <- NA
        }
        # names of features, accuracy a, b, and c
        result <-c(paste(feature1,feature2,sep=","), acc_a, acc_b, acc_c)
        results <- rbind.data.frame(results, result)
        count <- count+1
        # break if limit met
        if (count == maxFeatureSetSize) break
      }
    }
    # break if limit met
    if (count == maxFeatureSetSize) break
  }
  print(count)
  colnames(results) <- c("features","Y_acc","YS_acc","S_acc")
  # df with names and accuracies
  return(results)
}