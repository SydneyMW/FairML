numericSimpson <- function(data,xName,yName,zName,numZvals=NULL) {
  library(qeML)
  library(Kendall)
  # Ensure x/y columns are numeric
  data[,xName] <- as.numeric(data[,xName])
  data[,yName] <- as.numeric(data[,yName])
  # Check if z is categorical
  isFactor <- is.factor(data[,zName])
  
  if (is.null(numZvals)) {
    numZvals <- 4
  }
  
  # If z is categorical, partition by categories
  # If z is numeric, partition by numZvals
  if (isFactor) {
    partition <- split(data,data[,zName])
  } else {
    zMin <- min(data[,zName])
    zMax <- max(data[,zName])+0.0001
    rng <- (zMax - zMin) / numZvals
    
    data$zCategory <- data[,zName] %/% rng
    
    # Add range for plot
    for (i in 1:length(data$zCategory)) {
      zCategory <- data$zCategory[i]
      binMin <- zMin + (zCategory*rng)
      binMax <- zMin + ((zCategory+1)*rng)
      range <- paste("[",paste(binMin, binMax, sep=","), ")", sep="")
      data$zRange[i] <- range
    }
    # Partition by category
    partition <- split(data,data$zCategory)
  }
  
  # Create DF to store corr values
  corrDF <- data.frame(matrix(nrow=0, ncol=2))
  colnames(corrDF) <- c("category","corr")
  
  # If both x and y are continuous, use cor()
  # If at least one is dichotomous, use kendall's tau
  if ((length(unique(data[,xName])) > 2) || (length(unique(data[,yName])) > 2)) {
    usePearson <- TRUE
  } else {
    usePearson <- FALSE
  }
  
  # Collect correlation values
  if (usePearson) { # find pearson correlation
    for (i in 1:length(partition)) {
      df <- partition[[i]]
      if (isFactor) {
        category <- df[,zName][1]
      } else {
        category <- df$zRange[1]
      }
      corr <- cor(df[,xName], df[,yName])
      newDF <- data.frame(matrix(nrow=1,ncol=2))
      colnames(newDF) <- c("category","corr")
      newDF$category <- category
      newDF$corr <- corr
      corrDF <- rbind.data.frame(corrDF,newDF)
    }
    unconditionalCorr <- cor(data[,xName],data[,yName])
  } else { # find kendall's tau correlation
    for (i in 1:length(partition)) {
      df <- partition[[i]]
      if (isFactor) {
        category <- df[,zName][1]
      } else {
        category <- df$zRange[1]
      }
      corr <- Kendall(df[,xName], df[,yName])$tau[1]
      newDF <- data.frame(matrix(nrow=1,ncol=2))
      colnames(newDF) <- c("category","corr")
      newDF$category <- category
      newDF$corr <- corr
      corrDF <- rbind.data.frame(corrDF,newDF)
    }
    unconditionalCorr <- Kendall(data[,xName],data[,yName])$tau[1]
  }
  
  corrDF$corr <- as.numeric(corrDF$corr)
  
  # Plot correlation values in barplot
  names <- unique(corrDF$category)
  title <- paste("X = ",xName,", Y = ",yName,", Z = ",zName,sep="")
  xlab <- "Categories of Z"
  ylab <- paste("Z-Stratified Correlation between X and Y")
  sub <- paste("(Unconditional Correlation = ",unconditionalCorr,")", sep="")
  barplot(corrDF$corr, xlab=xlab, ylab=ylab, ylim=c(-1,1), main=title, sub=sub, col.sub="blue", names.arg=names)
  abline(h=unconditionalCorr,col="blue")
  abline(h=0,col="black")
}

numericSimpson(data, xName="Oscore", yName="Heroin", zName="Country")
