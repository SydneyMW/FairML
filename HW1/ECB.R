## EXTRA CREDIT PROBLEM B
## Create factor to represent two factors

superFactor <- function(f1,f2) {
  f3 <- as.factor(paste(f1,f2,sep="_"))
  return (f3)
}