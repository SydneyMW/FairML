## EXTRA CREDIT PROBLEM A
## Ethnicity-Expenditure Correlation Reduces when Age-Stratified

#install.packages(c("devtools"))
#devtools::install_github("ldurazo/kaggler")
spExample <- function() {
  library(readr)
  library(kaggler)
  
  # kaggle.json contains login credentials
  kgl_auth(creds_file = 'kaggle.json')
  
  # download california DDS expenditures dataset from kaggle
  response <- kgl_datasets_download_all(owner_dataset = "wduckett/californiaddsexpenditures")
  
  # download zip and unzip
  download.file(response[["url"]], "temp.zip", mode="wb")
  unzip_result <- unzip("temp.zip", overwrite = TRUE)
  
  # read csv and convert to df from multi-format
  dds <- read_csv("californiaDDSDataV2.csv")
  dds <- data.frame(dds)
  
  # convert char columns to factors
  # convert dichotomous column to numeric (binary)
  dds$Ethnicity <- as.factor(dds$Ethnicity)
  dds$GenderBin <- as.numeric(dds$Gender=="Female")
  dds$Age.Cohort <- as.factor(dds$Age.Cohort)
  
  # call numericSimpson to reveal expenditure vs. race, age-stratified
  data <- dds
  xName <- "Expenditures" # continuous numeric
  yName <- "Ethnicity" # continuous numeric
  zName <- "Age.Cohort" # categorical
  numericSimpson(data,xName,yName,zName,numZvals=NULL)
}