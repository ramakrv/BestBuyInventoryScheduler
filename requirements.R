# Libraries required for the analysis
listOfPackages <- c("pastecs","httr","datasets","forecast","stats","openxlsx",
                    "Hmisc","DT","data.table","zoo","reshape2","ggplot2","nnet","tseries",
                    "astsa","xgboost","keras","prophet","dplyr","glmnet","caret")


for (i in listOfPackages){
  if(!require(i,character.only = TRUE)){
    print(paste(i, "package is not found .. Installing ... !!!!"))
    install.packages(i,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
  else{
    suppressMessages(library(i,character.only=TRUE))
  }

}
print("=============================================================================")
print("==============================Libraries Loaded===============================")
print("=============================================================================")
