
ForecastForNextDay <- function(totaldata,DMNumberOfLags,DMcovariates,i,columnName,model){
  # function to generate next day forecast for the given data
  # Input: INput Data, Number of Lag variables, Covariates, rownumber of forecast day, which model output column, model file 
  # Output : Forecast values for the TestForecastDays for the given model and given column.
  
  
  for(j in 1:DMNumberOfLags) {
    totaldata[,paste("Lag_",j,sep="")] = Lag(totaldata[,paste(columnName,sep="")],j)
  } 
  Predictions = predict(model,totaldata[i,DMcovariates] )
  totaldata[i,paste(columnName,sep="")] <- Predictions
  return(totaldata)
}


FitGlmnet <- function (totaldata,myTimeControl,tuneLength.num,LagFormula){
  # Function to train the GLMNET model
  # Input : Data, control parameters for training, tuning length
  # Output : Model file
  eval(parse(text = paste("glmnet.mod <- train(glmnetOrders ~ BlackFridayWeeks+WinterSeason+HolidayWeek+Month+MDay+",LagFormula, ",data = totaldata[totaldata$Type == 'Actual',],method = 'glmnet', family = 'gaussian',trControl = myTimeControl,tuneLength=tuneLength.num)")))
  return(glmnet.mod)
}


FitGlmnet.pois <- function (totaldata,myTimeControl,tuneLength.num,LagFormula){
  # Function to train the GLMNET and Poisson family model
  # Input : Data, control parameters for training, tuning length
  # Output : Model file
  totaldata$glmnet.poisOrders <- totaldata$Orders
  eval(parse(text = paste("pois.mod <- train(glmnet.poisOrders ~ BlackFridayWeeks+WinterSeason+HolidayWeek+Month+MDay+",LagFormula, ",data = totaldata[totaldata$Type == 'Actual',],method = 'glmnet', family = 'poisson',trControl = myTimeControl,tuneLength=tuneLength.num)")))
  return(pois.mod)
}


Fitearth <- function (totaldata,myTimeControl,tuneLength.num,LagFormula){
  # Function to train the earth model
  # Input : Data, control parameters for training, tuning length
  # Output : Model file
  totaldata$earthOrders <- totaldata$Orders
  eval(parse(text = paste("earth.mod <- train(earthOrders ~ BlackFridayWeeks+WinterSeason+HolidayWeek+Month+MDay+",LagFormula, ",data = totaldata[totaldata$Type == 'Actual',],method = 'earth',trControl = myTimeControl,tuneLength=tuneLength.num)")))
  
  return(earth.mod)
}



FitRpart <- function (totaldata,myTimeControl,tuneLength.num,LagFormula){
  # Function to train the rpart model
  # Input : Data, control parameters for training, tuning length
  # Output : Model file
  totaldata$rpartOrders <- totaldata$Orders
  eval(parse(text = paste("rpart.mod <- train(rpartOrders ~ BlackFridayWeeks+WinterSeason+HolidayWeek+Month+MDay+",LagFormula, ",data = totaldata[totaldata$Type == 'Actual',],method = 'rpart',trControl = myTimeControl,tuneLength=tuneLength.num)")))
  
  return(rpart.mod)
}

FitParty<- function (totaldata,myTimeControl,tuneLength.num,LagFormula){
  # Function to train the party model
  # Input : Data, control parameters for training, tuning length
  # Output : Model file
  totaldata$partyOrders <- totaldata$Orders
  eval(parse(text = paste("party.mod <- train(partyOrders ~ BlackFridayWeeks+WinterSeason+HolidayWeek+Month+MDay+",LagFormula, ",data = totaldata[totaldata$Type == 'Actual',],method = 'ctree',trControl = myTimeControl,tuneLength=tuneLength.num)")))
  return(party.mod)
}

Fitrf <- function (totaldata,myTimeControl,tuneLength.num,LagFormula){
  # Function to train the Random Forecast model
  # Input : Data, control parameters for training, tuning length
  # Output : Model file
  totaldata$rfOrders <- totaldata$Orders
  eval(parse(text = paste("rf.mod <- train(rfOrders ~ BlackFridayWeeks+WinterSeason+HolidayWeek+Month+MDay+",LagFormula, ",data = totaldata[totaldata$Type == 'Actual',],method = 'rf',trControl = myTimeControl,tuneLength=tuneLength.num)")))
  
  return(rf.mod)
}

FitProphet <- function (totaldata,Product,HolidayList,TestForecastUnits,dirpath){
  
  # Function to train the Prophet model
  # Since prophet requires data to be in particular format, had to preprocessing again.
  # Input : Data, Product Name, Holiday List, TestForecastUnits, Directory path to save output
  # Output : data with prophet results
  for (i in unique(HolidayList$EventType)){
    eval(parse(text = paste(i, "<- data.frame(holiday=character(),ds=character(),lower_window=numeric(),Upper_window=numeric())", sep = '')))
  }
  for (i in 1:nrow(HolidayList)){
    eval(parse(text=paste("data1 = ",HolidayList$EventType[i])))
    eval(parse(text=paste("data2 = tibble(holiday = '",HolidayList$EventType[i],"',ds = seq(as.Date('",HolidayList$From[i],"'), as.Date('",HolidayList$To[i],"'),by='week'),lower_window = 0,upper_window = 1)", sep = '')))
    eval(parse(text = paste(HolidayList$EventType[i]," <- rbind(data1,data2)",sep="")))
  }
  tempstring = ""
  for(i in unique(HolidayList$EventType)){
    tempstring = paste(tempstring,",",i,sep="")
  }
  tempstring = substring(tempstring,2,nchar(tempstring))
  eval(parse(text = paste("holidays <- bind_rows(",tempstring,")",sep='')))
  tempdata1 = totaldata[totaldata$Type =="Actual",c("Date","Orders")]
  colnames(tempdata1) = c("ds","y")
  totaldata$Prophet = totaldata$Orders
  future <- data.frame(ds= seq(min(totaldata$Date[totaldata$Type=="Actual"]), max(totaldata$Date[totaldata$Type=="Actual"])+(7*TestForecastUnits), by="week"))
  m <- prophet(holidays = holidays)
  m <- add_country_holidays(m, country_name = 'CA')
  m <- fit.prophet(m, tempdata1)
  forecast <- predict(m, future)
  totaldata$Prophet[totaldata$Type=="Forecast"]= forecast$yhat
  plot(m,forecast)
  prophet_plot_components(m,forecast)
  forecasttrendPlot(totaldata,x = "Date",y = "Orders",TrendTitle =paste(" PROPHET - Trend and Forecast Line for Product",Product,sep=""),plotfilename = "Prophet.png",bycolor = "Type",dirpath)
  
  return(totaldata)
}

FitETS <- function(totaldata,Product,TestForecastUnits,dirpath){
  # Function to train the ETS model
  # Input : Data, Product Name, TestForecastUnits, Directory path to save output
  # Output : data with ETS results
  etsreg = ets(totaldata$Orders[totaldata$Type =="Actual"],model = "ZZZ",damped = NULL,lambda = "auto")
  predict_dataETS <- as.data.frame(forecast(etsreg,TestForecastUnits))
  totaldata$Orders[totaldata$Type=="Forecast"] = predict_dataETS$`Point Forecast`
  
  forecasttrendPlot(totaldata,x = "Date",y = "Orders",TrendTitle =paste("ETS - Trend and Forecast Line for Product",Product,sep=""),plotfilename = "ETS.png",bycolor = "Type", dirpath)
  totaldata$ETS <- totaldata$Orders
  return(totaldata)
  
}


FitNNET <- function(totaldata,Product,TestForecastUnits,dirpath){
  ## Neural Net FOrecast  for Non Linear Patterns
  # Function to train the NNET model
  # Input : Data, Product Name, TestForecastUnits, Directory path to save output
  # Output : data with results
  
  nnetdata = totaldata$Orders[totaldata$Type=="Actual"] + 1
  #nnetdata = log(nnetdata)
  fitreg = nnetar(nnetdata)
  predict_datannet <- as.data.frame(forecast(fitreg,TestForecastUnits))
  #predict_datannet$`Point Forecast` <- exp(predict_datannet$`Point Forecast`)
  totaldata$Orders[totaldata$Type=="Forecast"] = predict_datannet$`Point Forecast`
  forecasttrendPlot(totaldata,x = "Date",y = "Orders",TrendTitle =paste("Neural Net - Trend and Forecast Line for Series for product",Product,sep=""),plotfilename = "NNET.png",bycolor = "Type", dirpath)
  totaldata$NNET <- totaldata$Orders
  return(totaldata)
}



FitXGBoost <- function(totaldata,Product,TestForecastUnits,dirpath){
  ## XGBoost
  # Function to train the XGBoost model
  # Input : Data, Product Name, TestForecastUnits, Directory path to save output
  # Output : data with results
  xgtotaldata <- totaldata[,c("Date","Type","Orders","BlackFridayWeeks","WinterSeason","HolidayWeek","BackToSchool","Spring")]
  xgtotaldata$Orders[xgtotaldata$Type == "Forecast"] = 0
  xgtotaldata$WeekDay <- as.POSIXlt(as.Date(xgtotaldata$Date))$wday
  xgtotaldata$MDay <- mday(as.Date(xgtotaldata$Date))
  xgtotaldata$Month = month(xgtotaldata$Date)
  AvgxgtotaldataOrders = mean(xgtotaldata$Orders[xgtotaldata$Type=="Actual"])
  xgtotaldata$Orders = xgtotaldata$Orders-AvgxgtotaldataOrders
  #xgtotaldata$Year  = year(xgtotaldata$Date)
  training_data = xgtotaldata[xgtotaldata$Type=="Actual",]
  test_data = xgtotaldata[xgtotaldata$Type=="Forecast",]
  
  # Split data training and test data into X and Y
  X_train = data.matrix(training_data[,c("BlackFridayWeeks","WinterSeason" ,"HolidayWeek","BackToSchool","Spring" ,"WeekDay","MDay" ,"Month")])
  Y_train = data.matrix(training_data[,"Orders"])
  class(X_train)[1]
  class(Y_train)
  
  X_test = data.matrix(test_data[,c("BlackFridayWeeks","WinterSeason" ,"HolidayWeek","BackToSchool","Spring" ,"WeekDay","MDay" ,"Month")])
  Y_test = data.matrix(test_data[,"Orders"])
  class(X_test)[1]
  class(Y_test)
  
  
  # Train the xgboost model using the "xgboost" function
  dtrain = xgb.DMatrix(data = X_train, label = Y_train)
  xgModel = xgboost(data = dtrain, nround = 16, booster="gblinear",eta = 0.02, objective = "reg:squarederror",eval_metric  = "rmse")
  
  # Make the predictions on the test data
  predict_data = predict(xgModel, X_test)
  
  xgtotaldata$Orders[xgtotaldata$Type == "Forecast"] = predict_data
  xgtotaldata$Orders = xgtotaldata$Orders+AvgxgtotaldataOrders
  
  
  forecasttrendPlot(totaldata,x = "Date",y = "Orders",TrendTitle =paste(" XGBoost - Trend and Forecast Line for Product",Product,sep=""),plotfilename = "XGBoost.png",bycolor = "Type", dirpath)
  totaldata$XGBoost <- xgtotaldata$Orders
  
  return(totaldata)
}

FitAutoArima <- function (totaldata,Product,TestForecastUnits,dirpath){
  # Function to train the Multivariant Arima model
  # Input : Data, Product Name, TestForecastUnits, Directory path to save output
  # Output : data with results
  fitreg  <- auto.arima(ts(totaldata$Orders[totaldata$Type == "Actual"],start = as.Date(min(totaldata$Date)),frequency = 7),xreg = as.matrix(totaldata[totaldata$Type=="Actual",c("BlackFridayWeeks","WinterSeason","HolidayWeek","BackToSchool","Spring")]),trace=TRUE)
  summary(fitreg)
  predict_dataMREGArima = predict(fitreg, newxreg = as.matrix(totaldata[totaldata$Type=="Forecast",c("BlackFridayWeeks","WinterSeason","HolidayWeek","BackToSchool","Spring")]))
  totaldata$Orders[totaldata$Type=="Forecast"] = predict_dataMREGArima$pred
  forecasttrendPlot(totaldata,x = "Date",y = "Orders",TrendTitle =paste(" MultiVariant Arima - Trend and Forecast Line for Product",Product,sep=""),plotfilename = "MREGArima.png",bycolor = "Type", dirpath)
  totaldata$MAutoArima <- totaldata$Orders
  return(totaldata)
}

FitHoltWinters = function(totaldata,Product,TestForecastUnits,dirpath){
# Function to train the HoltWinters model
# Input : Data, Product Name, TestForecastUnits, Directory path to save output
# Output : data with results
  HoltMod = HoltWinters(ts(totaldata$Orders[totaldata$Type =="Actual"],start = as.Date(min(totaldata$Date)),frequency = 7),beta = TRUE,gamma = FALSE)
  plot(HoltMod)
  HoltModforecasts <- predict(HoltMod,  n.ahead = TestForecastUnits)
  totaldata$Orders[totaldata$Type=="Forecast"] = HoltModforecasts
  forecasttrendPlot(totaldata,x = "Date",y = "Orders",TrendTitle =paste(" Holts - Trend and Forecast Line for Product A ",sep=""),plotfilename = "Holts.png",bycolor = "Type", dirpath)
  plot(HoltMod,HoltModforecasts)
  totaldata$HoltWinters <- totaldata$Orders
  return(totaldata)
}


print("========================================================================================")
print("==================================Model Functions Loaded==============================")
print("========================================================================================")
