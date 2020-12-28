


main <- function() {
  
  starttime <- Sys.time()
   
  
  #Laoding Libraries from the command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  # InputFilePath = "E:/GIT_SBUInsights/COE/BestBuy/Data/DataScienceAssignment.xlsx"
  # Product = "A"
  # TestForecastUnits = 16
  # DMNumberOfLags = 8
  # BufferInventory = 0.3
  # MinimumOrderQuantity = 50
  # ExtEventPath = "E:/GIT_SBUInsights/COE/BestBuy/Data/HolidayData.csv"

  InputFilePath = args[1]
  Product = args[2]
  TestForecastUnits = as.numeric(args[3])
  DMNumberOfLags = as.numeric(args[4])
  BufferInventory = as.numeric(args[5])
  MinimumOrderQuantity = as.numeric(args[6])
  ExtEventPath = args[7]
  
  
  # Creating a directory to save our outputs 
  dirpath = paste(dirname(dirname(InputFilePath)), "/Output/Product", Product, "/", sep ="")
  dir.create(dirpath, showWarnings = FALSE)
  
  
  # Loading  below codes to load below function blocks
  # requirements.R :Loading libraries in to the environment
  # DataUtils.R :plotForecastErrors,  trendplot,  forecasttrendplot,  loadtestdata, loaddata, AddExternalVariables
  # ModelFunctions.R :ForecastForNextDay,  FitGlmnet,  FItGlmnet.pois,  Fitearth,  Fitrpart,,  Fitrf,,  FitProphet,  FitETS,,  FitNNET,,  FitXGBoost,  FitAutoArima,  FitHoltWinters
  
  
  
  source(paste(dirname(dirname(InputFilePath)), "/Scripts/", "requirements.R",sep = ""))
  source(paste(dirname(dirname(InputFilePath)), "/Scripts/", "DataUtils.R",sep = ""))
  source(paste(dirname(dirname(InputFilePath)), "/Scripts/", "ModelFunctions.R",sep = ""))
  
  #Load the  weekly data  for the corresponding product
  rawdata <- LoadData(InputFilePath = InputFilePath, Product = Product)
  
  #Load Holiday details in to the system
  HolidayList = read.csv(ExtEventPath)
  
  # Converting From and To columns to Date format
  HolidayList$From <- as.Date(HolidayList$From,format ="%m/%d/%Y")
  HolidayList$To <- as.Date(HolidayList$To,format ="%m/%d/%Y")
  
  # Adding External variables to the existing input data frame
  rawdata <- AddExtFeatures(rawdata, HolidayList)
  
  # Plot the trend chart to look at the trend of the actual data
  trendPlot(
    rawdata,
    x = "Date",
    y = "Orders" ,
    paste("Trend Line for Product", Product, sep = ""),
    paste("Product", Product, sep = ""),dirpath
  )
  
  
  # May be becasue of Covid lockout or Winter season or 
  # because of unavailability of product there are weeks with 0 orders because of inventory
  # Imputing the 0 values to NAs to further impute using interpolation
  
  rawdata$Orders[rawdata$Orders == 0] = NA
  x = ts(rawdata$Orders,
         start = as.Date(min(rawdata$Date)),
         frequency = 7)
  
  # Interpolating the NA values, so as to have smooth demand curve 
  x_interp = na.interp(x)
  rawdata$ActualOrders <- rawdata$Orders
  rawdata$Orders = as.numeric(x_interp)
  
  
  # Assigning the data to a new vairable to keep the authenticity of the actual data
  tempdata = rawdata[, c(
    "Date",
    "Orders",
    "BlackFridayWeeks",
    "WinterSeason",
    "HolidayWeek",
    "BackToSchool",
    "Spring"
  )]
  tempdata$Orders <-
    as.numeric(ts(
      tempdata$Orders,
      start = as.Date(min(tempdata$Date)),
      frequency = 7
    ))
  tempdata$Orders <- round(tempdata$Orders, 0)
  tempdata$Type = "Actual"
  tempdata$Date <- as.Date(tempdata$Date)
  
  
  # Generate test data using TestForecastUnits value
  testData <- LoadTestData(tempdata, TestForecastUnits,HolidayList)
  
  # Join actual and test data which is used for forecating using different models
  totaldata <- rbind(tempdata, testData)
  totaldata[is.na(totaldata)] <- 0
  
  print("=========================================================================================")
  print("Forecasting using MArima, ETS, NNET, HoltWinters,XGBoost,Prophet is........... Starting !")
  print("=========================================================================================")
  
  totaldata <- FitProphet(totaldata, Product,HolidayList,TestForecastUnits,dirpath = dirpath)
  totaldata <- FitETS(totaldata, Product,TestForecastUnits,dirpath = dirpath)
  totaldata <- FitNNET(totaldata, Product,TestForecastUnits,dirpath = dirpath)
  totaldata <- FitHoltWinters(totaldata, Product,TestForecastUnits,dirpath = dirpath)
  totaldata <- FitAutoArima(totaldata, Product,TestForecastUnits,dirpath = dirpath)
  totaldata <- FitXGBoost(totaldata, Product,TestForecastUnits,dirpath = dirpath)
  
  print("=========================================================================================")
  print("Forecasting using MArima, ETS, NNET, HoltWinters,XGBoost,Prophet is............... Done !")
  print("=========================================================================================")
  
  # Creating Month and Month Day variable using the date column
  totaldata$MDay <- mday(as.Date(totaldata$Date))
  totaldata$Month = month(totaldata$Date)
  
  # Converting them to factor variables
  totaldata$Month = as.factor(totaldata$Month)
  totaldata$MDay = as.factor(totaldata$MDay)
  
  
  #Creating Lag columns for dynamic models
  LagColumns = c()
  LagFormula = ""
  for (i in 1:DMNumberOfLags) {
    totaldata[, paste("Lag_", i, sep = "")] = Lag(totaldata$Orders, i)
    LagColumns = c(LagColumns, paste("Lag_", i, sep = ""))
    LagFormula = paste(LagFormula, "+", paste("Lag_", i, sep = ""), sep =
                         "")
  }
  LagFormula = substring(LagFormula, 2, nchar(LagFormula))
  
  #Imputing NAs with zero - Just checking
  totaldata[is.na(totaldata)] <- 0
  
  
  # Ordering the date variable - Just to make sure that date is in older to newest order
  totaldata <- totaldata[order(totaldata$Date, decreasing = FALSE), ]
  rownames(totaldata) <- NULL
  
  
  # Covariant considered for the model training
  DMcovariates <-
    c(
      "BlackFridayWeeks",
      "WinterSeason" ,
      "HolidayWeek",
      "BackToSchool",
      "Spring",
      "Month",
      "MDay",
      LagColumns
    )
  
  #### creating sampling seeds ####
  set.seed(123)
  seeds <- vector(mode = "list", length = (nrow(totaldata) + 100))
  for (i in 1:(nrow(totaldata) + 100 - 1))
    seeds[[i]] <- sample.int(1000, 5)
  
  ## For the last model:
  seeds[[(nrow(totaldata) + 100)]] <- sample.int(1000, 1)
  
  print("=========================================================================================")
  print("Training using RandomForest, Rpart, Party, Earth, GLMNET is................... Starting !")
  print("=========================================================================================")
  
  myTimeControl <- trainControl(
    method = "timeslice",
    initialWindow = 20,
    horizon = 4,
    fixedWindow = FALSE,
    allowParallel = TRUE,
    seeds = seeds
  )
  
  # Number of parameters to be used for creating grid control
  tuneLength.num <- 5
  
  # Fitting GLMNET model
  totaldata$glmnetOrders <- totaldata$Orders
  glmnet.mod <-
    FitGlmnet(totaldata, myTimeControl, tuneLength.num = tuneLength.num,LagFormula)
  # Fitting EARTH model
  totaldata$earthOrders <- totaldata$Orders
  earth.mod <-
    Fitearth(totaldata, myTimeControl, tuneLength.num = tuneLength.num,LagFormula)
  # Fitting RPART model
  totaldata$rpartOrders <- totaldata$Orders
  rpart.mod <-
    FitRpart(totaldata, myTimeControl, tuneLength.num = tuneLength.num,LagFormula)
  # Fitting PARTY model
  totaldata$partyOrders <- totaldata$Orders
  party.mod <-
    FitParty(totaldata, myTimeControl, tuneLength.num = tuneLength.num,LagFormula)
  # Fitting RF model
  totaldata$rfOrders <- totaldata$Orders
  rf.mod  <-
    Fitrf(totaldata, myTimeControl, tuneLength.num = tuneLength.num,LagFormula)
  
  print("=========================================================================================")
  print("Training using RandomForest, Rpart, Party, Earth, GLMNET is....................... Done !")
  print("=========================================================================================")
  print("=========================================================================================")
  print("Forecasting for the Next week is being done for the given forecast days....... Starting !")
  print("=========================================================================================")
  
  for (i in (nrow(totaldata[totaldata$Type == "Actual", ]) + 1):nrow(totaldata)) {
    totaldata <-
      ForecastForNextDay(totaldata,
                         DMNumberOfLags,DMcovariates,i,
                         columnName = "glmnetOrders",
                         model = glmnet.mod)
    totaldata <-
      ForecastForNextDay(totaldata,
                         DMNumberOfLags,DMcovariates,i,
                         columnName = "earthOrders",
                         model = earth.mod)
    totaldata <-
      ForecastForNextDay(totaldata,
                         DMNumberOfLags,DMcovariates,i,
                         columnName = "rfOrders",
                         model = rf.mod)
    totaldata <-
      ForecastForNextDay(totaldata,
                         DMNumberOfLags,DMcovariates,i,
                         columnName = "rpartOrders",
                         model = rpart.mod)
    totaldata <-
      ForecastForNextDay(totaldata,
                         DMNumberOfLags,DMcovariates,i,
                         columnName = "partyOrders",
                         model = party.mod)
  }
  
  print("=========================================================================================")
  print("Forecasting for the Next week is being done for forecast days...................... Done!")
  print("=========================================================================================")
  
  
  # Generating plots for GLMNET, PARTY, RPART, RF, EARTH
  forecasttrendPlot(
    totaldata,
    x = "Date",
    y = "glmnetOrders",
    TrendTitle = paste(" GLMNET - Trend and Forecast Line for Product", Product, sep =
                         ""),
    plotfilename = "GLMNET.png",
    bycolor = "Type", dirpath
  )
  forecasttrendPlot(
    totaldata,
    x = "Date",
    y = "earthOrders",
    TrendTitle = paste(" EARTH - Trend and Forecast Line for Product", Product, sep =
                         ""),
    plotfilename = "EARTH.png",
    bycolor = "Type", dirpath
  )
  forecasttrendPlot(
    totaldata,
    x = "Date",
    y = "rfOrders",
    TrendTitle = paste(
      " RandomForest - Trend and Forecast Line for Product",
      Product,
      sep = ""
    ),
    plotfilename = "RandomForest.png",
    bycolor = "Type", dirpath
  )
  forecasttrendPlot(
    totaldata,
    x = "Date",
    y = "rpartOrders",
    TrendTitle = paste(" RPART - Trend and Forecast Line for Product", Product, sep =
                         ""),
    plotfilename = "RPART.png",
    bycolor = "Type", dirpath
  )
  forecasttrendPlot(
    totaldata,
    x = "Date",
    y = "partyOrders",
    TrendTitle = paste(" PARTY - Trend and Forecast Line for Product", Product, sep =
                         ""),
    plotfilename = "PARTY.png",
    bycolor = "Type", dirpath
  )
  
  
  # Plotting all the individual plots in to One
  
  filePath <-
    paste(dirpath, 'All Models_Product', Product, ".png", sep = '')
  plot = ggplot(totaldata, aes(x = Date)) +
    geom_line(
      size = 1,
      aes(y = glmnetOrders, color = 'glmnetOrders'),
      position = position_dodge(0.8)
    ) +
    geom_line(
      size = 1,
      aes(y = earthOrders, color = 'earthOrders'),
      position = position_dodge(0.8)
    ) +
    geom_line(size = 1,
              aes(y = rfOrders, color = 'rfOrders'),
              position = position_dodge(0.8)) +
    geom_line(
      size = 1,
      aes(y = rpartOrders, color = 'rpartOrders'),
      position = position_dodge(0.8)
    ) +
    geom_line(
      size = 1,
      aes(y = partyOrders, color = 'partyOrders'),
      position = position_dodge(0.8)
    ) +
    geom_line(size = 1,
              aes(y = ETS, color = 'ETSOrders'),
              position = position_dodge(0.8)) +
    geom_line(size = 1,
              aes(y = NNET, color = 'NNETOrders'),
              position = position_dodge(0.8)) +
    geom_line(
      size = 1,
      aes(y = MAutoArima, color = 'MAutoArimaOrders'),
      position = position_dodge(0.8)
    ) +
    #geom_line(size=1,aes(y = HoltWinters,color = 'HoltsOrders'), position = position_dodge(0.8)) +
    geom_line(
      size = 1,
      aes(y = XGBoost, color = 'XGBoostOrders'),
      position = position_dodge(0.8)
    ) +
    geom_line(size = 1,
              aes(y = Prophet, color = 'Prophet'),
              position = position_dodge(0.8)) +
    ggtitle(paste('Forecasts from different models - Product', Product, collapse = '\n')) +
    theme(plot.title = element_text(
      size = 12,
      face = 'bold',
      hjust = 0.5
    )) +
    ggsave(
      filePath,
      plot = last_plot(),
      dpi = 600,
      limitsize = TRUE,
      height = 10 ,
      width = 10 * 1.5
    )
  
  print(plot)
  
  
  
  
  print("=========================================================================================")
  print("Ensembling using MEAN function...............................................Starting !  ")
  print("=========================================================================================")
  
  
  # Ensemble of selected models using MEAN of the selected models
  
  for (i in 1:nrow(totaldata)) {
    totaldata$EnsembleForecast[i] = mean(
      c(
        totaldata$MAutoArima[i],
        totaldata$NNET[i],
        totaldata$XGBoost[i],
        totaldata$Prophet[i],
        totaldata$ETS[i],
        totaldata$glmnetOrders[i],
        totaldata$partyOrders[i],
        totaldata$rfOrders[i],
        totaldata$rpartOrders[i],
        totaldata$earthOrders[i]
      ),
      na.rm = TRUE
    )
  }
  
  
  # Plot the ensemble plot
  forecasttrendPlot(
    totaldata,
    x = "Date",
    y = "EnsembleForecast",
    TrendTitle = paste(" Ensemble of models Forecast - for Product", Product, sep =
                         ""),
    plotfilename = "Ensemble.png",
    bycolor = "Type", dirpath
  )
  
  print("=========================================================================================")
  print("Ensembling using MEAN function...................................................Done !  ")
  print("=========================================================================================")
  print("Inventory Management.........................................................Starting !  ")
  print("=========================================================================================")
  
  
  # Creating a data frame with Date, Type, Ensemble forecast
  FinalData <- totaldata[, c("Date", "Type", "EnsembleForecast")]
  FinalData$EnsembleForecast <- round(FinalData$EnsembleForecast, 0)
  FinalData$Orders[FinalData$Type == "Actual"] <-
    round(rawdata$ActualOrders, 0)
  FinalData$Inventory[FinalData$Type == "Actual"] <-
    rawdata$Inventory
  FinalData$Cart_Adds[FinalData$Type == "Actual"] <-
    rawdata$Cart_Adds
  FinalData$Views[FinalData$Type == "Actual"] <- rawdata$Views
  
  
  FinalData$Orders[FinalData$Type == "Forecast"] <- 0
  FinalData$Inventory[FinalData$Type == "Forecast"] <- 0
  FinalData$Cart_Adds[FinalData$Type == "Forecast"] <- 0
  FinalData$Views[FinalData$Type == "Forecast"] <- 0
  
  FinalData[is.na(FinalData)] <- 0
  
  # Creating Starting Inventory of the week
  FinalData$Inventory_StartOfWeek[FinalData$Type == "Actual"] <-
    FinalData$Orders[FinalData$Type == "Actual"] + FinalData$Inventory[FinalData$Type ==
                                                                         "Actual"]
    FinalData$InventoryReplenished <- 0
  
    
  for (i in 1:nrow(FinalData)) {
    if (FinalData$Type[i] == "Actual" & (i > 1)) {
      FinalData$InventoryReplenished[i] =  FinalData$Inventory_StartOfWeek[i] -
        FinalData$Inventory[i - 1]
      
    }
    # Going through  each row to get the inventory replenished
    else if (FinalData$Type[i] == "Forecast") {
      FinalData$Orders[i] = FinalData$EnsembleForecast[i]
      #IdealInventory = mean(FinalData$EnsembleForecast[(i-4):(i-1)]) * (1+BufferInventory)
      IdealInventory = FinalData$EnsembleForecast[i] * (1 + BufferInventory)
      
      if (FinalData$Inventory[i - 1] >= IdealInventory) {
        FinalData$InventoryReplenished[i] = 0
      }
      else if (FinalData$Inventory[i - 1] < IdealInventory) {
        ExtraInventoryNeeded = (IdealInventory - FinalData$Inventory[i - 1])
        if ((ExtraInventoryNeeded %% MinimumOrderQuantity) > 0.0) {
          # BlockOf50: number of lots to purchase(lots* minimum order quantity )
          BlocksOf50 <-
            round((ExtraInventoryNeeded %/% MinimumOrderQuantity) + 1,
                  0)
        }
        else{
          BlocksOf50 <- round(ExtraInventoryNeeded %/% MinimumOrderQuantity, 0)
        }
        
        FinalData$InventoryReplenished[i] = BlocksOf50 * 50
      }
      
      FinalData$Inventory[i] <-
        FinalData$InventoryReplenished[i] + FinalData$Inventory[i - 1] - FinalData$Orders[i]
      
    }
  }
  FinalData$Inventory_StartOfWeek[FinalData$Type == "Forecast"] <-
    FinalData$Orders[FinalData$Type == "Forecast"] + FinalData$Inventory[FinalData$Type ==
                                                                           "Forecast"]
  print("=========================================================================================")
  print("Inventory Management..............................................................Done!  ")
  print("=========================================================================================")
  
  
  filePath <- paste(dirpath, 'OF&IS.png', sep = '')
  plot = ggplot(FinalData, aes(x = Date)) +
    geom_line(size = 1,
              aes(y = Orders, color = "Orders"),
              position = position_dodge(0.8)) +
    geom_line(
      size = 1,
      aes(y = InventoryReplenished, color = "InventoryReplenished"),
      position = position_dodge(0.8)
    ) +
    geom_vline(xintercept = as.numeric(max(FinalData$Date[FinalData$Type ==
                                                            "Actual"])), linetype = 4) +
    ggtitle(paste("Orders Forecast & Inventory Schedule", collapse = '\n')) +
    theme(plot.title = element_text(
      size = 12,
      face = 'bold',
      hjust = 0.5
    )) +
    ggsave(
      filePath,
      plot = last_plot(),
      dpi = 600,
      limitsize = TRUE,
      height = 10 ,
      width = 10 * 1.5
    )
  
  print(plot)
  
  

  write.csv(FinalData,
            paste(dirpath, "/Product", Product, ".csv", sep = ""),
            row.names = FALSE)
  
  
  print("=====================================================================================")
  print("Writing the output results to csv is ..........................................Done !")
  print(paste("Find results at",paste(dirpath, "/Product", Product, ".csv", sep = ""),sep=""))
  print("************************************* END *********************************************")
  
  endtime <- Sys.time()
  timetaken <- endtime - starttime
  print(paste("Total time taken for the execution process is..",round(timetaken,0),"(in seconds)",sep=""))
}

main()
