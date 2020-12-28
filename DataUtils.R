


plotForecastErrors <- function(forecasterrors){
  
  # Function to plot forecast Errors with histogram and the density graph
  # Input : Forecast Errors
  # Output : Print forecast error plots
  # Code function taken from outside source
  
  
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

trendPlot = function(data,x = "Date",y = "Orders",TrendTitle,plotfilename,dirpath){
  
  # Function to plot trend chart using ggplot
  # Input : Data, x column, y column, title, plot name to save, directory path
  # Output : Plot
  
  filePath = paste(dirpath,plotfilename,".png",sep="")
  # Area plot
  plot = ggplot(data, aes(x = eval(parse(text=x)), y = eval(parse(text=y)))) + 
    geom_line(alpha = 0.8, position = position_dodge(0.8),colour= "green",size = 1) +
    ggtitle(TrendTitle)+xlab(x) + ylab(y) + theme(legend.title =element_blank())+
    ggsave(filePath,plot = last_plot(), dpi = 600,limitsize = TRUE,height = 7 , width = 7 * 1.5)
  return(print(plot))
}


forecasttrendPlot = function(data,x = "Date",y = "Orders",TrendTitle ="Line Chart",plotfilename = "Chart",bycolor = "Type",dirpath){
  
  # Function to plot trend chart using ggplot
  # Input : Data, x column, y column, title, plot name to save, Group BY column "Type",directory path
  # Output : Plot
  
  filePath = paste(dirpath,plotfilename,".png",sep="")
  # Area plot
  plot = ggplot(data, aes(x = eval(parse(text=x)), y = eval(parse(text=y)))) + 
    geom_line(aes(color =  eval(parse(text=bycolor))),
              alpha = 0.8, position = position_dodge(0.8),size = 1) +
    ggtitle(TrendTitle)+xlab(x) + ylab(y) + theme(legend.title =element_blank())+
    ggsave(filePath,plot = last_plot(), dpi = 600,limitsize = TRUE,height = 7 , width = 7 * 1.5)
  return(print(plot))
}


LoadTestData <- function(tempdata,TestForecastUnits,HolidayList){
  # Function to generate test data
  # Input : Data, Forecast Days, HolidayList details
  # Output : TestData to appended to total data

  testData=data.frame(Date= seq(max(tempdata$Date)+7, max(tempdata$Date)+(7*TestForecastUnits), by="week"))

  testData$Orders = 0
  testData$Date = as.Date(testData$Date)
  testData$Type = "Forecast"
  
  testData <- AddExtFeatures(testData, HolidayList)
  
  return(testData)
}


LoadData <- function(InputFilePath, Product = Product){
  
  # Function to load the data
  # Input : Data, Forecast Days, HolidayList details
  # Output : Test Data to appended to total data
  
  rawdata <- read.xlsx(InputFilePath)
  colnames(rawdata) = c('Date', 'Product', 'Orders', 'Brand','Views', 'Cart_Adds', 'Price', 'Inventory')
  rawdata$Date = as.Date(as.character(rawdata$Date), format = "%m/%d/%Y")
  paste0("Duration of the Data is from  ",min(rawdata$Date), " to ",max(rawdata$Date))
  rawdata <- rawdata[rawdata$Product == Product, ]
  return(rawdata)
}


AddExtFeatures <- function(rawdata, HolidayList){
  ## Adding external impact variables
  # Input : data and external variables
  # Output : Output data after adding external variables
  for (i in unique(HolidayList$EventType)){
    rawdata[,paste(i,sep="")] <- 0
  }
  for(i in 1:nrow(HolidayList)){
    rawdata[(rawdata$Date>=as.Date(HolidayList$From[i])) & (rawdata$Date <= as.Date(HolidayList$To[i]) ),paste(HolidayList$EventType[i],sep="")] = 1
  }
  return(rawdata)
}



print("=============================================================================")
print("==============================Data Utils Loaded=========================")
print("=============================================================================")
