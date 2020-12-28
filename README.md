# BestBuyInventoryScheduler
Inventory scheduler project assignment
## System Specifications
	1. R Version = R version 4.0.3 (2020-10-10)
	2. OS: Windows
## Objective
	1.Analysis to forecast the orders at product level, so as to guide the Inventory Analysts in Inventory replenishment.
## Assumptions
	1. Products given are not volatile products ( like electronics – frequent updated versions)
	2. Considered external events like Black Friday, Christmas Eve, Back To School, Spring & Winter Season (its predominant in CA). Covid-19 impact is not considered in analysis, since the data availability is limited in its spread.
	3. Assumed Minimum Order Quantity for the product for Best buy to purchase the product from suppliers.
	4. Buffer Inventory of 30% is considered, keeping the randomness in demand of the products. (it needs to be validated and updated). In future, confidence interval of the forecast can be used to estimate the buffer stock
	5. Orders are placed on the availability of product, but at the same time, Inventory should be maintained based on the probable order inflow.
	6. In E-commerce environment, one of the strategy for the sale of the product is Views -> Cart_Adds -> Orders. 
	7. Based on the above two assumptions, Orders are forecasted to guide Inventory Analysts, since it has direct relation with product availability.
	8. Selection of models generally depends on its performance on cross-validation in validation environment, given the POC nature of analysis, models are selected based on the forecasted trend.
	9. Price is not considered for the analysis, but it plays significant role in converting views to orders. It is based on product also. without having 
		business knowledge, using price as a feature is not a good option

## Functional WorkFlow
	1. Load the data in to environment
	2. Filter the data for the product passed through command line arguments
	3. Check Statistical tests to check the stationarity of the series ( Box.test. adf.test, kpss.test, acf and pacf plots)
	4. Once stationary parameters are estimated, we can proceed ahead to validation so as to identify the best fit models
	5. Build different forecasting models, so as to regularize our output.
	6. All the model outputs are ensembled using mean function
	7. Based on the forecast, Inventory needed to be replenished is estimated
	8. Plot and save results
## Code WorkFlow
### Auxiliary Functions\
 	Below codes have functions which help us to do the analysis for the given problem
	1.  requirements.R :Loading libraries in to the environment
	2.  DataUtils.R :plotForecastErrors,  trendplot,  forecasttrendplot,  loadtestdata, loaddata, AddExternalVariables
  	3.  ModelFunctions.R :ForecastForNextDay,  FitGlmnet,  FItGlmnet.pois,  Fitearth,  Fitrpart,,  Fitrf,,  FitProphet,  FitETS,,  FitNNET,,  FitXGBoost,  FitAutoArima,  FitHoltWinters
### Main Function
	1. Consists of calling all the auxiliary functions to have outputs of different forecasting models
	
### Running the code
1. Have data with given column Names (week,	product,	orders,	brand,	views,	cart_adds,	price,	inventory)
2. Have external variables with column names like (EventType,	From,	To)
3. Open cmd in windows and run the below command to run the code ( Must need : R & internet connection to install required packages)

>Rscript <config.R filepath> <InputFilePath>   <TestForecastUnits>   <DMNumberOfLags> 	<BufferInventory> 	<MinimumOrderQuantity>  <ExternalEventpath>

### Example

  InputFilePath = "..../DataScienceAssignment.xlsx"
  Product = "A"
  TestForecastUnits = 16
  DMNumberOfLags = 8
  BufferInventory = 0.3
  MinimumOrderQuantity = 50
  ExtEventPath = "..../HolidayData.csv"

  
## Outputs
	1. Outputs of the analysis are saved in the "<DirPath of the input filepath>/Output/ProductA/"
	2. Output file ProductA.csv consists of the schedule of Inventory repenishment
	3. All the plots generated using each model, ensembled and final inventory schedule can also be found at the same location
