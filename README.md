# BestBuyInventoryScheduler
Inventory scheduler
## Objective
	1.Analysis to forecast the orders at product level, so as to guide the Inventory Analysts in Inventory replenishment.
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
### Auxiliary Functions
	1.  requirements.R :Loading libraries in to the environment
	2.  DataUtils.R :plotForecastErrors,  trendplot,  forecasttrendplot,  loadtestdata, loaddata, AddExternalVariables
  	3.  ModelFunctions.R :ForecastForNextDay,  FitGlmnet,  FItGlmnet.pois,  Fitearth,  Fitrpart,,  Fitrf,,  FitProphet,  FitETS,,  FitNNET,,  FitXGBoost,  FitAutoArima,  FitHoltWinters
### Main Function
	1. Consists of calling all the auxiliary functions to have outputs of different forecasting models
  
