Result.Asset.Name <- NA

Result.Train.Initial_Date <- NA
Result.Train.Final_Date <- NA
Result.Train.Size <- NA
Result.Train.Buy_and_Hold_Return <- NA

Result.Test.Initial_Date <- NA
Result.Test.Final_Date <- NA
Result.Test.Size <- NA
Result.Test.Buy_and_Hold_Return <- NA 


# Mostra um resumo em que cada linha representa uma estratégia sobre um dataset de preços de ativo
Result.StrategyList <- data.frame(

	strategy_name = character(),
	strategy_details = character(),
	
	mean_return_train = double(),
	mean_return_test = double(),
	
	median_return_train = double(),
	median_return_test = double(),
	
	min_return_train = double(),
	min_return_test = double(),
	
	max_return_train = double(),
	max_return_test = double(),
	
	firstqt_return_train = double(),
	firstqt_return_test = double(),
	
	thirdqt_return_train = double(),
	thirdqt_return_test = double(),
	
	sd_return_train = double(),
	sd_return_test = double(),
	
	cumulative_return_train = double(),
	cumulative_return_test = double(),
	
	quantity_operation_train = double(),
	quantity_operation_test = double()
	
)


Result.New_Result <- function(asset.name, asset.transaction_list.prices, asset.transaction_list.dates, strategy_name, strategy_details, predicted_decision_list, percent_train = 0.8)
{
  
	Result.Asset.Name <<- asset.name

	# Variável local
	asset.transaction_list.prices.train <- asset.transaction_list.prices[c(1 : (round(NROW(asset.transaction_list.prices) * percent_train, digits = 0)))]
	asset.transaction_list.prices.test <- asset.transaction_list.prices[c((round(NROW(asset.transaction_list.prices) * percent_train, digits = 0) + 1) : (NROW(asset.transaction_list.prices)))]
	asset.transaction_list.dates.train <- asset.transaction_list.dates[c(1 : (round(NROW(asset.transaction_list.dates) * percent_train, digits = 0)))]
	asset.transaction_list.dates.test <- asset.transaction_list.dates[c((round(NROW(asset.transaction_list.dates) * percent_train, digits = 0) + 1) : (NROW(asset.transaction_list.dates)))]
  
	# data da primeira negociação, do treino e do teste
	Result.Train.Initial_Date <<- as.character(as.Date(asset.transaction_list.dates.train[1], origin = "1970-01-01"))
	Result.Test.Initial_Date <<- as.character(as.Date(asset.transaction_list.dates.test[1], origin="1970-01-01"))
  
	# data da última negociação, do treino e do teste
	Result.Train.Final_Date <<- as.character(as.Date(asset.transaction_list.dates.train[NROW(asset.transaction_list.dates.train)], origin="1970-01-01"))
	Result.Test.Final_Date <<- as.character(as.Date(asset.transaction_list.dates.test[NROW(asset.transaction_list.dates.test)], origin="1970-01-01"))
  
	# Variável para guardar o tamanho de cada subconjunto
	Result.Train.Size <<- NROW(asset.transaction_list.prices.train)
	Result.Test.Size <<- NROW(asset.transaction_list.prices.test)
	
	# Variável local para calcular a diferença percentual entre último e primeiro preço negociados (Buy and Hold)
	Result.Train.Buy_and_Hold_Return <<- (asset.transaction_list.prices.train[Result.Train.Size]-asset.transaction_list.prices.train[1]) / asset.transaction_list.prices.train[1]
	Result.Test.Buy_and_Hold_Return <<- (asset.transaction_list.prices.test[Result.Test.Size]-asset.transaction_list.prices.test[1]) / asset.transaction_list.prices.test[1]
  
	
	this.setClassificationModelPerformance(asset.transaction_list.prices.train, asset.transaction_list.prices.test, strategy_name, strategy_details, predicted_decision_list, percent_train)
	
}

this.setClassificationModelPerformance <- function(asset.transaction_list.prices.train, asset.transaction_list.prices.test ,strategy_name, strategy_details, predicted_decision_list, percent_train)
{
	
	predicted_decision_list_train <- predicted_decision_list[c(1 : (round(NROW(predicted_decision_list) * percent_train, digits = 0)))]
	predicted_decision_list_test <- predicted_decision_list[c((round(NROW(predicted_decision_list) * percent_train, digits = 0) + 1) : (NROW(predicted_decision_list)))]

	quantity_operation_train <- Result.getQuantityOperation(predicted_decision_list_train)
	quantity_operation_test <- Result.getQuantityOperation(predicted_decision_list_test)
	
	if(quantity_operation_train > 0 && quantity_operation_test > 0)
	{

		return_list_train <<- Result.getReturnList(asset.transaction_list.prices.train, predicted_decision_list_train)
		return_list_test <<- Result.getReturnList(asset.transaction_list.prices.test, predicted_decision_list_test)
		
		mean_return_train <- summary(return_list_train)["Mean"]
		mean_return_test <- summary(return_list_test)["Mean"]
		
		median_return_train <- summary(return_list_train)["Median"]
		median_return_test <- summary(return_list_test)["Median"]
		
		min_return_train <- summary(return_list_train)["Min."]
		min_return_test <- summary(return_list_test)["Min."]
		
		max_return_train <- summary(return_list_train)["Max."]
		max_return_test <- summary(return_list_test)["Max."]
		
		firstqt_return_train <- summary(return_list_train)["1st Qu."]
		firstqt_return_test <- summary(return_list_test)["1st Qu."]
		
		thirdqt_return_train <- summary(return_list_train)["3rd Qu."]
		thirdqt_return_test <- summary(return_list_test)["3rd Qu."]
		
		sd_return_train <<- sd(return_list_train)
		sd_return_test <<- sd(return_list_test)
		
		cumulative_return_train <- Result.getCumulativeReturn(asset.transaction_list.prices.train, predicted_decision_list_train) 
		cumulative_return_test <- Result.getCumulativeReturn(asset.transaction_list.prices.test, predicted_decision_list_test)
	
		Result.StrategyList <<- rbind(
			Result.StrategyList, 
			cbind(
				strategy_name = strategy_name,
				strategy_details = strategy_details,
				
				mean_return_train = mean_return_train,
				mean_return_test = mean_return_test,
				
				median_return_train = median_return_train,
				median_return_test = median_return_test,
				
				min_return_train = min_return_train,
				min_return_test = min_return_test,
				
				max_return_train = max_return_train,
				max_return_test = max_return_test,
				
				firstqt_return_train = firstqt_return_train,
				firstqt_return_test = firstqt_return_test,
				
				thirdqt_return_train = thirdqt_return_train,
				thirdqt_return_test = thirdqt_return_test,
				
				sd_return_train = sd_return_train,
				sd_return_test = sd_return_test,
				
				cumulative_return_train = cumulative_return_train,
				cumulative_return_test = cumulative_return_test,
				
				quantity_operation_train = quantity_operation_train,
				quantity_operation_test = quantity_operation_test
				
			)
		)
	}

	print(paste("asset:", Result.Asset.Name, ";strategy:", strategy_name, ";strategy_details:", strategy_details, sep="", collapse = NULL))
		
}

Result.getCumulativeReturn <- function(asset.transaction.price_list, predicted_decision_list)
{
	initial_investment <- 1
	cumulative_return <- initial_investment
	
	last_buy_value <- 0
	
	sell_decision_return <- 0
	
	for (decision in 1 : NROW(predicted_decision_list))
	{
		if(predicted_decision_list[decision] == "Buy")
		{
			last_buy_value <- asset.transaction.price_list[decision]
		}
		else if(last_buy_value > 0 & predicted_decision_list[decision] == "Sell")
		{
			sell_decision_return <- ((asset.transaction.price_list[decision] - last_buy_value) / last_buy_value)
			
			cumulative_return <- cumulative_return * ( 1 + sell_decision_return) # valores absolutos
			
		}
	}
	cumulative_return <- cumulative_return - initial_investment
	
	return(cumulative_return)
	
}

Result.getQuantityOperation <- function(predicted_decision_list)
{
	last_operation <- "Hold"
	
	decision_count <- 0
	
	for (decision in 1 : NROW(predicted_decision_list))
	{
		
		if(predicted_decision_list[decision] == "Buy")
		{
			
			last_operation <- "Buy"
			
		}
		else if(last_operation == "Buy" & predicted_decision_list[decision] == "Sell")
		{
			decision_count <- decision_count + 1
			
			last_operation <- "Sell"
			
		}

	}
	
	return(decision_count)
	
}

Result.getReturnList <- function(asset.transaction.price_list, predicted_decision_list)
{
	last_buy_value <- 0
	
	sell_decision_return <- 0
	
	decision_count <- 0
	
	return_list <- double()
	
	for (decision in 1 : NROW(predicted_decision_list))
	{
		
		if(predicted_decision_list[decision] == "Buy")
		{
			
			last_buy_value <- asset.transaction.price_list[decision]
			
		}
		else if(last_buy_value > 0 & predicted_decision_list[decision] == "Sell")
		{
			decision_count <- decision_count + 1
			
			sell_decision_return <- (asset.transaction.price_list[decision] - last_buy_value) / last_buy_value
			
			return_list <- append(return_list, sell_decision_return)
			
		}
	}
	
	
	return(return_list)
	
	
}

Result.avoidFactors <- function()
{
	Result.StrategyList$strategy_name <<- as.character(Result.StrategyList$strategy_name)
	Result.StrategyList$strategy_details <<- as.character(Result.StrategyList$strategy_details)
	
	Result.StrategyList$cumulative_return_train <<- as.numeric(as.character(Result.StrategyList$cumulative_return_train))
	Result.StrategyList$cumulative_return_test <<- as.numeric(as.character(Result.StrategyList$cumulative_return_test))
	
	Result.StrategyList$quantity_operation_train <<- as.integer(as.character(Result.StrategyList$quantity_operation_train))
	Result.StrategyList$quantity_operation_test <<- as.integer(as.character(Result.StrategyList$quantity_operation_test))
	
	
	Result.StrategyList$mean_return_train <<- as.numeric(as.character(Result.StrategyList$mean_return_train))
	Result.StrategyList$mean_return_test <<- as.numeric(as.character(Result.StrategyList$mean_return_test))
	
	Result.StrategyList$median_return_train <<- as.numeric(as.character(Result.StrategyList$median_return_train))
	Result.StrategyList$median_return_test <<- as.numeric(as.character(Result.StrategyList$median_return_test))
	
	Result.StrategyList$min_return_train <<- as.numeric(as.character(Result.StrategyList$min_return_train))
	Result.StrategyList$min_return_test <<- as.numeric(as.character(Result.StrategyList$min_return_test))
	
	Result.StrategyList$max_return_train <<- as.numeric(as.character(Result.StrategyList$max_return_train))
	Result.StrategyList$max_return_test <<- as.numeric(as.character(Result.StrategyList$max_return_test))
	
	Result.StrategyList$firstqt_return_train <<- as.numeric(as.character(Result.StrategyList$firstqt_return_train))
	Result.StrategyList$firstqt_return_test <<- as.numeric(as.character(Result.StrategyList$firstqt_return_test))
	
	Result.StrategyList$thirdqt_return_train <<- as.numeric(as.character(Result.StrategyList$thirdqt_return_train))
	Result.StrategyList$thirdqt_return_test <<- as.numeric(as.character(Result.StrategyList$thirdqt_return_test))
	
	Result.StrategyList$sd_return_train <<- as.numeric(as.character(Result.StrategyList$sd_return_train))
	Result.StrategyList$sd_return_test <<- as.numeric(as.character(Result.StrategyList$sd_return_test))
	
}