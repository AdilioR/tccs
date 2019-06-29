# -> METADADOS

Asset.First_Date <- NA
Asset.Last_Date <- NA
Asset.Name <- NA

# daily, 5m
Asset.Frequency <- NA

Asset.SelectedStrategy.Name <- NA
Asset.SelectedStrategy.Details <- NA

# <- METADADOS

#lista de 
Asset.TransactionList <- data.frame(
	Date = numeric(),
	Open = numeric(),
	High = numeric(),
	Low = numeric(),
	Close = numeric(),
	Adj_Close = numeric(),
	Volume = numeric()
)

Asset.New_Asset <- function(asset.name, asset.date = NA, asset.open = NA, asset.high = NA, asset.low = NA, asset.close = NA, asset.adj_close, asset.volume = NA, asset.action = NA, frequency = 'daily', initial_date = NA)
{
	if(is.na(asset.open) && is.na(asset.high) && is.na(asset.low) && is.na(asset.close) && is.na(asset.adj_close))
	{
		#retorne msg comunicando necessidade de campo com valores
	}
	else
	{
		Asset.Name <<- asset.name
		Asset.Frequency <<- frequency
		
		Asset.TransactionList <<- rbind(
			Asset.TransactionList,
			cbind(
				Date = as.Date(asset.date, origin = "1970-01-01"),
				Open = as.numeric(asset.open),
				High = as.numeric(asset.high),
				Low = as.numeric(asset.low),
				Close = as.numeric(asset.close),
				Adj_Close = as.numeric(asset.adj_close),
				Volume = as.numeric(asset.volume)
			)
		)
		
		if(!is.na(initial_date))
		{
			this.setCleanData(initial_date)
		}
		
		Asset.First_Date <<- Asset.TransactionList$Date[1]
		Asset.Last_Date <<- Asset.TransactionList$Date[nrow(Asset.TransactionList)]
		
	}
}

this.setCleanData <- function(initial_date)
{
	# Preprocessamento: retirada de registros com data anterior a que deve ser a primeira 
	Asset.TransactionList <<- Asset.TransactionList[Asset.TransactionList$Date >= initial_date,]
	
	# Preprocessamento: retirada de registros com volume 0
	Asset.TransactionList <<- Asset.TransactionList[Asset.TransactionList$Volume > 0,]
	
	Asset.TransactionList$Date <<- as.Date(Asset.TransactionList$Date, origin = "1970-01-01")
}

Asset.setClasses <- function(strategy_name, strategy_details, predicted_decision_list)
{
	Asset.SelectedStrategy.Name <<- strategy_name
	Asset.SelectedStrategy.Details <<- strategy_details

	Asset.TransactionList$PredictedClasses <<- predicted_decision_list
}
