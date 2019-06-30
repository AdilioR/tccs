
Main.loadCsvIntoWorkspace <- function(asset.name, csv_fullpath, initial_date, header = TRUE, sep = ",")
{ 
	#Leitura csv para data.frame
	if(csv_fullpath == "local")
	{
		csv <- read.csv(paste(asset.name, "csv", sep = "."), header = header, sep = sep)
	}
	else
	{
		csv <- read.csv(text=csv_fullpath, header = header, sep = sep)
	}
	
	csv <- csv[csv$Open != "null" & csv$Date != "null" & csv$Low != "null" & csv$Close != "null" & csv$Adj.Close != "null" & csv$Volume != "null",]
	Asset.New_Asset(asset.name, as.character(csv$Date), as.character(csv$Open), as.character(csv$High), as.character(csv$Low), as.character(csv$Close), as.character(csv$Adj.Close), as.character(csv$Volume), initial_date = initial_date)
	rm(csv)
}

# Input: decisionList, lista com valores Buy, Sell, Hold
# arquivo com stratégias deve conter nome do identificador fornecido
# Gera linhas em Result.StrategyList, 
Main.setStrategyResults <- function(asset.name, asset.transaction_list.prices, asset.transaction_list.dates, strategy_name, strategy_details, predicted_decision_list, percent_train)
{

	Result.New_Result(asset.name, asset.transaction_list.prices, asset.transaction_list.dates, strategy_name, strategy_details, predicted_decision_list, percent_train)
	
}

Main.avoidFactors <- function()
{
	Result.avoidFactors()
}

Main.addSpecificSMAResultIntoAsset <- function(nFast, nSlow)
{
	predicted_decision_list <- TechnicalAnalysis.getDecisionList.SMA(nFast, nSlow)

	strategy_details <- paste("long-", nSlow, "_short-", nFast, sep = "")

	Asset.setClasses("SMA", strategy_details, predicted_decision_list)

}

Main.addSpecificEMAResultIntoAsset <- function(nFast, nSlow)
{
	predicted_decision_list <- TechnicalAnalysis.getDecisionList.EMA(nFast, nSlow) #lista ok

	strategy_details <- paste("long-", nSlow, "_short-", nFast, sep = "")

	Asset.setClasses("EMA", strategy_details, predicted_decision_list)

}

Main.addSpecificMACDResultIntoAsset <- function(nFast, nSlow, nSig)
{
	predicted_decision_list <- TechnicalAnalysis.getDecisionList.MACD(nFast, nSlow, nSig) #lista ok

	strategy_details <- paste("long-", nSlow, "_short-", nFast, "_nsig-", nSig, sep = "")

	Asset.setClasses("MACD", strategy_details, predicted_decision_list)

}

Main.addSpecificFASTSTOCHResultIntoAsset <- function(nFastK, nFastD)
{
	predicted_decision_list <- TechnicalAnalysis.getDecisionList.FASTSTOCH(nFastK, nFastD) #lista ok

	strategy_details <- paste("nfastk-", nFastK, "_nfastd-", nFastD, sep = "")

	Asset.setClasses("Fast Stochastic", strategy_details, predicted_decision_list)

}

Main.addSpecificRSIResultIntoAsset <- function(MaLength, nivelInferior, nivelSuperior)
{
	predicted_decision_list <- TechnicalAnalysis.getDecisionList.RSI(MaLength, nivelInferior, nivelSuperior) #lista ok
	
	strategy_details <- paste("MAType-", "SMA_", "MaLength-", MALength, "_inf-level-", nivelInferior, "_sup-level-", nivelSuperior, sep = "")

	Asset.setClasses("RSI", strategy_details, predicted_decision_list)
}