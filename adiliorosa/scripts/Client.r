# Remove todos os objetos do workspace
rm(list = ls())

# Carrega a biblioteca de análise técnica
require(TTR)

# Carrega os scripts necessários para gerar e avaliar as estratégias
source("https://raw.githubusercontent.com/AdilioR/tccs/master/adiliorosa/scripts/Asset.R")
source("https://raw.githubusercontent.com/AdilioR/tccs/master/adiliorosa/scripts/Result.R")
source("https://raw.githubusercontent.com/AdilioR/tccs/master/adiliorosa/scripts/TechnicalAnalysis.r")
source("https://raw.githubusercontent.com/AdilioR/tccs/master/adiliorosa/scripts/Main.R")

asset.name <<- "ABEV3.SA"

# Carrega o CSV para a memória
Main.loadCsvIntoWorkspace(asset.name, csv_fullpath = "https://raw.githubusercontent.com/AdilioR/tccs/adilio/adiliorosa/rworkingdirectory/ABEV3.SA.csv", as.Date("1970-01-01"), header = TRUE, sep = ",")

# Prepara os dados caso sejam empregados subconjuntos do dataset original
setup <- function()
{
	Asset.First_Date <<- Asset.TransactionList$Date[1]
	Asset.Last_Date <<- Asset.TransactionList$Date[nrow(Asset.TransactionList)]
	rm(list=ls(pattern="Result."))
}


start <- function(sample.name)
{

  # SMA
  for(nFast in seq(from=10, to=25, by=5)) # 1 a 199 no tcc, de 1 em 1
  {


    for(nSlow in seq(from=(nFast + 1), to=36, by=5))# 2 a 200 no tcc , de 1 em 1
    {
      predicted_decision_list <- TechnicalAnalysis.getDecisionList.SMA(nFast, nSlow) #lista ok
      Main.setStrategyResults(asset.name = asset.name, asset.transaction_list.prices = Asset.TransactionList$Adj_Close, asset.transaction_list.dates = Asset.TransactionList$Date, strategy_name = "SMA", strategy_details = paste("long-", nSlow, "_short-", nFast, sep = ""), predicted_decision_list = predicted_decision_list, percent_train = 0.8)
    }

  }
  
  # MACD
  for(nFast in seq(from=10, to=25, by=5)) # 1 a 199 no tcc, de 1 em 1
  {
    
    for(nSlow in seq(from=(nFast + 1), to=36, by=5)) # 2 a 200 no tcc , de 1 em 1
    {
      nSig <- 9
      predicted_decision_list <- TechnicalAnalysis.getDecisionList.MACD(nFast, nSlow, nSig)
      Main.setStrategyResults(asset.name = asset.name, asset.transaction_list.prices = Asset.TransactionList$Adj_Close, asset.transaction_list.dates = Asset.TransactionList$Date, strategy_name = "MACD", strategy_details = paste("long-", nSlow, "_short-", nFast, "_nsig-", nSig, sep = ""), predicted_decision_list = predicted_decision_list, percent_train = 0.8)
      
    }
  }
  
 
	# RSI
	for(MALength in seq(from=9, to=12, by=1)) # 9 a 30 no TCC
	{ # Cada ?dice ?um tamanho de m?ia m?el
   
		for(nivelInferior in seq(from=30, to=35, by=1))
		{
   
			for(nivelSuperior in seq(from=60, to=65, by=1))
			{
				predicted_decision_list <- TechnicalAnalysis.getDecisionList.RSI(MALength, nivelInferior, nivelSuperior)
				Main.setStrategyResults(asset.name = asset.name, asset.transaction_list.prices = Asset.TransactionList$Adj_Close, asset.transaction_list.dates = Asset.TransactionList$Date, strategy_name = "RSI", strategy_details = paste("MAType-", "SMA_", "MaLength-", MALength, "_inf-level-", nivelInferior, "_sup-level-", nivelSuperior, sep = ""), predicted_decision_list = predicted_decision_list, percent_train = 0.8)
			}
		}
	}
  
  
  Main.avoidFactors()
  
  save.image(file = paste(asset.name, sample.name, "workspace.RData", sep = "_"))
  
  print(paste("Finalizado as: ", Sys.time()))
  
  
	
}

subsample1 <- Asset.TransactionList[c(1 : (round(NROW(Asset.TransactionList) * 0.25, digits = 0))),]
subsample2 <- Asset.TransactionList[c((round(NROW(Asset.TransactionList) * 0.25, digits = 0) + 1) : (round(NROW(Asset.TransactionList) * 0.50, digits = 0))),]
subsample3 <- Asset.TransactionList[c((round(NROW(Asset.TransactionList) * 0.50, digits = 0) + 1) : (round(NROW(Asset.TransactionList) * 0.75 + 1, digits = 0))),]
subsample4 <- Asset.TransactionList[c((round(NROW(Asset.TransactionList) * 0.75, digits = 0) + 2) : (NROW(Asset.TransactionList))),]

start("FullSample")

Asset.TransactionList <- subsample1
rm(subsample1)
setup()
start("Subsample1")

Asset.TransactionList <- subsample2
rm(subsample2)
setup()
start("Subsample2")

Asset.TransactionList <- subsample3
rm(subsample3)
setup()
start("Subsample3")

Asset.TransactionList <- subsample4
rm(subsample4)
setup()
start("Subsample4")

carregarDataset <- function(fileNameWithExtension)
{
  setwd(workingDirectory)
  load(fileNameWithExtension)
}

