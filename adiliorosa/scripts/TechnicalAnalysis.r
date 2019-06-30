

TechnicalAnalysis.getDecisionList.SMA <- function(nFast, nSlow)
{	
	
	TechnicalAnalysis.indicator.short <- SMA(Asset.TransactionList$Adj_Close, n = nFast) 
	TechnicalAnalysis.indicator.long <- SMA(Asset.TransactionList$Adj_Close, n = nSlow) # lenght
	
	#retornando apenas 1 valor //?
	TechnicalAnalysis.DecisionList	<- this.getCrossoverDecisionList(short = TechnicalAnalysis.indicator.short, long = TechnicalAnalysis.indicator.long)
	
	return (TechnicalAnalysis.DecisionList)
}

TechnicalAnalysis.getDecisionList.EMA <- function(nFast, nSlow)
{	

	TechnicalAnalysis.indicator.short <- EMA(Asset.TransactionList$Adj_Close, n = nFast) 
	TechnicalAnalysis.indicator.long <- EMA(Asset.TransactionList$Adj_Close, n = nSlow) # lenght

	#retornando apenas 1 valor //?
	TechnicalAnalysis.DecisionList	<- this.getCrossoverDecisionList(short = TechnicalAnalysis.indicator.short, long = TechnicalAnalysis.indicator.long)

	return (TechnicalAnalysis.DecisionList)
}

TechnicalAnalysis.getDecisionList.MACD <- function(nFast, nSlow, nSig)
{  # //? todo: fazer variar também o tipo de ma (sms, ema)
	macdObj <- MACD(Asset.TransactionList$Adj_Close, nFast=nFast, nSlow=nSlow, nSig=nSig, maType="EMA")
	TechnicalAnalysis.DecisionList	<- this.getCrossoverDecisionList(short = macdObj[,"macd"], long = macdObj[,"signal"])

	return (TechnicalAnalysis.DecisionList)
}

TechnicalAnalysis.getDecisionList.FASTSTOCH <- function(nFastK, nFastD)
{
	faststochastic <- stoch(Asset.TransactionList$Adj_Close, nFastK = nFastK, nFastD = nFastD, nSlowD = 3, bounded = TRUE, smooth = 1)
	TechnicalAnalysis.DecisionList	<- this.getCrossoverDecisionList(short = faststochastic[,"fastK"], long = faststochastic[,"fastD"])
	return (TechnicalAnalysis.DecisionList)
}

TechnicalAnalysis.getDecisionList.RSI <- function(ma_length, inf_level, sup_level)
{ # //? todo: fazer variar também o tipo de ma (sms, ema)

	rsi_results <- RSI(Asset.TransactionList$Adj_Close, n = ma_length, maType = "SMA")

	TechnicalAnalysis.DecisionList <- this.decisionFaixasOscilador(indicador = rsi_results, nivelInferior = inf_level, nivelSuperior = sup_level)

	return (TechnicalAnalysis.DecisionList)

}

this.getCrossoverDecisionList <- function(short, long)
{
	#Inicialização
	decisionList <- vector("character", nrow(Asset.TransactionList))
	decisionList[1] <- "Hold"
	
	bought <- FALSE
		
	for (i in 2 : nrow(Asset.TransactionList))
	{
		if(!((is.na(short[i])) || (is.na(long[i]))))
		{
			if((is.na(short[i-1])) || (is.na(long[i-1])))
			{
				i <- i+1
			}
			if(short[i] > long[i] && short[i-1] < long[i-1])
			{
				if(bought == FALSE)
				{
					decisionList[i] <- "Buy"
					if(i + 1 <= nrow(Asset.TransactionList)){
						bought <- TRUE
					}
				}
				else
				{
					decisionList[i] <- "Hold"
				}
			}
			else if(short[i] < long[i] && short[i-1] > long[i-1])
			{
				if(bought == TRUE)
				{
					decisionList[i] <- "Sell"
					bought <- FALSE
				}
				else
				{
					decisionList[i] <- "Hold"
				}
			}
			else
			{
				decisionList[i] <- "Hold"
			}
		}
	}
	
	decisionList[decisionList == ""] <- "Hold"
	return (decisionList)
}

this.decisionFaixasOscilador <- function(indicador, nivelInferior, nivelSuperior)
{
  #Inicialização
	decisionList <- vector("character", nrow(Asset.TransactionList))
	decisionList[1] <- "Hold"
 
  
  #ativo_subset$retorno_percentual_operacao <<- NA
  #retorno_absoluto_operacao <<- ativo_subset$retorno_percentual_operacao
  
  comprado <- FALSE
  for (i in 2:nrow(Asset.TransactionList))
  {
    if(!(is.na(indicador[i])))
    {
      
      if(indicador[i] <= nivelInferior)
      {
        if(comprado == FALSE)
        {
          #ativo_subset$operacao[i] <<- "Buy"
          decisionList[i] <- "Buy"
          if(i + 1 <= nrow(Asset.TransactionList)){
            comprado <- TRUE
          }
          ultimo_valor_compra <- Asset.TransactionList$Adj_Close[i]
        }
        else
        {
			decisionList[i] <- "Hold"
        }
      }
      else if(indicador[i] >= nivelSuperior)
      {
        if(comprado == TRUE)
        {
          
          decisionList[i] <- "Sell"
          comprado <- FALSE
          #retorno_absoluto_operacao[i] <<- ativo_subset$Adj_Close[i] - ultimo_valor_compra
          #ativo_subset$retorno_percentual_operacao[i] <<- 100 * (retorno_absoluto_operacao[i] / ultimo_valor_compra) # x significa x% de retorno percentual
        }
        else
        {

			decisionList[i] <- "Hold"
        }
      }
      else
      {
        decisionList[i] <- "Hold"
       
      }
    }
  }
  decisionList[decisionList == ""] <- "Hold"
  return(decisionList)
}