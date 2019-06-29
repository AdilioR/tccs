decisionBestDecision <- function(preco)
{
  ativo$retorno_percentual_operacao <<- NA
  retorno_absoluto_operacao <<- ativo$retorno_percentual_operacao
  ativo$bestDecision <<- NA
  
  comprado <- FALSE
  
  distanciaOperacaoAnterior <- NA
  valorOperacaoAnterior <- NA
  
  for (i in 3:length(preco) - 1) #pq i in 2 não está funcionando? 2 -1 = 0 no cálculo
  {
    print(i)
    print(preco[i])
    if(preco[i - 1] >= preco[i] && preco[i] < preco[i + 1])
    {
      if(comprado == FALSE)
      {
        if(is.na(distanciaOperacaoAnterior) || distanciaOperacaoAnterior > 3) #aprimorar para operacoes posteriores
        {
          ativo$bestDecision[i] <<- "Buy"
          if(i + 1 <= nrow(ativo))
          {
            comprado <- TRUE
          }
          distanciaOperacaoAnterior <- 0
          #ultimo_valor_compra <- ativo$Adj.Close[i]
        }
        else
        {
          ativo$bestDecision[i] <<- NA
          if(is.na(distanciaOperacaoAnterior))
          {
            distanciaOperacaoAnterior <- 1
          }
          else
          {
            distanciaOperacaoAnterior <- distanciaOperacaoAnterior + 1
          }
        }
      }
      else
      {
        ativo$bestDecision[i] <<- NA
        if(is.na(distanciaOperacaoAnterior))
        {
          distanciaOperacaoAnterior <- 1
        }
        else
        {
          distanciaOperacaoAnterior <- distanciaOperacaoAnterior + 1
        }
      }
    }
    else if(preco[i - 1] <= preco[i] && preco[i] > preco[i + 1])
    {
      if(comprado == TRUE)
      {
        if(!is.na(distanciaOperacaoAnterior) && distanciaOperacaoAnterior > 3) #aprimorar para operacoes posteriores
        {
          ativo$bestDecision[i] <<- "Sell"
          if(i + 1 <= nrow(ativo)){
            comprado <- FALSE
          }
          distanciaOperacaoAnterior <- 0
          #ultimo_valor_compra <- ativo$Adj.Close[i]
        }
        else
        {
          ativo$bestDecision[i] <<- NA
          if(is.na(distanciaOperacaoAnterior))
          {
            distanciaOperacaoAnterior <- 1
          }
          else
          {
            distanciaOperacaoAnterior <- distanciaOperacaoAnterior + 1
          }
        }
      }
      else
      {
        ativo$bestDecision[i] <<- NA
        if(is.na(distanciaOperacaoAnterior))
        {
          distanciaOperacaoAnterior <- 1
        }
        else
        {
          distanciaOperacaoAnterior <- distanciaOperacaoAnterior + 1
        }
      }
    }
  }
}
