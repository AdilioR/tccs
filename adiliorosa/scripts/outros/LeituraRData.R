setwd("C:/StockMarket/rworkingdirectory")
require(ggplot2)

  load("Resumo.rda")
  
  

  load("Resumo.abev3.rda")
  load("Resumo.bbdc4.rda")
  load("Resumo.petr4.rda")
  load("Resumo.vale5.rda")
  Global.Resumo.itub4 <- Global.Resumo[Global.Resumo$nome_ativo == "itub4",]
  
  load("my_work_space.RData")
  macd<-ativo_subset
  load("teste.SMA.longo_56_curto_45.1.0.2.rda")
  sma<-ativo_subset
  

  # indicador.points_treino X indicador.points_teste, cores-> indicador
    g = ggplot(Result.StrategyList, aes(x = indicador.points_treino, y = indicador.points_teste))
    g + geom_point(aes(color = strategy_name), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.bbdc4, aes(x = indicador.points_treino, y = indicador.points_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.itub4, aes(x = indicador.points_treino, y = indicador.points_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.petr4, aes(x = indicador.points_treino, y = indicador.points_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.vale5, aes(x = indicador.points_treino, y = indicador.points_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo, aes(x = indicador.points_treino, y = indicador.points_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    # Teste de correlação: Spearman rank correlation test
    cor.test(Result.StrategyList$indicador.points_treino, Result.StrategyList$indicador.points_teste, method='spearman', alternative='two.sided', approximation='exact')
      # Resultado:
#        Spearman's rank correlation rho
#    
#        data:  Global.Resumo$indicador.points_treino and Global.Resumo$indicador.points_teste
#        S = 2.9309e+12, p-value < 2.2e-16
#        alternative hypothesis: true rho is not equal to 0
#        sample estimates:
#        rho 
#        0.7863558 
    # Teste de correlação: Kendall's tau correlation coefficient test
    cor.test(Global.Resumo$indicador.points_treino, Global.Resumo$indicador.points_teste, method='kendal', alternative='two.sided', approximation='exact')
      # Resultado:
#        Kendall's rank correlation tau
#    
#        data:  Global.Resumo$indicador.points_treino and Global.Resumo$indicador.points_teste
#        z = 185.36, p-value < 2.2e-16
#        alternative hypothesis: true tau is not equal to 0
#        sample estimates:
#              tau 
#        0.6020135 
 
  
  # indicador.points_treino X retorno_acumulado_treino, cores-> indicador
    g = ggplot(Global.Resumo.abev3, aes(x = indicador.points_treino, y = retorno_acumulado_treino))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.bbdc4, aes(x = indicador.points_treino, y = retorno_acumulado_treino))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.itub4, aes(x = indicador.points_treino, y = retorno_acumulado_treino))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.petr4, aes(x = indicador.points_treino, y = retorno_acumulado_treino))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.vale5, aes(x = indicador.points_treino, y = retorno_acumulado_treino))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo, aes(x = indicador.points_treino, y = retorno_acumulado_treino))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    # Teste de correlação: Spearman rank correlation test
    cor.test(Global.Resumo$indicador.points_treino, Global.Resumo$retorno_acumulado_treino, method='spearman', alternative='two.sided', approximation='exact')
    # Resultado:
#    Spearman's rank correlation rho
#
#    data:  Global.Resumo$indicador.points_treino and Global.Resumo$retorno_acumulado_treino
#    S = 9.602e+12, p-value < 2.2e-16
#    alternative hypothesis: true rho is not equal to 0
#    sample estimates:
#          rho 
#    0.3000862 
    
    # Teste de correlação: Kendall's tau correlation coefficient test
    cor.test(Global.Resumo$indicador.points_treino, Global.Resumo$retorno_acumulado_treino, method='kendal', alternative='two.sided', approximation='exact')
    # Resultado:
      
  
  # indicador.points_treino X retorno_acumulado_teste, cores-> indicador
    g = ggplot(Global.Resumo.abev3, aes(x = indicador.points_treino, y = retorno_acumulado_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.bbdc4, aes(x = indicador.points_treino, y = retorno_acumulado_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.itub4, aes(x = indicador.points_treino, y = retorno_acumulado_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.petr4, aes(x = indicador.points_treino, y = retorno_acumulado_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.vale5, aes(x = indicador.points_treino, y = retorno_acumulado_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo, aes(x = indicador.points_treino, y = retorno_acumulado_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
  # retorno_acumulado_treino X retorno_acumulado_teste, cores-> indicador
    g = ggplot(Global.Resumo.abev3, aes(x = retorno_acumulado_treino, y = retorno_acumulado_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.bbdc4, aes(x = retorno_acumulado_treino, y = retorno_acumulado_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.itub4, aes(x = retorno_acumulado_treino, y = retorno_acumulado_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.petr4, aes(x = retorno_acumulado_treino, y = retorno_acumulado_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.vale5, aes(x = retorno_acumulado_treino, y = retorno_acumulado_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo, aes(x = retorno_acumulado_treino, y = retorno_acumulado_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
  
  # indicador.points_teste X retorno_acumulado_teste, cores-> indicador
    g = ggplot(Global.Resumo.abev3, aes(x = indicador.points_teste, y = retorno_acumulado_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.bbdc4, aes(x = indicador.points_teste, y = retorno_acumulado_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.itub4, aes(x = indicador.points_teste, y = retorno_acumulado_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.petr4, aes(x = indicador.points_teste, y = retorno_acumulado_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo.vale5, aes(x = indicador.points_teste, y = retorno_acumulado_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
    g = ggplot(Global.Resumo, aes(x = indicador.points_teste, y = retorno_acumulado_teste))
    g + geom_point(aes(color = indicador), size=3, show.legend = TRUE)
    
  # Apenas registros com pontuação positiva no treino
    
    Global.Resumo.abev3 <- Global.Resumo.abev3[Global.Resumo.abev3$indicador.points_treino > 0,]
    Global.Resumo.bbdc4 <- Global.Resumo.bbdc4[Global.Resumo.bbdc4$indicador.points_treino > 0,]
    Global.Resumo.itub4 <- Global.Resumo.itub4[Global.Resumo.itub4$indicador.points_treino > 0,]
    Global.Resumo.petr4 <- Global.Resumo.petr4[Global.Resumo.petr4$indicador.points_treino > 0,]
    Global.Resumo.vale5 <- Global.Resumo.vale5[Global.Resumo.vale5$indicador.points_treino > 0,]
    Global.Resumo <- Global.Resumo[Global.Resumo$indicador.points_treino > 0,]
    
  # Apenas registros com retorno positivo no treino
    
##################### old ########################
  
Resumo.abev3.melhores.treino <- Resumo.abev3[as.numeric(as.character(Resumo.abev3$indicador.points_treino)) > 0,]
Resumo.abev3.piores.treino <- Resumo.abev3[as.numeric(as.character(Resumo.abev3$indicador.points_treino)) <= 0,]



#color = indicador

g = ggplot(Resumo.abev3.piores.treino, aes(x = as.numeric(indicador.points_treino), y = as.numeric(indicador.points_teste)))
g + geom_point(aes(color = min_distance_between_operations), size=3, show.legend = TRUE)
#ver cores por mindistancebetop
#ver tambem cores por faixas de minimo e maximo das bandas do rsi

+scale_x_discrete(breaks=c(0,10,50,100,150))+scale_y_discrete(breaks=c(0,10,50,100,150))


  scale_x_discrete()
  scale_x_discrete(breaks=Resumo.abev3.melhores.treino$indicador.points_treino)
#+scale_x_continuous(breaks=seq(1900, 2000, 10))
 # scale_x_continuous(breaks=ii) 

g = ggplot(Resumo.abev3.melhores.piores, aes(x = indicador.points_treino, y = indicador.points_teste))
g + geom_point(aes(color = indicador), size=1, show.legend = TRUE)



Resumo.teste <- Global.Resumo[Global.Resumo$nome_ativo == "teste",]



Resumo.abev3 <- Resumo.abev3[order(as.numeric(as.character(Resumo.abev3$indicador.points_treino))),]

Resumo.abev3.SMA <- Resumo.abev3[Resumo.abev3$indicador == "SMA",]
#Esse funciona. Está ordenado.
Resumo.abev3.SMA <- Resumo.abev3.SMA[order(as.numeric(as.character(Resumo.abev3.SMA$indicador.points_treino))),]

_______________

load("abev3.SMA.longo_26_curto_11.1.0.2.rda")

#Resumo.abev3.melhores.treino <- Resumo.abev3[as.numeric(as.character(Resumo.abev3$indicador.points_treino)) > 105,]
Resumo.abev3.melhores.treino <- Resumo.abev3[as.numeric(as.character(Resumo.abev3$indicador.points_treino)) > 0,]
Resumo.abev3.melhores.treino <- Resumo.abev3.melhores.treino[order(as.numeric(as.character(Resumo.abev3.melhores.treino$indicador.points_treino))),]

rm(Resumo.abev3.melhores.treino)
#Resumo.abev3.melhores.teste <- Resumo.abev3[as.numeric(as.character(Resumo.abev3$indicador.points_teste)) > 46.67999,]
Resumo.abev3.melhores.teste <- Resumo.abev3[as.numeric(as.character(Resumo.abev3$indicador.points_teste)) > 0,]

Resumo.abev3.melhores.retornos.teste <- Resumo.abev3[order(as.numeric(as.character(Resumo.abev3$retorno_acumulado_teste))),]

rm(Global.Resumo)
# -> Datasets de Ativos

# ->> Ativos com todos os registros (adequados para Classificação)

load("itub4.rda")
load("itub4.RSI.MAlength16nivelinferior20nivelsuperior90.2.0.2.rda")

load("abev3.rda")
load("bbdc4.rda")
load("brfs3.rda")
load("bvmf3.rda")
load("ciel3.rda")
load("itsa4.rda")
load("petr3.rda")
load("petr4.rda")
load("ugpa3.rda")
load("vale5.rda")




load("itub4.MACD.nFast_1_nSlow_2_nSig_2.1.0.2.rda")




# <<- Ativos com todos os registros

# ->> Ativos 63 dias (mais de 60 subsets para cada ativo)

load("abev3-63-1.rda")
load("abev3-63-2.rda")
load("abev3-63-3.rda")
load("abev3-63-4.rda")
load("abev3-63-5.rda")
load("abev3-63-6.rda")
load("abev3-63-7.rda")
load("abev3-63-8.rda")
load("abev3-63-9.rda")

load("bbdc4-63-1.rda")
load("bbdc4-63-2.rda")
load("bbdc4-63-3.rda")
load("bbdc4-63-4.rda")
load("bbdc4-63-5.rda")
load("bbdc4-63-6.rda")
load("bbdc4-63-7.rda")
load("bbdc4-63-8.rda")
load("bbdc4-63-9.rda")

load("brfs3-63-1.rda")
load("brfs3-63-2.rda")
load("brfs3-63-3.rda")
load("brfs3-63-4.rda")
load("brfs3-63-5.rda")
load("brfs3-63-6.rda")
load("brfs3-63-7.rda")
load("brfs3-63-8.rda")
load("brfs3-63-9.rda")

load("bvmf3-63-1.rda")
load("bvmf3-63-2.rda")
load("bvmf3-63-3.rda")
load("bvmf3-63-4.rda")
load("bvmf3-63-5.rda")
load("bvmf3-63-6.rda")
load("bvmf3-63-7.rda")
load("bvmf3-63-8.rda")
load("bvmf3-63-9.rda")

load("ciel3-63-1.rda")
load("ciel3-63-2.rda")
load("ciel3-63-3.rda")
load("ciel3-63-4.rda")
load("ciel3-63-5.rda")
load("ciel3-63-6.rda")
load("ciel3-63-7.rda")
load("ciel3-63-8.rda")
load("ciel3-63-9.rda")

load("itsa4-63-1.rda")
load("itsa4-63-2.rda")
load("itsa4-63-3.rda")
load("itsa4-63-4.rda")
load("itsa4-63-5.rda")
load("itsa4-63-6.rda")
load("itsa4-63-7.rda")
load("itsa4-63-8.rda")
load("itsa4-63-9.rda")

load("petr3-63-1.rda")
load("petr3-63-2.rda")
load("petr3-63-3.rda")
load("petr3-63-4.rda")
load("petr3-63-5.rda")
load("petr3-63-6.rda")
load("petr3-63-7.rda")
load("petr3-63-8.rda")
load("petr3-63-9.rda")

load("petr4-63-1.rda")
load("petr4-63-2.rda")
load("petr4-63-3.rda")
load("petr4-63-4.rda")
load("petr4-63-5.rda")
load("petr4-63-6.rda")
load("petr4-63-7.rda")
load("petr4-63-8.rda")
load("petr4-63-9.rda")

load("ugpa3-63-1.rda")
load("ugpa3-63-2.rda")
load("ugpa3-63-3.rda")
load("ugpa3-63-4.rda")
load("ugpa3-63-5.rda")
load("ugpa3-63-6.rda")
load("ugpa3-63-7.rda")
load("ugpa3-63-8.rda")
load("ugpa3-63-9.rda")

load("vale5-63-1.rda")
load("vale5-63-2.rda")
load("vale5-63-3.rda")
load("vale5-63-4.rda")
load("vale5-63-5.rda")
load("vale5-63-6.rda")
load("vale5-63-7.rda")
load("vale5-63-8.rda")
load("vale5-63-9.rda")

# <<- Ativos 63 dias

# ->> Ativos 126 dias

load("abev3-126-1.rda")
load("abev3-126-2.rda")
load("abev3-126-3.rda")
load("abev3-126-4.rda")
load("abev3-126-5.rda")
load("abev3-126-6.rda")
load("abev3-126-7.rda")
load("abev3-126-8.rda")
load("abev3-126-9.rda")

load("bbdc4-126-1.rda")
load("bbdc4-126-2.rda")
load("bbdc4-126-3.rda")
load("bbdc4-126-4.rda")
load("bbdc4-126-5.rda")
load("bbdc4-126-6.rda")
load("bbdc4-126-7.rda")
load("bbdc4-126-8.rda")
load("bbdc4-126-9.rda")

load("brfs3-126-1.rda")
load("brfs3-126-2.rda")
load("brfs3-126-3.rda")
load("brfs3-126-4.rda")
load("brfs3-126-5.rda")
load("brfs3-126-6.rda")
load("brfs3-126-7.rda")
load("brfs3-126-8.rda")
load("brfs3-126-9.rda")

load("bvmf3-126-1.rda")
load("bvmf3-126-2.rda")
load("bvmf3-126-3.rda")
load("bvmf3-126-4.rda")
load("bvmf3-126-5.rda")
load("bvmf3-126-6.rda")
load("bvmf3-126-7.rda")
load("bvmf3-126-8.rda")
load("bvmf3-126-9.rda")

load("ciel3-126-1.rda")
load("ciel3-126-2.rda")
load("ciel3-126-3.rda")
load("ciel3-126-4.rda")
load("ciel3-126-5.rda")
load("ciel3-126-6.rda")
load("ciel3-126-7.rda")
load("ciel3-126-8.rda")
load("ciel3-126-9.rda")

load("itsa4-126-1.rda")
load("itsa4-126-2.rda")
load("itsa4-126-3.rda")
load("itsa4-126-4.rda")
load("itsa4-126-5.rda")
load("itsa4-126-6.rda")
load("itsa4-126-7.rda")
load("itsa4-126-8.rda")
load("itsa4-126-9.rda")

load("petr3-126-1.rda")
load("petr3-126-2.rda")
load("petr3-126-3.rda")
load("petr3-126-4.rda")
load("petr3-126-5.rda")
load("petr3-126-6.rda")
load("petr3-126-7.rda")
load("petr3-126-8.rda")
load("petr3-126-9.rda")

load("petr4-126-1.rda")
load("petr4-126-2.rda")
load("petr4-126-3.rda")
load("petr4-126-4.rda")
load("petr4-126-5.rda")
load("petr4-126-6.rda")
load("petr4-126-7.rda")
load("petr4-126-8.rda")
load("petr4-126-9.rda")

load("ugpa3-126-1.rda")
load("ugpa3-126-2.rda")
load("ugpa3-126-3.rda")
load("ugpa3-126-4.rda")
load("ugpa3-126-5.rda")
load("ugpa3-126-6.rda")
load("ugpa3-126-7.rda")
load("ugpa3-126-8.rda")
load("ugpa3-126-9.rda")

load("vale5-126-1.rda")
load("vale5-126-2.rda")
load("vale5-126-3.rda")
load("vale5-126-4.rda")
load("vale5-126-5.rda")
load("vale5-126-6.rda")
load("vale5-126-7.rda")
load("vale5-126-8.rda")
load("vale5-126-9.rda")

# <<- Ativos 126 dias

# ->> Ativos 252 dias

load("abev3-252-1.rda")
load("abev3-252-2.rda")
load("abev3-252-3.rda")
load("abev3-252-4.rda")
load("abev3-252-5.rda")
load("abev3-252-6.rda")
load("abev3-252-7.rda")
load("abev3-252-8.rda")
load("abev3-252-9.rda")

load("bbdc4-252-1.rda")
load("bbdc4-252-2.rda")
load("bbdc4-252-3.rda")
load("bbdc4-252-4.rda")
load("bbdc4-252-5.rda")
load("bbdc4-252-6.rda")
load("bbdc4-252-7.rda")
load("bbdc4-252-8.rda")
load("bbdc4-252-9.rda")

load("brfs3-252-1.rda")
load("brfs3-252-2.rda")
load("brfs3-252-3.rda")
load("brfs3-252-4.rda")
load("brfs3-252-5.rda")
load("brfs3-252-6.rda")
load("brfs3-252-7.rda")
load("brfs3-252-8.rda")
load("brfs3-252-9.rda")

load("bvmf3-252-1.rda")
load("bvmf3-252-2.rda")
load("bvmf3-252-3.rda")
load("bvmf3-252-4.rda")
load("bvmf3-252-5.rda")
load("bvmf3-252-6.rda")
load("bvmf3-252-7.rda")
load("bvmf3-252-8.rda")
load("bvmf3-252-9.rda")

load("ciel3-252-1.rda")
load("ciel3-252-2.rda")
load("ciel3-252-3.rda")
load("ciel3-252-4.rda")
load("ciel3-252-5.rda")
load("ciel3-252-6.rda")
load("ciel3-252-7.rda")
load("ciel3-252-8.rda")
load("ciel3-252-9.rda")

load("itsa4-252-1.rda")
load("itsa4-252-2.rda")
load("itsa4-252-3.rda")
load("itsa4-252-4.rda")
load("itsa4-252-5.rda")
load("itsa4-252-6.rda")
load("itsa4-252-7.rda")
load("itsa4-252-8.rda")
load("itsa4-252-9.rda")

load("petr3-252-1.rda")
load("petr3-252-2.rda")
load("petr3-252-3.rda")
load("petr3-252-4.rda")
load("petr3-252-5.rda")
load("petr3-252-6.rda")
load("petr3-252-7.rda")
load("petr3-252-8.rda")
load("petr3-252-9.rda")

load("petr4-252-1.rda")
load("petr4-252-2.rda")
load("petr4-252-3.rda")
load("petr4-252-4.rda")
load("petr4-252-5.rda")
load("petr4-252-6.rda")
load("petr4-252-7.rda")
load("petr4-252-8.rda")
load("petr4-252-9.rda")

load("ugpa3-252-1.rda")
load("ugpa3-252-2.rda")
load("ugpa3-252-3.rda")
load("ugpa3-252-4.rda")
load("ugpa3-252-5.rda")
load("ugpa3-252-6.rda")
load("ugpa3-252-7.rda")
load("ugpa3-252-8.rda")
load("ugpa3-252-9.rda")

load("vale5-252-1.rda")
load("vale5-252-2.rda")
load("vale5-252-3.rda")
load("vale5-252-4.rda")
load("vale5-252-5.rda")
load("vale5-252-6.rda")
load("vale5-252-7.rda")
load("vale5-252-8.rda")
load("vale5-252-9.rda")

# <<- Ativos 252 dias

# <- Datasets de Ativos

# -> Datasets de Resumos

load("Resumo.rda")

load("itub4.RSI.MAlength16nivelinferior20nivelsuperior90.2.0.2.rda")

Global.Resumo <- Global.Resumo[Global.Resumo$nome_ativo == "itub4",]
resumo.mindist1 <- Global.Resumo[Global.Resumo$min_distance_between_operations == 1,]
resumo.mindist2 <- Global.Resumo[Global.Resumo$min_distance_between_operations == 2,]
resumo.mindist3 <- Global.Resumo[Global.Resumo$min_distance_between_operations == 3,]
resumo.mindist4 <- Global.Resumo[Global.Resumo$min_distance_between_operations == 4,]
resumo.mindist5 <- Global.Resumo[Global.Resumo$min_distance_between_operations == 5,]

resumo.mindist1.win <- resumo.mindist1[as.double(as.character(resumo.mindist1$indicador.points)) > 0,]
resumo.mindist2.win <- resumo.mindist2[as.double(as.character(resumo.mindist2$indicador.points)) > 0,]
resumo.mindist3.win <- resumo.mindist3[as.double(as.character(resumo.mindist3$indicador.points)) > 0,]
resumo.mindist4.win <- resumo.mindist4[as.double(as.character(resumo.mindist4$indicador.points)) > 0,]
resumo.mindist5.win <- resumo.mindist5[as.double(as.character(resumo.mindist5$indicador.points)) > 0,]

write.csv(resumo.mindist1.win, file = "resumo.mindist1.win.csv")
write.csv(resumo.mindist2.win, file = "resumo.mindist2.win.csv")
write.csv(resumo.mindist3.win, file = "resumo.mindist3.win.csv")
write.csv(resumo.mindist4.win, file = "resumo.mindist4.win.csv")
write.csv(resumo.mindist5.win, file = "resumo.mindist5.win.csv")

Resumo.abev3 <- Global.Resumo[Global.Resumo$nome_ativo == "abev3",]
Resumo.bbdc4 <- Global.Resumo[Global.Resumo$nome_ativo == "bbdc4",]
Resumo.itub4 <- Global.Resumo[Global.Resumo$nome_ativo == "itub4",]
Resumo.petr4 <- Global.Resumo[Global.Resumo$nome_ativo == "petr4",]
Resumo.vale5 <- Global.Resumo[Global.Resumo$nome_ativo == "vale5",]

rm(bbas3.csv, bbdc4.csv, ateste, bteste, i, listaDataFrames, temp, teste)