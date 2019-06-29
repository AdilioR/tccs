require(ggplot2)

#scatter plot
plot(Resumo.abev3$indicador.points_treino ~ Resumo.abev3$indicador.points_teste)

g = ggplot(Resumo.abev3, aes(x = indicador.points_treino, y = indicador.points_teste))
g + geom_point(aes(color = indicador), size=5, show.legend = TRUE)


load("ciel3-63-13.rda")

#precos do ativo
g = ggplot(ativo_subset, aes(x = Date, y = Adj.Close))
g + geom_point(aes(color = operacao_SMA10), size=5, show.legend = TRUE)



Resumo63 <- Resumo[Resumo$qtd_dias_negociacao == 63,]
Resumo126 <- Resumo[Resumo$qtd_dias_negociacao == 126,]
Resumo252 <- Resumo[Resumo$qtd_dias_negociacao == 252,]



#plot(ativo_total$PrecoIncreaseOrDecrease.percent ~ ativo_total$VolumeIncreaseOrDecrease.percent)

boxplot(Resumo$retorno_acumulado ~ indicador, data = Resumo, xlab = "Qtd dias negociação", ylab = "Retorno acumulado", main = "Retorno acumulado", outline = FALSE)

boxplot(Resumo$retorno_acumulado ~ qtd_dias_negociacao, data = Resumo, xlab = "Qtd dias negociação", ylab = "Retorno acumulado", main = "Retorno acumulado", outline = FALSE)

#ADICIONAR BEH
boxplot(Resumo126$retorno_acumulado ~ Resumo126$indicador, data = Resumo, xlab = "Qtd dias negociação", ylab = "Retorno acumulado", main = "Retorno acumulado 63 dias", outline = TRUE, names = c("SMA(10)", "SMA(26)", "EMA(10)", "EMA(26)", "MACD", "STO", "OBV", "RSI"))
boxplot(Resumo126$retorno_acumulado ~ Resumo126$indicador, data = Resumo, xlab = "Qtd dias negociação", ylab = "Retorno acumulado", main = "Retorno acumulado 63 dias", outline = FALSE, names = c("SMA(10)", "SMA(26)", "EMA(10)", "EMA(26)", "MACD", "STO", "OBV", "RSI"))

boxplot(Resumo126$retorno_acumulado ~ Resumo126$indicador, data = Resumo, xlab = "Qtd dias negociação", ylab = "Retorno acumulado", main = "Retorno acumulado 63 dias", outline = TRUE, names = c("SMA(10)", "SMA(26)", "EMA(10)", "EMA(26)", "MACD", "STO", "OBV", "RSI"))
boxplot(Resumo126$retorno_acumulado ~ Resumo126$indicador, data = Resumo, xlab = "Qtd dias negociação", ylab = "Retorno acumulado", main = "Retorno acumulado 63 dias", outline = FALSE, names = c("SMA(10)", "SMA(26)", "EMA(10)", "EMA(26)", "MACD", "STO", "OBV", "RSI"))

boxplot(Resumo126$retorno_acumulado ~ Resumo126$indicador, data = Resumo, xlab = "Qtd dias negociação", ylab = "Retorno acumulado", main = "Retorno acumulado 63 dias", outline = TRUE, names = c("SMA(10)", "SMA(26)", "EMA(10)", "EMA(26)", "MACD", "STO", "OBV", "RSI"))
boxplot(Resumo126$retorno_acumulado ~ Resumo126$indicador, data = Resumo, xlab = "Qtd dias negociação", ylab = "Retorno acumulado", main = "Retorno acumulado 63 dias", outline = FALSE, names = c("SMA(10)", "SMA(26)", "EMA(10)", "EMA(26)", "MACD", "STO", "OBV", "RSI"))


# #plot(ativo_2014_2016$Adj.Close ~ ativo_2014_2016$Date)
# #hist(ativo_2014_2016$Adj.Close)
# 
# plot(ativo_2014_2016$retorno_percentual_operacao ~ ativo_2014_2016$Date)
# SMA26-plot-retorno-date
# 
# hist(ativo_2014_2016$retorno_percentual_operacao)
# SMA26-hist-retorno
# 
# plot(ativo_2014_2016$retorno_percentual_operacao ~ ativo_2014_2016$Adj.Close)
# SMA26-plot-retornoXpreco
# 
# #boxplot(ativo_2014_2016$Adj.Close)
# 
# boxplot(ativo_2014_2016$retorno_percentual_operacao) #FAZER DO BEH
# SMA26-boxplot-retorno
# 
# #qqnorm: quanto mais próximo de y=x, mas próximo de distribuição normal
# #qqnorm(ativo_2014_2016$Adj.Close)
# #qqline(ativo_2014_2016$Adj.Close)
# 
# qqnorm(ativo_2014_2016$retorno_percentual_operacao)
# qqline(ativo_2014_2016$retorno_percentual_operacao)
# SMA26-qqnorm-retorno
# 
# qqplot(ativo_2014_2016$Adj.Close, ativo_2014_2016$retorno_percentual_operacao)
# fm <- lm(ativo_2014_2016$retorno_percentual_operacao ~ ativo_2014_2016$Adj.Close)
# abline(coef(fm), lty=4)
# SMA26-qqplot-precoXretorno
# 

# analise visual


  g = ggplot(ativo_subset, aes(x = Date, y = Adj.Close))
  g + geom_point(aes(color = bestDecision), size=5, show.legend = TRUE)

 g = ggplot(ativo_subset, aes(x = Date, y = Adj.Close))
g + geom_point(aes(color = goodDecision), size=5, show.legend = TRUE)
 
 g = ggplot(ativo_subset, aes(x = Date, y = Adj.Close))
 g + geom_point(aes(color = goodArea), size=5, show.legend = TRUE)
 
 ## rsi
 #g = ggplot(ativo_subset, aes(x = Date, y = Adj.Close))
 #g + geom_point(aes(color = operacao_class_RSI), size=8, show.legend = TRUE)

# 
#ggplot(ativo_2016, aes(group = bestDecision, x = bestDecision, y = Adj.Close)) + geom_boxplot()
# SMA26-boxplot-preco por operacao
