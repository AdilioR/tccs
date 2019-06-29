source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphics.R")
loadlibrary("reshape")
loadlibrary("dplyr")
loadlibrary("RColorBrewer")
loadlibrary("gridExtra")

col_set <- brewer.pal(20, 'Spectral')
col_2 <- col_set[c(4,9)]
col_2b <- col_set[c(9,3)]    
col_3 <- col_set[c(4,9,11)]
col_4 <- col_set[c(3,5,7,9)]  
col_bsh <- c(col_set[c(4, 10)], "#cccccc")

plot_size(4, 3)

#Asset.TransactionList.simplelinebsh$variable <- factor(Asset.TransactionList.simplelinebsh$variable, levels = c("Buy", "Sell", "Hold"))
col_bsh <- c(col_set[c(4, 10)], "#cccccc")

correlation <- function()
{
	//medio
	cor.test(Result.StrategyList$mean_return_train, Result.StrategyList$mean_return_test, method='kendal', alternative='two.sided', exact=FALSE)
	cor.test(Result.StrategyList$mean_return_train, Result.StrategyList$mean_return_test, method='spearman', alternative='two.sided', exact=FALSE)
	cor.test(Result.StrategyList$mean_return_train, Result.StrategyList$mean_return_test, method='pearson', alternative='two.sided', exact=FALSE)
	
	//acumulado
	cor.test(Result.StrategyList$cumulative_return_train, Result.StrategyList$cumulative_return_test, method='kendal', alternative='two.sided', exact=FALSE)
	cor.test(Result.StrategyList$cumulative_return_train, Result.StrategyList$cumulative_return_test, method='spearman', alternative='two.sided', exact=FALSE)
	cor.test(Result.StrategyList$cumulative_return_train, Result.StrategyList$cumulative_return_test, method='pearson', alternative='two.sided', exact=FALSE)
	
	//medio sma
	mediosma <- Result.StrategyList[Result.StrategyList$strategy_name=="SMA",]
	cor.test(mediosma$mean_return_train, mediosma$mean_return_test, method='kendal', alternative='two.sided', exact=FALSE)
	cor.test(mediosma$mean_return_train, mediosma$mean_return_test, method='spearman', alternative='two.sided', exact=FALSE)
	cor.test(mediosma$mean_return_train, mediosma$mean_return_test, method='pearson', alternative='two.sided', exact=FALSE)
	
	//acumulado sma
	mediosma <- Result.StrategyList[Result.StrategyList$strategy_name=="SMA",]
	cor.test(mediosma$cumulative_return_train, mediosma$cumulative_return_test, method='kendal', alternative='two.sided', exact=FALSE)
	cor.test(mediosma$cumulative_return_train, mediosma$cumulative_return_test, method='spearman', alternative='two.sided', exact=FALSE)
	cor.test(mediosma$cumulative_return_train, mediosma$cumulative_return_test, method='pearson', alternative='two.sided', exact=FALSE)
	
	//medio macd
	mediomacd <- Result.StrategyList[Result.StrategyList$strategy_name=="MACD",]
	cor.test(mediomacd$mean_return_train, mediomacd$mean_return_test, method='kendal', alternative='two.sided', exact=FALSE)
	cor.test(mediomacd$mean_return_train, mediomacd$mean_return_test, method='spearman', alternative='two.sided', exact=FALSE)
	cor.test(mediomacd$mean_return_train, mediomacd$mean_return_test, method='pearson', alternative='two.sided', exact=FALSE)
	
	//acumulado macd
	mediomacd <- Result.StrategyList[Result.StrategyList$strategy_name=="MACD",]
	cor.test(mediomacd$cumulative_return_train, mediomacd$cumulative_return_test, method='kendal', alternative='two.sided', exact=FALSE)
	cor.test(mediomacd$cumulative_return_train, mediomacd$cumulative_return_test, method='spearman', alternative='two.sided', exact=FALSE)
	cor.test(mediomacd$cumulative_return_train, mediomacd$cumulative_return_test, method='pearson', alternative='two.sided', exact=FALSE)
	
	//medio rsi
	mediorsi <- Result.StrategyList[Result.StrategyList$strategy_name=="RSI",]
	cor.test(mediorsi$mean_return_train, mediorsi$mean_return_test, method='kendal', alternative='two.sided', exact=FALSE)
	cor.test(mediorsi$mean_return_train, mediorsi$mean_return_test, method='spearman', alternative='two.sided', exact=FALSE)
	cor.test(mediorsi$mean_return_train, mediorsi$mean_return_test, method='pearson', alternative='two.sided', exact=FALSE)
	
	//acumulado rsi
	mediorsi <- Result.StrategyList[Result.StrategyList$strategy_name=="RSI",]
	cor.test(mediorsi$cumulative_return_train, mediorsi$cumulative_return_test, method='kendal', alternative='two.sided', exact=FALSE)
	cor.test(mediorsi$cumulative_return_train, mediorsi$cumulative_return_test, method='spearman', alternative='two.sided', exact=FALSE)
	cor.test(mediorsi$cumulative_return_train, mediorsi$cumulative_return_test, method='pearson', alternative='two.sided', exact=FALSE)


//////////////////
	# Teste de correlação: Kendall's tau correlation coefficient test
    cor.test(Result.StrategyList$mean_return_train, Result.StrategyList$mean_return_test, method='kendal', alternative='two.sided', approximation='exact')
	
     
	# Teste de correlação: Spearman rank correlation test
    cor.test(Result.StrategyList$average_return_train, Result.StrategyList$average_return_train, method='spearman', alternative='two.sided', approximation='exact')
    
	# Teste de correlação: Kendall's tau correlation coefficient test
    cor.test(Result.StrategyList$cumulative_return_train, Result.StrategyList$cumulative_return_train, method='kendal', alternative='two.sided', approximation='exact')
     
	# Teste de correlação: Spearman rank correlation test
    
	
	cor.test(as.numeric(Result.StrategyList$cumulative_return_train), as.numeric(Result.StrategyList$cumulative_return_test), method='kendal', alternative='two.sided', approximation='exact')
    cor.test(as.numeric(Result.StrategyList$cumulative_return_train), as.numeric(Result.StrategyList$cumulative_return_test), method='spearman', alternative='two.sided', approximation='exact')
	
}

graphics <- function()
{
	require(ggplot2)
  	g = ggplot(Result.StrategyList, aes(x = indicador.points_treino, y = indicador.points_teste))
  	g + geom_point(aes(color = strategy_name), size=3, show.legend = TRUE)
  
  	# min distance
  	g = ggplot(Result.StrategyList, aes(x = indicador.points_treino, y = indicador.points_teste))
  	g + geom_point(aes(color = min_distance_between_operations), size=3, show.legend = TRUE)
  
  	# precision tolerance
  	g = ggplot(Result.StrategyList, aes(x = indicador.points_treino, y = indicador.points_teste))
  	g + geom_point(aes(color = precision_tolerance), size=3, show.legend = TRUE)


  	#retorno medio
  	g = ggplot(Result.StrategyList, aes(x = mean_return_train, y = mean_return_test))
  	g + geom_point(aes(color = strategy_name), size=3, show.legend = TRUE)
	
	# retorno acumulado
  	g = ggplot(Result.StrategyList, aes(x = cumulative_return_train, y = cumulative_return_test))
  	g + geom_point(aes(color = strategy_name), size=3, show.legend = TRUE)
	
	#retorno medio apenas rsi
  	g = ggplot(Result.StrategyList[Result.StrategyList$strategy_name == 'RSI',], aes(x = mean_return_train, y = mean_return_test))
  	g + geom_point(aes(color = strategy_name), size=3, show.legend = TRUE)
	
	#retorno medio apenas sma
  	g = ggplot(Result.StrategyList[Result.StrategyList$strategy_name == 'SMA',], aes(x = mean_return_train, y = mean_return_test))
  	g + geom_point(aes(color = strategy_name), size=3, show.legend = TRUE)
	
	#retorno medio apenas macd
  	g = ggplot(Result.StrategyList[Result.StrategyList$strategy_name == 'MACD',], aes(x = mean_return_train, y = mean_return_test))
  	g + geom_point(aes(color = strategy_name), size=3, show.legend = TRUE)
	

	Asset.TransactionList.Partial <- Asset.TransactionList[Asset.TransactionList$Date > "2017-01-01",]
  	#expected classes
  	g = ggplot(Asset.TransactionList.Partial, aes(x = Date, y = Adj_Close))
	g + geom_point(aes(color = ExpectedClasses), size=3, show.legend = TRUE)
	#predicted classes
	g = ggplot(Asset.TransactionList.Partial, aes(x = Date, y = Adj_Close))
	g + geom_point(aes(color = PredictedClasses), size=2, show.legend = TRUE)
}


PrepareDataToVisualization <- function()
{
	Asset.TransactionList.simpleline <<- Asset.TransactionList
	Asset.TransactionList.simpleline$variable <- "transaction"
	
	names(Asset.TransactionList.simpleline)[names(Asset.TransactionList.simpleline) == "Date"] <- "x"
	names(Asset.TransactionList.simpleline)[names(Asset.TransactionList.simpleline) == "Adj_Close"] <- "value"
	
	generate.simplescatterplot.series(Asset.TransactionList.simpleline)
	
	
}

PrepareDataToVisualizationBSH <- function()
{
	Asset.TransactionList.simplelinebsh <<- Asset.TransactionList
	
	names(Asset.TransactionList.simplelinebsh)[names(Asset.TransactionList.simplelinebsh) == "Date"] <<- "x"
	names(Asset.TransactionList.simplelinebsh)[names(Asset.TransactionList.simplelinebsh) == "Adj_Close"] <<- "value"
	names(Asset.TransactionList.simplelinebsh)[names(Asset.TransactionList.simplelinebsh) == "PredictedClasses"] <<- "variable"
	
	
	generate.scatterplot.series(Asset.TransactionList.simplelinebsh)
	
}

PrepareDataToVisualizationStrategyMean <- function()
{
	Asset.TransactionList.scatterplot <<- Result.StrategyList
	
	names(Asset.TransactionList.scatterplot)[names(Asset.TransactionList.scatterplot) == "mean_return_train"] <<- "x"
	names(Asset.TransactionList.scatterplot)[names(Asset.TransactionList.scatterplot) == "mean_return_test"] <<- "value"
	names(Asset.TransactionList.scatterplot)[names(Asset.TransactionList.scatterplot) == "strategy_name"] <<- "variable"
	
	
	generate.scatterplot.series.StrategyMean(Asset.TransactionList.scatterplot)
	
}
generate.scatterplot.series.StrategyMean <- function(dataset)
{
	grf <- plot.scatter(dataset %>% filter(variable %in% c('SMA', 'MACD', 'RSI')), 
                     colors=col_3, label_x = "Retorno médio dentro da amostra", label_y = "Retorno médio fora da amostra") 
	grf <- grf + theme(axis.text.x = element_text(angle=0, hjust=1))
	
	grf <- grf + scale_fill_manual(values=c("SMA"=col_3[1], "MACD"=col_3[2], "RSI"=col_3[3]))

	plot(grf)
}


generate.plot.series <- function(dataset)
{
	grf <- plot.series(dataset %>% filter(variable %in% c('Buy', 'Sell', 'Hold')),colors=col_bsh) #preto, 4 e 10
	grf <- grf + theme(axis.text.x = element_text(angle=45, hjust=1))
	
	plot(grf)
}

generate.simpleplot.series <- function(dataset)
{
	grf <- simpleplot.series(dataset %>% filter(variable %in% c('transaction')),colors="black") 
	grf <- grf + theme(axis.text.x = element_text(angle=45, hjust=1))
	
	plot(grf)
}

generate.scatterplot.series <- function(dataset)
{
	grf <- plot.scatter(dataset %>% filter(variable %in% c('Buy', 'Sell', 'Hold')), 
                     colors=col_bsh, label_x = "Data da Transação", label_y = "Preço de Fechamento Ajustado") 
	grf <- grf + theme(axis.text.x = element_text(angle=45, hjust=1))
	
	grf <- grf + scale_fill_manual(values=c("Buy"=col_bsh[1], "Sell"=col_bsh[2], "Hold"=col_bsh[3]))

	plot(grf)
}

generate.simplescatterplot.series <- function(dataset)
{
	grf <- simpleplot.scatter(dataset %>% filter(variable %in% c('transaction')), 
                     colors="black", label_x = "Data da Transação", label_y = "Preço de Fechamento Ajustado") 
	grf <- grf + theme(axis.text.x = element_text(angle=45, hjust=1))
	

	plot(grf)
}


simpleplot.scatter <- function(series, label_series = "", label_x = "", label_y = "", colors = NULL) {
  grf <- ggplot(data=series) + geom_point(aes(x = x, y = value, colour=variable), size=1)
  if (!is.null(colors)) {
    grf <- grf + scale_color_manual(values=colors)
  }
  grf <- grf + labs(color=label_series)
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) 
  grf <- grf + theme(legend.position = "none") + theme(legend.key = element_blank()) 
  return(grf)
}

simpleplot.series <- function(series, label_series = "", label_x = "", label_y = "", colors = NULL) {
  grf <- ggplot(data=series) + geom_point(aes(x = x, y = value, colour=variable), size=1.5) + geom_line(aes(x = x, y = value, colour=variable, group=variable), size=1)
  if (!is.null(colors)) {
    grf <- grf + scale_color_manual(values=colors)
  }
  grf <- grf + labs(color=label_series)
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) 
  grf <- grf + theme(legend.position = "none") + theme(legend.key = element_blank()) 
  return(grf)
}

getDetailedDecisions <- function()
{
  Main.addSpecificEMAResultIntoAsset(nFast = 4, nSlow = 5)
  
}

newgraphics.boxplot <- function()
{
	#source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphics.R")

	loadlibrary("reshape")
	loadlibrary("RColorBrewer")
	loadlibrary("gridExtra")
	loadlibrary("dplyr")
	
	col.set <- brewer.pal(11, 'Spectral')
	mycolors <- col.set[c(1,5,9)]
	
	dataviz_cumulative_return_test <<- subset(Result.StrategyList, as.integer(quantity_operation_test) > 0 & as.integer(quantity_operation_test) > 0, select=c(strategy_name, cumulative_return_test))
	
	names(dataviz_cumulative_return_test)[names(dataviz_cumulative_return_test) == "strategy_name"] <<- "variable"
	names(dataviz_cumulative_return_test)[names(dataviz_cumulative_return_test) == "cumulative_return_test"] <<- "value"
	
	dataviz_cumulative_return_test$value <<- as.double(dataviz_cumulative_return_test$value)
	
	as.data.frame(dataviz_cumulative_return_test)
	
	grfb <<- plot.boxplot(dataviz_cumulative_return_test, colors=mycolors, label_y="Retorno acumulado no período")
	options(repr.plot.width=4, repr.plot.height=3)
	plot(grfb)
}

plot.scatter <- function(series, label_series = "", label_x = "", label_y = "", colors = NULL, size=1) {
  grf <- ggplot(data=series) + geom_point(aes(x = x, y = value, color=variable), size=size)
  if (!is.null(colors)) {
    grf <- grf + scale_color_manual(values=colors)
  }
  grf <- grf + labs(color=label_series)
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) 
  grf <- grf + theme(legend.position = "bottom") + theme(legend.key = element_blank()) 
  return(grf)
}