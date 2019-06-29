
setwd("C:/StockMarket/rworkingdirectory")

source("C:/StockMarket/scripts/PreprocessamentoFunctions.R")
source("C:/StockMarket/scripts/ClassificationFunctions.R")
 
pre_load_libraries()
class_load_libraries()

# Subconjunto, com mesma quantidade de registros, mas apenas colunas importantes para Classificação
# Campos PrecoIncreaseOrDecrease e VolumeIncreaseOrDecrease podem ser úteis também
# Necessário dataframe ativo_subset carregado no Workspace. Usar LeituraRData.R para isso.
ativoClassificacao = ativo_subset[,c("operacao_class_SMA10", "operacao_class_SMA26", "operacao_class_EMA10", "operacao_class_EMA26", "operacao_class_MACD", "operacao_class_FAST_STOCHASTIC", "operacao_class_OBV", "operacao_class_RSI", "goodArea")]

# Preprocessing: substituição de NAs por "Hold" 
ativoClassificacao[is.na(ativoClassificacao)] <- "Hold"

# Divisão do dataset entre treino e teste: 80% treino e 20% teste
ativoClassificacao_train = ativoClassificacao[1 : (nrow(ativoClassificacao) * 0.8),]
ativoClassificacao_test = ativoClassificacao[(nrow(ativoClassificacao) * 0.8) : nrow(ativoClassificacao),]

# -> Árvores de decisão (não funcionando)

# Pacote para Árvores de decisao
install.packages("tree")
library(tree)
 
# Construção do modelo. Warning: NAs introduced by coercion
tree_model = tree(goodArea~., ativoClassificacao_train)

# Error: cannot plot singlenode tree
plot(tree_model)
text(tree_model, pretty = 0)

# Error: type "class" only for classification trees
tree_pred = predict(tree_model, ativoClassificacao_test, type="class")

# <- Árvores de decisão 
 
### CLASSIFICACAO NAYVE BAYES

label <- ativoClassificacao$goodArea
class_naiveBayes(ativoClassificacao_train, ativoClassificacao_test, label) #erro: could not find function "decodeClassLabels"
 
 
 
 # redes neurais
 #ativoClassificacao[is.na(ativoClassificacao)] <- 0 #Hold
 
 #ativoClassificacao$operacao_class_RSI[ativoClassificacao$operacao_class_RSI == "Sell"] <- 2
 
 
 #wine_tt = sample.random(ativoClassificacao)
 
 #wine_train = wine_tt[[1]]
 #wine_test = wine_tt[[2]]
 

 #wine_train = normalize.minmax(wine_train)
 #wine_train_par = wine_train[[2]]
 #wine_train = wine_train[[1]]

 #wine_test <- wine_test[[1]]
 
 #mlp_pred = class_mlp_nnet(wine_train, wine_test, "goodArea")
 #mpp_pred1 <- mlp_pred[[1]]
 #mpp_pred2 <- mlp_pred[[2]]
 #mpp_pred3 <- mlp_pred[[3]]
 #mpp_pred4 <- mlp_pred[[4]]
 
 #ativo_2016$previsao_mlp_pred <- mpl_pred

################################# Esboços ################################
 
 ##### carregar em json
 #require(jsonlite)