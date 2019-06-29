setwd("C:/StockMarket/rworkspace")

load("abev3-300-1.rda")
load("bbdc4.rda")
load("brfs3.rda")
load("bvmf3.rda")
load("ciel3.rda")
load("itsa4.rda")
load("petr3.rda")
load("petr4.rda")
load("ugpa3.rda")
load("vale5.rda")
load("ciel3-63-1.rda")
load("ciel3-63-2.rda")

load("ciel3-252-1.rda")
load("ciel3-252-2.rda")
load("ciel3-252-3.rda")
load("ciel3-252-4.rda")
load("ciel3-252-5.rda")


require(fBasics)


cor.test(ativo_total$PrecoIncreaseOrDecrease.percent, ativo_total$VolumeIncreaseOrDecrease.percent,method="pearson",alternative="two.sided",conf.level = 0.95)

