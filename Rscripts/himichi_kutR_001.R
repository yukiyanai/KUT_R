
###  パッケージの読み込み  ###
library(lavaan)
###  データの読み込み  ###
dat <- read.csv("csvdata.csv")
### 終わりだよ～(●・▽・●) ###


###  媒介分析(媒介変数1つ)  ###
###  Step1 単回帰分析  ###
res_lm <- lm(IRI_EC ~ FFMQ_AW, data=dat) 
summary(res_lm)
###  Step2 重回帰分析  ###
res_lm2 <- lm(IRI_EC ~ FFMQ_AW + TAS_All, data=dat) 
summary(res_lm2)
###  Step3 間接効果の推定   ###
model1 <- '
	IRI_EC  ~ c*FFMQ_AW  ### path c ###
	TAS_All ~ a*FFMQ_AW  ### path a ###
	IRI_EC  ~ b*TAS_All  ### path b ###
	Ind := a*b           ### indirect effects ###
	Total := c+(a*b)     ### total effects ###
'
fit1 <- sem(model1, data=dat, bootstrap=2000, se="bootstrap")
parameterEstimates(fit1, ci=T, level=0.95, boot.ci.type="bca.simple")
### 終わりだよ～(●・▽・●) ###

###  媒介分析(媒介変数2つ)  ###
###  Step2 重回帰分析(単回帰分析は省略)  ###
res_lm2 <- lm(IRI_EC ~ FFMQ_AW + EC_All + TAS_All, data=dat) 
summary(res_lm2)
###  Step3 間接効果の推定  ###
model2 <- '
	IRI_EC  ~ c*FFMQ_AW             ### path c  ###
	TAS_All ~ a1*FFMQ_AW            ### path a1 ###
	EC_All  ~ a2*FFMQ_AW            ### path a2 ###
	IRI_EC  ~ b1*TAS_All            ### path b1 ###
	IRI_EC  ~ b2*EC_All             ### path b2 ###
	Ind1 := a1*b1                   ### indirect effects1 ###
	Ind2 := a2*b2                   ### indirect effects2 ###
	Ind  := (a1*b1)+(a2*b2)         ### total indirect effects ###
	Total := c+(a1*b1)+(a2*b2)      ### total effects ###
	TAS_All ~~ EC_All
'
fit2 <- sem(model2, data=dat, bootstrap=2000, se="bootstrap")
parameterEstimates(fit2, ci=T, level=0.95, boot.ci.type="bca.simple")
### 終わりだよ～(●・▽・●) ###


###  構造方程式モデリング  ###
model3 <- '
	EC =~ Q1S2+Q1S9+Q1S20+Q1S22+Q1S4_v+Q1S14_v+Q1S18_v
	AW =~ Q2S5_v+Q2S8_v+Q2S13_v+Q2S18_v+Q2S23_v+Q2S28_v+Q2S34_v+Q2S38_v
	EC ~ AW
'
fit3 <- sem(model3, data=dat, missing="ml", estimator="ML")
summary(fit3, fit.measures=T)
### 終わりだよ～(●・▽・●) ###


###  確証的因子分析  ###
###  4因子  ###
model4 <- '
	PD=~Q1S6+Q1S10+Q1S17+Q1S24+Q1S27+Q1S13_v+Q1S19_v
	EC=~Q1S2+Q1S9+Q1S20+Q1S22+Q1S4_v+Q1S14_v+Q1S18_v
	PT=~Q1S8+Q1S11+Q1S21+Q1S25+Q1S28+Q1S3_v+Q1S15_v
	FS=~Q1S1+Q1S5+Q1S16+Q1S23+Q1S26+Q1S7_v+Q1S12_v
	'
cfares <- cfa(model4, data=dat,orthogonal=F, std.lv=F, estimator="ML", missing="ml") 
summary(cfares, fit.measures=T, standardized=T)
### 終わりだよ～(●・▽・●) ###

###  5因子  ###
model5 <- '
	PD=~Q1S6+Q1S10+Q1S17+Q1S24+Q1S27+Q1S13_v+Q1S19_v
	EC=~Q1S2+Q1S9+Q1S20+Q1S22+Q1S4_v+Q1S14_v+Q1S18_v
	PT=~Q1S8+Q1S11+Q1S21+Q1S25+Q1S28+Q1S3_v+Q1S15_v
	FS=~Q1S1+Q1S5+Q1S16+Q1S23+Q1S26+Q1S7_v+Q1S12_v
	Rev=~Q1S13_v+Q1S19_v+Q1S4_v+Q1S14_v+Q1S18_v+Q1S3_v+Q1S15_v+Q1S7_v+Q1S12_v
	PD~~0*Rev  ###PDとRevは無相関###
	EC~~0*Rev　###ECとRevは無相関###
	PT~~0*Rev　###PTとRevは無相関###
	FS~~0*Rev　###FSとRevは無相関###
	'
cfares2 <- cfa(model5, data=dat, orthogonal=F, std.lv=F, estimator="ML", missing="ml")
summary(cfares2, fit.measures=T, standardized=T) 
### 終わりだよ～(●・▽・●) ###
