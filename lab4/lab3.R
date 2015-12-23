data <- read.csv2("~/Documents/UTFSM/ILI-280/StatisticsLabs/lab4/winequality/winequality-red.csv")
attach(data)
lm_red<-lm(formula=quality~fixed.acidity+ volatile.acidity + citric.acid + residual.sugar + chlorides+ free.sulfur.dioxide+total.sulfur.dioxide+density+ pH+sulphates+alcohol,data=data)