
red_data<-read.table("~/Documents/UTFSM/ILI-280/StatisticsLabs/lab4/winequality-red.csv",header=TRUE,sep=";")
white_data<-read.table("~/Documents/UTFSM/ILI-280/StatisticsLabs/lab4/winequality/winequality-white.csv",header=TRUE,sep=";")

red_lm<-lm(formula = quality~.,data=red_data)
white_lm<-lm(formula = quality~.,data=white_data)

