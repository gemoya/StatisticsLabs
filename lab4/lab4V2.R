red_data<-read.table("~/Documents/UTFSM/ILI-280/StatisticsLabs/lab4/winequality-red.csv",header=TRUE,sep=";")
white_data<-read.table("~/Documents/UTFSM/ILI-280/StatisticsLabs/lab4/winequality/winequality-white.csv",header=TRUE,sep=";")

#red_data<-read.table("C:/Users/Carlos/Google Drive/ESTACA/LEC/LEC4/moodle/winequality-red.csv",header=TRUE,sep=";")
#white_data<-read.table("C:/Users/Carlos/Google Drive/ESTACA/LEC/LEC4/moodle/winequality-white.csv",header=TRUE,sep=";")

red_lm<-lm(formula = quality~.,data=red_data)
white_lm<-lm(formula = quality~.,data=white_data)

total_data<-rbind(red_data,white_data)
total_lm<-lm(formula=quality~.,data=total_data)


#Criterio alpha= 0,05
#drop1(red_lm, test = "F")
drop1(total_lm,test = "F")



### BACKWARD lm_red ###
redbackward1 = update(red_lm, .~. -  density)
#drop1(backward1, test = "F")

redbackward2 <- update(redbackward1, .~. - fixed.acidity)
#drop1(backward2, test = "F")

redbackward3 <- update(redbackward2, .~. - residual.sugar)
#drop1(backward3, test = "F")

redbackward4 <- update(redbackward3, .~. - citric.acid)
#drop1(backward4, test = "F")


### FORWARD lm_red ###
redforward = red_lm
# hay que usar add1... aun no cacho bien como se usa :c 

redforwardFinal = lm(formula = quality~alcohol,data=red_data)
redforward =update(red_lm, .~. -  alcohol)
