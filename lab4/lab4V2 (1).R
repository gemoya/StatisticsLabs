#red_data<-read.table("~/Documents/UTFSM/ILI-280/StatisticsLabs/lab4/winequality-red.csv",header=TRUE,sep=";")
#white_data<-read.table("~/Documents/UTFSM/ILI-280/StatisticsLabs/lab4/winequality/winequality-white.csv",header=TRUE,sep=";")

#red_data<-read.table("C:/Users/Carlos/Google Drive/ESTACA/LEC/LEC4/moodle/winequality-red.csv",header=TRUE,sep=";")
#white_data<-read.table("C:/Users/Carlos/Google Drive/ESTACA/LEC/LEC4/moodle/winequality-white.csv",header=TRUE,sep=";")

red_data<-read.table("~/Documents/StatisticsLabs/lab4/winequality-red.csv",header=TRUE,sep=";")
white_data<-read.table("~/Documents/StatisticsLabs/lab4/winequality-white.csv",header=TRUE,sep=";")


wine_data<-rbind(red_data,white_data)

lm_wine <- lm(formula = quality~.,data=wine_data)
#Criterio alpha= 0,05
drop1(lm_wine, test = "F")

### BACKWARD lm_red ###
backward1 = update(lm_wine, .~. -  citric.acid)
drop1(backward1, test = "F")

backward2 <- update(backward1, .~. - chlorides)
drop1(backward2, test = "F")
summary(backward2)

### FORWARD ###

# lm_wine2: modelo con todas las variables, iremos eliminando una por una y observando los Pr
lm_wine2 <- lm(formula = quality~1, data=wine_data)
add1(lm_wine2, scope = ~fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, test = "F")

lm_wine2<-update(lm_wine2, .~. + alcohol)
add1(lm_wine2, scope = ~fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, test = "F")

lm_wine2<-update(lm_wine2, .~. + free.sulfur.dioxide)
add1(lm_wine2, scope = ~fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, test = "F")

lm_wine2<-update(lm_wine2, .~. + volatile.acidity)
add1(lm_wine2, scope = ~fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, test = "F")

lm_wine2<-update(lm_wine2, .~. + sulphates)
add1(lm_wine2, scope = ~fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, test = "F")

lm_wine2<-update(lm_wine2, .~. + residual.sugar)
add1(lm_wine2, scope = ~fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, test = "F")

lm_wine2<-update(lm_wine2, .~. + total.sulfur.dioxide)
add1(lm_wine2, scope = ~fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, test = "F")

lm_wine2<-update(lm_wine2, .~. + chlorides)
add1(lm_wine2, scope = ~fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, test = "F")

lm_wine2<-update(lm_wine2, .~. + pH)

add1(lm_wine2, scope = ~fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, test = "F")

lm_wine2<-update(lm_wine2, .~. + density)
add1(lm_wine2, scope = ~fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, test = "F")


lm_wine2<-update(lm_wine2, .~. + fixed.acidity)
add1(lm_wine2, scope = ~fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, test = "F")


add1(lm_wine2, scope = ~fixed.acidity + volatile.acidity + citric.acid + 
       residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + 
       density + pH + sulphates + alcohol, test = "F")



lm_wine2 <- lm(formula = quality~ fixed.acidity + volatile.acidity + citric.acid + 
                residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + 
                density + pH + sulphates + alcohol, data=wine_data)

# lm_wine3: modelo que empieza con 0 variables y se le iran agregando una por una
lm_wine3 <- lm(formula = quality~1,data=wine_data)
summary(lm_wine3)

#observamos los Pr de lm_wine2, tenemos 5 candidatos con el mismo valor de Pr, seleccionamos el que tenga mayor correlacion con quality
#volatile.acidity       -0.26569948
#residual.sugar         -0.03698048
#total.sulfur.dioxide   -0.04138545
#sulphates               0.038485446
#alcohol                 0.444318520
#Seleccionamos alcohol, entonces lo agregamos a lm_wine3 y lo eleminamos de lm_wine2.
add1(lm_wine3, scope = ~alcohol, test = "F")
summary(lm_wine3)

lm_wine2 <- update(lm_wine2, .~. - alcohol)
summary(lm_wine2)

# Se repiten los pasos hasta que en lm_wine2 solo queden variables con Pr>alpha...



