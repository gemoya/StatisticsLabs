load("~/Documents/UTFSM/ILI-280/StatisticsLabs/lab1/lab1Data.Rdata")
attach(data)

cv_mpg<-(sd(mpg)/mean(mpg))
cv_cylinders<-(sd(cylynders)/mean(cylinders))
cv_displacement<-(sd(displacement)/mean(displacement))
cv_horsepower<-(sd(horsepower)/mean(horsepower))
cv_weight<-(sd(weight)/mean(weight))
cv_acceleration<-(sd(acceleration)/mean(acceleration))
cv_model_year<-(sd(model_year)/mean(model_year))

hist(model_year)


cov(displacement, cylinders)/(sd(displacement)*sd(cylinders))
cov(cylinders, acceleration)/(sd(cylinders)*sd(acceleration))

plot(cylinders,horsepower)
plot(model_year,horsepower)
bloxplot(split(horsepower,cylinders))
bloxplot(split(horsepower,model_year))
bloxplot(split(mpg,model_year))


test2<-split(acceleration,model_year)
test3<-split(horsepower,model_year)

(split(cylinders,acceleration))
(split(model_year,horsepower))

consumo1<-consumo1$`70`
consumo<-(split(mpg,model_year))
consumo71<-consumo1$`71`
consumo71<-consumo$`71`
consumo72<-consumo$`72`
consumo73<-consumo$`73`
consumo73<-consumo$`73`

