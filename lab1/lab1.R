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

