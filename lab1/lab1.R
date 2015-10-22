load("~/Documents/UTFSM/ILI-280/StatisticsLabs/lab1/lab1Data.Rdata")
attach(data)

cv_mpg<-(sd(mpg)/mean(mpg))
cv_cylinders<-(sd(cylynders)/mean(cylinders))
cv_displacement<-(sd(displacement)/mean(displacement))
cv_horsepower<-(sd(horsepower)/mean(horsepower))
