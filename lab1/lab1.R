load("~/Documents/UTFSM/ILI-280/StatisticsLabs/lab1/lab1Data.Rdata")
attach(data)
sd(mpg)
mean(mpg)
sd(cylinders)
mean(cylinders)
sd(displacement)
mean(displacement)
sd(horsepower)
mean(horsepower)
sd(weight)
mean(weight)
sd(acceleration)
mean(acceleration)
sd(model_year)
mean(model_year)
sd(origin)
mean(origin)


cv_mpg<-(sd(mpg)/mean(mpg))
cv_cylinders<-(sd(cylynders)/mean(cylinders))
cv_displacement<-(sd(displacement)/mean(displacement))
cv_horsepower<-(sd(horsepower)/mean(horsepower))
cv_weight<-(sd(weight)/mean(weight))
cv_acceleration<-(sd(acceleration)/mean(acceleration))
cv_model_year<-(sd(model_year)/mean(model_year))
cv_origin<-(sd(origin)/mean(origin))

jpeg('model_year_histogram.jpg')
hist(model_year)
dev.off()
jpeg('bp_mpg_year.jpg')
boxplot(split(mpg,model_year))
dev.off()
jpeg('bp_cylinders_year.jpg')
boxplot(split(model_year,cylinders))
dev.off()
jpeg('bp_displacement_year.jpg')
boxplot(split(displacement,model_year))
dev.off()
jpeg('bp_horsepower_year.jpg')
boxplot(split(horsepower,model_year))
dev.off()
jpeg('bp_weight_year.jpg')
boxplot(split(weight,model_year))
dev.off()
jpeg('bp_acceleration_year.jpg')
boxplot(split(acceleration,model_year))
dev.off()
jpeg('bp_origin_year.jpg')
boxplot(split(model_year,origin))
dev.off()

jpeg('boxplot_displacement_cylinders.jpg')
boxplot(split(displacement,cylinders))
dev.off()

jpeg('boxplot_horsepower_cylinders.jpg')
boxplot(split(horsepower,cylinders))
dev.off()
jpeg('boxplot_horsepower_model_year.jpg')
boxplot(split(horsepower,model_year))
dev.off()
cov_dis_cyl<-cov(displacement, cylinders)
r_displacement_cylinders<-cov(displacement, cylinders)/(sd(displacement)*sd(cylinders))
cov_cyl_accel<-cov(cylinders,acceleration)
r_cyl_accel<-cov(cylinders, acceleration)/(sd(cylinders)*sd(acceleration))

plot(cylinders,horsepower)
plot(model_year,horsepower)
boxplot(split(horsepower,cylinders))
boxplot(split(horsepower,model_year))
boxplot(split(mpg,model_year))


test2<-split(acceleration,model_year)
test3<-split(horsepower,model_year)

(split(cylinders,acceleration))
(split(model_year,horsepower))


consumo1<-consumo1$`70`
jpeg('boxplot_mpg_model_year.jpg')
boxplot(split(mpg,model_year))
dev.off()
jpeg('boxplot_mpg_origin.jpg')
boxplot(split(mpg,origin))
dev.off()
jpeg('boxplot_horsepower_origin.jpg')
boxplot(split(horsepower,origin))
dev.off()
jpeg('boxplot_acceleration_origin.jpg')
boxplot(split(acceleration,origin))
dev.off()
mpg_origin<-split(mpg,origin)
mpg_1<-mpg_origin$`1`

horsepower_origin<-split(horsepower,origin)
horsepower_1<-horsepower_origin$`1`

acceleration_origin<-split(acceleration,origin)
acceleration_1<-acceleration_origin$`1`

mean(acceleration_1)
mean(acceleration)
sd(acceleration)
sd(acceleration_1)

cov_a1_h1<-cov(acceleration_1,horsepower_1)
r_a1_h1<-cov(acceleration_1,horsepower_1)/(sd(acceleration_1)*sd(horsepower_1))


consumo71<-consumo1$`71`
consumo71<-consumo$`71`
consumo72<-consumo$`72`
consumo73<-consumo$`73`
consumo73<-consumo$`73`

