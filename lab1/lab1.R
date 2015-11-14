load("lab1Data.Rdata")
attach(data)
sd_mpg<-sd(mpg)
mean_mpg<-mean(mpg)
sd_cylinders<-sd(cylinders)
mean_cylinders<-mean(cylinders)
sd_displacement<-sd(displacement)
mean<-displacement<-mean(displacement)
sd_horsepower<-sd(horsepower)
mean_horsepower<-mean(horsepower)
sd_weight<-sd(weight)
mean_weight<-mean(weight)
sd_acceleration<-sd(acceleration)
mean_acceleration<-mean(acceleration)
sd_model_year<-sd(model_year)
mean_model_year<-mean(model_year)
sd_origin<-sd(origin)
mean_origin<-mean(origin)


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

jpeg('boxplot_acceleration_cylinders.jpg')
boxplot(split(acceleration,cylinders))
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
horsepower_3<-horsepower_origin$`3`

acceleration_origin<-split(acceleration,origin)
acceleration_3<-acceleration_origin$`3`


cov_a3_h3<-cov(acceleration_3,horsepower_3)
r_a3_h3<-cov(acceleration_3,horsepower_3)/(sd(acceleration_3)*sd(horsepower_3))

mpg_70_5<-mpg[which(data$cylinders == 5 & (data$model_year == 70 ))]
mpg_71_5<-mpg[which(data$cylinders == 5 & (data$model_year == 71 ))]
mpg_72_5<-mpg[which(data$cylinders == 5 & (data$model_year == 72 ))]
mpg_73_5<-mpg[which(data$cylinders == 5 & (data$model_year == 73 ))]

mpg_74_5<-mpg[which(data$cylinders == 5 & (data$model_year == 74 ))]
mpg_75_5<-mpg[which(data$cylinders == 5 & (data$model_year == 75 ))]
mpg_76_5<-mpg[which(data$cylinders == 5 & (data$model_year == 76 ))]
mpg_77_5<-mpg[which(data$cylinders == 5 & (data$model_year == 77 ))]
mpg_78_5<-mpg[which(data$cylinders == 5 & (data$model_year == 78 ))]
mpg_79_5<-mpg[which(data$cylinders == 5 & (data$model_year == 79 ))]

car_name_7<-car_name[which(data$cylinders == 5 & (data$model_year == 79 ))]


Naranjo_car <- car_name[which(data$mpg==46.6 & data$acceleration == 17.9)]