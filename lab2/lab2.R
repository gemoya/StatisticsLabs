p1  <-sample(c("A","C","C"),1000, replace=T, prob = c(4/15,4/15,4/15))
#p1  <-sample(c("A","C","C"),1000, replace=T, prob = c(0.33,0.33,0.33))


totalp4_100 <- 100
muestra <- rbinom(totalp3_100,10,0.33)
frec <- sapply(split(muestra,muestra),length)
frec_relat  <- frec/totalp4_100
frec_acum <- cumsum(frec_relat)
p_real <- pbinom(0:as.integer(max(names(frec_acum))), 10, 0.33)
resta <- abs(frec_acum - p_real)
suma <- sum(resta)
suma
jpeg("p4_teo_100.jpg")
plot(p_real,type="b",col="blue",xlab="n°",ylab="Probabilidad teórica", main="Con n = 100")
dev.off()
jpeg("p4_emp_100.jpg")
plot(frec_acum,type="b",col="red",xlab="n°",ylab="Probabilidad empírica", main="Con n = 100")
dev.off()

totalp3_500 <- 500
muestra <- rbinom(totalp3_500,10,0.33)
frec <- sapply(split(muestra,muestra),length)
frec_relat  <- frec/totalp3_500
frec_acum <- cumsum(frec_relat)
p_real <- pbinom(0:as.integer(max(names(frec_acum))), 10, 0.33)
resta <- abs(frec_acum - p_real)
suma <- sum(resta)
jpeg("p3_teo_1000.jpg")
plot(p_real,type="b",col="blue",xlab="n°",ylab="Probabilidad teórica",main="Con n = 500")
dev.off()
jpeg("p3_emp_1000.jpg")
plot(frec_acum,type="b",col="red",xlab="n°",ylab="Probabilidad empírica", main="Con n = 500")
dev.off()

totalp3_1000 <- 1000
muestra <- rbinom(totalp3_1000,10,0.33)
frec <- sapply(split(muestra,muestra),length)
frec_relat  <- frec/totalp3_1000
frec_acum <- cumsum(frec_relat)
p_real <- pbinom(0:as.integer(max(names(frec_acum))), 10, 0.33)
resta <- abs(frec_acum - p_real)
suma <- sum(resta)
jpeg("p3_teo_10000.jpg")
plot(p_real,type="b",col="blue",xlab="n°",ylab="Probabilidad teórica")
dev.off()
jpeg("p3_emp_10000.jpg")
plot(frec_acum,type="b",col="red",xlab="n°",ylab="Probabilidad empírica")
dev.off()


totalp3_1500 <- 1500
muestra <- rbinom(totalp3_1500,10,0.33)
frec <- sapply(split(muestra,muestra),length)
frec_relat  <- frec/totalp3_1500
frec_acum <- cumsum(frec_relat)
p_real <- pbinom(0:as.integer(max(names(frec_acum))), 10, 0.33)
resta <- abs(frec_acum - p_real)
suma <- sum(resta)
jpeg("p3_teo_100000.jpg")
plot(p_real,type="b",col="blue",xlab="n°",ylab="Probabilidad teórica")
dev.off()
jpeg("p3_emp_100000.jpg")
plot(frec_acum,type="b",col="red",xlab="n°",ylab="Probabilidad empírica")
dev.off()

