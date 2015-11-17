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
jpeg("p4_teo_500.jpg")
plot(p_real,type="b",col="blue",xlab="n°",ylab="Probabilidad teórica",main="Con n = 500")
dev.off()
jpeg("p4_emp_500.jpg")
plot(frec_acum,type="b",col="red",xlab="n°",ylab="Probabilidad empírica", main="Con n = 500")
dev.off()

totalp4_1000 <- 1000
muestra <- rbinom(totalp4_1000,10,0.33)
frec <- sapply(split(muestra,muestra),length)
frec_relat  <- frec/totalp4_1000
frec_acum <- cumsum(frec_relat)
p_real <- pbinom(0:as.integer(max(names(frec_acum))), 10, 0.33)
resta <- abs(frec_acum - p_real)
suma <- sum(resta)
jpeg("p4_teo_1000.jpg")
plot(p_real,type="b",col="blue",xlab="n°",ylab="Probabilidad teórica", main="Con n = 1000")
dev.off()
jpeg("p4_emp_1000.jpg")
plot(frec_acum,type="b",col="red",xlab="n°",ylab="Probabilidad empírica", main="Con n = 1000")
dev.off()


totalp4_1500 <- 1500
muestra <- rbinom(totalp4_1500,10,0.33)
frec <- sapply(split(muestra,muestra),length)
frec_relat  <- frec/totalp4_1500
frec_acum <- cumsum(frec_relat)
p_real <- pbinom(0:as.integer(max(names(frec_acum))), 10, 0.33)
resta <- abs(frec_acum - p_real)
suma <- sum(resta)
frec_acum
p_real
suma
jpeg("p4_teo_1500.jpg")
plot(p_real,type="b",col="blue",xlab="n°",ylab="Probabilidad teórica", main="Con n = 1500")
dev.off()
jpeg("p4_emp_1500.jpg")
plot(frec_acum,type="b",col="red",xlab="n°",ylab="Probabilidad empírica", main="Con n = 1500")
dev.off()

totalp4_1800 <- 100000
muestra <- rbinom(totalp4_1800,10,0.33)
frec <- sapply(split(muestra,muestra),length)
frec_relat  <- frec/totalp4_1800
frec_acum <- cumsum(frec_relat)
p_real <- pbinom(0:as.integer(max(names(frec_acum))), 10, 0.33)
resta <- abs(frec_acum - p_real)
suma <- sum(resta)
frec_acum
p_real
suma
jpeg("p4_teo_100000.jpg")
plot(p_real,type="b",col="blue",xlab="n°",ylab="Probabilidad teórica", main="Con n = 100000")
dev.off()
jpeg("p4_emp_100000.jpg")
plot(frec_acum,type="b",col="red",xlab="n°",ylab="Probabilidad empírica", main="Con n = 100000")
dev.off()

# pregunta 5
totalp5_100 <- 1500
muestra <- rcauchy(totalp5_100, location = 0, scale = 2.5)
frec <- sapply(split(muestra,muestra),length)
frec_relat  <- frec/totalp5_100
frec_acum <- cumsum(frec_relat)
p_real <- pcauchy(0:as.integer(max(names(frec_acum))), location = 0, scale = 2.5)
resta <- abs(frec_acum - p_real)
suma <- sum(resta)
frec_acum
p_real
suma


muestra_p5 <- rcauchy(100, location = 0, scale = 2.5 )
c1 <- cut(muestra_p5, breaks = seq(min(muestra_p5), max(muestra_p5), length.out = 15), include.lowest = FALSE)
frec <- sapply(split(c1,c1),length)
frec_relat <- frec/100
frec_acum <- cumsum(frec_relat)
p_real <- pcauchy(seq(min(muestra_p5),max(muestra_p5),length.out = 15)[-1], location = 0, scale = 2.5 )
resta<- abs(frec_acum - p_real)
suma <- sum(resta)
resta
suma

errores <- c()
for(i in c(10,20,30,40,50,100,200,300,400,500,1000)){
  muestra_p5 <- rcauchy(i, location = 0, scale = 2.5 )
  c1 <- cut(muestra_p5, breaks = seq(min(muestra_p5), max(muestra_p5), length.out = 15), include.lowest = FALSE)
  frec <- sapply(split(c1,c1),length)
  frec_relat <- frec/i
  frec_acum <- cumsum(frec_relat)
  p_real <- pcauchy(seq(min(muestra_p5),max(muestra_p5),length.out = 15)[-1], location = 0, scale = 2.5 )
  resta<- abs(frec_acum - p_real)
  suma <- sum(resta)
  
  errores<-cbind(errores,resta)
  
}

jpeg("p5_boxplot.jpg")
boxplot(errores)
dev.off()