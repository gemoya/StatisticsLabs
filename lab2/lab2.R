#https://rpubs.com/matabuelas/monty_hall
#p1
n1 <- 10
n2 <- 50
n3 <- 100
n4 <- 250
n5 <- 500
n6 <- 1000

R1 = c()
R2 = c()
R3 = c()
R4 = c()
R5 = c()
R6 = c()

f <- function(n){
  vector <- c()
  for(i in 1:100) {
    puertas=replicate(n,sample(c(1,1,1,1,0),replace=F))
    experimento= t(puertas)
    
    jugador= replicate(n,sample(c(1,2,3),size=1))
    idpos= 5*(0:(n-1))
    poselm= which(t(experimento==0))-idpos
    cuantos=sum(jugador==poselm)
    probnc= cuantos/n
    # Probabilidad de acertar si cambiamos de puerta
    probc= 1-probnc
    
    vector <- c(vector, probc)
  }
  
  return(vector)
}



R1 <- f(n1)
R2 <- f(n2)
R3 <- f(n3)
R4 <- f(n4)
R5 <- f(n5)
R6 <- f(n6)


data <- data.frame(R1,R2,R3,R4,R5,R6)

boxplot(R1,R2,R3,R4,R5,R6)

boxplot(data, las = 2, names = c("n=10","n=50","n=100","n=250","n=500","n=1000"), horizontal = FALSE)


#p2

n10    = 10
n50    = 50
n100   = 100
n250   = 250
n500   = 500
n1000  = 1000


f <- function(n){
  prob <- c()
  for(j in 1:100){
    e1 <- runif(n, 0, 60)
    e2 <- runif(n, 0, 60)
    dif <- abs(e1-e2)
    R=c()
    for(i in dif){
      if(i<10)
        R<- c(R,1)
      else
        R<- c(R,0)
    }
    prob <- c(prob,mean(R))
  }
  return(prob)
}


R10  <- f(n10)
R50  <- f(n50)
R100 <- f(n100)
R250 <- f(n250)
R500 <- f(n500)
R1000 <- f(n1000)



data <- data.frame(R10,R100,R500,R1000)
boxplot(data, las = 2, names = c("n=10","n=100","n=500","n=1000"), horizontal = FALSE)






#p4

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

errores_5a <- c()
c1 <- c()
dif_5a <- c()
for(i in c(10,20,30,40,50,100,200,300,400,500,1000,1500,2000,5000,10000,100000)){
  muestra_p5 <- rcauchy(i, location = 0, scale = 2.5)
  c1 <- cut(muestra_p5, breaks = seq(min(muestra_p5), max(muestra_p5), length.out = 15), include.lowest = FALSE)
  frec <- sapply(split(c1,c1),length)
  frec_relat <- frec/i
  frec_acum <- cumsum(frec_relat)
  p_real <- pcauchy(seq(min(muestra_p5),max(muestra_p5),length.out = 15)[-1], location = 0, scale = 2.5 )

  dif<- abs(frec_acum - p_real)
  precision <- sum(dif)
  
  dif_5a<- cbind(dif_5a,dif)
  errores_5a<-cbind(errores_5a,precision)
  
}

jpeg("p5a_boxplot_dif.jpg")
boxplot(dif_5a)
dev.off()
jpeg("p5a_boxplot_conv.jpg")
boxplot(errores_5a)
dev.off()




errores_5b <- c()
c1 <- c()
dif_5b <- c()
for(i in c(10,20,30,40,50,100,200,300,400,500,1000,1500,2000,5000,10000,100000)){
  muestra_p5 <- rcauchy(i, location = 25, scale = 30)
  c1 <- cut(muestra_p5, breaks = seq(min(muestra_p5), max(muestra_p5), length.out = 15), include.lowest = FALSE)
  frec <- sapply(split(c1,c1),length)
  frec_relat <- frec/i
  frec_acum <- cumsum(frec_relat)
  p_real <- pcauchy(seq(min(muestra_p5),max(muestra_p5),length.out = 15)[-1], location = 25, scale = 30 )

  dif<- abs(frec_acum - p_real)
  precision <- sum(dif)
  
  dif_5b<- cbind(dif_5b,dif)
  errores_5b<-cbind(errores_5b,precision)
  
}

jpeg("p5b_boxplot_dif.jpg")
boxplot(dif_5b)
dev.off()

jpeg("p5b_boxplot_conv.jpg")
boxplot(errores_5b)
dev.off()

#p6
errores_6<-c()
c1 <- c()
dif_6 <- c()
for(i in c(10,20,30,40,50,100,200,300,400,500,1000,1500,2000,5000,10000,100000)){
  muestra_p6 <- rweibull(i, 10, 40)
  c1 <- cut(muestra_p6, breaks = seq(min(muestra_p6), max(muestra_p6), length.out = 15), include.lowest = FALSE)
  frec <- sapply(split(c1,c1),length)
  frec_relat <- frec/i
  frec_acum <- cumsum(frec_relat)
  p_real <- pweibull(seq(min(muestra_p6),max(muestra_p6),length.out = 15)[-1], 10, 40 )
  dif<- abs(frec_acum - p_real)
  precision <- sum(dif)
  
  dif_6<- cbind(dif_6,dif)
  errores_6<-cbind(errores_6,precision)
  
}
#boxplots
jpeg("p6_boxplot_dif.jpg")
boxplot(dif_6)
dev.off()

jpeg("p6_boxplot_conv.jpg")
boxplot(errores_6)
dev.off()
