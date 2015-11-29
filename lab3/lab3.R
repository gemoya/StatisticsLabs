attach(datosLec3.RData)
#p1
n1 <- length(x1)
x1 <- rpois(n1,x1)
l1<- function(p, x1) { 
    - sum(dpois(x1, lambda = p, log = TRUE)) 
}
 
 #<- optim(par=c(mean(x1)), fn = l1, x = x1)
optimo1<-optimize(f=l1, interval=c(min(x1),max(x1)), x=x1)
lambda_est<-round(optimo1$minimum)