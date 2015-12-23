attach(datosLec3.RData)
#p1
x1 <- clientesPorHora
l_1 <- function(lam1,x1){
  -(-(length(x1)*lam1)+(log(lam1)*sum(x1)))
}
lam_opt<-optimize(f=l_1, interval = c(min(x1), max(x1)), x=x1)
lambda1_est <- lam_opt$minimum

#p2
x2 <- tiempoDeServicio
l_2 <- function(beta1,x2){
  -(sum(log(1/beta1) - (x2/beta1)))
}
beta_opt<-optimize(f=l_2, interval = c(min(x2), max(x2)), x=x2)
lambda2_est <- 1/(beta_opt$minimum)

muestra<-ventasDiarias

func <- function(p,x){
  media <- p[1]
  desviacion <- p[2]
  
  -( -0.5*(1/desviacion^2) * (sum(x-media)^2) * length(x)*log(desviacion*sqrt(2*pi)) )  
}

resultado <- optim( par = c(30000,10000), 
                    fn=func, 
                    method = c("L-BFGS-B"), 
                    lower = min(muestra), upper = max(muestra), 
                    x=muestra)

print(resultado)