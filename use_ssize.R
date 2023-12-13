#ALEATORIO ESTRATIFICADO
ssize.es <- function(n=c(),p=c(0.5),z=c(0.95),e=c(0.05),nr=c(0),ah=NA) {
  a=qnorm(1-(1-z)/2)
  N=sum(n)

  if(max(is.na(ah))==1){
    ah = n/N
  } else {
    ah = ah
  }
  numerador = (sum(n^2*p*(1-p)/ah,na.rm = T))
  denominador = (N*e/a)^2+sum(n*p*(1-p),na.rm = T)
  muestra = ceiling(numerador/denominador*ah*(1/(1-nr)))
  muestra
}

#----------
#ALEATORIO SIMPLE
ssize.as <- function(n=c(),p=c(0.5),z=c(0.95),e=c(0.05),nr=c(0)) {
  a=qnorm(1-(1-z)/2)
  numerador = ((a^2)*(p*(1-p))*n)
  denominador = ((a^2)*(p*(1-p))+(e^2)*(n-1))
  muestra = ceiling(numerador/denominador*(1/(1-nr)))
  muestra
}
#------------------
#ERROR MUESTRAL PARA UN TAMAÑO DE MUESTRA DADO
error.m <- function(N=c(),n=c(),p=c(0.5),
                    z=c(0.95),nr=c(0)) {
    a=qnorm(1-(1-z)/2)
    n.1<-n*(1-nr)
    e.1<-round(a*sqrt(p*(1-p)/n.1)*sqrt(1-n.1/N),5)
    e.1
}
