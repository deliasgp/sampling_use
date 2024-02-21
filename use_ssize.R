#ALEATORIO ESTRATIFICADO
ssize.es <- function(n=c(),p=c(0.5),z=c(0.95),e=c(0.05),nr=c(0)) {
      a=qnorm(1-(1-z)/2)
      N=sum(n)
      ah = n/N     
      numerador = (sum(n^2*p*(1-p)/ah,na.rm = T))
      denominador = (N*e/a)^2+sum(n*p*(1-p),na.rm = T)
      muestra = ceiling(numerador/denominador*ah*(1/(1-nr)))
      return(muestra)
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

# DATA FRAME CON TAMAÑOS DE MUESTRA POR ESTRATO
ssize_es_df <- function(data = NA, dominio = NA, estratos= NA, e = NA, nr = NA){
  res <- ungroup(data) %>% 
    group_by_at(.vars = estratos) %>%
    summarise(N_h=n())

   res <- res %>% #POBLACION DEL SUB MARCO
    group_by_at(.vars = c(dominio)) %>%
    mutate(n_h=ssize.es(n=N_h,e=e[1],nr = nr)) %>% #MUESTREO ESTRATIFICADO
    ungroup()
   #-------------------------------------------------*
   if (length(e)==2) {
     res <- res %>% 
       mutate(n_h_sob=ssize.as(n=N_h,e=e[2]), #SOBRE MUESTRA ALEATORIA SIMPLE
              n_h_fin=case_when(
                (n_h < n_h_sob)==T & ( n_h_sob < N_h)==T ~ n_h_sob,
                (n_h < n_h_sob)==T & ( n_h_sob < n_h)==T ~ n_h_sob,
                TRUE ~ n_h)
       ) %>% #CUANDO LA POBLACION SEA MENOR 
       mutate(ss_final=case_when(( n_h >=  N_h )==T |  (n_h_fin >= N_h )==T ~ N_h,
                                 TRUE ~ n_h_fin)
       ) %>% 
       filter(N_h>0) %>% ungroup()
   }
   #-------------------------------------------------*
   return(res)
}
