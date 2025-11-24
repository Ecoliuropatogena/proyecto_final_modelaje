          # Simulación con DeSolve #
library(deSolve)


Ebola <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    #murcielago
    dSm <- s*Sm - e*Sm - l*Lm*Sm - o*Sm
    dLm <- l*Lm*Sm - e*Lm - o*Lm
    
    #humanos
    dS1 <- -b11*S1*I1 - b12*S1*I2 - b13*S1*I3 - b14*S1*I4 - b1d*S1*D + l - u*S1 - b1m*S1*Lm 
    dS2 <- -b21*S2*I1 - b22*S2*I2 - b23*S2*I3 - b24*S2*I4 - b2d*S2*D + l - u*S2
    
    #expuestos
    dE  <-  b11*S1*I1 + b12*S1*I2 + b13*S1*I3 + b14*S1*I4 + b1d*S1*D +
      b21*S2*I1 + b22*S2*I2 + b23*S2*I3 + b24*S2*I4 + b2d*S2*D + b1m*S1*Lm - u*E - y*E
    
    #infectaos, recuperados y defunciones
    dI1 <-    y*E - k1*I1 - a1*I1 - s1*I1
    dI2 <-  k1*I1 - k2*I2 - a2*I2 - s2*I2
    dI3 <-  k2*I2 - k3*I3 - a3*I3 - s3*I3
    dI4 <-  k3*I3 - a4*I4 - s4*I4
    dR  <-  s1*I1 + s2*I2 + s3*I3 + s4*I4 - u*R
    dD  <-  a1*I1 + a2*I2 + a3*I3 + a4*I4
      
    return(list(c(dSm, dLm, dS1, dS2, dE, dI1, dI2, dI3, dI4, dR, dD)))
  })
}

pars <- c(s = 0.8 , e = 0.08, l= 0.1, o= 1,
          b11=0.3 , b12= 0.8, b13= 0.5, b14= 0.5, b1d = 0.05, b1m = 0.03, u = 0.02,
          b21= 0.3, b22= 0.8, b23= 0.5, b24= 0.5, b2d = 0.05, b1m = 0.03, y = 0.02,
          k1 = 0.1, a1 = 0.1, s1 = 0.1,
          k2 = 0.1, a2 = 0.1, s2 = 0.1,
          k3 = 0.1, a3 = 0.1, s3 = 0.1,
                    a4 = 0.1, s4 = 0.1)

condiciones_iniciales <- c(Sm=500 , Lm=300   , S1=500, S2= 100 ,
                           E=5    , I1= 1    , I2= 0 , I3= 0   , I4= 0,
                           R=1    , D = 1)

tiempo <- seq(0,10, by= 0.05)

out <- ode(condiciones_iniciales, tiempo, Ebola, pars)

matplot(out[, 1], out[, 2:11], type = "l", xlab = "tiempo", ylab = "población",
        main = "SEIQR", lwd = 2)
legend("topright", legend = c("Suceptibles m", "Reservorio m", "Suceptibles 1", "Suceptibles 2", "Expuestos",
                              "Infectados 1", "Infectados 2", "Infectados 3", "Infectados 4", "Recuperados", "Defunciones"),
       col = 1:5, lty = 1, cex = 0.8)


