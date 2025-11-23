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
      b21*S2*I1 + b22*S2*I2 + b23*S2*I3 + b24*S2*I4 + b2d*S2*D + b1m*S1*Lm - uE - yE
    
    #infectaos, recuperados y defunciones
    dI1 <-     yE - k1*I1 - a1*I1 - s1*I1
    dI2 <-  k1*I1 - k2*I2 - a2*I2 - s2*I2
    dI3 <-  k2*I2 - k3*I3 - a3*I3 - s3*I3
    dI4 <-  k3*I3 - a4*I4 - s4*I4
    dR  <-  s1*I1 + s2*I2 + s3*I3 + s4*I4 - uR
    dD  <-  a1*I1 + a2*I2 + a3*I3 + a4*I4
      
    return(list(c(dSm, dLm, dS1, dS2, dE, dI1, dI2, dI3, dI4, dR, dD)))
  })
}

