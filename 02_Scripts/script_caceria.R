       # Modificando el numero de infectados por murcielago #


library(deSolve)
library(rootSolve)

Ebola <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
# --- cantidades auxiliares ---
Nhum <- S1 + S2 + E + I1 + I2 + I3 + I4 + R

expos_caceria <- b1m * c_m * o * Lm * (S1 / Nhum)

# --- murciélagos ---
dSm <- s - e*Sm - l*Lm*Sm - o*Sm
dLm <- l*Lm*Sm - e*Lm - o*Lm

# --- humanos S1 y S2 ---
dS1 <- -b11*S1*I1 - b12*S1*I2 - b13*S1*I3 - b14*S1*I4 -
  b1d*S1*D + n - u*S1 - expos_caceria

dS2 <- -b21*S2*I1 - b22*S2*I2 - b23*S2*I3 - b24*S2*I4 -
  b2d*S2*D + n - u*S2

# --- expuestos ---
dE <-  b11*S1*I1 + b12*S1*I2 + b13*S1*I3 + b14*S1*I4 +
  b1d*S1*D +
  b21*S2*I1 + b22*S2*I2 + b23*S2*I3 + b24*S2*I4 +
  b2d*S2*D +
  expos_caceria - u*E - y*E

# --- infectados ---
dI1 <- y*E - k1*I1 - a1*I1 - s1*I1
dI2 <- k1*I1 - k2*I2 - a2*I2 - s2*I2
dI3 <- k2*I2 - k3*I3 - a3*I3 - s3*I3
dI4 <- k3*I3 - a4*I4 - s4*I4

# --- recuperados ---
dR <- s1*I1 + s2*I2 + s3*I3 + s4*I4 - u*R

# --- defunciones ---
dD <- a1*I1 + a2*I2 + a3*I3 + a4*I4 - ent*D

return(list(c(dSm, dLm, dS1, dS2, dE, dI1, dI2, dI3, dI4, dR, dD)))
  })
}




# ====== parametros === #
pars <- c(
  s = 0.8 , e = 0.08, l = 0.1, o = 1,
  
  b11 = 0.3, b12 = 0.8, b13 = 0.5, b14 = 0.5,
  b1d = 0.05, 
  b1m = 0.03,   # se mantiene, ahora es prob. de transmisión por murciélago cazado
  
  c_m = 3,      # NUEVO: número promedio de humanos expuestos por murciélago infectado cazado
  
  u = 0.02, n = 0.5,
  
  b21 = 0.3, b22 = 0.8, b23 = 0.5, b24 = 0.5,
  b2d = 0.05,
  
  y = 0.02,
  
  k1 = 0.1, a1 = 0.1, s1 = 0.1,
  k2 = 0.1, a2 = 0.1, s2 = 0.1,
  k3 = 0.1, a3 = 0.1, s3 = 0.1,
  a4 = 0.1, s4 = 0.1,
  
  ent = 0.5
)





# ======== Condiciones iniciales =========== #

condiciones_iniciales <- c(Sm=500 , Lm=300   , S1=500, S2= 100 ,
                           E=5    , I1= 1    , I2= 0 , I3= 0   , I4= 0,
                           R=1    , D = 1)

# --------------- 1) PUNTO DE EQUILIBRIO ---------------

eq <- runsteady(
  y = condiciones_iniciales,
  time = c(0, 1000), # Aumenté el tiempo para asegurar convergencia
  func = Ebola,
  parms = pars
)

print("--- Nuevo Punto de Equilibrio ---")
print(eq$y)

# --------------- 2) JACOBIANO EN EL EQUILIBRIO ---------------
J <- jacobian.full(
  y = eq$y,
  func = Ebola,
  parms = pars
)

# --------------- 3) ESTABILIDAD ---------------
print("--- Eigenvalores (Estabilidad) ---")
vals <- eigen(J)$values
print(vals)

max_real <- max(Re(vals))

if(max_real < 0) {
  print(paste("RESULTADO: El sistema ahora es ESTABLE. Max Eigenvalor:", max_real))
} else {
  print(paste("RESULTADO: El sistema sigue INESTABLE. Max Eigenvalor:", max_real))
}




