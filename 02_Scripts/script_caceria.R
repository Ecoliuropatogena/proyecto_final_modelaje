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
# pars <- c(
  # s = 0.8 , e = 0.08, l = 0.1, o = 1,
  
  # b11 = 0.3, b12 = 0.8, b13 = 0.5, b14 = 0.5,
  # b1d = 0.05, 
  # b1m = 0.03,   # se mantiene, ahora es prob. de transmisión por murciélago cazado
  
  # c_m = 3,      # NUEVO: número promedio de humanos expuestos por murciélago infectado cazado
  
  # u = 0.02, n = 0.5,
  
  # b21 = 0.3, b22 = 0.8, b23 = 0.5, b24 = 0.5,
  # b2d = 0.05,
  
  # y = 0.02,
  
  # k1 = 0.1, a1 = 0.1, s1 = 0.1,
  # k2 = 0.1, a2 = 0.1, s2 = 0.1,
  # k3 = 0.1, a3 = 0.1, s3 = 0.1,
  # a4 = 0.1, s4 = 0.1,
  
#   ent = 0.5
# )

# ------------------------------------------------------------------------------
# Simulaciones con parametros y datos referenciados:

# De latencia a infectado 1 en dias
y <- 1/12.7

# Tasa de transmision:
  # promedio de lo reportado para los 3 paises afectados para nuestra tasa
beta.base <- (0.27+0.45+0.28)/3 #(Althaus, 2014)
    # Althaus, C. L. (2014). 
    # Estimating the Reproduction Number of Ebola Virus (EBOV) During the 2014 Outbreak in West Africa. PLoS Currents, 6. 
    # https://doi.org/10.1371/currents.outbreaks.91afb5e0f279e7f29e7056095255b288

# BETAS DE LA ECUACIÓN DE S1.
b11 <- 0.3
b12 <- 0.8
b13 <- 0.5
b14 <- 0.5
b1m <- 0.03   # se mantiene, ahora es prob. de transmisión por murciélago cazado

# BETAS DE LA ECUACION S2
b22 <- 0.8
b23 <- 0.5
b24 <- 0.5

# como esta es de doctores estos se lavan las manos y tocan el cadaver
b2d <- beta.base * 2.25 * 2.1

# De la poblacion general, participan en ritos y en comidas comunitarias
b1d <- beta.base * 4.22 * 2.84

# Cuidado del paciente al inicio de la enfermedad
b21 <- beta.base * 6.0

# Cambio de fase en la infeccion  en dias
k1 <- 1/3
k2 <- 1/2
k3 <- 1/1

# Letalidad por periodo de infección:
  # Se tiene calculado el CFR general del brote que es del 85.6%, esto lo tomamos
  # como letalidad, pero al nosotros ampliar los tipos de infectados, de manera arbitraria
  # repartimos ese 0.88 entre las 4 fases siguiendo la coherencia de la infeccion
a1 <- 0.05
a2 <- 0.15
a3 <- 0.30
a4 <- 0.38

# Tasas de recuperación:
  # Siguiendo la logica, la tasa de recuperación debe de ser mayor en s1 que en s4
  # Se calculo en base a los pacientes que fueron hospitalizados desde incios de sintomas
  # y los que tardaron más en ser atendidos
s1 <- 1/9.2
s2 <- 1/ 10.5
s3 <- 1/ 13.2
s4 <- 1/15.6

# murcielago
# Estos si son arbitrarios:
  # Tasa natalidad murcielagos
s <- 0.2
  # Conversión a reservorio
l <- 0.4
  # Mortalidad murcielagos
e <- 0.2
  # Tasa de caza de murcielagos:
o <- 0.3
  # número promedio de humanos expuestos por murciélago infectado cazado
c_m = 3  

# Tasa de natalidad humanos:
  # basado en esta pagina:https://datosmacro.expansion.com/demografia/natalidad?anio=2013
  # Tasa de natalidad guinea 2013: 38.38%
  # Sierra leona: 36.60%
  # Liberia 36.23%
nat <- (38.38/1000)+(36.60/1000)+ (36.23/1000) 
n <- nat/3

# Tasa de mortalidad:
u <- 0.02

# DEFUNCIONES: en las ecuaciones lo puse con theta
  # este igual es arbitrario
ent <- 0.5  

parametros.ref <- c(y,
                    b11, b12, b13, b14, b1m, 
                    b22, b23, b24,
                    b2d, b1d, 
                    k1, k2, k3, 
                    a1 ,a2 ,a3, a4, 
                    s1, s2, s3, s4,
                    s, l, e, o, cm,
                    n, u, ent)
#-------------------------------------------------------------------------------
tiempo.red <- seq(0,10, by= 0.05)

# ======== Condiciones iniciales =========== #

condiciones_iniciales <- c(Sm=500 , Lm=300   , S1=500, S2= 100 ,
                           E=5    , I1= 1    , I2= 0 , I3= 0   , I4= 0,
                           R=1    , D = 1)
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Graficas:
out.ref <- ode(condiciones_iniciales, tiempo.red, Ebola, parametros.ref)
matplot(out.ref[, 1], out.ref[, 2:11], type = "l", xlab = "tiempo", ylab = "población",
        main = "SEIR Ébola", lwd = 2)
legend("topright", legend = c("Suceptibles m", "Reservorio m", "Suceptibles 1", "Suceptibles 2", "Expuestos",
                              "Infectados 1", "Infectados 2", "Infectados 3", "Infectados 4", "Recuperados", "Defunciones"),
       col = 1:5, lty = 1, cex = 0.8)

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


#--------------------------------------------------------------------------------
# Estabilidad con los parametros "reales":

eq_1 <- runsteady(
  y = condiciones_iniciales,
  time = c(0, 1000), # Aumenté el tiempo para asegurar convergencia
  func = Ebola,
  parms = parametros.ref
)

print("--- Nuevo Punto de Equilibrio ---")
print(eq_1$y)

# --------------- 2) JACOBIANO EN EL EQUILIBRIO ---------------
J.1 <- jacobian.full(
  y = eq_1$y,
  func = Ebola,
  parms = parametros.ref
)

# --------------- 3) ESTABILIDAD ---------------
print("--- Eigenvalores (Estabilidad) ---")
vals.1 <- eigen(J.1)$values
print(vals.1)

max_real.1 <- max(Re(vals.1))

if(max_real.1 < 0) {
  print(paste("RESULTADO: El sistema ahora es ESTABLE. Max Eigenvalor:", max_real.1))
} else {
  print(paste("RESULTADO: El sistema sigue INESTABLE. Max Eigenvalor:", max_real.1))
}


