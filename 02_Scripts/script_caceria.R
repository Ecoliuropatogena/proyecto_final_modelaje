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
            # tardas 12 días en pasar de latente a infectado
            # entonces por día la latencia avanza de la siguiente manera
            # 1/12

# Tasa de transmision:
  # promedio de lo reportado para los 3 paises afectados para nuestra tasa
beta.base <- (0.27+0.45+0.28)/3 #(Althaus, 2014)
    # Althaus, C. L. (2014). 
    # Estimating the Reproduction Number of Ebola Virus (EBOV) During the 2014 Outbreak in West Africa. PLoS Currents, 6. 
    # https://doi.org/10.1371/currents.outbreaks.91afb5e0f279e7f29e7056095255b288



# BETAS DE LA ECUACIÓN DE S1.
b11 <- 0.27
b12 <- 0.33
b13 <- 0.39
b14 <- 0.45
b1m <- 0.03   # se mantiene, ahora es prob. de transmisión por murciélago cazado

# BETAS DE LA ECUACION S2
  # Cuidado del paciente al inicio de la enfermedad
b21 <- beta.base * 6.0

b22 <- 0.8
b23 <- 0.5
b24 <- 0.5

# como esta es de doctores estos se lavan las manos y tocan el cadaver
b2d <- beta.base * 2.25 * 2.1

# De la poblacion general, participan en ritos y en comidas comunitarias
b1d <- beta.base * 4.22 * 2.84


# Cambio de fase en la infeccion  en dias
k1 <- 1/6   
k2 <- 1/2
k3 <- 1/1

# Letalidad por periodo de infección:
  # Se tiene calculado el CFR general del brote que es del 88%, esto lo tomamos
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

s1 <- 1/ 9.2    # Por día avanzas aprox 1/9 en el proceso de recuperación 
s2 <- 1/ 10.5   # Por día avanzas 1/10 de la infección
s3 <- 1/ 13.2   
s4 <- 1/ 15.6

# murcielago
# Estos si son arbitrarios:
  # Tasa natalidad murcielagos
s <- 1/360
  # Conversión a reservorio
l <- 1/75
  # Mortalidad murcielagos
e <- 1/1960
  # Tasa de caza de murcielagos:
o <- 1/1000
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
                    s, l, e, o, c_m,
                    n, u, ent)



#========================= versión en limpio ==========

beta.base <- (0.27 + 0.45 + 0.28) / 3 

# Vector de parámetros consolidado
parametros.ref <- c(
  # --- Dinámica de Murciélagos ---
  s = 1/360,    # Tasa natalidad murciélagos
  e = 1/1960,   # Mortalidad natural murciélagos (Vida larga)
  l = 1/75,     # Tasa de conversión a reservorio (Susceptible -> Infectado)
  o = 1/1000,   # Tasa de caza de murciélagos
  c_m = 3,      # Número promedio de humanos expuestos por murciélago cazado
  
  # --- Demografía Humana ---
  # Tasa de natalidad promedio (Guinea, Sierra Leona, Liberia 2013)
  n = ((38.38/1000) + (36.60/1000) + (36.23/1000)) / 3,
  u = 0.02,     # Tasa de mortalidad general humana
  
  # --- Betas (Tasas de Transmisión) ---
  # Ecuación S1 (Población general)
  b11 = 0.27, b12 = 0.33, b13 = 0.39, b14 = 0.45, 
  b1m = 0.03,   # Prob. de transmisión por contacto con murciélago
  b1d = beta.base * 4.22 * 2.84, # Transmisión en ritos/comidas comunitarias
  
  # Ecuación S2 (Personal de salud/Hospital)
  b21 = beta.base * 6.0, # Cuidado inicial del paciente
  b22 = 0.8, b23 = 0.5, b24 = 0.5,
  b2d = beta.base * 2.25 * 2.1, # Doctores tocando cadáveres/lavado
  
  # --- Progresión de la Enfermedad ---
  y = 1/12.7,   # Tasa de latencia a infectado (1/días)
  
  # Fases de Infección (k=cambio fase, a=letalidad, s=recuperación)
  # Fase 1
  k1 = 1/6, a1 = 0.05, s1 = 1/9.2,
  # Fase 2
  k2 = 1/2, a2 = 0.15, s2 = 1/10.5,
  # Fase 3
  k3 = 1/1, a3 = 0.30, s3 = 1/13.2,
  # Fase 4 (Solo letalidad y recuperación, no hay cambio k4)
  a4 = 0.38, s4 = 1/15.6,
  
  # --- Tasa de Entierro ---
  ent = 0.5     # Tasa de entierro seguro (aprox 2 días)
)





#-------------------------------------------------------------------------------
tiempo.red <- seq(0,10, by= 1)

# ======== Condiciones iniciales =========== #

condiciones_iniciales <- c(Sm=1000 , Lm=30   , S1=2700, S2= 300 ,
                           E=50    , I1= 10    , I2= 0 , I3= 0   , I4= 0,
                           R=10    , D = 1)
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Graficas:
out.ref <- ode(condiciones_iniciales, tiempo.red, Ebola, parametros.ref)
matplot(out.ref[, 1], out.ref[, 2:12], type = "l", xlab = "tiempo", ylab = "población",
        main = "SEIR Ébola", lwd = 2)
legend("topright", legend = c("Suceptibles m", "Reservorio m", "Suceptibles 1", "Suceptibles 2", "Expuestos",
                              "Infectados 1", "Infectados 2", "Infectados 3", "Infectados 4", "Recuperados", "Defunciones"),
       col = 1:5, lty = 1, cex = 1)



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

# ----------------------------------------------------------------------------------
# calculo del R0 #


# Convertir los parámetros a lista para usar pars$parametro
parametros.ref <- as.list(parametros.ref)

# ------------------------------
# Cálculo del R0 paso a paso
# ------------------------------

# 1. Fuerza total de infección (todos los b1x y b2x)
beta_total <- parametros.ref$b11 + parametros.ref$b12 + parametros.ref$b13 + parametros.ref$b14 +
  parametros.ref$b21 + parametros.ref$b22 + parametros.ref$b23 + parametros.ref$b24

# 2. Tasa total de salida de los infectados (recuperación + muerte)
salida_total <- parametros.ref$k1 + parametros.ref$k2 + parametros.ref$k3 + parametros.ref$a4

# 3. Corrección por entierro seguro
ajuste_entierro <- 1 - parametros.ref$ent

# 4. Cálculo final del R0
R0 <- beta_total / salida_total * ajuste_entierro

# Mostrar resultado
R0
