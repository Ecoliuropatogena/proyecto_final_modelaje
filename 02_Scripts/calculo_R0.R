                 # calculo del R0 #


# Convertir los parámetros a lista para usar pars$parametro
pars <- as.list(pars)

# ------------------------------
# Cálculo del R0 paso a paso
# ------------------------------

# 1. Fuerza total de infección (todos los b1x y b2x)
beta_total <- pars$b11 + pars$b12 + pars$b13 + pars$b14 +
  pars$b21 + pars$b22 + pars$b23 + pars$b24

# 2. Tasa total de salida de los infectados (recuperación + muerte)
salida_total <- pars$k1 + pars$k2 + pars$k3 + pars$a4

# 3. Corrección por entierro seguro
ajuste_entierro <- 1 - pars$ent

# 4. Cálculo final del R0
R0 <- beta_total / salida_total * ajuste_entierro

# Mostrar resultado
R0

