#!/usr/bin/env Rscript
# =============================================================================
# m19_exito.R — M19 · Perfiles de éxito y algoritmo de mejora · v1 (01/11/2026)
# Proyecto PLATAFORMA LEEDUCA (UMA). Autor: PJ López-Pérez, con asistencia de Claude.
#
# Qué hace
#   1. Simula dos cohortes en los mismos colegios (SIN y CON Preleo, como 2013/2014)
#      con una "verdad" conocida: tipos de cambio por tramo, maleabilidad por
#      sesión, dosis de Preleo, error de medida y efecto Mateo.
#   2. Estima los tipos de cambio (primeras diferencias) de forma ingenua (MCO) y
#      corregida por fiabilidad (método de momentos con las fiabilidades del manual).
#   3. Estima la maleabilidad con el contraste SIN/CON dentro de colegio.
#   4. Ajusta el modelo de probabilidad de éxito y lo valida en colegios no usados.
#   5. Compara el plan del algoritmo con estrategias habituales (reparto uniforme,
#      "lo más débil primero", solo decodificación) aplicándolas al mundo simulado.
#   6. Prueba el bucle cerrado (actualizar) con 1 y con 4 monitoreos por trimestre.
#   7. Genera el conjunto de datos sintéticos para la práctica / concurso de clase.
#
# Uso:   Rscript m19_exito.R [carpeta_salida]
# Solo R base. Tiempo aproximado: 1–2 minutos.
# Con datos reales: ver la sección 9 (qué sustituir).
# =============================================================================

invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")))
args <- commandArgs(trailingOnly = TRUE)
SALIDA <- if (length(args)) args[1] else "salida_m19"
dir.create(SALIDA, showWarnings = FALSE, recursive = TRUE)
SEMILLA <- 20261101
N_BOOT <- 200

# ---- 0. Verdad del simulador ------------------------------------------------
K   <- c("pse", "pal", "ran", "cf", "let", "voc")
ETQ <- c(pse = "Pseudopalabras", pal = "Palabras", ran = "RAN",
         cf = "Conciencia fonológica", let = "Conocimiento de letras", voc = "Vocabulario")
TRAMOS <- c("bajo (<P25)", "medio (P25–P50)", "alto (>P50)")

# Tipos de cambio verdaderos: ppm por DT de cambio en la habilidad, según el tramo
BETA_V <- rbind(pse = c(8, 5, 2), pal = c(6, 4, 2), ran = c(7, 4, 1),
                cf  = c(3, 2, .5), let = c(1, .5, 0), voc = c(1, 1, .5))
M_V    <- c(pse = .40, pal = .40, ran = .15, cf = .50, let = .50, voc = .20) # DT / 10 sesiones
M_SD_IND <- .35          # heterogeneidad individual de la maleabilidad (log-normal)
DECAE  <- 0.95           # rendimiento decreciente de cada sesión adicional en el trimestre
DOSIS_PRELEO <- c(pse = 6, pal = 4, ran = 0, cf = 6, let = 4, voc = 0) # sesiones/trimestre
MU     <- c(pse = .35, pal = .35, ran = .15, cf = .25, let = .20, voc = .15) # crecimiento natural
CARGA  <- c(pse = .7, pal = .7, ran = .5, cf = .6, let = .6, voc = .5)
GAMMA  <- 0.08           # efecto Mateo: ppm de ganancia por ppm de nivel previo, por trimestre
T_MAX  <- 6              # momentos 6.1, 6.2, 6.3, 7.1, 7.2, 7.3
ERR_S  <- 0.40           # error de medida en habilidades (DT); fiabilidad ≈ .86
ERR_VL <- 3              # error de medida en velocidad lectora (ppm)
Q25    <- qnorm(.25)
M_PRIOR <- c(ran = .10, voc = .25) # sin dosis Preleo: valor de la literatura (a propósito, no exacto)

MOMENTO <- function(t) c("6.1", "6.2", "6.3", "7.1", "7.2", "7.3")[t]

# ---- 1. Utilidades ----------------------------------------------------------
ganancia_sesiones <- function(m, e) (m / 10) * (1 - DECAE^e) / (1 - DECAE)
tramo <- function(z) ifelse(z < Q25, 1L, ifelse(z < 0, 2L, 3L))
col_mat <- function(lst, n) matrix(unlist(lst), nrow = n, dimnames = list(NULL, names(lst)))

# Integral de una pendiente por tramos entre s0 y s1 (cortes en P25 y P50 de la norma)
integral_tramos <- function(s0, s1, b, c1, c2) {
  cortes <- c(-Inf, c1, c2, Inf)
  lo <- pmin(s0, s1); hi <- pmax(s0, s1); sg <- sign(s1 - s0)
  tot <- 0
  for (r in 1:3) tot <- tot + b[r] * pmax(0, pmin(hi, cortes[r + 1]) - pmax(lo, cortes[r]))
  sg * tot
}

auc <- function(p, y) {
  r <- rank(p); n1 <- sum(y); n0 <- sum(!y)
  (sum(r[y]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# ---- 2. Simulador -----------------------------------------------------------
# Un paso de trimestre en el mundo "verdadero"
avanzar <- function(S0, VL0, ses, crec, mult, u, norma, eps_s, eps_vl) {
  n <- nrow(S0)
  gan <- col_mat(lapply(K, function(k) ganancia_sesiones(M_V[k] * mult[, k], ses[, k])), n)
  colnames(gan) <- K
  S1 <- S0 + crec + eps_s + gan
  contrib <- Reduce(`+`, lapply(K, function(k) {
    c1 <- norma$m[k] + Q25 * norma$s[k]; c2 <- norma$m[k]
    integral_tramos(S0[, k], S1[, k], BETA_V[k, ], c1, c2)
  }))
  dVL <- 3 + contrib + GAMMA * (VL0 - 30) + 1.5 * u + eps_vl
  list(S = S1, VL = pmax(0, VL0 + dVL))
}

simular <- function(n_esc = 40, n_por = 25, semilla = SEMILLA, err_s = ERR_S, err_vl = ERR_VL) {
  set.seed(semilla)
  esc  <- rep(rep(seq_len(n_esc), each = n_por), 2)
  coh  <- rep(c("SIN", "CON"), each = n_esc * n_por)
  n    <- length(esc)
  fid  <- runif(n_esc, .4, 1)
  expo <- ifelse(coh == "CON", fid[esc], 0)
  u    <- rnorm(n_esc)[esc]
  a    <- rnorm(n)
  nK   <- length(K)
  S    <- array(NA_real_, c(n, nK, T_MAX), dimnames = list(NULL, K, NULL))
  VL   <- matrix(NA_real_, n, T_MAX)
  S[, , 1] <- col_mat(lapply(K, function(k) CARGA[k] * a + sqrt(1 - CARGA[k]^2) * rnorm(n)), n)
  VL[, 1]  <- pmax(0, 25 + 8 * a + 3 * u + rnorm(n, 0, 5))
  crec <- col_mat(lapply(K, function(k) MU[k] + rnorm(n, 0, .30)), n); colnames(crec) <- K
  mult <- matrix(exp(rnorm(n * nK, -M_SD_IND^2 / 2, M_SD_IND)), n, nK, dimnames = list(NULL, K))
  eps_s  <- array(rnorm(n * nK * T_MAX, 0, .15), c(n, nK, T_MAX))
  eps_vl <- matrix(rnorm(n * T_MAX, 0, 4), n)
  ses_preleo <- round(outer(expo, DOSIS_PRELEO)); colnames(ses_preleo) <- K
  normas <- vector("list", T_MAX)
  sin <- coh == "SIN"
  for (t in 2:T_MAX) {
    normas[[t - 1]] <- list(m = colMeans(S[sin, , t - 1]), s = apply(S[sin, , t - 1], 2, sd))
    st <- avanzar(S[, , t - 1], VL[, t - 1], ses_preleo, crec, mult, u, normas[[t - 1]],
                  eps_s[, , t], eps_vl[, t])
    S[, , t] <- st$S; VL[, t] <- st$VL
  }
  normas[[T_MAX]] <- list(m = colMeans(S[sin, , T_MAX]), s = apply(S[sin, , T_MAX], 2, sd))
  e_s  <- array(rnorm(length(S), 0, err_s), dim(S))
  e_vl <- matrix(rnorm(length(VL), 0, err_vl), n)
  list(n = n, esc = esc, coh = coh, expo = expo, u = u, crec = crec, mult = mult,
       S = S, VL = VL, Sobs = S + e_s, VLobs = pmax(VL + e_vl, 0), e_vl = e_vl,
       normas = normas, eps_s = eps_s, eps_vl = eps_vl, err_s = err_s, err_vl = err_vl)
}

# Normas observadas (referencia SIN) y puntuaciones z por momento
normas_obs <- function(sim) {
  sin <- sim$coh == "SIN"
  lapply(seq_len(T_MAX), function(t) list(
    m = colMeans(sim$Sobs[sin, , t]), s = apply(sim$Sobs[sin, , t], 2, sd),
    mvl = mean(sim$VLobs[sin, t]), svl = sd(sim$VLobs[sin, t])))
}
z_obs <- function(sim, t, no) sweep(sweep(sim$Sobs[, , t], 2, no[[t]]$m), 2, no[[t]]$s, "/")

# ---- 3. estimar_beta: tipos de cambio por tramo -----------------------------
# Datos largos de transiciones t-1 -> t
# desfase = 1: tramo con la puntuación observada en t-1 (comparte error con el cambio);
# desfase = 2: tramo con la observada en t-2 (independiente del error del cambio; pierde 6.1->6.2).
transiciones <- function(sim, no, filas = seq_len(sim$n), desfase = 1) {
  do.call(rbind, lapply((1 + desfase):T_MAX, function(t) {
    z0 <- z_obs(sim, t - desfase, no)[filas, , drop = FALSE]
    dS <- sim$Sobs[filas, , t] - sim$Sobs[filas, , t - 1]
    data.frame(fila = filas, esc = sim$esc[filas], t = t,
               dVL = sim$VLobs[filas, t] - sim$VLobs[filas, t - 1],
               VLprev = sim$VLobs[filas, t - 1],
               setNames(as.data.frame(dS), paste0("d_", K)),
               setNames(as.data.frame(apply(z0, 2, tramo)), paste0("tr_", K)))
  }))
}

# Estimador de momentos: (X'X/n - D)^-1 (X'y/n - c), con D y c del error de medida conocido.
# err_s: DT del error de cada habilidad (en el manual: sqrt(1 - fiabilidad) en unidades z).
estimar_beta <- function(dat, err_s = ERR_S, err_vl = ERR_VL, corregir = TRUE) {
  cols <- list()
  for (k in K) for (r in 1:3) cols[[paste(k, r, sep = "_")]] <- dat[[paste0("d_", k)]] * (dat[[paste0("tr_", k)]] == r)
  Xs <- do.call(cbind, cols)
  Xc <- model.matrix(~ factor(t) + factor(esc), dat)
  X  <- cbind(Xs, VLprev = dat$VLprev, Xc)
  y  <- dat$dVL; n <- nrow(X)
  XtX <- crossprod(X) / n; Xty <- crossprod(X, y) / n
  if (corregir) {
    D <- numeric(ncol(X)); names(D) <- colnames(X)
    for (k in K) for (r in 1:3) D[paste(k, r, sep = "_")] <- 2 * err_s^2 * mean(dat[[paste0("tr_", k)]] == r)
    D["VLprev"] <- err_vl^2
    XtX <- XtX - diag(D)
    Xty["VLprev", ] <- Xty["VLprev", ] + err_vl^2
  }
  b <- solve(XtX, Xty)[, 1]
  list(beta = matrix(b[names(cols)], length(K), 3, byrow = TRUE, dimnames = list(K, TRAMOS)),
       gamma = unname(b["VLprev"]))
}

boot_escuelas <- function(dat, f, B = N_BOOT) {
  escs <- unique(dat$esc); trozos <- split(dat, dat$esc)
  replicate(B, {
    m <- sample(escs, replace = TRUE)
    d <- do.call(rbind, lapply(seq_along(m), function(i) { x <- trozos[[as.character(m[i])]]; x$esc <- i; x }))
    f(d)
  }, simplify = FALSE)
}

# ---- 4. estimar_maleabilidad: contraste SIN/CON dentro de colegio -----------
# Para cada habilidad con dosis Preleo, regresa el cambio observado sobre la ganancia que
# produciría m = 1 con las sesiones recibidas; la pendiente es m (DT / 10 sesiones).
estimar_maleabilidad <- function(sim, filas = seq_len(sim$n)) {
  out <- sapply(K, function(k) {
    if (DOSIS_PRELEO[k] == 0) return(NA_real_)
    x <- ganancia_sesiones(1, round(sim$expo[filas] * DOSIS_PRELEO[k]))
    d <- do.call(rbind, lapply(2:T_MAX, function(t) data.frame(
      y = sim$Sobs[filas, k, t] - sim$Sobs[filas, k, t - 1], x = x, esc = sim$esc[filas], t = t)))
    unname(coef(lm(y ~ x + factor(esc) + factor(t), d))["x"])
  })
  out[is.na(out)] <- M_PRIOR[names(out)[is.na(out)]]
  out
}

# ---- 5. estimar_exito: probabilidad de éxito al final de 2.º ----------------
# Éxito = velocidad lectora observada en 7.3 >= P25 del grupo SIN.
datos_exito <- function(sim, no, t_pred = 3) {
  corte <- quantile(sim$VLobs[sim$coh == "SIN", T_MAX], .25)
  z <- z_obs(sim, t_pred, no)
  data.frame(esc = sim$esc, coh = sim$coh, preleo = sim$expo,
             as.data.frame(z), vl = (sim$VLobs[, t_pred] - no[[t_pred]]$mvl) / no[[t_pred]]$svl,
             exito = sim$VLobs[, T_MAX] >= corte)
}
estimar_exito <- function(d) glm(exito ~ pse + pal + ran + cf + let + voc + vl + preleo,
                                 binomial, d)

# ---- 6. asignar_esfuerzo: algoritmo voraz con rendimientos decrecientes ------
# z0: matriz n×K de z observadas; beta: K×3; m: maleabilidad por habilidad (DT/10 ses);
# B: sesiones del trimestre; tope: no se trabaja una habilidad ya por encima de P75.
asignar_esfuerzo <- function(z0, beta, m, B = 12, tope = qnorm(.75), mult = NULL) {
  n <- nrow(z0); ses <- matrix(0L, n, length(K), dimnames = list(NULL, K)); z <- z0
  if (is.null(mult)) mult <- matrix(1, n, length(K))
  for (b in seq_len(B)) {
    marg <- sapply(K, function(k) {
      g <- (m[k] * mult[, which(K == k)] / 10) * DECAE^ses[, k]
      v <- beta[k, ][tramo(z[, k])] * g
      ifelse(z[, k] > tope, -Inf, v)
    })
    marg <- matrix(marg, n)
    j <- max.col(marg, ties.method = "first")
    idx <- cbind(seq_len(n), j)
    ses[idx] <- ses[idx] + 1L
    z[idx] <- z[idx] + (m[K[j]] * mult[idx] / 10) * DECAE^(ses[idx] - 1)
  }
  ses
}

# Estrategias de comparación
estrategia_uniforme <- function(n, B) {
  base <- B %/% length(K); resto <- B %% length(K)
  matrix(rep(base + c(rep(1, resto), rep(0, length(K) - resto)), each = n), n,
         dimnames = list(NULL, K))
}
estrategia_debil <- function(z0, B, paso = .04) {
  n <- nrow(z0); ses <- matrix(0L, n, length(K), dimnames = list(NULL, K)); z <- z0
  for (b in seq_len(B)) { j <- max.col(-z, "first"); idx <- cbind(seq_len(n), j)
    ses[idx] <- ses[idx] + 1L; z[idx] <- z[idx] + paso }
  ses
}
estrategia_decod <- function(n, B) {
  s <- matrix(0L, n, length(K), dimnames = list(NULL, K)); s[, "pse"] <- ceiling(B / 2); s[, "pal"] <- B %/% 2; s
}

# ---- 7. actualizar: bucle cerrado de la maleabilidad individual -------------
# Compara la ganancia observada en cada habilidad trabajada con la prevista y ajusta el
# multiplicador individual por contracción (peso = fiabilidad de la ganancia observada).
actualizar <- function(mult, ses, gan_obs, gan_prev, var_ruido, var_verdad = M_SD_IND^2) {
  w <- var_verdad / (var_verdad + var_ruido)
  r <- ifelse(ses > 0 & gan_prev > 0.02, gan_obs / gan_prev, 1)
  mult_nuevo <- mult * exp(w * log(pmin(pmax(r, .25), 4)))
  pmin(pmax(mult_nuevo, .3), 3)
}

# Evalúa una estrategia en el mundo verdadero (números aleatorios comunes)
evaluar <- function(sim, filas, ses34, corte, ses45 = NULL) {
  S <- sim$S[filas, , 3]; VL <- sim$VL[filas, 3]; dVL1 <- NA
  for (t in 4:T_MAX) {
    ses <- matrix(0L, length(filas), length(K), dimnames = list(NULL, K))
    if (t == 4) ses <- ses34
    if (t == 5 && !is.null(ses45)) ses <- ses45
    st <- avanzar(S, VL, ses, sim$crec[filas, ], sim$mult[filas, ], sim$u[filas],
                  sim$normas[[t - 1]], sim$eps_s[filas, , t], sim$eps_vl[filas, t])
    if (t == 4) dVL1 <- st$VL - VL
    S <- st$S; VL <- st$VL
  }
  VLo <- VL + sim$e_vl[filas, T_MAX]
  c(ganancia_trim = mean(dVL1), vl_7.3 = mean(VL), exito = mean(VLo >= corte))
}

# =============================================================================
# 8. EJECUCIÓN
# =============================================================================
cat("M19 · simulación con verdad conocida\n")
sim <- simular()
no  <- normas_obs(sim)
ent <- sim$esc <= 20          # colegios de entrenamiento
pru <- !ent                   # colegios de prueba

# 8.1 Tipos de cambio: ingenuo frente a corregido
dat_ent  <- transiciones(sim, no, which(ent), desfase = 1)
dat_ent2 <- transiciones(sim, no, which(ent), desfase = 2)
b_ing  <- estimar_beta(dat_ent, corregir = FALSE)
b_cor1 <- estimar_beta(dat_ent, corregir = TRUE)
b_cor  <- estimar_beta(dat_ent2, corregir = TRUE)   # estimador recomendado
cat("  bootstrap por colegio (", N_BOOT, ")...\n")
bs <- boot_escuelas(dat_ent2, function(d) estimar_beta(d, corregir = TRUE)$beta)
bs_arr <- simplify2array(bs)
lo <- apply(bs_arr, 1:2, quantile, .025); hi <- apply(bs_arr, 1:2, quantile, .975)

tab_beta <- do.call(rbind, lapply(K, function(k) data.frame(
  habilidad = unname(ETQ[k]), tramo = TRAMOS, verdad = unname(BETA_V[k, ]),
  ingenuo = round(b_ing$beta[k, ], 2), corregido_t1 = round(b_cor1$beta[k, ], 2),
  corregido = round(b_cor$beta[k, ], 2),
  ic95_inf = round(lo[k, ], 2), ic95_sup = round(hi[k, ], 2),
  cubre = BETA_V[k, ] >= lo[k, ] & BETA_V[k, ] <= hi[k, ])))
tab_beta$cubre[is.na(tab_beta$cubre)] <- FALSE
write.csv(tab_beta, file.path(SALIDA, "M19_T1_tipos_de_cambio.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cobertura <- mean(tab_beta$cubre)
rec <- function(b) { w <- BETA_V >= 2; mean(b[w] / BETA_V[w]) }  # casillas con efecto apreciable
ate_ing <- rec(b_ing$beta); ate_cor1 <- rec(b_cor1$beta); ate_cor <- rec(b_cor$beta)

# 8.2 Maleabilidad
m_hat <- estimar_maleabilidad(sim, which(ent))
tab_m <- data.frame(habilidad = ETQ[K], verdad = M_V, estimada = round(m_hat, 3),
                    fuente = ifelse(DOSIS_PRELEO > 0, "contraste SIN/CON (Preleo)", "literatura (prior)"))
write.csv(tab_m, file.path(SALIDA, "M19_T2_maleabilidad.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# 8.3 Probabilidad de éxito
dex <- datos_exito(sim, no)
mod <- estimar_exito(dex[ent, ])
p   <- predict(mod, dex[pru, ], type = "response")
y   <- dex$exito[pru]
mod_vl <- glm(exito ~ vl, binomial, dex[ent, ])
p_vl <- predict(mod_vl, dex[pru, ], type = "response")
cal <- coef(glm(y ~ qlogis(pmin(pmax(p, 1e-4), 1 - 1e-4)), binomial))
tab_exito <- data.frame(
  indicador = c("AUC perfil completo", "AUC solo velocidad lectora", "Brier perfil completo",
                "Brier solo velocidad", "Calibración: intercepto", "Calibración: pendiente",
                "Tasa de éxito (prueba)"),
  valor = round(c(auc(p, y), auc(p_vl, y), mean((p - y)^2), mean((p_vl - y)^2), cal[1], cal[2], mean(y)), 3))
write.csv(tab_exito, file.path(SALIDA, "M19_T3_probabilidad_exito.csv"), row.names = FALSE, fileEncoding = "UTF-8")
coef_exito <- data.frame(variable = names(coef(mod)), coef = round(coef(mod), 3))
write.csv(coef_exito, file.path(SALIDA, "M19_T3b_coeficientes_exito.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# 8.4 Comparación de estrategias (12 sesiones en el trimestre 6.3 -> 7.1)
B <- 12
filas <- which(pru & sim$coh == "SIN")
z3 <- z_obs(sim, 3, no)[filas, ]
corte <- quantile(sim$VLobs[sim$coh == "SIN", T_MAX], .25)
n_f <- length(filas)
estr <- list(
  "Sin sesiones extra"            = matrix(0L, n_f, length(K), dimnames = list(NULL, K)),
  "Reparto uniforme"              = estrategia_uniforme(n_f, B),
  "Lo más débil primero"          = estrategia_debil(z3, B),
  "Solo decodificación"           = estrategia_decod(n_f, B),
  "Algoritmo (β ingenuo)"         = asignar_esfuerzo(z3, b_ing$beta, m_hat, B),
  "Algoritmo (β corregido)"       = asignar_esfuerzo(z3, b_cor$beta, m_hat, B),
  "Oráculo (parámetros reales)"   = asignar_esfuerzo(z3, BETA_V, M_V, B, mult = sim$mult[filas, ]))
bajos <- (sim$VLobs[filas, 3] - no[[3]]$mvl) / no[[3]]$svl < Q25
res <- do.call(rbind, lapply(names(estr), function(nm) {
  todos <- evaluar(sim, filas, estr[[nm]], corte)
  sub   <- evaluar(sim, filas[bajos], estr[[nm]][bajos, , drop = FALSE], corte)
  sesiones_ran <- mean(estr[[nm]][, "ran"])
  data.frame(estrategia = nm, ganancia_trim_ppm = round(todos[1], 2), exito_pct = round(100 * todos[3], 1),
             bajos_ganancia_ppm = round(sub[1], 2), bajos_exito_pct = round(100 * sub[3], 1),
             sesiones_RAN_media = round(sesiones_ran, 1))
}))
rownames(res) <- NULL
write.csv(res, file.path(SALIDA, "M19_T4_estrategias.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# 8.5 Bucle cerrado: dos trimestres de plan, con y sin actualización (1 y 4 monitoreos)
bucle <- function(n_monit) {
  f <- filas[bajos]; nf <- length(f)
  ses1 <- asignar_esfuerzo(z3[bajos, , drop = FALSE], b_cor$beta, m_hat, B)
  # ganancia observada en el trimestre = cambio observado - crecimiento natural medio
  S4 <- sim$S[f, , 3] + sim$crec[f, ] + sim$eps_s[f, , 4] +
        col_mat(lapply(K, function(k) ganancia_sesiones(M_V[k] * sim$mult[f, k], ses1[, k])), nf)
  ruido <- sqrt(2) * ERR_S / sqrt(n_monit)
  gan_obs <- (S4 - sim$S[f, , 3]) + matrix(rnorm(nf * length(K), 0, ruido), nf) -
             matrix(MU, nf, length(K), byrow = TRUE)
  gan_prev <- col_mat(lapply(K, function(k) ganancia_sesiones(m_hat[k], ses1[, k])), nf)
  # ruido de la ganancia observada: error de medida + crecimiento natural no explicado
  var_r <- (ruido^2 + .30^2 / n_monit + .15^2) / pmax(gan_prev, .05)^2
  mult_n <- actualizar(matrix(1, nf, length(K)), ses1, gan_obs, gan_prev, var_r)
  w_med <- mean((M_SD_IND^2 / (M_SD_IND^2 + var_r))[ses1 > 0])
  z4 <- sweep(sweep(S4 + matrix(rnorm(nf * length(K), 0, ERR_S), nf), 2, no[[4]]$m), 2, no[[4]]$s, "/")
  ses2_sin <- asignar_esfuerzo(z4, b_cor$beta, m_hat, B)
  ses2_con <- asignar_esfuerzo(z4, b_cor$beta, m_hat, B, mult = mult_n)
  cbind(rbind(sin_actualizar = evaluar(sim, f, ses1, corte, ses2_sin),
              con_actualizar = evaluar(sim, f, ses1, corte, ses2_con)), peso_actualizacion = c(0, w_med))
}
set.seed(SEMILLA + 7)
bc <- do.call(rbind, lapply(c(1, 4, 12), function(nm)
  data.frame(monitoreos = nm, plan = c("sin actualizar", "con actualizar"), bucle(nm))))
bc$peso_actualizacion <- round(bc$peso_actualizacion, 3)
bc$exito <- round(100 * bc$exito, 1); bc$vl_7.3 <- round(bc$vl_7.3, 1); bc$ganancia_trim <- round(bc$ganancia_trim, 2)
rownames(bc) <- NULL
write.csv(bc, file.path(SALIDA, "M19_T5_bucle_cerrado.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# 8.6 Figuras
png(file.path(SALIDA, "M19_F1_recuperacion_tipos_de_cambio.png"), 1400, 900, res = 150)
op <- par(mar = c(10, 4.5, 2.5, 1))
xs <- seq_len(nrow(tab_beta))
plot(xs, tab_beta$verdad, pch = 4, cex = 1.3, lwd = 2, ylim = range(c(tab_beta$ic95_inf, tab_beta$ic95_sup, tab_beta$ingenuo, 0)),
     xaxt = "n", xlab = "", ylab = "ppm por DT de cambio", main = "Tipos de cambio: verdad, MCO ingenuo y estimador corregido")
abline(v = seq(3.5, 15.5, 3), col = "grey85")
segments(xs + .15, tab_beta$ic95_inf, xs + .15, tab_beta$ic95_sup, col = "#0f6e56", lwd = 2)
points(xs + .15, tab_beta$corregido, pch = 19, col = "#0f6e56")
points(xs - .15, tab_beta$ingenuo, pch = 17, col = "#c7612f")
axis(1, at = seq(2, 17, 3), labels = ETQ[K], las = 2, cex.axis = .8, tick = FALSE)
legend("topright", c("verdad", "corregido (IC 95 %)", "ingenuo (MCO)"), pch = c(4, 19, 17),
       col = c("black", "#0f6e56", "#c7612f"), bty = "n", cex = .85)
mtext("Dentro de cada habilidad: tramo bajo, medio y alto", 1, 8.5, cex = .75)
par(op); dev.off()

png(file.path(SALIDA, "M19_F2_estrategias.png"), 1400, 750, res = 150)
op <- par(mar = c(4.5, 13, 2.5, 1))
ord <- order(res$bajos_exito_pct)
barplot(res$bajos_exito_pct[ord], names.arg = res$estrategia[ord], horiz = TRUE, las = 1,
        col = ifelse(grepl("Algoritmo \\(β corr|Oráculo", res$estrategia[ord]), "#0f6e56", "#9fb8ad"),
        xlab = "% de éxito al final de 2.º (niños con velocidad < P25 en 6.3)", cex.names = .8,
        main = "Mismo número de sesiones, distinto reparto")
par(op); dev.off()

# ---- 8.7 Generador de datos para la práctica / concurso de clase ------------
generar_practica <- function(semilla = 2027, n_esc = 12, n_por = 25, n_casos = 30) {
  s <- simular(n_esc = n_esc, n_por = n_por, semilla = semilla)
  nn <- normas_obs(s)
  pct <- function(z) pmin(99, pmax(1, round(100 * pnorm(z))))
  z <- z_obs(s, 3, nn)
  corte <- quantile(s$VLobs[s$coh == "SIN", T_MAX], .25)
  set.seed(semilla + 1)
  d <- data.frame(id = sprintf("A%04d", sample(1e4 - 1, s$n)),
                  colegio = sprintf("C%02d", s$esc),
                  preleo = round(s$expo, 2),
                  pseudopalabras_pct = pct(z[, "pse"]), palabras_pct = pct(z[, "pal"]),
                  ran_pct = pct(z[, "ran"]), conciencia_fonologica_pct = pct(z[, "cf"]),
                  vocabulario_pct = pct(z[, "voc"]),
                  vl_6.2_ppm = round(s$VLobs[, 2]), vl_6.3_ppm = round(s$VLobs[, 3]),
                  vl_7.3_ppm = round(s$VLobs[, T_MAX]),
                  exito_7.3 = as.integer(s$VLobs[, T_MAX] >= corte),
                  dato_invalido = "")
  # Casos inválidos sembrados a propósito (≈ 4 %): los grupos deben detectarlos
  m <- ceiling(.04 * nrow(d)); idx <- sample(nrow(d), m)
  tipos <- rep(c("caida_imposible", "tiempo_x1000", "duplicado", "incoherente"), length.out = m)
  for (i in seq_along(idx)) {
    j <- idx[i]
    if (tipos[i] == "caida_imposible") d$vl_6.3_ppm[j] <- max(0, d$vl_6.2_ppm[j] - 45)
    if (tipos[i] == "tiempo_x1000")    d$ran_pct[j] <- 999
    if (tipos[i] == "duplicado")       d$id[j] <- d$id[(j %% nrow(d)) + 1]
    if (tipos[i] == "incoherente")   { d$vl_6.3_ppm[j] <- 0; d$palabras_pct[j] <- 95 }
    d$dato_invalido[j] <- tipos[i]
  }
  casos <- c(sample(idx, 2), sample(setdiff(seq_len(nrow(d)), idx), n_casos - 2))
  casos <- sample(casos)
  entrenamiento <- d[-casos, setdiff(names(d), "dato_invalido")]
  publico_casos <- d[casos, setdiff(names(d), c("vl_7.3_ppm", "exito_7.3", "dato_invalido"))]
  clave <- d[c(casos, idx), c("id", "vl_7.3_ppm", "exito_7.3", "dato_invalido")]
  clave <- clave[!duplicated(rownames(clave)), ]
  write.csv(entrenamiento, file.path(SALIDA, "PRACTICA_1_entrenamiento.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(publico_casos, file.path(SALIDA, "PRACTICA_2_casos_a_informar.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(clave, file.path(SALIDA, "PRACTICA_3_CLAVE_OCULTA_profesor.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  c(n_entrenamiento = nrow(entrenamiento), n_casos = nrow(publico_casos), invalidos = m)
}
pr <- generar_practica()

# ---- 8.8 Resumen -------------------------------------------------------------
fmt <- function(df) paste(c(paste("|", paste(names(df), collapse = " | "), "|"),
                           paste("|", paste(rep("---", ncol(df)), collapse = " | "), "|"),
                           apply(df, 1, function(r) paste("|", paste(r, collapse = " | "), "|"))), collapse = "\n")
al <- res[res$estrategia == "Algoritmo (β corregido)", ]
un <- res[res$estrategia == "Reparto uniforme", ]
db <- res[res$estrategia == "Lo más débil primero", ]
resumen <- c(
  "# M19 · Resultados de la simulación con verdad conocida (v1)", "",
  sprintf("Generado: %s · semilla %d · %d niños (40 colegios × 25 × 2 cohortes), 6 momentos (6.1–7.3).",
          format(Sys.time(), "%d/%m/%Y %H:%M"), SEMILLA, sim$n),
  "Todos los números proceden de datos simulados. Sirven para comprobar que el procedimiento recupera la verdad, no describen a ningún niño real.", "",
  "## 1. Tipos de cambio",
  "Recuperación media en las casillas con efecto real ≥ 2 ppm/DT:",
  sprintf("- MCO ingenuo sobre primeras diferencias: %.0f %% del valor real (atenuación: el cambio de una prueba corta es poco fiable).", 100 * ate_ing),
  sprintf("- Corregido por fiabilidad, tramo en t-1: %.0f %% (el tramo comparte error con el cambio).", 100 * ate_cor1),
  sprintf("- Corregido por fiabilidad, tramo en t-2 (recomendado): %.0f %%; el IC 95 %% bootstrap por colegio cubre el valor real en %d de 18 casillas.", 100 * ate_cor, sum(tab_beta$cubre)),
  sprintf("- Efecto Mateo (γ, verdad %.2f): ingenuo %.3f, corregido %.3f.", GAMMA, b_ing$gamma, b_cor$gamma), "",
  fmt(tab_beta[, c("habilidad", "tramo", "verdad", "ingenuo", "corregido_t1", "corregido", "ic95_inf", "ic95_sup")]), "",
  "## 2. Maleabilidad (DT por 10 sesiones)", "", fmt(tab_m), "",
  "## 3. Probabilidad de éxito al final de 2.º (predicción en 6.3, colegios no usados)", "", fmt(tab_exito), "",
  "## 4. Mismo número de sesiones (12), distinto reparto", "",
  "Niños de colegios de prueba, cohorte SIN; 'bajos' = velocidad lectora < P25 en 6.3.", "", fmt(res), "",
  sprintf("- En los niños bajos, el algoritmo corregido consigue %.1f %% de éxito frente a %.1f %% con reparto uniforme y %.1f %% con 'lo más débil primero'.",
          al$bajos_exito_pct, un$bajos_exito_pct, db$bajos_exito_pct),
  sprintf("- 'Lo más débil primero' dedica de media %.1f sesiones a RAN (predice mucho, se mueve poco); el algoritmo, %.1f.",
          db$sesiones_RAN_media, al$sesiones_RAN_media), "",
  "## 5. Bucle cerrado (dos trimestres de plan, niños bajos)", "", fmt(bc), "",
  "## 6. Datos para la práctica de clase",
  sprintf("- PRACTICA_1_entrenamiento.csv: %d alumnos con el criterio visible.", pr[1]),
  sprintf("- PRACTICA_2_casos_a_informar.csv: %d casos sin criterio (incluye 2 inválidos sembrados).", pr[2]),
  sprintf("- PRACTICA_3_CLAVE_OCULTA_profesor.csv: criterio real y tipo de dato inválido (%d sembrados en total). No distribuir.", pr[3]), "",
  "## Límites de esta simulación",
  "- El mundo simulado tiene la misma forma funcional que el modelo (salvo la integral por tramos); con datos reales habrá error de especificación.",
  "- No incluye confusión por crecimiento estable del niño correlacionado con la lectura; es la amenaza principal de las primeras diferencias y debe tratarse con la maleabilidad cuasi-experimental (Preleo) y, al final, con el ensayo.",
  "- El tramo se asigna con la puntuación observada en t-1, que comparte error con el cambio; el sesgo es pequeño aquí pero conviene asignarlo con una medida paralela en los datos reales.",
  "- La maleabilidad de RAN y vocabulario no está identificada por Preleo; se usa un valor de la literatura deliberadamente inexacto.")
writeLines(resumen, file.path(SALIDA, "M19_RESUMEN.md"), useBytes = TRUE)
cat(paste(resumen, collapse = "\n"), "\n")

# =============================================================================
# 9. CON DATOS REALES (cohorte 2010 en adelante): qué sustituir
#   - sim$Sobs / sim$VLobs  -> matrices niño × tarea × momento desde la matriz larga verificada,
#     en z con referencia SIN por cohorte × momento (o percentiles convertidos con qnorm).
#   - sim$esc, sim$coh, sim$expo -> colegio_id, cohorte y exposición Preleo (condición × fidelidad)
#     de 06_PRELEO_COLEGIOS / tabla maestra de colegios.
#   - ERR_S -> por tarea, sqrt(1 - fiabilidad) con α/ω del manual (vector con nombre); ERR_VL
#     -> DT del error de la tarea de velocidad (DT × sqrt(1 - fiabilidad)).
#   - DOSIS_PRELEO -> sesiones por componente y trimestre según el programa.
#   - Las secciones 8.4–8.5 (estrategias y bucle) solo se pueden reproducir en simulación;
#     con datos reales se sustituyen por la validación fuera de política (M19, apartado 5).
# =============================================================================
