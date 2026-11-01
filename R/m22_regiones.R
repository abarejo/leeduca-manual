# =============================================================================
# m22_regiones.R  (Claude, 01/11/2026) · v2
# M22 · Comparación de los indicadores LEEDUCA entre regiones (Málaga, Cantabria, Castilla y León, Baleares)
# y frente a la referencia histórica de Málaga 2010–2015. Proyecto interno (dinámicas poblacionales).
#
# Entrada (SOLO LECTURA):
#   DIAGNOSTICO/R/servidor_2023_2026_largo_v2.csv.gz           copias del servidor oct 2023 – sep 2026 (de inventario_servidor.R v2)
#   DICCIONARIO_MAESTRO_2026/codigos_sin_catalogo_identificados_v1.csv   nombre de cada actividad → versión en catalán (prefijo BL_VAL_)
#   DICCIONARIO_MAESTRO_2026/centros_region_v2.csv (región, comunidad, provincia, municipio, titularidad, estrato_PJ; localizados
#     uno a uno el 01/11/2026). Si no existe, centros_region_v1.csv o centros_region_PROPUESTA_v1.csv
#   DICCIONARIO_MAESTRO_2026/puntuaciones_largo_cohortes_2010_2015_v2.csv.gz   referencia histórica (Málaga)
#
# Correcciones respecto a inventario_servidor.R:
#   · «VAL» en los nombres de actividad tiene dos sentidos. «BL_DF_VAL_…» es Velocidad de Acceso al Léxico (RAN), en castellano.
#     Solo las actividades que EMPIEZAN por «BL_VAL_» son versiones en catalán; se aplicaron en Baleares en marzo–abril de 2024.
#     El guion reclasifica la lengua con ese criterio y enlaza cada versión catalana con su código base castellano.
#
# Qué hace:
#   1. Clasifica cada registro por región y lengua; marca si la aplicación parece cribado de aula (≥ 10 niños de la misma edad
#      en el mismo centro y mes) o aplicación selectiva.
#   2. Primera aplicación por niño × código base × curso escolar. Medida: aciertos (exactitud) o log(tiempo total) (RAN).
#   3. Para cada código base × edad con ≥ 30 niños y ≥ 3 centros en Málaga y en al menos otra región: descriptivos por región,
#      d frente a Málaga, modelo multinivel y ~ región + trimestre + (1 | centro) (lme4), d ajustada e ICC de centro.
#   4. Alfa de Cronbach por región (respuestas Acierto/Fallo ítem a ítem) como comprobación mínima del instrumento.
#   5. Compara Málaga 2023–2026 con Málaga 2010–2015 (mismo código, aciertos) por edad y trimestre.
#   6. Figuras M22_F1–F4 en DIAGNOSTICO/R/figs.
#
# Uso: Rscript m22_regiones.R --raiz "C:/.../PLATAFORMA"
# Salida (DIAGNOSTICO/R): M22_regiones_R_v2.xlsx; figs/M22_F*.png|pdf (F6: titularidad en Málaga). Nada con datos personales.
# =============================================================================
args0 <- commandArgs(trailingOnly = FALSE); aqui <- dirname(normalizePath(sub("^--file=", "", args0[grep("^--file=", args0)])))
source(file.path(aqui, "leeduca_comun.R"), encoding = "UTF-8"); R <- rutas_leeduca(); A <- args_leeduca()
suppressPackageStartupMessages(library(lme4))
d04 <- file.path(R$raiz, "04_DICCIONARIO_PROCESADO_2026"); dm <- file.path(d04, "DICCIONARIO_MAESTRO_2026")
f_srv <- A$servidor %||% file.path(R$salida, "servidor_2023_2026_largo_v2.csv.gz")
f_cod <- file.path(dm, "codigos_sin_catalogo_identificados_v1.csv")
f_reg <- A$regiones %||% Filter(file.exists, file.path(dm, c("centros_region_v2.csv", "centros_region_v1.csv", "centros_region_PROPUESTA_v1.csv")))[1]
N_MIN <- 30; C_MIN <- 3; CRIBADO_MIN <- 10
set.seed(20261101)

# ---- 1. datos y clasificación ------------------------------------------------
D <- fread(f_srv, colClasses = "character", encoding = "UTF-8",
           select = c("alumno", "Centro", "curso_escolar", "trimestre", "fecha", "edad", "Prueba", "uso", "tarea_ref", "tarea_canonica",
                      "Sesion_programada", "aciertos", "t_total", "Respuestas_usuario"))
D[, `:=`(edad = as.integer(edad), aciertos = as.numeric(aciertos), t_total = as.numeric(t_total), fecha = as.IDate(fecha), trimestre = as.integer(trimestre))]
D <- D[grepl("^Evaluaci", uso) & !is.na(edad) & edad %between% c(3, 8) & !is.na(alumno) & !is.na(Centro) & Centro != ""]

# lengua: solo las actividades cuyo nombre empieza por BL_VAL_ (o BL_VAL-) son versiones en catalán
COD <- fread(f_cod, sep = ";", colClasses = "character", encoding = "UTF-8")
setnames(COD, names(COD)[1], "Prueba")
CAT <- COD[grepl("^BL_VAL[_-]", Nombre), .(Prueba, Nombre, codigo_base)]
# versiones catalanas sin código base entre paréntesis (batería de 3 años): se enlazan por el nombre sin el prefijo VAL_
nrm <- function(x) gsub("[^a-z0-9]", "", tolower(iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")))
COD[, nombre_n := nrm(Nombre)]; CAT[, nombre_n := nrm(sub("^BL_VAL[_-] ?", "BL_", Nombre))]
eq <- COD[!grepl("^BL_VAL[_-]", Nombre) & !is.na(nombre_n), .(nombre_n, codigo_eq = Prueba, registros = as.numeric(registros))][order(-registros)][, .SD[1], by = nombre_n]
CAT <- merge(CAT, eq[, .(nombre_n, codigo_eq)], by = "nombre_n", all.x = TRUE)
CAT[is.na(codigo_base) | codigo_base == "", codigo_base := codigo_eq]; CAT[, c("nombre_n", "codigo_eq") := NULL]
D[, lengua := fifelse(Prueba %in% CAT$Prueba, "catalán", "castellano")]
D <- merge(D, CAT[, .(Prueba, codigo_base)], by = "Prueba", all.x = TRUE)
D[, cb := fcoalesce(codigo_base, Prueba)]           # código base: la versión catalana se compara con su original castellano
# nombre de la tarea: el de la versión castellana del mismo código base, si existe
NOM <- D[lengua == "castellano", .N, by = .(cb, tarea_ref)][order(-N)][, .SD[1], by = cb][, .(cb, tarea = tarea_ref)]
D <- merge(D, NOM, by = "cb", all.x = TRUE); D[is.na(tarea), tarea := tarea_ref]
D[, tarea := sub("^BL_DF_(LO_SEM_|LO_|CF_)?3_", "", tarea)]
D[cb == "197", tarea := "Morfología verbal de tiempo (pseudopalabras)"]; D[tarea == "RIMAS", tarea := "Juicio de rima"]
D[, es_ran := grepl("^Denominaci.n r.pida", tarea)]

# región
REG <- fread(f_reg, sep = ";", colClasses = "character", encoding = "UTF-8")
for (v in c("Titularidad", "estrato_PJ", "Provincia", "Municipio")) if (!v %in% names(REG)) REG[, (v) := ""]
REG <- REG[, .(Centro, region, titularidad = Titularidad, estrato = estrato_PJ, provincia = Provincia, municipio = Municipio)]
# unión por nombre normalizado (mayúsculas, tildes y espacios no cuentan)
REG[, cn := nrm(Centro)]; REG <- REG[, .SD[1], by = cn]; D[, cn := nrm(Centro)]
D <- merge(D, REG[, !"Centro"], by = "cn", all.x = TRUE); D[is.na(region), region := "sin identificar"]
D <- D[!region %in% c("UMA (adultos)", "UMA / práctica / pruebas", "sin centro")]
D[region %in% c("Andalucía (otras provincias)", "C. Valenciana", "Otras comunidades"), region := "otras comunidades"]
D[is.na(titularidad) | titularidad == "", titularidad := "sin dato"]
D[, grupo := fifelse(lengua == "catalán", paste0(region, " (catalán)"), region)]
# (comprobación) ¿hay niños de Baleares con versiones castellanas? se mantienen como grupo aparte si los hay
# cribado de aula: ≥ CRIBADO_MIN niños de la misma edad evaluados en el mismo centro y mes
D[, mes := format(fecha, "%Y-%m")]
CR <- D[, .(ninos_mes = uniqueN(alumno)), by = .(Centro, edad, mes)]
D <- merge(D, CR, by = c("Centro", "edad", "mes")); D[, cribado := ninos_mes >= CRIBADO_MIN]
SEL <- D[, .(ninos = uniqueN(alumno), centros = uniqueN(Centro), pct_ninos_cribado = round(100 * uniqueN(alumno[cribado]) / uniqueN(alumno), 1),
             pct_sesion_0 = round(100 * mean(Sesion_programada == "0"), 1)), by = .(grupo, edad)][order(grupo, edad)]
CEN <- D[, .(region, grupo, titularidad, provincia, municipio, ninos = uniqueN(alumno), edad_min = min(edad), edad_max = max(edad), desde = min(fecha), hasta = max(fecha),
             pct_cribado = round(100 * mean(cribado), 1), pct_catalan = round(100 * mean(lengua == "catalán"), 1)), by = Centro][, .SD[1], by = Centro][order(region, -ninos)]
CEN[, Centro_seud := paste0("C", .I)]      # el nombre del centro no sale en el libro; aquí queda solo en el xlsx interno

# ---- 2. una medida por niño × código base × curso escolar ----------------------
D[, y := fifelse(es_ran, suppressWarnings(log(t_total)), aciertos)]
D <- D[!is.na(y) & is.finite(y)]
D[es_ran & (t_total <= 1 | t_total > 600), y := NA]     # tiempos imposibles (RAN)
D <- D[!is.na(y)]
setorder(D, alumno, cb, curso_escolar, fecha)
P <- D[, .SD[1], by = .(alumno, cb, curso_escolar)]
P[, medida := fifelse(es_ran, "log(segundos)", "aciertos")]

# ---- 3. celdas comparables y modelos -------------------------------------------
cel <- P[, .(n = uniqueN(alumno), centros = uniqueN(Centro)), by = .(cb, tarea, medida, edad, grupo)]
ok  <- cel[n >= N_MIN & centros >= C_MIN]
cel_ok <- ok[, .(grupos = .N, malaga = any(grupo == "Málaga")), by = .(cb, tarea, medida, edad)][grupos >= 2 & malaga]
message("celdas (código × edad) comparables: ", nrow(cel_ok))

desc <- function(v) list(M = mean(v), DT = sd(v), Md = median(v), P16 = cuantil(v, .16), P84 = cuantil(v, .84), suelo = 100 * mean(v == min(v)))
RES <- list(); MOD <- list()
for (i in seq_len(nrow(cel_ok))) {
  k <- cel_ok[i]; S <- P[cb == k$cb & edad == k$edad & grupo %in% ok[cb == k$cb & edad == k$edad, grupo]]
  S[, grupo := relevel(factor(grupo), ref = "Málaga")]
  ref <- S[grupo == "Málaga"]; sd_ref <- sd(ref$y)
  dd <- S[, c(.(n = .N, centros = uniqueN(Centro), pct_cribado = round(100 * mean(cribado), 1)), desc(y)), by = grupo]
  dd[, d_bruta := (M - ref[, mean(y)]) / sd_ref]
  if (k$medida == "log(segundos)") dd[, `:=`(seg_mediana = exp(Md), d_bruta = -d_bruta)]   # en RAN, d positiva = más rápido
  RES[[i]] <- cbind(cel_ok[i, .(cb, tarea, medida, edad)], dd)
  # multinivel: región + trimestre + (1 | centro)
  m <- tryCatch(lmer(y ~ grupo + factor(trimestre) + (1 | Centro), data = S, REML = TRUE), error = function(e) NULL)
  if (is.null(m)) next
  co <- summary(m)$coefficients; vc <- as.data.frame(VarCorr(m)); icc <- vc$vcov[1] / sum(vc$vcov)
  m0 <- tryCatch(lmer(y ~ factor(trimestre) + (1 | Centro), data = S, REML = TRUE), error = function(e) NULL)
  vc0 <- if (!is.null(m0)) as.data.frame(VarCorr(m0)) else NULL
  var_centro_sin_region <- if (!is.null(vc0)) vc0$vcov[1] else NA
  g <- grep("^grupo", rownames(co), value = TRUE)
  MOD[[i]] <- data.table(cb = k$cb, tarea = k$tarea, medida = k$medida, edad = k$edad, grupo = sub("^grupo", "", g),
                         beta = co[g, 1], ET = co[g, 2], t = co[g, 3], n_total = nrow(S), centros_total = uniqueN(S$Centro),
                         icc_centro = icc, var_centro_sin_region = var_centro_sin_region, var_centro_con_region = vc$vcov[1],
                         pct_var_centro_explicada_por_region = 100 * (1 - vc$vcov[1] / var_centro_sin_region))
  MOD[[i]][, `:=`(d_ajustada = beta / sd_ref, d_inf = (beta - 1.96 * ET) / sd_ref, d_sup = (beta + 1.96 * ET) / sd_ref)]
  if (k$medida == "log(segundos)") MOD[[i]][, `:=`(d_ajustada = -d_ajustada, d_inf = -d_sup, d_sup = -d_inf)]
}
RES <- rbindlist(RES, fill = TRUE); MOD <- rbindlist(MOD, fill = TRUE)
num <- function(X) { nn <- names(X)[sapply(X, is.numeric)]; X[, (nn) := lapply(.SD, round, 3), .SDcols = nn]; X }
RES <- num(RES); MOD <- num(MOD)
setorder(RES, edad, tarea, grupo); setorder(MOD, edad, tarea, grupo)

# resumen por grupo y edad: mediana de d ajustada y cuántas celdas se alejan de 0
RESUMEN <- MOD[, .(celdas = .N, d_mediana = round(median(d_ajustada), 2), d_min = round(min(d_ajustada), 2), d_max = round(max(d_ajustada), 2),
                   celdas_d_menor_m03 = sum(d_sup < -0.3), celdas_d_mayor_03 = sum(d_inf > 0.3), icc_centro_mediana = round(median(icc_centro), 3),
                   pct_var_centro_explicada_mediana = round(median(pct_var_centro_explicada_por_region, na.rm = TRUE), 1)), by = .(grupo, edad)][order(grupo, edad)]

# ---- 4. alfa por región (respuestas Acierto/Fallo ítem a ítem) ------------------
alfa <- function(M) { M <- M[complete.cases(M), , drop = FALSE]; k <- ncol(M); if (nrow(M) < 30 || k < 3) return(NA_real_)
  vi <- apply(M, 2, var); vt <- var(rowSums(M)); if (vt == 0) return(NA_real_); k / (k - 1) * (1 - sum(vi) / vt) }
tok <- function(s) { x <- trimws(strsplit(s, ",")[[1]]); x <- x[nzchar(x)]; x <- tolower(x)
  if (!length(x) || !all(x %in% c("acierto", "fallo", "error"))) return(NULL); as.integer(x == "acierto") }
AL <- list()
IT <- P[!es_ran & !is.na(Respuestas_usuario) & grepl("^,? ?(Acierto|Fallo|acierto|error)", Respuestas_usuario)]
for (k in unique(IT[, paste(cb, edad)])) {
  S <- IT[paste(cb, edad) == k]
  for (gr in unique(S$grupo)) {
    s <- S[grupo == gr]; if (nrow(s) < N_MIN) next
    L <- lapply(s$Respuestas_usuario, tok); L <- L[!sapply(L, is.null)]; if (length(L) < N_MIN) next
    kk <- as.integer(names(which.max(table(lengths(L))))); L <- L[lengths(L) == kk]; if (length(L) < N_MIN) next
    M <- do.call(rbind, L)
    AL[[length(AL) + 1]] <- data.table(cb = s$cb[1], tarea = s$tarea[1], edad = s$edad[1], grupo = gr, n = nrow(M), items = kk,
                                       alfa = round(alfa(M), 3), media_items = round(mean(rowSums(M)), 2), pct_techo = round(100 * mean(rowSums(M) == kk), 1))
  }
}
AL <- rbindlist(AL); if (nrow(AL)) { AL <- AL[cb %in% cel_ok$cb]; setorder(AL, edad, tarea, grupo) }

# ---- 5. Málaga 2023–2026 frente a Málaga 2010–2015 (mismo código, aciertos) -----
H <- leer_largo(R$largo, c("cod_base", "medida", "edad", "trimestre", "sujeto", "valor", "cohorte"))
H <- H[medida == "aciertos", .(cb = cod_base, edad = as.integer(edad), trimestre = as.integer(as.numeric(trimestre)), sujeto, cohorte, v = as.numeric(valor))][!is.na(v) & edad %between% c(3, 8)]
H <- H[, .SD[1], by = .(sujeto, cb, edad, trimestre)]
hs <- H[, .(n_2010_15 = .N, M_2010_15 = mean(v), DT_2010_15 = sd(v), cohortes = paste(sort(unique(cohorte)), collapse = " ")), by = .(cb, edad, trimestre)][n_2010_15 >= N_MIN]
PN <- P[grupo == "Málaga" & medida == "aciertos"]; setorder(PN, alumno, cb, curso_escolar, trimestre, fecha); PN <- PN[, .SD[1], by = .(alumno, cb, curso_escolar, trimestre)]
ns <- PN[, .(n_2023_26 = .N, centros_2023_26 = uniqueN(Centro), M_2023_26 = mean(y), DT_2023_26 = sd(y)), by = .(cb, tarea, edad, trimestre)][n_2023_26 >= N_MIN]
HIST <- merge(ns, hs, by = c("cb", "edad", "trimestre"))     # mismo código, misma edad y mismo trimestre
HIST[, d_2023_vs_2010 := round((M_2023_26 - M_2010_15) / DT_2010_15, 2)]
HIST <- num(HIST); setorder(HIST, edad, trimestre, tarea)

# ---- 5b. dentro de Málaga: titularidad (y estrato si PJ lo ha anotado) ------------
# Aproximación al nivel socioeconómico del centro mientras no haya ISE: concertado frente a público.
# Con pocos centros concertados, el contraste es descriptivo: se informa el número de centros de cada lado.
M <- P[region == "Málaga" & lengua == "castellano" & titularidad %in% c("Pública", "Concertada")]
M[, estrato_g := fifelse(!is.na(estrato) & estrato != "", estrato, titularidad)]
TIT <- list()
for (k in unique(M[, paste(cb, edad)])) {
  S <- M[paste(cb, edad) == k]; ref <- S[estrato_g == "Pública"]
  if (nrow(ref) < N_MIN || uniqueN(ref$Centro) < C_MIN) next
  for (g in setdiff(unique(S$estrato_g), "Pública")) {
    s <- S[estrato_g == g]; if (nrow(s) < N_MIN) next
    d <- (mean(s$y) - mean(ref$y)) / sd(ref$y); if (S$medida[1] == "log(segundos)") d <- -d
    S2 <- S[estrato_g %in% c("Pública", g)][, g2 := as.integer(estrato_g == g)]
    m <- tryCatch(lmer(y ~ g2 + factor(trimestre) + (1 | Centro), data = S2), error = function(e) NULL)
    da <- if (!is.null(m)) fixef(m)[["g2"]] / sd(ref$y) * ifelse(S$medida[1] == "log(segundos)", -1, 1) else NA
    TIT[[length(TIT) + 1]] <- data.table(cb = S$cb[1], tarea = S$tarea[1], medida = S$medida[1], edad = S$edad[1], grupo = g,
                                         n_grupo = nrow(s), centros_grupo = uniqueN(s$Centro), n_publica = nrow(ref), centros_publica = uniqueN(ref$Centro),
                                         d_bruta = round(d, 2), d_ajustada = round(da, 2))
  }
}
TIT <- rbindlist(TIT); if (nrow(TIT)) setorder(TIT, edad, tarea)
CENT_M <- P[region == "Málaga", .(ninos = uniqueN(alumno)), by = .(titularidad, estrato)][order(titularidad)]

# ---- 5c. secundaria (concertados de Málaga, 12–17 años): curva 10 años → secundaria → adultos ---------------
# Los dos concertados de Málaga (2025) aplicaron la batería de 10–11 años / adultos, no la de infantil. Se compara con los mismos
# códigos en: cohortes 2010–2015 a 10–11 años (colegios públicos), colegios 2023–2026 a 9–11 años y estudiantes de la UMA.
# Edad y estrato van juntos (no hay concertados de 10–11 ni públicos de 12–17): es una descripción, no un contraste causal.
COD_SEC <- c("10682", "10746", "10748", "10747", "10752")   # 10732 (línea numérica) se puntúa por error, no por aciertos: fuera
S0 <- fread(f_srv, colClasses = "character", encoding = "UTF-8", select = c("alumno", "Centro", "fecha", "edad", "Prueba", "aciertos"))[Prueba %in% COD_SEC]
S0[, `:=`(edad = as.integer(edad), aciertos = as.numeric(aciertos), cn = nrm(Centro))]
S0 <- merge(S0, REG[, .(cn, region, titularidad)], by = "cn", all.x = TRUE)
S0[, muestra := fcase(grepl("leonxiii|cerradodecalderon", cn), "Concertados Málaga 2025",
                      region == "UMA / práctica / pruebas" | grepl("universidad|psicolog", cn), "UMA (adultos)",
                      region %in% c("Málaga", "Cantabria", "Castilla y León") & edad %between% c(9, 11), "Colegios públicos 2023–26",
                      default = NA_character_)]
S0 <- S0[!is.na(muestra) & !is.na(aciertos) & (muestra != "UMA (adultos)" | edad %between% c(18, 40))]
setorder(S0, alumno, Prueba, fecha); S0 <- S0[, .SD[1], by = .(alumno, Prueba)]
S0[, tramo := fifelse(muestra == "UMA (adultos)", "18+", fifelse(edad >= 16, "16–17", fifelse(edad >= 14, "14–15", as.character(edad))))]
HS <- leer_largo(R$largo, c("cod_base", "medida", "edad", "sujeto", "valor"))[medida == "aciertos" & cod_base %in% COD_SEC & edad %in% c("10", "11")]
HS <- HS[, .(alumno = sujeto, Prueba = cod_base, aciertos = as.numeric(valor), tramo = edad, muestra = "Cohortes 2010–15 (públicos)")][!is.na(aciertos)]
HS <- HS[, .SD[1], by = .(alumno, Prueba, tramo)]
SEC <- rbind(S0[, .(muestra, tramo, Prueba, aciertos)], HS[, .(muestra, tramo, Prueba, aciertos)])
NOMSEC <- c(`10682` = "Comprensión (Woodcock-Muñoz)", `10746` = "Homógrafos", `10748` = "Sumas y restas de dos dígitos",
            `10747` = "Magnitudes simbólicas", `10752` = "Magnitudes no simbólicas", `10732` = "Línea numérica 0–100")
SEC[, tarea := NOMSEC[Prueba]]
SECT <- SEC[, .(n = .N, M = round(mean(aciertos), 2), DT = round(sd(aciertos), 2), Md = median(aciertos), P25 = cuantil(aciertos, .25), P75 = cuantil(aciertos, .75),
                pct_techo = round(100 * mean(aciertos == max(SEC[Prueba == .BY$Prueba, aciertos])), 1)), by = .(tarea, Prueba, muestra, tramo)][n >= 10]
ord_tramo <- c("9", "10", "11", "12", "13", "14–15", "16–17", "18+")
SECT[, tramo := factor(tramo, levels = ord_tramo)]; setorder(SECT, tarea, tramo, muestra)

# ---- 6. figuras -----------------------------------------------------------------
dir.create(R$figs, showWarnings = FALSE, recursive = TRUE)
col_g <- c("Cantabria" = PAL[["mar"]], "Castilla y León" = PAL[["ocre"]], "Baleares (catalán)" = PAL[["anil"]], "Baleares" = "#9B8FD0",
           "otras comunidades" = PAL[["tinta"]], "sin identificar" = PAL[["gris"]], "Málaga" = PAL[["coral"]])
MOD[, etiqueta := fifelse(medida == "log(segundos)", paste0(tarea, " (tiempo)"), tarea)]
F1 <- ggplot(MOD, aes(x = d_ajustada, y = reorder(etiqueta, d_ajustada), colour = grupo)) +
  geom_vline(xintercept = 0, colour = PAL[["tinta"]], linewidth = .3) +
  geom_vline(xintercept = c(-.3, .3), colour = PAL[["gris"]], linetype = 3, linewidth = .3) +
  geom_errorbarh(aes(xmin = d_inf, xmax = d_sup), height = 0, linewidth = .4, alpha = .8) + geom_point(size = 1.6) +
  facet_grid(edad ~ ., scales = "free_y", space = "free_y", labeller = labeller(edad = function(x) paste(x, "años"))) +
  scale_colour_manual(values = col_g) + tema_leeduca(7.5) +
  labs(title = "M22 · Diferencia con Málaga 2023–2026 por tarea y edad (d ajustada, IC 95 %)",
       subtitle = "Modelo multinivel: y ~ región + trimestre + (1 | centro). Primera aplicación por niño y curso. En RAN, d > 0 = más rápido.",
       x = "d (en DT de Málaga)", y = NULL)
guardar_fig(F1, "M22_F1_d_por_tarea", R$figs, 174, 20 + 4.2 * nrow(MOD) / max(1, uniqueN(MOD$edad)) * uniqueN(MOD$edad) / 2)

# F2: distribuciones a los 4 años en tareas clave (ECDF por grupo)
clave <- c("2093", "1054", "1923", "523", "124", "1542", "2095", "67", "2129", "217")
S2 <- merge(P[edad == 4 & cb %in% clave], ok[edad == 4, .(cb, edad, grupo)], by = c("cb", "edad", "grupo"))
S2 <- S2[, if (uniqueN(grupo) >= 2) .SD, by = cb]
S2[, etiqueta := fifelse(medida == "log(segundos)", paste0(tarea, " (s)"), tarea)]
S2[, x := fifelse(medida == "log(segundos)", exp(y), y)]
if (nrow(S2)) {
  F2 <- ggplot(S2, aes(x = x, colour = grupo)) + stat_ecdf(linewidth = .6) + facet_wrap(~ etiqueta, scales = "free_x", ncol = 4) +
    scale_colour_manual(values = col_g) + tema_leeduca(7) + theme(strip.text = element_text(size = 6)) +
    labs(title = "M22 · Distribución acumulada a los 4 años por región", subtitle = "Cada curva: niños distintos (primera aplicación del curso). En RAN, segundos: a la izquierda es más rápido.",
         x = NULL, y = "proporción de niños ≤ x")
  guardar_fig(F2, "M22_F2_ecdf_4_anios", R$figs, 174, 95)
}
# F3: niños por grupo × edad en las celdas comparables
F3d <- P[, .(ninos = uniqueN(alumno)), by = .(grupo, edad)]
F3 <- ggplot(F3d, aes(x = factor(edad), y = ninos, fill = grupo)) + geom_col(position = position_dodge(width = .8), width = .75) +
  scale_fill_manual(values = col_g) + tema_leeduca(7.5) + labs(title = "M22 · Niños evaluados 2023–2026 por región y edad", x = "edad (años)", y = "niños distintos")
guardar_fig(F3, "M22_F3_n_region_edad", R$figs, 120, 60)
# F4: alfa por región
if (nrow(AL)) {
  AL[, etiqueta := paste0(tarea, " · ", edad, " a")]
  F4 <- ggplot(AL[!is.na(alfa)], aes(x = alfa, y = reorder(etiqueta, alfa), colour = grupo)) + geom_point(size = 1.8) +
    geom_vline(xintercept = c(.7, .8), colour = PAL[["gris"]], linetype = 3, linewidth = .3) + scale_colour_manual(values = col_g) + tema_leeduca(7.5) +
    labs(title = "M22 · Alfa de Cronbach por región (respuestas ítem a ítem)", x = "α", y = NULL) + xlim(0, 1)
  guardar_fig(F4, "M22_F4_alfa_region", R$figs, 150, 20 + 5 * uniqueN(AL$etiqueta))
}
# F5: Málaga 2023–26 frente a 2010–15
if (nrow(HIST)) {
  F5 <- ggplot(HIST, aes(x = d_2023_vs_2010, y = reorder(paste0(tarea, " · ", edad, ".", trimestre), d_2023_vs_2010), colour = factor(edad))) + geom_vline(xintercept = 0, colour = PAL[["tinta"]], linewidth = .3) +
    geom_vline(xintercept = c(-.3, .3), colour = PAL[["gris"]], linetype = 3, linewidth = .3) + geom_point(size = 1.8) +
    scale_colour_manual(values = unname(PAL[c("mar", "coral", "anil", "ocre", "tinta", "gris")])) + tema_leeduca(7.5) +
    labs(title = "M22 · Málaga 2023–2026 frente a Málaga 2010–2015 (mismo código, aciertos)", x = "d (en DT de 2010–2015)", y = NULL, colour = "edad")
  guardar_fig(F5, "M22_F5_deriva_temporal", R$figs, 150, 20 + 4 * nrow(HIST))
}

# F6: Málaga concertada (o estrato anotado) frente a pública
if (nrow(TIT)) {
  TIT[, etiqueta := paste0(fifelse(medida == "log(segundos)", paste0(tarea, " (tiempo)"), tarea), " · ", edad, " a")]
  F6 <- ggplot(TIT, aes(x = d_ajustada, y = reorder(etiqueta, d_ajustada), colour = grupo)) +
    geom_vline(xintercept = 0, colour = PAL[["tinta"]], linewidth = .3) + geom_vline(xintercept = c(-.3, .3), colour = PAL[["gris"]], linetype = 3, linewidth = .3) +
    geom_point(size = 1.8) + scale_colour_manual(values = c(Concertada = PAL[["anil"]], alto = PAL[["mar"]], medio = PAL[["ocre"]], bajo = PAL[["coral"]]), na.value = PAL[["tinta"]]) +
    tema_leeduca(7.5) + labs(title = "M22 · Málaga: centros concertados frente a públicos (d ajustada)",
                             subtitle = paste0("Aproximación al estrato del centro. Concertados: ", paste(unique(TIT$centros_grupo), collapse = "–"), " centros; d > 0 = mejor rendimiento."),
                             x = "d (en DT de los centros públicos)", y = NULL)
  guardar_fig(F6, "M22_F6_malaga_titularidad", R$figs, 150, 20 + 4.5 * nrow(TIT))
}

# F7: curva 10 años → secundaria → adultos con los mismos códigos
if (nrow(SECT)) {
  F7 <- ggplot(SECT, aes(x = tramo, y = M, colour = muestra, group = muestra)) +
    geom_linerange(aes(ymin = P25, ymax = P75), linewidth = .5, alpha = .6, position = position_dodge(width = .35)) +
    geom_point(aes(size = n), position = position_dodge(width = .35)) + geom_line(position = position_dodge(width = .35), linewidth = .3) +
    facet_wrap(~ tarea, scales = "free_y", ncol = 3) + scale_size_area(max_size = 3, guide = "none") +
    scale_colour_manual(values = c(`Cohortes 2010–15 (públicos)` = PAL[["gris"]], `Colegios públicos 2023–26` = PAL[["coral"]],
                                   `Concertados Málaga 2025` = PAL[["anil"]], `UMA (adultos)` = PAL[["mar"]])) + tema_leeduca(7) +
    labs(title = "M22 · Mismas tareas de 9 años a la universidad", subtitle = "Media y P25–P75 de aciertos. En secundaria solo hay dos centros concertados: edad y estrato no se pueden separar.",
         x = "edad (años)", y = "aciertos")
  guardar_fig(F7, "M22_F7_secundaria_adultos", R$figs, 174, 100)
}

# ---- 7. salida --------------------------------------------------------------------
CLAS <- merge(COD[, .(Prueba, Nombre, lengua_inventario_v2 = fifelse(grepl("valenc", clase_uso), "valenciano/catalán", "castellano"))],
              CAT[, .(Prueba, lengua_m22 = "catalán", codigo_base_castellano = codigo_base)], by = "Prueba", all.x = TRUE)
CLAS[is.na(lengua_m22), lengua_m22 := "castellano"]; CLAS <- CLAS[lengua_inventario_v2 != lengua_m22 | lengua_m22 == "catalán"][order(lengua_m22, Nombre)]
escribir_xlsx(list(`0_Codigos_lengua` = CLAS, `1_Centros` = CEN[, .(Centro_seud, Centro, region, grupo, titularidad, provincia, municipio, ninos, edad_min, edad_max, desde, hasta, pct_cribado, pct_catalan)],
                   `2_Seleccion` = SEL, `3_Celdas_comparables` = cel[cb %in% cel_ok$cb][order(edad, tarea, grupo)], `4_Descriptivos` = RES,
                   `5_Modelo_multinivel` = MOD[, .(cb, tarea, medida, edad, grupo, n_total, centros_total, beta, ET, d_ajustada, d_inf, d_sup, icc_centro, pct_var_centro_explicada_por_region)],
                   `6_Resumen` = RESUMEN, `7_Alfa_region` = AL, `8_Malaga_2010_vs_2023` = HIST, `9_Malaga_titularidad` = TIT, `10_Malaga_centros_tit` = CENT_M, `11_Secundaria_adultos` = SECT),
              file.path(R$salida, "M22_regiones_R_v2.xlsx"),
              paste("m22_regiones.R v1 (01/11/2026). Comparación entre regiones de los indicadores 2023–2026 y frente a Málaga 2010–2015.",
                    "Lengua: solo las actividades cuyo nombre empieza por BL_VAL_ son versiones en catalán (Baleares, marzo–abril 2024); BL_DF_VAL_ es RAN en castellano.",
                    "Región y titularidad del centro:", basename(f_reg), "(v2: centros localizados uno a uno; confianza en la tabla). Hoja 9: Málaga concertada (o estrato anotado por PJ) frente a pública. cribado = ≥", CRIBADO_MIN, "niños de la misma edad en el mismo centro y mes.",
                    "d ajustada = coeficiente de región del modelo y ~ región + trimestre + (1|centro), dividido por la DT de Málaga; en RAN (log segundos) el signo se invierte para que d > 0 = más rápido.",
                    "Celdas con ≥", N_MIN, "niños y ≥", C_MIN, "centros en Málaga y en al menos otra región. Uso interno: no publicar rankings de centros ni de comunidades."))
print(SEL); print(RESUMEN); print(MOD[, .(tarea, edad, grupo, n_total, d_ajustada, d_inf, d_sup, icc_centro)]); if (nrow(AL)) print(AL[, .(tarea, edad, grupo, n, items, alfa)]); print(HIST[, .(tarea, edad, trimestre, n_2023_26, n_2010_15, d_2023_vs_2010)])
