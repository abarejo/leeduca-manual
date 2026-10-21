# =============================================================================
# exportar_agregados.R — pasa las tablas AGREGADAS de los análisis al libro
# -----------------------------------------------------------------------------
# Se ejecuta en el PC del proyecto, después de los scripts de análisis
# (04_DICCIONARIO_PROCESADO_2026/scripts/R/ejecutar_todo.R).
# Lee los Excel de resultados (DIAGNOSTICO/R/*_R_v*.xlsx) y escribe cada hoja
# como CSV en datos/agregados/<ficha>/<hoja>.csv dentro de este repositorio.
#
# PRIVACIDAD: el libro es público o puede llegar a serlo. Este script
#   · solo exporta los Excel de la lista EXPORTAR (nunca los .rds ni el de ISE/sexo);
#   · se detiene si una hoja tiene columnas que identifiquen a un niño, un
#     profesor o un colegio con nombre (sujeto, NIE, nombre, fecha_nto, colegio…)
#     o si tiene más de 2.000 filas (una tabla agregada nunca las tiene);
#   · no publica baremos de pruebas comerciales (OMITIR_HOJAS, OMITIR_COLUMNAS).
#
# Uso (desde la carpeta del repositorio):
#   Rscript analisis/exportar_agregados.R --diag "C:/.../04_DICCIONARIO_PROCESADO_2026/DIAGNOSTICO/R"
# =============================================================================
suppressPackageStartupMessages({ library(readxl) })
a <- commandArgs(trailingOnly = TRUE)
diag <- if (length(i <- which(a == "--diag"))) a[i + 1] else
  "C:/Users/Usuario/Documents/DRIVE/LEEDUCA/PLATAFORMA/04_DICCIONARIO_PROCESADO_2026/DIAGNOSTICO/R"
destino <- "datos/agregados"

# ficha del libro  <-  Excel de resultados
EXPORTAR <- c(
  RAN = "ficha_RAN_R_v1.xlsx",
  LET = "ficha_LET_R_v1.xlsx",
  LET_rigor = "rigor_LET_R_v1.xlsx",
  VEL = "velocidad_lectora_R_v1.xlsx",
  VEL_contexto = "velocidad_contexto_R_v1.xlsx",
  VEL_indicadores = "ficha_velocidad_indicadores_R_v2.xlsx",
  GEN_trimestre = "baremo_trimestre_vs_curso_R_v1.xlsx",
  CF523 = "ficha_CF523_R_v1.xlsx",
  TECLE = "ficha_TECLE_R_v1.xlsx",
  TECLE_papel = "ficha_TECLE_papel_R_v2.xlsx",
  TECLE_equiv = "ficha_TECLE_equivalencia_R_v1.xlsx",
  CATALOGO = "catalogo_tareas_R_v1.xlsx",
  ESTRUCTURA = "estructura_bateria_R_v1.xlsx",
  REDES = "red_tareas_v1.xlsx"
)
PROHIBIDAS <- "^(sujeto|nie|nombre|apellidos?|fecha_?n(ac|to)|colegio(_txt)?|centro|profesor|docente|email)$"
MAX_FILAS <- 2000
# DERECHOS: los baremos de pruebas comerciales (PROLEC-R, EDICOLE…) se usaron para
# comparar en el PC, pero no se publican en el libro; se citan con su referencia.
# Los percentiles del TECLE en papel 2020/21 son la base del estudio normativo publicado por el grupo
# (Carrillo Gállego et al., 2024): no se publican aquí sin acuerdo con su autora principal.
OMITIR_HOJAS <- c("1c_PROLEC_R_en_ppm", "P3_Percentiles_NO_PUBLICAR")
OMITIR_COLUMNAS <- "^(PROLEC|EDICOLE_baremo|clave|opcion_modal)"  # clave y opcion_modal = respuestas correctas de TECLE: solo en el PC

slug <- function(x) { x <- iconv(x, "UTF-8", "ASCII//TRANSLIT"); gsub("[^A-Za-z0-9_]+", "_", x) }
dir.create(destino, recursive = TRUE, showWarnings = FALSE)
total <- 0
for (ficha in names(EXPORTAR)) {
  f <- file.path(diag, EXPORTAR[[ficha]])
  if (!file.exists(f)) { message("· falta ", EXPORTAR[[ficha]], " (se omite)"); next }
  dir.create(file.path(destino, ficha), showWarnings = FALSE)
  for (h in setdiff(excel_sheets(f), c("LEEME", OMITIR_HOJAS))) {
    t <- read_excel(f, h)
    t <- t[, !grepl(OMITIR_COLUMNAS, names(t)), drop = FALSE]
    malas <- grep(PROHIBIDAS, names(t), ignore.case = TRUE, value = TRUE)
    if (length(malas)) stop("PRIVACIDAD: ", EXPORTAR[[ficha]], " / ", h, " tiene columnas identificativas: ", paste(malas, collapse = ", "))
    if (nrow(t) > MAX_FILAS) stop("PRIVACIDAD: ", EXPORTAR[[ficha]], " / ", h, " tiene ", nrow(t), " filas; no parece una tabla agregada")
    write.csv(t, file.path(destino, ficha, paste0(slug(h), ".csv")), row.names = FALSE, fileEncoding = "UTF-8", na = "")
    total <- total + 1
  }
  message("OK ", ficha, " <- ", EXPORTAR[[ficha]])
}
message(total, " tablas exportadas a ", destino, ". Revisa con `git diff --stat` antes de subir.")

# Fichas automáticas (scripts/R/ficha_generica.R): una carpeta AUTO_<tarea> por cada Excel de DIAGNOSTICO/R/fichas_auto
# (si una tarea tiene varias versiones, _R_v1, _R_v2…, se exporta solo la más reciente)
fa <- list.files(file.path(diag, "fichas_auto"), pattern = "^ficha_.*_R_v[0-9]+\\.xlsx$", full.names = TRUE)
ver <- as.integer(sub(".*_R_v([0-9]+)\\.xlsx$", "\\1", basename(fa))); tar <- sub("^ficha_(.*)_R_v[0-9]+\\.xlsx$", "\\1", basename(fa))
fa <- fa[order(tar, -ver)][!duplicated(tar[order(tar, -ver)])]
for (f in fa) {
  ficha <- paste0("AUTO_", sub("^ficha_(.*)_R_v[0-9]+\\.xlsx$", "\\1", basename(f))); dir.create(file.path(destino, ficha), showWarnings = FALSE)
  for (h in setdiff(excel_sheets(f), "LEEME")) {
    t <- read_excel(f, h); t <- t[, !grepl(OMITIR_COLUMNAS, names(t)), drop = FALSE]
    malas <- grep(PROHIBIDAS, names(t), ignore.case = TRUE, value = TRUE)
    if (length(malas)) stop("PRIVACIDAD: ", basename(f), " / ", h, " tiene columnas identificativas: ", paste(malas, collapse = ", "))
    if (nrow(t) > MAX_FILAS) stop("PRIVACIDAD: ", basename(f), " / ", h, " tiene demasiadas filas")
    write.csv(t, file.path(destino, ficha, paste0(slug(h), ".csv")), row.names = FALSE, fileEncoding = "UTF-8", na = "")
  }
  message("OK ", ficha)
}

# Niveles de certeza (capítulo «Cómo estimamos el riesgo»): solo dos hojas agregadas de certeza_cortes_baremos_v2.xlsx
# (el archivo tiene también la hoja Etiquetas_NIE, por niño, que NUNCA se exporta)
fc <- file.path(dirname(diag), "certeza_cortes_baremos_v2.xlsx")
if (file.exists(fc)) {
  dir.create(file.path(destino, "RIESGO"), showWarnings = FALSE)
  write.csv(read_excel(fc, "Etiquetas_resumen"), file.path(destino, "RIESGO", "etiquetas_por_cohorte.csv"), row.names = FALSE, fileEncoding = "UTF-8", na = "")
  est <- read_excel(fc, "Estabilidad_entre_cursos"); est <- est[est$cohorte == "TODAS", ]
  write.csv(est, file.path(destino, "RIESGO", "estabilidad_todas.csv"), row.names = FALSE, fileEncoding = "UTF-8", na = "")
  message("OK RIESGO <- certeza_cortes_baremos_v2.xlsx (2 hojas agregadas)")
}
