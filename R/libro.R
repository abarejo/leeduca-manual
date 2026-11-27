# Funciones comunes del libro: leer tablas agregadas, mostrarlas y dar estilo a las figuras.
# Cada capítulo empieza con: source(here::here("R/libro.R")) o source("../R/libro.R")
suppressPackageStartupMessages({ library(ggplot2) })

raiz_libro <- function() {
  d <- getwd()
  while (!file.exists(file.path(d, "_quarto.yml")) && dirname(d) != d) d <- dirname(d)
  d
}

# Lee datos/agregados/<ficha>/<hoja>.csv (tablas agregadas exportadas con analisis/exportar_agregados.R)
leer <- function(ficha, hoja) {
  f <- file.path(raiz_libro(), "datos", "agregados", ficha, paste0(hoja, ".csv"))
  if (!file.exists(f)) stop("No existe la tabla ", ficha, "/", hoja, ". Ejecuta analisis/exportar_agregados.R")
  t <- tryCatch(utils::read.csv(f, check.names = FALSE, encoding = "UTF-8", stringsAsFactors = FALSE), error = function(e) data.frame())
  if (ncol(t) <= 1 && nrow(t) == 0) return(data.frame())   # hoja vacía (sin filas ni columnas útiles)
  # identificadores (momento, cohorte, código, sesión…) se muestran tal cual, sin formato numérico
  ids <- grepl("^(mom|momento|desde|hasta|cohortes?|sesion(es)?|cod|codigo|curso|trim|trimestres?|forma|edad)$", names(t), ignore.case = TRUE)
  t[ids] <- lapply(t[ids], function(x) if (is.numeric(x)) format(x, trim = TRUE, drop0trailing = TRUE) else x)
  t
}

# Números con coma decimal y punto de miles, como en el resto del manual
# (27/11/2026) En los capítulos en inglés (archivos *.en.qmd) se usa punto decimal y coma de miles.
libro_en_ingles <- function() {
  f <- tryCatch(knitr::current_input(), error = function(e) NULL)
  !is.null(f) && grepl("\\.en\\.(qmd|Rmd|md|rmarkdown|knit\\.md)$", f, ignore.case = TRUE)
}
num_es <- function(x, d = 2) {
  if (libro_en_ingles()) return(formatC(x, format = "f", digits = d, big.mark = ",", decimal.mark = "."))
  formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")
}

# Tabla para el libro: redondea, usa coma decimal y renombra columnas si se pide
tabla <- function(df, nombres = NULL, digitos = 2, pie = NULL) {
  df <- as.data.frame(df)
  for (j in seq_along(df)) if (is.numeric(df[[j]])) {
    entero <- all(is.na(df[[j]]) | abs(df[[j]] - round(df[[j]])) < 1e-9)
    df[[j]] <- ifelse(is.na(df[[j]]), "", num_es(df[[j]], if (entero) 0 else digitos))
  }
  if (!is.null(nombres)) names(df) <- nombres
  knitr::kable(df, caption = pie, align = ifelse(vapply(df, function(x) all(grepl("^[-−0-9.,% ]*$", x)), TRUE), "r", "l"))
}

# Todas las tablas de una ficha, plegadas, para quien quiera el detalle completo
tablas_completas <- function(ficha) {
  dir <- file.path(raiz_libro(), "datos", "agregados", ficha)
  if (libro_en_ingles() && !isTRUE(getOption("leeduca.aviso_tablas"))) {
    cat("\n\n*These tables are printed as produced by the analysis scripts; sheet and column names are in Spanish (see the [glossary](/glosario.en.qmd)).*\n\n")
    options(leeduca.aviso_tablas = TRUE)
  }
  for (f in sort(list.files(dir, pattern = "\\.csv$"))) {
    h <- sub("\\.csv$", "", f); if (!nrow(leer(ficha, h))) next
    cat("\n\n<details><summary>", gsub("_", " ", h), " <code>datos/agregados/", ficha, "/", f, "</code></summary>\n\n", sep = "")
    print(tabla(leer(ficha, h)))
    cat("\n\n</details>\n")
  }
}

# Paleta y tema LEEDUCA (validados para daltonismo; los mismos que en los scripts de análisis)
# 02/11/2026: los nombres antiguos se conservan, pero apuntan a la paleta del grupo (mar = azul, coral = naranja,
# anil = verde oscuro, ocre = amarillo oscuro). Paleta anterior: mar #008FA3, coral #D64545, anil #5E4FA2, ocre #B07A10.
PAL <- c(mar = "#2689B7", coral = "#E57813", anil = "#7C8710", ocre = "#C9920E", tinta = "#22223B", gris = "#888888",
         rejilla = "#E4E1D6", papel = "#F5F4EF")
# Paleta del grupo LEEDUCA (02/11/2026): colores de la web (leeduca.es, verde #ABBB10) y de las presentaciones de
# J. L. Luque (azul de títulos #2689B7, naranja de cabecera #E57813, amarillo #F1B115, fondo crema #F5F4EF).
# Para figuras nuevas; acompañar siempre el color con forma, rótulo o tipo de línea (accesibilidad).
PAL_GRUPO <- c(azul = "#2689B7", verde = "#ABBB10", verde_osc = "#7C8710", naranja = "#E57813", amarillo = "#F1B115",
               crema = "#F5F4EF", tinta = "#22223B", gris = "#888888", rejilla = "#E4E1D6")
# Paleta categórica del grupo: predeterminada para todos los ggplot del libro (colores discretos)
PAL_CAT <- c("#2689B7", "#E57813", "#7C8710", "#F1B115", "#22223B", "#ABBB10", "#888888", "#1F6F96")
options(ggplot2.discrete.colour = PAL_CAT, ggplot2.discrete.fill = PAL_CAT)
tema_grupo <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(panel.background = element_rect(fill = PAL_GRUPO[["crema"]], colour = NA), plot.background = element_rect(fill = "white", colour = NA),
          panel.grid.major = element_line(colour = PAL_GRUPO[["rejilla"]], linewidth = .3), panel.grid.minor = element_blank(),
          axis.text = element_text(colour = PAL_GRUPO[["gris"]]), axis.title = element_text(colour = PAL_GRUPO[["tinta"]]),
          plot.title = element_text(face = "bold", colour = PAL_GRUPO[["azul"]], hjust = 0), plot.title.position = "plot",
          plot.subtitle = element_text(colour = PAL_GRUPO[["tinta"]]),
          strip.text = element_text(face = "bold", hjust = 0, colour = PAL_GRUPO[["tinta"]]),
          legend.position = "bottom", legend.title = element_blank())
}
tema_leeduca <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(panel.background = element_rect(fill = PAL[["papel"]], colour = NA), plot.background = element_rect(fill = "white", colour = NA),
          panel.grid.major = element_line(colour = PAL[["rejilla"]], linewidth = .3), panel.grid.minor = element_blank(),
          axis.line = element_line(colour = PAL[["gris"]], linewidth = .3), axis.text = element_text(colour = PAL[["gris"]]),
          axis.title = element_text(colour = PAL[["tinta"]]), plot.title = element_text(face = "bold", colour = PAL[["tinta"]], hjust = 0),
          plot.title.position = "plot", strip.text = element_text(face = "bold", hjust = 0, colour = PAL[["tinta"]]),
          legend.position = "bottom", legend.title = element_blank())
}

# ---------------------------------------------------------------------------
# Rango de puntuaciones (21/10/2026). Los descriptivos de las fichas traen minimo, P1, P99 y maximo.
#   · «Rango habitual» = P1–P99: el 98 % central, poco sensible a errores de registro.
#   · «Mín.–máx.» = rango observado tras la limpieza.
#   · Aviso de valor extremo: el máximo (o el mínimo) se aleja del P99 (o del P1) más de 2 DT. Suele ser un error
#     de tiempo o de registro y conviene revisarlo antes de dar el rango como dato de la prueba.
nf <- function(x, d) ifelse(abs(x - round(x)) < 1e-9, num_es(x, 0), num_es(x, d))
rango_txt <- function(a, b, d = 1) ifelse(is.na(a) | is.na(b), "—", ifelse(abs(a - b) < 1e-9, nf(a, d), paste0(nf(a, d), "–", nf(b, d))))
extremo <- function(D) with(D, (maximo - P99) > 2 * DT | (P1 - minimo) > 2 * DT)
tabla_rango <- function(D, grupo = NULL, d = 1, posible = NULL) {
  D <- as.data.frame(D); D <- D[is.finite(D$minimo), ]
  out <- data.frame(D[, c(grupo, "mom", "N")], habitual = rango_txt(D$P1, D$P99, d), observado = rango_txt(D$minimo, D$maximo, d), check.names = FALSE)
  if (!is.null(posible)) out$posible <- posible
  en <- libro_en_ingles()
  out$aviso <- ifelse(extremo(D) %in% TRUE, if (en) "extreme value: check" else "valor extremo: revisar", "")
  nm <- if (en) c(if (!is.null(grupo)) c(medida = "Measure", forma = "Form")[grupo] %||% grupo, "Time point", "N", "Usual range (P1–P99)", "Min.–max.", if (!is.null(posible)) "Possible range", "Warning")
        else c(if (!is.null(grupo)) c(medida = "Medida", forma = "Forma")[grupo] %||% grupo, "Momento", "N", "Rango habitual (P1–P99)", "Mín.–máx.", if (!is.null(posible)) "Rango posible", "Aviso")
  tabla(out, unname(nm), digitos = d)
}
`%||%` <- function(a, b) if (is.null(a) || all(is.na(a))) b else a
fig_rango <- function(D, grupo = NULL, unidad = "Puntuación") {
  D <- as.data.frame(D); D <- D[is.finite(D$minimo), ]; D$mom <- factor(D$mom, unique(D$mom[order(as.numeric(D$mom))]))
  D$ext <- extremo(D) %in% TRUE
  p <- ggplot(D, aes(mom)) +
    geom_linerange(aes(ymin = minimo, ymax = maximo), colour = PAL[["gris"]], linewidth = .5) +
    geom_linerange(aes(ymin = P1, ymax = P99), colour = PAL[["mar"]], linewidth = 3.2, alpha = .45) +
    geom_point(aes(y = Md), colour = PAL[["tinta"]], size = 2) +
    geom_point(data = D[D$ext, ], aes(y = maximo), shape = 4, colour = PAL[["coral"]], size = 2.6, stroke = 1.1) +
    labs(x = if (libro_en_ingles()) "Time point (age.term)" else "Momento (edad.trimestre)", y = unidad,
         caption = if (libro_en_ingles()) "Band: usual range (P1–P99). Thin line: minimum–maximum. Dot: median. Red cross: extreme value worth checking."
                   else "Banda: rango habitual (P1–P99). Línea fina: mínimo–máximo. Punto: mediana. Aspa roja: valor extremo que conviene revisar.") +
    tema_leeduca() + theme(plot.caption = element_text(colour = PAL[["gris"]], hjust = 0, size = 8))
  if (!is.null(grupo)) p <- p + facet_wrap(stats::as.formula(paste("~", grupo)), scales = "free", ncol = 2)
  p
}

# ---- configuración real de las actividades en la plataforma (skill leeduca-ficha-actividad) ----
# Tabla agregada en datos/agregados/PLATAFORMA/actividades_plataforma.csv: una fila por código, leída de la propia
# actividad (editar_actividad.php) sin modificarla. Sin listas completas de ítems ni claves: solo 1–2 ejemplos.
plataforma_actividades <- function() {
  f <- file.path(raiz_libro(), "datos", "agregados", "PLATAFORMA", "actividades_plataforma.csv")
  if (!file.exists(f)) return(data.frame())
  utils::read.csv(f, encoding = "UTF-8", check.names = FALSE, stringsAsFactors = FALSE, colClasses = "character")
}
plataforma_tabla <- function(codigos = NULL, tarea = NULL) {
  P <- plataforma_actividades(); if (!nrow(P)) return(invisible(FALSE))
  if (!is.null(tarea)) P <- P[P$tarea_canonica %in% tarea, ]
  if (!is.null(codigos)) P <- P[P$codigo %in% as.character(codigos), ]
  if (!nrow(P)) return(invisible(FALSE))
  en <- libro_en_ingles()
  if (en) cat("The platform is the reference: what the activity actually presents is what is measured. Configuration read from the activity itself on ",
      P$extraido[1], " (without modifying it). Cell contents are in Spanish.\n\n", sep = "") else
  cat("La plataforma es la referencia: lo que reproduce la actividad es lo que se mide. Configuración leída de la propia actividad el ",
      P$extraido[1], " (sin modificarla).\n\n", sep = "")
  rownames(P) <- NULL; T <- P[, c("codigo", "nombre_corto", "presentacion", "n_items", "items_practica", "ejemplos", "tiempo", "orden", "puntuacion", "momentos_etiqueta")]
  print(tabla(T, if (en) c("Code", "Name on the platform", "Presentation", "Test items", "Practice items", "Example items", "Time limit",
                   "Order", "Scoring", "Age.term (label 3)") else c("Código", "Nombre en la plataforma", "Presentación", "Ítems de prueba", "Ítems de práctica", "Ejemplos de ítems", "Límite de tiempo",
                   "Orden", "Puntuación", "Edad.trimestre (etiqueta 3)"), digitos = 0))
  cat("\n\n")
  inc <- P[nzchar(P$incidencia), ]
  if (nrow(inc)) { cat(if (en) "**Differences from the dictionary or the instructions (in Spanish):**\n\n" else "**Diferencias con el diccionario o las instrucciones:**\n\n"); cat(paste0("- **", inc$codigo, ".** ", inc$incidencia, collapse = "\n"), "\n\n") }
  cat(if (en) "*Age.term: provisional reading of the activity's label 3 (3rd digit = age, 4th = term; 0 = annual), to be confirmed with the engineer.*\n\n" else "*Edad.trimestre: lectura provisional de la etiqueta 3 de la actividad (3.ª cifra = edad, 4.ª = trimestre; 0 = anual), pendiente de confirmar con el ingeniero.*\n\n")
  invisible(TRUE)
}

# Capturas de la actividad en la plataforma (figuras/plataforma/<código>.jpg; primer ítem, sin guardar datos).
# `pre` = ruta relativa desde la página hasta la raíz del libro ("" capítulos, "../" fichas, "../../" fichas/auto).
# Nunca hay capturas de pruebas comerciales (TECLE, EDICOLE, Woodcock, PROLEC, Raven): no se copian a figuras/plataforma.
plataforma_capturas <- function(codigos, pre = "", ncol = 3) {
  codigos <- unique(as.character(codigos))
  f <- file.path(raiz_libro(), "figuras", "plataforma", paste0(codigos, ".jpg")); codigos <- codigos[file.exists(f)]
  if (!length(codigos)) return(invisible(FALSE))
  P <- plataforma_actividades(); nom <- setNames(P$nombre_corto, P$codigo)
  lab <- ifelse(codigos %in% names(nom), paste0(codigos, " · ", trimws(nom[codigos])), codigos)
  en <- libro_en_ingles()
  cat(if (en) "**How it looks on the platform** (first item or example; 2026 screenshot; the interface is in Spanish). " else "**Así se ve en la plataforma** (primer ítem o ejemplo; captura de 2026). ")
  if (any(codigos %in% c("1931", "1932", "1933", "1934", "1935", "1936"))) cat(if (en) "For the reading lists only the first two items are shown." else "En las listas de lectura solo se muestran los dos primeros ítems.")
  cat("\n\n")
  cat(sprintf("::: {layout-ncol=%d}\n", min(ncol, length(codigos))))
  w <- if (length(codigos) == 1) " width=\"40%\"" else ""
  cat(paste0("![", lab, "](", pre, "figuras/plataforma/", codigos, ".jpg){.lightbox", w, " fig-alt=\"", if (en) "Screenshot of activity " else "Captura de la actividad ", codigos, "\"}", collapse = "\n\n"))
  cat("\n:::\n\n"); invisible(TRUE)
}
