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
  t <- utils::read.csv(f, check.names = FALSE, encoding = "UTF-8", stringsAsFactors = FALSE)
  # identificadores (momento, cohorte, código, sesión…) se muestran tal cual, sin formato numérico
  ids <- grepl("^(mom|momento|desde|hasta|cohortes?|sesion(es)?|cod|codigo|curso|trim|trimestres?|forma|edad)$", names(t), ignore.case = TRUE)
  t[ids] <- lapply(t[ids], function(x) if (is.numeric(x)) format(x, trim = TRUE, drop0trailing = TRUE) else x)
  t
}

# Números con coma decimal y punto de miles, como en el resto del manual
num_es <- function(x, d = 2) formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")

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
  for (f in sort(list.files(dir, pattern = "\\.csv$"))) {
    h <- sub("\\.csv$", "", f)
    cat("\n\n<details><summary>", gsub("_", " ", h), " <code>datos/agregados/", ficha, "/", f, "</code></summary>\n\n", sep = "")
    print(tabla(leer(ficha, h)))
    cat("\n\n</details>\n")
  }
}

# Paleta y tema LEEDUCA (validados para daltonismo; los mismos que en los scripts de análisis)
PAL <- c(mar = "#008FA3", coral = "#D64545", anil = "#5E4FA2", ocre = "#B07A10", tinta = "#22223B", gris = "#8D8F99",
         rejilla = "#E6E3DC", papel = "#FBFAF7")
tema_leeduca <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(panel.background = element_rect(fill = PAL[["papel"]], colour = NA), plot.background = element_rect(fill = "white", colour = NA),
          panel.grid.major = element_line(colour = PAL[["rejilla"]], linewidth = .3), panel.grid.minor = element_blank(),
          axis.line = element_line(colour = PAL[["gris"]], linewidth = .3), axis.text = element_text(colour = PAL[["gris"]]),
          axis.title = element_text(colour = PAL[["tinta"]]), plot.title = element_text(face = "bold", colour = PAL[["tinta"]], hjust = 0),
          plot.title.position = "plot", strip.text = element_text(face = "bold", hjust = 0, colour = PAL[["tinta"]]),
          legend.position = "bottom", legend.title = element_blank())
}
