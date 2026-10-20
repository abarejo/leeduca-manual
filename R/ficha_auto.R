# Ficha automática de una tarea a partir de datos/agregados/AUTO_<tarea>/ (salida de scripts/R/ficha_generica.R) y del catálogo.
# Uso en un .qmd:  ```{r} #| results: asis
#                  ficha_auto("LEC_PAL_2sil") ```
# Genera texto, tablas y figuras con la misma estructura para todas las tareas. El marco teórico y la interpretación se escriben a mano.
FAM_NOM <- c(A = "A · Precisión", B = "B · Denominación rápida", C = "C · Fluidez lectora oral", D = "D · Eficiencia con tiempo limitado",
             E = "E · Decisión con tiempo de respuesta", F = "F · Amplitud y memoria", G = "G · Comprensión")
PANT <- c("imágenes en pantalla" = "formato_imagenes.png", "lectura en voz alta de una lista" = "formato_lista.png",
          "denominación rápida de una serie" = "formato_ran.png", "elección entre opciones" = "formato_decision.png")
r2 <- function(x) sub("^(-?)0", "\\1", sprintf("%.2f", x))
rango <- function(v, f = r2) { v <- v[is.finite(v)]; if (!length(v)) return("—"); if (length(v) == 1 || diff(range(v)) < .005) f(v[1]) else paste0(f(min(v)), "–", f(max(v))) }

ficha_auto <- function(TAREA) {
  rl <- raiz_libro(); dir <- file.path(rl, "datos", "agregados", paste0("AUTO_", TAREA))
  C <- utils::read.csv(file.path(rl, "datos", "agregados", "CATALOGO", "Catalogo.csv"), encoding = "UTF-8", check.names = FALSE, stringsAsFactors = FALSE)
  ct <- C[C$tarea_canonica == TAREA, ]
  lee <- function(h) if (file.exists(file.path(dir, paste0(h, ".csv")))) leer(paste0("AUTO_", TAREA), h) else data.frame()
  FI <- lee("0_Ficha"); MU <- lee("1_Muestra"); DS <- lee("2_Descriptivos_SIN"); ES <- lee("3_Fiabilidad_estabilidad"); VC <- lee("4a_Validez_concurrente")
  VE <- lee("4b_Validez_frente_etiqueta"); BC <- lee("4c_Baja_comprension_posterior"); BA <- lee("5_Baremo_SIN"); EF <- lee("6_Efectos_variables")
  fam <- ct$familia; circ <- isTRUE(as.logical(FI$circular_con_etiqueta))
  out <- function(...) cat(..., "\n\n", sep = "")

  out('::: {.callout-note appearance="minimal"}\n**Ficha automática** (', FAM_NOM[[fam]], '; clase de calidad ', ct$clase, '). Generada el ', format(Sys.Date(), "%d/%m/%Y"),
      ' con `scripts/R/ficha_generica.R --tarea ', TAREA, '`. Las cifras salen de los datos; **falta el marco teórico, la interpretación y la revisión del equipo**.\n:::')
  # ---- en una frase ----
  est <- if (nrow(ES)) median(ES$rho) else NA
  auc_txt <- if (nrow(VE)) paste0("AUC ", rango(VE$AUC), if (circ) " frente a la dificultad persistente (circular: la etiqueta usa esta tarea)" else " frente a la dificultad lectora persistente") else NULL
  ed <- as.numeric(strsplit(as.character(ct$edades), ", ")[[1]]); ed_txt <- if (length(ed) > 1) paste0("de ", min(ed), " a ", max(ed), " años") else paste0("a los ", ed, " años")
  out("**En una frase.** Mide ", ct$constructo, " ", ed_txt, " (", sum(MU$N), " aplicaciones en ", length(unique(MU$cohorte)), " cohortes). ",
      if (is.finite(est)) paste0("Estabilidad entre momentos ρ = ", r2(est), ". ") else "", if (!is.null(auc_txt)) paste0(auc_txt, ".") else "")
  # ---- descripción ----
  out("## Descripción")
  pant <- if (!is.na(ct$formato) && ct$formato %in% names(PANT)) sprintf('\n\n![Ejemplo del formato de pantalla (captura de 2018; no es necesariamente un ítem de esta tarea).](../../figuras/pantallas/%s){width=45%%}', PANT[[ct$formato]]) else ""
  out("| | |\n|---|---|\n| Códigos | ", ct$codigos, " |\n| Batería | ", ifelse(is.na(ct$bateria) | ct$bateria == "", "—", ct$bateria), " |\n| Formato | ", ifelse(is.na(ct$formato), "—", ct$formato),
      " |\n| Ítems | ", ifelse(is.na(ct$n_items), "—", ct$n_items), " |\n| Familia de ficha | ", FAM_NOM[[fam]], " |\n| Puntuación | ", FI$puntuacion, " |\n| Consigna (plataforma) | ", ifelse(is.na(ct$descripcion), "—", ct$descripcion), " |", pant)
  # ---- muestra y descriptivos ----
  out("## Muestra y descriptivos")
  print(tabla(MU[, c("mom", "cohorte", "sesion", "curso_escolar", "N", "SIN", "CON", "colegios")], c("Momento", "Cohorte", "Sesión", "Curso escolar", "N", "Sin Preleo", "Con Preleo", "Colegios"), digitos = 0))
  cat("\n\n")
  if (nrow(DS)) { print(tabla(DS[, c("mom", "N", "cohortes", "M", "DT", "Md", "asimetria", "pct_suelo", "pct_techo")],
                              c("Momento", "N", "Cohortes", "Media", "DT", "Mediana", "Asimetría", "% suelo", "% techo"), digitos = 2)); cat("\n\n") }
  avisos <- character()
  if (nrow(DS) && any(DS$pct_techo > 15, na.rm = TRUE)) avisos <- c(avisos, sprintf("Efecto techo en %s (más del 15 %% de los niños con la puntuación máxima).", paste(DS$mom[DS$pct_techo > 15 & !is.na(DS$pct_techo)], collapse = ", ")))
  if (nrow(DS) && any(DS$pct_suelo > 15, na.rm = TRUE)) avisos <- c(avisos, sprintf("Efecto suelo en %s (más del 15 %% con cero aciertos).", paste(DS$mom[DS$pct_suelo > 15 & !is.na(DS$pct_suelo)], collapse = ", ")))
  if (length(unique(unlist(strsplit(ct$codigos, " ")))) > 1) avisos <- c(avisos, "La tarea tiene varios códigos (versiones o formas): comprobar que los momentos son comparables antes de unir baremos.")
  # ---- fiabilidad ----
  out("## Fiabilidad")
  if (nrow(ES)) {
    out("Estabilidad entre momentos en los mismos niños: ρ de ", rango(ES$rho), " (mediana ", r2(median(ES$rho)), ", ", nrow(ES), " pares de momentos). ",
        if (!is.na(ct$alfa_2021)) paste0("Consistencia interna en los análisis de 2021: α = ", r2(ct$alfa_2021), ". ") else "Falta la consistencia interna con las respuestas ítem a ítem. ",
        if (fam %in% c("B", "C", "D")) "Al ser una tarea con tiempo, la estabilidad es la estimación de fiabilidad preferible." else "")
    print(tabla(ES[, c("cohorte", "desde", "hasta", "n", "r", "rho")], c("Cohorte", "Desde", "Hasta", "n", "r", "ρ"), digitos = 2)); cat("\n\n")
  } else out("No hay dos momentos con los mismos niños (al menos 80). ", if (!is.na(ct$alfa_2021)) paste0("α (2021) = ", r2(ct$alfa_2021), ".") else "")
  # ---- validez ----
  out("## Validez")
  propio <- if (startsWith(TAREA, "RAN_")) "Denominación rápida (RAN)" else if (startsWith(TAREA, "CF_")) "Conciencia fonológica" else if (TAREA == "LEC_LETRAS") "Conocimiento de letras" else
    if (grepl("^LEC_(PAL|PSEUDO|SILABAS|VEL|TECLE|LECT)", TAREA)) "Lectura de palabras y texto" else if (startsWith(TAREA, "HFON_MEMVERB")) "Memoria verbal" else ""
  if (nrow(VC)) {
    pr <- VC[VC$dominio == propio, ]; ot <- VC[VC$dominio != propio, ]; ot <- ot[!duplicated(ot$mom), ]; ot <- ot[order(as.numeric(ot$mom)), ]
    out("**Concurrente.** ", if (nrow(pr)) paste0("Con otras tareas de su mismo dominio (", propio, "), r = ", rango(pr$r), ". ") else "",
        if (nrow(ot)) paste0("Fuera de su dominio, la relación más alta es con: ", paste(sprintf("%s (%s, r = %s)", ot$dominio, ot$mom, r2(ot$r)), collapse = "; "), ".") else "") }
  if (nrow(VE) || nrow(BC)) {
    out("**Frente a la dificultad lectora y la comprensión.** ", if (nrow(VE)) paste0("Frente a la dificultad persistente, AUC ", rango(VE$AUC), if (circ) " (circular: solo acuerdo)" else "", ". ") else "",
        if (nrow(BC)) paste0("Frente a la baja comprensión uno o dos cursos después (criterio no circular), AUC ", rango(BC$AUC), ".") else "")
    f2 <- file.path(rl, "figuras", paste0("Ficha_auto_", TAREA, "_F2_validez.png")); if (file.exists(f2)) out("![Validez: AUC con intervalo de confianza del 95 %.](../../figuras/Ficha_auto_", TAREA, "_F2_validez.png){width=85%}") }
  # ---- baremo ----
  out("## Baremo provisional")
  if (nrow(BA)) {
    print(tabla(BA[, c("mom", "N", "cohortes", "P5", "P10", "P16", "P25", "P50", "P75", "P90")], c("Momento", "N", "Cohortes", "P5", "P10", "P16", "P25", "P50", "P75", "P90"), digitos = 1)); cat("\n\n")
    if (fam == "B") out("En la denominación rápida, menos tiempo es mejor: el P10 es el tiempo del 10 % más lento.")
    f1 <- file.path(rl, "figuras", paste0("Ficha_auto_", TAREA, "_F1_baremo.png")); if (file.exists(f1)) out("![Baremo provisional (centros sin Preleo).](../../figuras/Ficha_auto_", TAREA, "_F1_baremo.png){width=85%}")
  } else out("No hay momentos con al menos 100 niños de referencia.")
  # ---- efectos ----
  if (nrow(EF)) { out("## Variables que la afectan")
    print(tabla(EF[, c("variable", "mom", "n", "efecto", "p")], c("Variable", "Momento", "n", "Efecto", "p"), digitos = 3)); cat("\n\n") }
  # ---- publicaciones ----
  out("## Para publicaciones")
  extra_es <- paste0(if (nrow(ES)) paste0(" La estabilidad entre momentos fue ρ = ", rango(ES$rho), ".") else "", if (nrow(BC)) paste0(" Frente a la baja comprensión posterior, AUC = ", rango(BC$AUC), ".") else "")
  extra_en <- paste0(if (nrow(ES)) paste0(" Stability across time points was ρ = ", rango(ES$rho), ".") else "", if (nrow(BC)) paste0(" Against later poor reading comprehension, AUC = ", rango(BC$AUC), ".") else "")
  out("**Español.** ", ct$texto_es, extra_es)
  out("**English.** ", ct$texto_en, extra_en)
  # ---- pendientes ----
  out("## Pendientes e incidencias")
  if (!is.na(ct$aviso) && ct$aviso != "") avisos <- c(avisos, ct$aviso)
  if (circ) avisos <- c(avisos, "La etiqueta de dificultad persistente usa esta tarea: la validez frente a ella es circular.")
  avisos <- c(avisos, "Escribir el marco teórico y la interpretación; revisar con el equipo.")
  cat(paste0("- ", avisos, collapse = "\n"), "\n\n")
  out("<details><summary>Tablas completas <code>datos/agregados/AUTO_", TAREA, "/</code></summary>")
  tablas_completas(paste0("AUTO_", TAREA)); cat("\n\n</details>\n")
}
