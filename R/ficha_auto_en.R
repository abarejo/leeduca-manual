# English version of ficha_auto() (27/11/2026). Same data and structure as R/ficha_auto.R; only the text changes.
# Use in fichas/auto/<TASK>.en.qmd:  ficha_auto_en("LEC_PAL_2sil")
# The theoretical framework is read from fichas/marcos/<id>.en.md (if it does not exist, the section is omitted).
URL_ES <- "https://abarejo.github.io/leeduca-manual/"
FAM_NOM_EN <- c(A = "A · Accuracy", B = "B · Rapid naming", C = "C · Oral reading fluency", D = "D · Time-limited efficiency",
                E = "E · Decision with response time", F = "F · Span and memory", G = "G · Comprehension")
DOM_EN <- c("Lectura de palabras y texto" = "Word and text reading", "Conciencia fonológica" = "Phonological awareness", "Memoria verbal" = "Verbal memory",
            "RAN (velocidad)" = "RAN (speed)", "Conocimiento de letras" = "Letter knowledge", "RAN colores (misma sesión)" = "RAN colors (same session)",
            "Denominación rápida (RAN)" = "Rapid naming (RAN)", "Morfosintaxis oral" = "Oral morphosyntax", "Lectura de sílabas y palabras" = "Syllable and word reading",
            "Comprensión lectora" = "Reading comprehension", "Vocabulario y comprensión oral" = "Vocabulary and oral comprehension", "Vocabulario" = "Vocabulary")
VAR_EN <- c("Colegio (CCI)" = "School (ICC)", "Edad relativa (ρ con la edad en meses)" = "Relative age (ρ with age in months)",
            "ISE del centro (ρ)" = "School SES (ρ)", "Sexo (d niña − niño; codificación en revisión)" = "Sex (d girl − boy; coding under review)")
PUNT_EN <- c("Aciertos" = "Correct responses", "Aciertos por minuto" = "Correct responses per minute",
             "Tiempo (s; menos es mejor)" = "Time (s; lower is better)", "Aciertos − errores/3" = "Correct − errors/3")
tr <- function(x, map) ifelse(x %in% names(map), map[as.character(x)], x)
and_en <- function(v) if (length(v) < 2) paste(v) else paste(paste(head(v, -1), collapse = ", "), "and", tail(v, 1))

ficha_auto_en <- function(TAREA) {
  rl <- raiz_libro(); dir <- file.path(rl, "datos", "agregados", paste0("AUTO_", TAREA))
  C <- utils::read.csv(file.path(rl, "datos", "agregados", "CATALOGO", "Catalogo.csv"), encoding = "UTF-8", check.names = FALSE, stringsAsFactors = FALSE)
  ct <- C[C$tarea_canonica == TAREA, ]
  lee <- function(h) if (file.exists(file.path(dir, paste0(h, ".csv")))) leer(paste0("AUTO_", TAREA), h) else data.frame()
  FI <- lee("0_Ficha"); MU <- lee("1_Muestra"); DS <- lee("2_Descriptivos_SIN"); ES <- lee("3_Fiabilidad_estabilidad"); VC <- lee("4a_Validez_concurrente")
  VE <- lee("4b_Validez_frente_etiqueta"); BC <- lee("4c_Baja_comprension_posterior"); BA <- lee("5_Baremo_SIN"); EF <- lee("6_Efectos_variables"); EQ <- lee("5b_Equivalencia_formas")
  for (nm in c("MU", "DS", "BA")) { d <- get(nm); if ("forma" %in% names(d)) { d$forma <- as.character(d$forma); assign(nm, d) } }
  if (nrow(EQ)) { EQ$forma_1 <- as.character(EQ$forma_1); EQ$forma_2 <- as.character(EQ$forma_2) }
  ff <- "forma" %in% names(DS) && length(unique(DS$forma)) > 1
  etq <- function(D) if (ff && "forma" %in% names(D)) paste0(D$mom, " (form ", D$forma, ")") else D$mom
  fam <- ct$familia; circ <- isTRUE(as.logical(FI$circular_con_etiqueta))
  out <- function(...) cat(..., "\n\n", sep = "")
  nombre <- if (!is.na(ct$name_en) && nzchar(ct$name_en)) ct$name_en else ct$tarea
  fm <- if (!is.na(MARCO_DE[TAREA])) file.path(rl, "fichas", "marcos", paste0(MARCO_DE[[TAREA]], ".en.md")) else ""

  out('::: {.callout-note appearance="minimal"}\n**Automatically generated task sheet** (', FAM_NOM_EN[[fam]], '; quality class ', ct$clase, '). Generated on ', format(Sys.Date(), "%B %d, %Y"),
      ' with `scripts/R/ficha_generica_v3.R --tarea ', TAREA, '`. Figures come from the data; ',
      if (nzchar(fm) && file.exists(fm)) "the theoretical framework is shared by the tasks of the same construct. **Interpretation and team review are missing.**" else "**the theoretical framework, interpretation and team review are missing**.",
      ' Spanish name: *', ct$tarea, '*. [Spanish version](', URL_ES, 'fichas/auto/', TAREA, '.html).\n:::')
  if (ct$clase %in% c("B", "C")) out('::: {.callout-warning appearance="simple"}\n**Class ', ct$clase, '.** ',
      if (ct$clase == "B") "It has enough data to be described, but it lacks some of the guarantees of class A (stability across time points, item-level reliability or validity). Use with caution and not for individual decisions."
      else "It was given to few children, at few time points or in different versions. The task sheet documents the task and its first data; figures are descriptive and cannot yet be used as norms.",
      ' [What each class means](../../limites.en.qmd#sec-clases).\n:::')
  # ---- in one sentence ----
  est <- if (nrow(ES)) median(ES$rho) else NA
  auc_txt <- if (nrow(VE)) paste0("AUC ", rango(VE$AUC), if (circ) " against persistent difficulty (circular: the label uses this task)" else " against persistent reading difficulty") else NULL
  ed <- as.numeric(strsplit(as.character(ct$edades), ", ")[[1]]); ed_txt <- if (length(ed) > 1) paste0("from age ", min(ed), " to ", max(ed)) else paste0("at age ", ed)
  out("**In one sentence.** ", nombre, ", ", ed_txt, " (", num_es(sum(MU$N), 0), " administrations in ", length(unique(MU$cohorte)), " cohorts", if (ff) paste0("; ", length(unique(DS$forma)), " forms") else "", "). ",
      if (is.finite(est)) paste0("Stability across time points ρ = ", r2(est), ". ") else "", if (!is.null(auc_txt)) paste0(auc_txt, ".") else "")
  # ---- description ----
  out("## Description")
  PL <- plataforma_actividades(); PL <- if (nrow(PL)) PL[PL$tarea_canonica == TAREA, ] else PL
  items_txt <- if (nrow(PL)) paste0(paste0(ifelse(is.na(PL$n_items) | PL$n_items == "", "—", PL$n_items), " (", PL$codigo, ")", collapse = "; "), " according to the platform") else ifelse(is.na(ct$n_items), "—", ct$n_items)
  cods0 <- strsplit(as.character(ct$codigos), " ")[[1]]
  pant <- if (any(file.exists(file.path(rl, "figuras", "plataforma", paste0(cods0, ".jpg"))))) "" else if (!is.na(ct$formato) && ct$formato %in% names(PANT)) sprintf('\n\n![Example of the screen format (2018 screenshot; not necessarily an item from this task).](../../figuras/pantallas/%s){width=45%%}', PANT[[ct$formato]]) else ""
  desc <- if (!is.na(ct$descripcion_en) && nzchar(ct$descripcion_en)) ct$descripcion_en else if (!is.na(ct$descripcion)) paste0(ct$descripcion, " *(Spanish)*") else "—"
  fmt <- if (!is.na(ct$formato_en) && nzchar(ct$formato_en)) ct$formato_en else ifelse(is.na(ct$formato), "—", ct$formato)
  out("| | |\n|---|---|\n| Codes | ", ct$codigos, " |\n| Battery | ", ifelse(is.na(ct$bateria) | ct$bateria == "", "—", sub(" y ", " and ", ct$bateria)), " |\n| Format | ", fmt,
      " |\n| Items | ", items_txt, " |\n| Task family | ", FAM_NOM_EN[[fam]], " |\n| Score | ", tr(FI$puntuacion, PUNT_EN), " |\n| Instructions (platform) | ", desc, " |", pant)
  # ---- platform configuration ----
  cods <- strsplit(as.character(ct$codigos), " ")[[1]]
  tiene_pl <- nrow(plataforma_actividades()) && any(plataforma_actividades()$tarea_canonica == TAREA)
  tiene_cap <- any(file.exists(file.path(rl, "figuras", "plataforma", paste0(cods, ".jpg"))))
  if (tiene_pl || tiene_cap) { out("## How it is programmed on the platform"); if (tiene_pl) plataforma_tabla(tarea = TAREA); plataforma_capturas(cods, pre = "../../") }
  # ---- theoretical framework ----
  if (nzchar(fm) && file.exists(fm)) { out("## Theoretical framework"); out(paste(readLines(fm, encoding = "UTF-8", warn = FALSE), collapse = "\n")) }
  # ---- sample and descriptives ----
  out("## Sample and descriptive statistics")
  if (ff && "forma" %in% names(MU)) print(tabla(MU[, c("mom", "forma", "cohorte", "sesion", "curso_escolar", "N", "SIN", "CON", "colegios")], c("Time point", "Form", "Cohort", "Session", "School year", "N", "Non-Preleo", "Preleo", "Schools"), digitos = 0)) else
  print(tabla(MU[, c("mom", "cohorte", "sesion", "curso_escolar", "N", "SIN", "CON", "colegios")], c("Time point", "Cohort", "Session", "School year", "N", "Non-Preleo", "Preleo", "Schools"), digitos = 0))
  cat("\n\n")
  if (nrow(DS)) { cs <- c(if (ff) "forma", "mom", "N", "cohortes", "M", "DT", "Md", "asimetria", "pct_suelo", "pct_techo")
                  print(tabla(DS[, cs], c(if (ff) "Form", "Time point", "N", "Cohorts", "Mean", "SD", "Median", "Skewness", "% floor", "% ceiling"), digitos = 2)); cat("\n\n") }
  avisos <- character()
  if (nrow(DS) && "minimo" %in% names(DS)) {
    pos <- if (fam %in% c("A", "F", "G") && !is.na(ct$n_items)) paste0("0–", ct$n_items) else NULL
    uni <- c(A = "correct responses", B = "seconds", C = "correct items per minute", D = "correct − errors/3", E = "correct responses", F = "correct responses", G = "correct responses")[[fam]]
    out("**Score range** (non-Preleo group, in ", uni, "). The usual range goes from P1 to P99 (98% of the children). The minimum–maximum is the one observed after cleaning",
        if (!is.null(pos)) paste0("; the possible range is ", pos, " correct responses") else "", ".")
    print(tabla_rango(DS, grupo = if (ff) "forma" else NULL, posible = if (!is.null(pos)) rep(pos, sum(is.finite(DS$minimo))) else NULL)); cat("\n\n")
    if (any(extremo(DS) %in% TRUE)) avisos <- c(avisos, sprintf("Extreme value at %s: the maximum or minimum is more than 2 SD away from P99 or P1. Check in the raw data (possible timing or recording error).",
                                                    paste(etq(DS)[extremo(DS) %in% TRUE], collapse = ", ")))
  }
  if (nrow(DS) && any(DS$pct_techo > 15, na.rm = TRUE)) avisos <- c(avisos, sprintf("Ceiling effect at %s (more than 15%% of children with the maximum score).", paste(etq(DS)[DS$pct_techo > 15 & !is.na(DS$pct_techo)], collapse = ", ")))
  if (nrow(DS) && any(DS$pct_suelo > 15, na.rm = TRUE)) avisos <- c(avisos, sprintf("Floor effect at %s (more than 15%% with zero correct).", paste(etq(DS)[DS$pct_suelo > 15 & !is.na(DS$pct_suelo)], collapse = ", ")))
  suelo10 <- suelo25 <- character()
  if (nrow(BA) && nrow(DS) && "minimo" %in% names(DS) && fam != "B") {
    k <- if (ff && "forma" %in% names(BA)) c("mom", "forma") else "mom"
    mm <- merge(BA[, c(k, "P10", "P25")], DS[, c(k, "minimo")], by = k)
    suelo10 <- etq(mm)[is.finite(mm$P10) & is.finite(mm$minimo) & mm$P10 <= mm$minimo]
    suelo25 <- etq(mm)[is.finite(mm$P25) & is.finite(mm$minimo) & mm$P25 <= mm$minimo]
    if (length(suelo10)) avisos <- c(avisos, sprintf("Floor effect in the norms at %s: at least 10%% of children have the worst possible score, so P10 does not exist. At those time points the \"severe\" and \"very severe\" levels must not be assigned with this task (study M3).", and_en(suelo10)))
  }
  if (ff) { nd <- if (nrow(EQ)) sum(EQ$veredicto == "escalas distintas") else NA
    avisos <- c(avisos, if (is.na(nd)) "The task has several forms that never coincide at the same time point: their equivalence cannot be checked; norms are given by form." else
      sprintf("The task has %d forms; in %d of %d comparisons they have different scales. Norms are given by form (see \"Forms\").", length(unique(DS$forma)), nd, nrow(EQ)))
  } else if (length(unique(unlist(strsplit(ct$codigos, " ")))) > 1) avisos <- c(avisos, "The task has several codes (versions or forms): check that time points are comparable before pooling norms.")
  # ---- reliability ----
  out("## Reliability")
  if (nrow(ES)) {
    out("Stability across time points in the same children: ρ from ", rango(ES$rho), " (median ", r2(median(ES$rho)), ", ", nrow(ES), " pairs of time points). ",
        if (!is.na(ct$alfa_2021)) paste0("Internal consistency in the 2021 analyses: α = ", r2(ct$alfa_2021), ". ") else "Internal consistency from item-level responses is missing. ",
        if (fam %in% c("B", "C", "D")) "As a timed task, stability is the preferred reliability estimate." else "")
    print(tabla(ES[, c("cohorte", "desde", "hasta", "n", "r", "rho")], c("Cohort", "From", "To", "n", "r", "ρ"), digitos = 2)); cat("\n\n")
  } else out("There are no two time points with the same children (at least 80). ", if (!is.na(ct$alfa_2021)) paste0("α (2021) = ", r2(ct$alfa_2021), ".") else "")
  # ---- validity ----
  out("## Validity")
  propio <- if (startsWith(TAREA, "RAN_")) "Denominación rápida (RAN)" else if (startsWith(TAREA, "CF_")) "Conciencia fonológica" else if (TAREA == "LEC_LETRAS") "Conocimiento de letras" else
    if (grepl("^LEC_(PAL|PSEUDO|SILABAS|VEL|TECLE|LECT)", TAREA)) "Lectura de palabras y texto" else if (startsWith(TAREA, "HFON_MEMVERB")) "Memoria verbal" else ""
  if (nrow(VC)) {
    pr <- VC[VC$dominio == propio, ]; ot <- VC[VC$dominio != propio, ]; ot <- ot[!duplicated(ot$mom), ]; ot <- ot[order(as.numeric(ot$mom)), ]
    out("**Concurrent.** ", if (nrow(pr)) paste0("With other tasks in its own domain (", tr(propio, DOM_EN), "), r = ", rango(pr$r), ". ") else "",
        if (nrow(ot)) paste0("Outside its domain, the highest relation is with: ", paste(sprintf("%s (%s, r = %s)", tr(ot$dominio, DOM_EN), ot$mom, r2(ot$r)), collapse = "; "), ".") else "") }
  if (nrow(VE) || nrow(BC)) {
    out("**Against reading difficulty and comprehension.** ", if (nrow(VE)) paste0("Against persistent difficulty, AUC ", rango(VE$AUC), if (circ) " (circular: agreement only)" else "", ". ") else "",
        if (nrow(BC)) paste0("Against poor comprehension one or two school years later (non-circular criterion), AUC ", rango(BC$AUC), ".") else "")
    f2 <- file.path(rl, "figuras", paste0("Ficha_auto_", TAREA, "_F2_validez.png")); if (file.exists(f2)) out("![Validity: AUC with 95% confidence interval (labels in Spanish).](../../figuras/Ficha_auto_", TAREA, "_F2_validez.png){width=85%}") }
  WC <- criterio_ext("WOODCOCK", TAREA); MC <- if (startsWith(TAREA, "MATE_")) criterio_ext("MATE_CRITERIO", TAREA, "AUC_mate") else data.frame()
  if (nrow(WC) || nrow(MC)) {
    out("**Against an external criterion, years later.** ",
        if (nrow(WC)) paste0("Against low Woodcock-Muñoz reading fluency in 2021/22 ([chapter](", URL_ES, "criterio-woodcock.html), *in Spanish*), AUC ", rango(WC$auc),
                             " (", nrow(WC), if (nrow(WC) == 1) " time point" else " time points", "; ", num_es(min(WC$n), 0), "–", num_es(max(WC$n), 0), " children). ") else "",
        if (nrow(MC)) paste0("Against low arithmetic at ages 10–11 ([provisional criterion](", URL_ES, "cognicion-numerica.html#sec-mate-criterio), *in Spanish*), AUC ", rango(MC$auc), ".") else "") }
  # ---- norms ----
  out("## Provisional norms")
  if (nrow(BA)) {
    cb <- c(if (ff && "forma" %in% names(BA)) "forma", "mom", "N", "cohortes", "P5", "P10", "P16", "P25", "P50", "P75", "P90")
    print(tabla(BA[, cb], c(if (ff && "forma" %in% names(BA)) "Form", "Time point", "N", "Cohorts", "P5", "P10", "P16", "P25", "P50", "P75", "P90"), digitos = 1)); cat("\n\n")
    if (ff) out("Each form (platform code) has its own norms, because the forms need not measure on the same scale.")
    if (fam == "B") out("In rapid naming, less time is better: P10 is the time of the slowest 10%.")
    if (length(suelo10)) out("**Floor effect.** At ", and_en(suelo10), " P10 coincides with the worst possible score",
                             if (length(suelo25)) paste0(" (at ", and_en(suelo25), ", P25 too)") else "",
                             ": many children are tied at the minimum and the norms do not distinguish between them. At those time points this task cannot be used to grade difficulty.")
    out("These are norms by time point (school year × term). For dates between assessments and time points without a normative sample, study M3 proposes continuous norms on **months of instruction**, not on age, with relative age within the school year as an adjustment.")
    f1 <- file.path(rl, "figuras", paste0("Ficha_auto_", TAREA, "_F1_baremo.png")); if (file.exists(f1)) out("![Provisional norms (non-Preleo schools; labels in Spanish).](../../figuras/Ficha_auto_", TAREA, "_F1_baremo.png){width=85%}")
  } else out("There are no time points with at least 100 reference children.")
  if (ff && nrow(EQ)) {
    out("### Forms: can they be pooled?")
    out("Forms given at the same time point are compared. They are considered **equivalent** if the mean difference is smaller than 0.20 SD and the SD ratio is between 0.80 and 1.25. If the same children took them, **linear equating** is also computed, which puts the second form on the scale of the first, together with the correlation between forms (parallel-forms reliability). If the groups differ, the difference mixes form and cohort.")
    EQ$igualacion <- ifelse(is.finite(EQ$pendiente), sprintf("%s = %s × %s %s %s", EQ$forma_1, num_es(EQ$pendiente, 2), EQ$forma_2, ifelse(EQ$intercepto < 0, "−", "+"), num_es(abs(EQ$intercepto), 1)), "—")
    EQ$r <- ifelse(is.finite(EQ$r_entre_formas), r2(EQ$r_entre_formas), "—")
    EQ$grupos <- ifelse(EQ$mismos_ninos >= 100, "same children", "different groups")
    EQ$veredicto <- tr(EQ$veredicto, c("escalas distintas" = "different scales", equivalentes = "equivalent"))
    print(tabla(EQ[, c("mom", "forma_1", "forma_2", "grupos", "d_2_menos_1", "razon_DT", "veredicto", "r", "igualacion")],
                c("Time point", "Form 1", "Form 2", "Comparison", "d (2 − 1)", "SD ratio", "Verdict", "r between forms", "Equating"), digitos = 2)); cat("\n\n")
  }
  # ---- item properties ----
  PI <- lee("7_Propiedades_items")
  if (nrow(PI)) {
    out("## Item properties")
    out("Besides its length, each word in the list is described by its **age of acquisition** (the age at which it is learned orally; norms by Alonso et al., 2015) and by its **orthographic neighbors** (words spelled almost the same; EsPal). A word that is learned early or has many neighbors is read faster than its length would suggest. To protect the test, only summaries of the list are given here, not the words.")
    PI$aoa_rango <- paste0(num_es(PI$aoa_min, 1), "–", num_es(PI$aoa_max, 1))
    PI$cob <- paste(PI$items_con_aoa, "of", PI$items)
    PI$forma <- as.character(PI$forma)
    print(tabla(PI[, c("forma", "letras_media", "cob", "aoa_media", "aoa_rango", "pct_aoa_hasta_4", "vecinas_media", "pct_sin_vecinas")],
                c("Form", "Letters (mean)", "Items with norms", "Age of acquisition (mean, years)", "Range", "% learned by age 4", "Neighbors (mean)", "% without neighbors"), digitos = 1)); cat("\n\n")
    out("The other forms of the task will be described once their items have been verified.")
  }
  # ---- effects ----
  if (nrow(EF)) { out("## Variables that affect it")
    EF$variable <- tr(EF$variable, VAR_EN)
    print(tabla(EF[, c("variable", "mom", "n", "efecto", "p")], c("Variable", "Time point", "n", "Effect", "p"), digitos = 3)); cat("\n\n") }
  # ---- for publications ----
  out("## For publications")
  extra_en <- paste0(if (nrow(ES)) paste0(" Stability across time points was ρ = ", rango(ES$rho), ".") else "", if (nrow(BC)) paste0(" Against later poor reading comprehension, AUC = ", rango(BC$AUC), ".") else "")
  out(ct$texto_en, extra_en)
  # ---- pending ----
  out("## Pending issues")
  if (!is.na(ct$aviso) && ct$aviso != "") avisos <- c(avisos, paste0(ct$aviso, " *(Spanish)*"))
  if ("nota_puntuacion" %in% names(ct) && !is.na(ct$nota_puntuacion) && ct$nota_puntuacion != "") avisos <- c(avisos, paste0(ct$nota_puntuacion, " *(Spanish)*"))
  if ("correccion" %in% names(ct) && !is.na(ct$correccion) && ct$correccion != "") avisos <- c(avisos, paste0("**Dictionary correction.** ", ct$correccion, " *(Spanish)*"))
  if (circ) avisos <- c(avisos, "The persistent difficulty label uses this task: validity against it is circular.")
  avisos <- c(avisos, "Write the interpretation; review with the team.")
  cat(paste0("- ", avisos, collapse = "\n"), "\n\n")
  out("<details><summary>Full tables <code>datos/agregados/AUTO_", TAREA, "/</code></summary>")
  tablas_completas(paste0("AUTO_", TAREA)); cat("\n\n</details>\n")
}
