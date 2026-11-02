# figuras_ficha_auto_paleta.R (Claude, 02/11/2026) · Regenera en la paleta del grupo las dos figuras de cada ficha automática
#   figuras/Ficha_auto_<tarea>_F1_baremo.png   bandas P10–P90 y P25–P75, mediana y p16 (referencia: niños sin Preleo)
#   figuras/Ficha_auto_<tarea>_F2_validez.png  AUC (IC 95 %) frente a la dificultad persistente y a la baja comprensión posterior
# Lee solo los agregados del libro: datos/agregados/AUTO_<tarea>/0_Ficha.csv, 5_Baremo_SIN.csv, 4b_Validez_frente_etiqueta.csv
# y 4c_Baja_comprension_posterior.csv (salida de scripts/R/ficha_generica.R en el PC). Mismo contenido que las figuras
# originales; solo cambian los colores y el tema (PAL_GRUPO y tema_grupo de R/libro.R).
# Uso (desde la raíz del libro): Rscript analisis/figuras_ficha_auto_paleta.R
suppressPackageStartupMessages({ library(ggplot2); library(data.table) })
source(file.path("R", "libro.R"))
G <- PAL_GRUPO
tema <- tema_grupo(11) + theme(axis.line = element_line(colour = G[["gris"]], linewidth = .3),
                               plot.caption = element_text(colour = G[["tinta"]], size = 8.5, hjust = 1),
                               legend.position = "bottom", legend.title = element_blank())
guardar <- function(p, f) ggsave(f, p, width = 8.8, height = 4.4, dpi = 200, device = ragg::agg_png, bg = "white")

n1 <- n2 <- 0
for (d in list.dirs(file.path("datos", "agregados"), recursive = FALSE)) {
  if (!startsWith(basename(d), "AUTO_")) next
  t <- sub("^AUTO_", "", basename(d)); lee <- function(x) { f <- file.path(d, x); if (file.exists(f) && file.size(f) > 5) { x <- fread(f, encoding = "UTF-8"); if (nrow(x) && ncol(x) > 1) x else NULL } else NULL }
  fi <- lee("0_Ficha.csv"); if (is.null(fi)) next
  nom <- fi$tarea[1]; pun <- fi$puntuacion[1]
  # --- F1 baremo ---
  B <- lee("5_Baremo_SIN.csv")
  if (!is.null(B) && nrow(B)) {
    B[, mom := sprintf("%.1f", as.numeric(mom))]
    p <- ggplot(B, aes(mom)) +
      geom_linerange(aes(ymin = P10, ymax = P90), colour = G[["azul"]], linewidth = 9, alpha = .28) +
      geom_linerange(aes(ymin = P25, ymax = P75), colour = G[["azul"]], linewidth = 9, alpha = .85) +
      geom_errorbar(aes(ymin = P16, ymax = P16), colour = G[["naranja"]], width = .18, linewidth = 1.3) +
      geom_point(aes(y = P50), colour = G[["tinta"]], size = 3.6) +
      labs(title = paste("Baremo:", nom), x = "Momento (edad.trimestre)", y = pun,
           caption = "Bandas: P10–P90 y P25–P75; punto: mediana; raya naranja: p16. Referencia: niños sin Preleo.") + tema
    guardar(p, file.path("figuras", paste0("Ficha_auto_", t, "_F1_baremo.png"))); n1 <- n1 + 1
  }
  # --- F2 validez ---
  V1 <- lee("4b_Validez_frente_etiqueta.csv"); V2 <- lee("4c_Baja_comprension_posterior.csv")
  V <- rbind(if (!is.null(V1) && nrow(V1)) V1[, .(m = as.numeric(mom), x = sprintf("%.1f", as.numeric(mom)), AUC, IC_inf, IC_sup, c = "Dificultad persistente")],
             if (!is.null(V2) && nrow(V2)) V2[, .(m = as.numeric(desde) + as.numeric(sub(".* a los ([0-9]+) años.*", "\\1", criterio)) / 100,
                                                   x = paste0(sprintf("%.1f", as.numeric(desde)), " → ", sub(".* a los ([0-9]+) años.*", "\\1", criterio), " años"),
                                                   AUC, IC_inf, IC_sup, c = "Baja comprensión posterior")])
  if (!is.null(V) && nrow(V)) {
    V <- V[is.finite(AUC)]; V[, x := factor(x, levels = unique(x[order(m)]))]
    p <- ggplot(V, aes(x, AUC, colour = c, shape = c)) +
      geom_hline(yintercept = .5, linetype = "dotted", colour = G[["gris"]]) +
      geom_pointrange(aes(ymin = IC_inf, ymax = IC_sup), size = .6, linewidth = .9) +
      scale_colour_manual(values = c(`Dificultad persistente` = G[["naranja"]], `Baja comprensión posterior` = G[["azul"]])) +
      scale_shape_manual(values = c(`Dificultad persistente` = 17, `Baja comprensión posterior` = 16)) +
      scale_y_continuous(limits = c(min(.4, min(V$IC_inf, na.rm = TRUE)), 1)) +
      labs(title = paste("Validez:", nom), x = NULL, y = "AUC (IC 95 %)") + tema
    guardar(p, file.path("figuras", paste0("Ficha_auto_", t, "_F2_validez.png"))); n2 <- n2 + 1
  }
}
cat("F1:", n1, " F2:", n2, "\n")
