# Figuras de la ficha RAN, dibujadas desde las tablas agregadas (datos/agregados/RAN).
# Son las mismas que genera analisis (scripts/R/ficha_RAN.R en el PC), pero aquí se
# reconstruyen a partir de los CSV publicados: cualquiera puede reproducirlas.

COLV <- c("Números" = PAL[["anil"]], "Colores" = PAL[["coral"]], "Objetos" = PAL[["ocre"]], "Compuesto RAN de L2" = PAL[["gris"]])

fig_ran_validez <- function() {
  pv <- leer("RAN", "4b_Validez_predictiva")
  pv$x <- paste0(pv$mom, "\n(", pv$cohortes, ")")
  cm <- leer("RAN", "4c_Numeros_vs_colores")
  cm <- cm[!is.na(cm$AUC_compuesto_RAN_certeza_v2), ]
  cmp <- data.frame(variante = "Compuesto RAN de L2", x = paste0(cm$mom, "\n(", cm$cohorte, ")"),
                    AUC = cm$AUC_compuesto_RAN_certeza_v2, IC_inf = NA, IC_sup = NA)
  fa <- rbind(pv[, c("variante", "x", "AUC", "IC_inf", "IC_sup")], cmp)
  fa$variante <- factor(fa$variante, names(COLV))
  ggplot(fa, aes(x, AUC, colour = variante)) +
    geom_hline(yintercept = .5, colour = PAL[["gris"]], linetype = "dotted") +
    geom_pointrange(data = fa[fa$variante != "Compuesto RAN de L2", ], aes(ymin = IC_inf, ymax = IC_sup), position = position_dodge(.55), size = .4, na.rm = TRUE) +
    geom_point(data = fa[fa$variante == "Compuesto RAN de L2", ], position = position_dodge(.55), shape = 4, size = 3, stroke = 1.2) +
    scale_colour_manual(values = COLV, drop = TRUE) + coord_cartesian(ylim = c(.45, .85)) +
    labs(x = "Momento (cohortes)", y = "AUC (IC 95 %)") + tema_leeduca()
}

fig_ran_baremos <- function() {
  br <- leer("RAN", "5_Baremo_SIN")
  br$variante <- factor(br$variante, names(COLV))
  d <- position_dodge(.6)
  ggplot(br, aes(mom, P50, colour = variante, group = variante)) +
    geom_linerange(aes(ymin = P10, ymax = P90), linewidth = 3, alpha = .25, position = d) +
    geom_linerange(aes(ymin = P25, ymax = P75), linewidth = 3, alpha = .6, position = d) +
    geom_point(size = 2.2, position = d, colour = PAL[["tinta"]]) +
    geom_point(aes(y = P16), shape = 95, size = 7, position = d, colour = PAL[["coral"]]) +
    scale_colour_manual(values = COLV, drop = TRUE) +
    labs(x = "Momento (edad.trimestre)", y = "Ítems correctos por minuto") + tema_leeduca()
}
