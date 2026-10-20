# Diferencias entre el grupo Preleo y el grupo de comparación, reunidas desde las fichas
# (tablas agregadas de cada ficha; d de Cohen sin ajustar, dentro de cada cohorte).

preleo_diferencias <- function() {
  let <- leer("LET", "6a_Efectos_variables")
  let <- unique(let[grepl("^Preleo", let$variable), c("forma", "mom", "cohorte", "n", "efecto_d", "p")])
  let <- data.frame(tarea = paste("Letras", sub(" .*", "", let$forma)), mom = let$mom, cohorte = let$cohorte, n = let$n, d = let$efecto_d, p = let$p)

  ran <- leer("RAN", "6a_Efectos_variables")
  ran <- ran[grepl("^Preleo", ran$variable), ]
  ran <- data.frame(tarea = paste("RAN", tolower(ran$variante)), mom = ran$mom, cohorte = sub(".*cohorte ", "", ran$variable),
                    n = ran$n, d = ran$efecto_d, p = ran$p)

  vel <- leer("VEL_indicadores", "4a_Efectos_detalle")
  vel <- vel[grepl("^Preleo", vel$variable), ]
  vel <- data.frame(tarea = vel$medida, mom = vel$mom, cohorte = sub(".*cohorte ", "", vel$variable), n = vel$n, d = vel$d, p = vel$p)

  out <- rbind(let, ran, vel)
  out$cohorte <- as.character(out$cohorte)
  out$edad <- as.numeric(sub("\\..*", "", out$mom)) + (as.numeric(sub(".*\\.", "", out$mom)) - 1) / 3
  out[order(out$cohorte, out$tarea, out$edad), ]
}

fig_preleo_diferencias <- function(D = preleo_diferencias()) {
  D$grupo <- ifelse(grepl("^Letras|^RAN", D$tarea), "Precursores (letras, RAN)", "Lectura (velocidad en ppm)")
  D$Cohorte <- paste("Cohorte", D$cohorte)
  ggplot(D, aes(edad, d)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -.2, ymax = .2, fill = PAL[["rejilla"]], alpha = .6) +
    geom_hline(yintercept = 0, colour = PAL[["gris"]]) +
    geom_point(aes(colour = Cohorte, shape = grupo), size = 2.6, alpha = .85) +
    scale_colour_manual(values = c("Cohorte 2010" = PAL[["mar"]], "Cohorte 2013" = PAL[["ocre"]], "Cohorte 2014" = PAL[["coral"]])) +
    scale_shape_manual(values = c(16, 17)) +
    scale_x_continuous(breaks = 4:8, labels = paste(4:8, "años")) +
    labs(x = NULL, y = "d (Preleo − comparación)") + tema_leeduca() +
    theme(legend.box = "vertical")
}
