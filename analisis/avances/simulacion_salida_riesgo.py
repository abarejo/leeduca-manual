"""Salidas del riesgo por error de medida (capítulo «Del laboratorio al manual»).

Niños con nivel real fijo, evaluados dos veces con una prueba de fiabilidad r.
Se marca en riesgo a quien queda por debajo de P25 en la primera evaluación y se
cuenta cuántos superan P25 en la segunda. No usa datos de alumnos.
"""
import numpy as np

rng = np.random.default_rng(1)
n = 2_000_000
print("fiabilidad  sale_sin_cambiar  realmente_bajo_P25")
for r in (0.70, 0.80, 0.85, 0.90, 0.95):
    t = rng.standard_normal(n) * np.sqrt(r)
    x1 = t + rng.standard_normal(n) * np.sqrt(1 - r)
    x2 = t + rng.standard_normal(n) * np.sqrt(1 - r)
    corte = np.quantile(x1, 0.25)
    marcados = x1 < corte
    sale = (x2[marcados] >= corte).mean()
    real = (t[marcados] < np.quantile(t, 0.25)).mean()
    print(f"{r:.2f}        {sale:.3f}             {real:.3f}")


# Zona «a vigilar» (P20–P30) frente a un único corte en P25 (decisión del 08/11/2026)
print("\nfiabilidad  cambia_con_corte_P25  cambia_riesgo_sin_riesgo_con_zona  en_zona  aciertan_P25  aciertan_P20")
for r in (0.70, 0.85, 0.95):
    t = rng.standard_normal(n) * np.sqrt(r)
    x1 = t + rng.standard_normal(n) * np.sqrt(1 - r)
    x2 = t + rng.standard_normal(n) * np.sqrt(1 - r)
    c25, a, b = np.quantile(x1, [0.25, 0.20, 0.30])
    tramo = lambda x: np.where(x < a, 0, np.where(x > b, 2, 1))
    t1, t2 = tramo(x1), tramo(x2)
    cambia = ((x1 < c25) != (x2 < c25)).mean()
    cambia_zona = (((t1 == 0) & (t2 == 2)) | ((t1 == 2) & (t2 == 0))).mean()
    real = t < np.quantile(t, 0.25)
    print(f"{r:.2f}        {cambia:.3f}                 {cambia_zona:.3f}                              {(t1 == 1).mean():.3f}    {real[x1 < c25].mean():.3f}         {real[x1 < a].mean():.3f}")
