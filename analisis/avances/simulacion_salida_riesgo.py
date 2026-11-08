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
