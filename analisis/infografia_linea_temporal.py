# infografia_linea_temporal.py (Claude, 02/11/2026) · Infografía 2: línea temporal de la base LEEDUCA
# Cohortes (año de nacimiento) por curso escolar 2014/15–2021/22: seguimiento trimestral, evaluaciones amplias de
# 2020/21 (batería TECLE en papel) y 2021/22, confinamiento de marzo de 2020 y condición Preleo por cohorte.
# Lee solo datos agregados: datos/agregados/CAP4/N0.csv y N1.csv. Paleta del grupo (PAL_GRUPO en R/libro.R).
# Uso: python analisis/infografia_linea_temporal.py  →  figuras/infografias/linea_temporal.{svg,png}
import os
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, Circle
from matplotlib import font_manager as fm

AQUI = os.path.dirname(os.path.abspath(__file__)); RAIZ = os.path.dirname(AQUI)
D = os.path.join(RAIZ, "datos", "agregados", "CAP4"); OUT = os.path.join(RAIZ, "figuras", "infografias")
os.makedirs(OUT, exist_ok=True)
for f in fm.findSystemFonts():
    if "Inter" in f: fm.fontManager.addfont(f)
plt.rcParams["font.family"] = "Inter" if any("Inter" in f.name for f in fm.fontManager.ttflist) else "DejaVu Sans"

AZUL, VERDE, VERDE_OSC, NARANJA, AMARILLO, CREMA, TINTA, GRIS = "#2689B7", "#ABBB10", "#7C8710", "#E57813", "#F1B115", "#F5F4EF", "#22223B", "#888888"
N0 = pd.read_csv(os.path.join(D, "N0.csv"), encoding="utf-8-sig"); N1 = pd.read_csv(os.path.join(D, "N1.csv"), encoding="utf-8-sig")
N1 = N1[N1.ninos.notna()]
N1["ed"] = N1.mom.astype(float).apply(int); N1["tr"] = ((N1.mom.astype(float) - N1.ed) * 10).round().astype(int)
N1["curso"] = N1.cohorte + N1.ed

CURSOS = list(range(2015, 2022)); COH = [2010, 2011, 2012, 2013, 2014, 2015]
ETQ_ED = {3: "3 años", 4: "4 años", 5: "5 años", 6: "1.º", 7: "2.º", 8: "3.º", 9: "4.º", 10: "5.º", 11: "6.º"}
PRELEO = {2010: ("Preleo a los 5 años en 19 colegios;\n11 colegios sin programa", VERDE_OSC),
          2011: ("Toda la cohorte con Preleo", VERDE_OSC),
          2012: ("Sin programa (por confirmar)", GRIS),
          2013: ("Sin programa: grupo\nde comparación", GRIS),
          2014: ("Con Preleo, en los mismos\ncolegios que 2013", VERDE_OSC),
          2015: ("Con Preleo", VERDE_OSC)}

fig = plt.figure(figsize=(13, 7.4), dpi=150); ax = fig.add_axes([0, 0, 1, 1]); ax.set_xlim(0, 130); ax.set_ylim(0, 74); ax.axis("off")
fig.patch.set_facecolor("white")
ax.text(3, 70.5, "Siete cursos, seis cohortes y dos diseños", fontsize=19, fontweight="bold", color=AZUL, va="center")
ax.text(3, 66.6, "Cuándo se evaluó a cada cohorte de la base LEEDUCA y qué tipo de evaluación fue", fontsize=11, color=TINTA, va="center")

x0, cw, y0, rh = 21, 11.4, 56, 8.0           # origen de la rejilla, ancho de curso, y de la 1.ª fila, alto de fila
for j, c in enumerate(CURSOS):
    ax.text(x0 + j * cw + cw / 2, y0 + 3.6, f"{c}/{str(c + 1)[2:]}", ha="center", va="center", fontsize=9.5, color=TINTA, fontweight="bold")
# confinamiento: marzo de 2020 (entre el 2.º y el 3.er trimestre de 2019/20)
xc = x0 + CURSOS.index(2019) * cw + cw * 0.70
ax.plot([xc, xc], [y0 - 6 * rh + 1.0, y0 + 1.8], color=NARANJA, lw=1.4, ls=(0, (3, 2)))
ax.text(xc, y0 - 6 * rh - 0.6, "Confinamiento\n(marzo de 2020)", ha="center", va="top", fontsize=8.5, color=NARANJA)

for i, coh in enumerate(COH):
    y = y0 - (i + 1) * rh + 1.2
    n = int(N0.loc[N0.cohorte == coh, "ninos"].iloc[0])
    ax.text(3, y + rh / 2 - 0.9, f"Cohorte {coh}", fontsize=11.5, fontweight="bold", color=TINTA, va="center")
    ax.text(3, y + rh / 2 - 3.6, f"{n:,} niños".replace(",", "."), fontsize=9, color=GRIS, va="center")
    d = N1[N1.cohorte == coh]
    for j, c in enumerate(CURSOS):
        dd = d[d.curso == c]
        if dd.empty: continue
        ed = c - coh; x = x0 + j * cw + 0.5
        if c == 2020: fc, ec, txt = NARANJA, NARANJA, "TECLE\nen papel"
        elif c == 2021: fc, ec, txt = AMARILLO, AMARILLO, "Tabletas,\ntiempo límite"
        else: fc, ec, txt = AZUL, AZUL, None
        box = FancyBboxPatch((x, y), cw - 1.0, rh - 1.6, boxstyle="round,pad=0,rounding_size=1.1",
                             facecolor=fc if c >= 2020 else "white", edgecolor=ec, linewidth=1.6)
        ax.add_patch(box)
        col_t = "white" if c >= 2020 else TINTA
        ax.text(x + (cw - 1.0) / 2, y + rh - 3.4, ETQ_ED.get(ed, f"{ed} años"), ha="center", va="center", fontsize=9.5, fontweight="bold", color=col_t if c == 2020 else TINTA)
        if txt:
            ax.text(x + (cw - 1.0) / 2, y + 1.9, txt, ha="center", va="center", fontsize=7.2, color=col_t if c == 2020 else TINTA, linespacing=1.0)
        else:   # tres puntos = tres trimestres; lleno si hay datos
            for k in (1, 2, 3):
                hay = (dd.tr == k).any()
                ax.add_patch(Circle((x + (cw - 1.0) / 2 + (k - 2) * 2.3, y + 2.0), 0.75, facecolor=AZUL if hay else "white", edgecolor=AZUL, lw=1.1))
    ptxt, pcol = PRELEO[coh]
    ax.text(x0 + len(CURSOS) * cw + 1.0, y + rh / 2 - 0.8, ptxt, fontsize=8.6, color=pcol, va="center", linespacing=1.15,
            fontweight="bold" if pcol == VERDE_OSC else "normal")
ax.text(x0 + len(CURSOS) * cw + 1.0, y0 + 3.6, "Condición Preleo", fontsize=9.5, fontweight="bold", color=TINTA, va="center")

# leyenda
ly = 3.2
ax.add_patch(FancyBboxPatch((3, ly - 1.4), 5, 2.8, boxstyle="round,pad=0,rounding_size=0.8", facecolor="white", edgecolor=AZUL, lw=1.6))
for k in (0, 1, 2): ax.add_patch(Circle((4.3 + k * 1.2, ly), 0.42, facecolor=AZUL if k < 2 else "white", edgecolor=AZUL, lw=1))
ax.text(9.5, ly, "Seguimiento trimestral en la plataforma (punto lleno = trimestre con datos)", fontsize=8.8, color=TINTA, va="center")
ax.add_patch(FancyBboxPatch((66, ly - 1.4), 5, 2.8, boxstyle="round,pad=0,rounding_size=0.8", facecolor=NARANJA, edgecolor=NARANJA))
ax.text(72.5, ly, "Feb. 2021: TECLE en papel", fontsize=8.8, color=TINTA, va="center")
ax.add_patch(FancyBboxPatch((95, ly - 1.4), 5, 2.8, boxstyle="round,pad=0,rounding_size=0.8", facecolor=AMARILLO, edgecolor=AMARILLO))
ax.text(101.5, ly, "2021/22: plataforma en tabletas, tareas\ncon tiempo límite (comprensión, velocidad…)", fontsize=8.8, color=TINTA, va="center", linespacing=1.1)
ax.text(127, 66.6, "Fuente: tabla larga v2 (agregados del capítulo 4) y asignación Preleo v6 (condición solo en el seguimiento trimestral)", fontsize=7.5, color=GRIS, ha="right", va="center")
for ext in ("svg", "png"):
    fig.savefig(os.path.join(OUT, f"linea_temporal.{ext}"), dpi=200 if ext == "png" else None, facecolor="white")
print("OK", OUT)
