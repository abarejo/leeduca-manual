# infografia_dos_ciclos.py (Claude, 02/11/2026) · Infografía 3: los dos ciclos de la lectura (M26)
# Primero la exactitud, después la automatización, y en qué ventana divergen los niños con dificultad grave y leve.
# Lee solo agregados de M26 (m26_articulo_v6.R): datos/agregados/M26/v6/R2.csv (dominio de la exactitud) y R3.csv
# (efecto de longitud). Paleta del grupo. Uso: python analisis/infografia_dos_ciclos.py → figuras/infografias/dos_ciclos.{svg,png}
import os
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch
from matplotlib import font_manager as fm

AQUI = os.path.dirname(os.path.abspath(__file__)); RAIZ = os.path.dirname(AQUI)
D = os.path.join(RAIZ, "datos", "agregados", "M26", "v6"); OUT = os.path.join(RAIZ, "figuras", "infografias")
os.makedirs(OUT, exist_ok=True)
for f in fm.findSystemFonts():
    if "Inter" in f: fm.fontManager.addfont(f)
plt.rcParams["font.family"] = "Inter" if any("Inter" in f.name for f in fm.fontManager.ttflist) else "DejaVu Sans"
AZUL, VERDE, VERDE_OSC, NARANJA, AMARILLO, CREMA, TINTA, GRIS = "#2689B7", "#ABBB10", "#7C8710", "#E57813", "#F1B115", "#F5F4EF", "#22223B", "#888888"
G = {"Típico": ("Lectura típica", AZUL, "o", "-"), "Leve": ("Dificultad leve", AMARILLO, "s", "--"), "Grave": ("Dificultad grave", NARANJA, "^", "-")}

R2 = pd.read_csv(os.path.join(D, "R2.csv"), encoding="utf-8-sig"); R3 = pd.read_csv(os.path.join(D, "R3.csv"), encoding="utf-8-sig")
MOMS = ["5.2", "5.3", "6.1", "6.2", "6.3", "7.1", "7.2", "7.3", "8.1", "8.3"]; X = {m: i for i, m in enumerate(MOMS)}
R2["m"] = R2["mom"].map(lambda v: f"{float(v):.1f}"); R3["m"] = R3["mom"].map(lambda v: f"{float(v):.1f}")

fig = plt.figure(figsize=(13, 8.2), dpi=150); fig.patch.set_facecolor("white")
T = fig.add_axes([0, 0, 1, 1]); T.set_xlim(0, 130); T.set_ylim(0, 82); T.axis("off")
T.text(3, 78.5, "Aprender a leer en español: dos ciclos", fontsize=20, fontweight="bold", color=AZUL, va="center")
T.text(3, 74.6, "Primero se domina la exactitud; después, la lectura se automatiza. Las dificultades graves y leves se separan en ventanas distintas.",
       fontsize=10.8, color=TINTA, va="center")

# --- banda de los dos ciclos sobre el eje de momentos ---
ax0, ax1 = 0.075, 0.62                     # posición horizontal de los paneles de datos (en fracción de figura)
def xfig(i): return (ax0 + (i + 0.5) / len(MOMS) * (ax1 - ax0)) * 130
def banda(i0, i1, y, col, txt):
    xa, xb = xfig(i0) - 3.2, xfig(i1) + 3.2
    T.add_patch(FancyBboxPatch((xa, y - 1.9), xb - xa, 3.8, boxstyle="round,pad=0,rounding_size=1.9", facecolor=col, edgecolor=col))
    T.add_patch(plt.Polygon([[xb - 0.6, y - 2.9], [xb + 2.6, y], [xb - 0.6, y + 2.9]], closed=True, facecolor=col, edgecolor=col))
    T.text(xa + 1.6, y, txt, fontsize=9.8, fontweight="bold", color="white", va="center")
banda(0, 7, 67.5, AZUL, "CICLO 1 · EXACTITUD: leer bien")
banda(4, 9, 62.3, VERDE_OSC, "CICLO 2 · AUTOMATIZACIÓN: leer deprisa y sin esfuerzo")

# --- panel A: dominio de la exactitud ---
A = fig.add_axes([ax0, 0.36, ax1 - ax0, 0.33])
for g in ["Típico", "Leve", "Grave"]:
    s = R2[(R2.g8 == g) & R2.m.isin(MOMS)]; lab, col, mk, ls = G[g]; xs = s.m.map(X)
    A.fill_between(xs, s.ic_inf, s.ic_sup, color=col, alpha=.15, lw=0)
    A.plot(xs, s.pct_domina, color=col, marker=mk, ls=ls, lw=2.2, ms=5.5, label=lab)
A.axhline(50, color=GRIS, lw=.7, ls=":")
for g, m in [("Típico", "6.2"), ("Leve", "7.1"), ("Grave", "7.3")]:
    col = G[g][1]; A.plot([X[m]], [50], marker="|", ms=14, mew=2.2, color=col)
    A.text(X[m] + 0.12, 51.5, f"la mitad, a {m}", fontsize=8.3, color=col if g != "Leve" else "#B07A10", fontweight="bold", ha="left", va="bottom")
A.set_xlim(-0.5, len(MOMS) - 0.5); A.set_ylim(0, 102); A.set_xticks(range(len(MOMS))); A.set_xticklabels([])
A.set_ylabel("Dominan la exactitud (%)", fontsize=9, color=TINTA)
A.set_title("A · ¿Cuándo leen ya con exactitud?", loc="left", fontsize=10.5, fontweight="bold", color=TINTA)
A.legend(loc="upper left", frameon=False, fontsize=8.5)
for sp in ("top", "right"): A.spines[sp].set_visible(False)
A.tick_params(colors=GRIS, labelsize=8)

# --- panel B: efecto de longitud ---
B = fig.add_axes([ax0, 0.08, ax1 - ax0, 0.22])
s0 = R3[R3.marcador.str.startswith("Longitud")]
for g in ["Típico", "Leve", "Grave"]:
    s = s0[s0.g8 == g]; lab, col, mk, ls = G[g]
    B.plot(s.m.map(X), s.cociente, color=col, marker=mk, ls=ls, lw=2.2, ms=5)
B.axhline(1, color=GRIS, lw=.7, ls=":")
B.set_xlim(-0.5, len(MOMS) - 0.5); B.set_xticks(range(len(MOMS))); B.set_xticklabels(MOMS, fontsize=8.5)
B.set_xlabel("Momento de evaluación (edad.trimestre)", fontsize=9, color=TINTA); B.set_ylabel("Palabras largas / cortas", fontsize=9, color=TINTA)
B.set_title("B · ¿Cuánto cuesta una palabra larga? (efecto de longitud)", loc="left", fontsize=10.5, fontweight="bold", color=TINTA)
for sp in ("top", "right"): B.spines[sp].set_visible(False)
B.tick_params(colors=GRIS, labelsize=8)
B.text(8.0, 1.035, "típicos: se automatiza", fontsize=8, color=AZUL, ha="center", va="center", fontweight="bold")
B.text(8.15, 1.56, "graves: siguen leyendo por partes", fontsize=8, color=NARANJA, ha="center", va="center", fontweight="bold")
B.set_ylim(0.98, 1.62)

# --- tarjetas de mensajes ---
def tarjeta(y, h, titulo, cuerpo, col):
    T.add_patch(FancyBboxPatch((85, y), 42, h, boxstyle="round,pad=0,rounding_size=1.5", facecolor=CREMA, edgecolor=col, lw=1.8))
    T.add_patch(FancyBboxPatch((85, y), 1.6, h, boxstyle="square,pad=0", facecolor=col, edgecolor=col))
    T.text(88.5, y + h - 2.6, titulo, fontsize=10.5, fontweight="bold", color=col, va="center")
    T.text(88.5, y + h - 5.2, cuerpo, fontsize=8.8, color=TINTA, va="top", linespacing=1.3)
tarjeta(52, 14.5, "1 · La exactitud llega en 1.º", "La mitad de los lectores típicos ya lee\npalabras y pseudopalabras sin errores a\nmitad de 1.º; casi todos, al acabar 2.º.", AZUL)
tarjeta(34.5, 15.5, "2 · Grave y leve: dos ventanas", "Al final de 1.º y al empezar 2.º, lo que\nseñala una dificultad grave es la exactitud\n(AUC .66–.67). El lector leve ya es exacto,\npero lento.", NARANJA)
tarjeta(17, 15.5, "3 · Después manda la velocidad", "Desde 2.º, la velocidad es la mejor señal\nde cualquier dificultad (AUC .84 a 7.1). En\nlos graves, el coste de las palabras largas\ncrece en lugar de bajar.", VERDE_OSC)
T.text(85, 12.5, "Qué hacer: medir la velocidad en cada evaluación\ndesde mitad de 1.º y mirar la exactitud al final de 1.º.",
       fontsize=9, color=TINTA, fontweight="bold", va="top", linespacing=1.3)
T.text(127, 2.2, "Fuente: M26 (idea de J. L. Luque). 1.777 niños de las cohortes 2010–2012; grupos definidos con la lectura a los 8 años o más.",
       fontsize=7.2, color=GRIS, ha="right")
for ext in ("svg", "png"):
    fig.savefig(os.path.join(OUT, f"dos_ciclos.{ext}"), dpi=200 if ext == "png" else None, facecolor="white")
print("OK")
