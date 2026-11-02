# infografia_construccion.py (Claude, 02/11/2026) · Infografía 1: cómo se construye el manual LEEDUCA
# De la plataforma a los informes, con los capítulos del manual y los estudios M que alimentan cada paso.
# No usa datos. Paleta del grupo. Uso: python analisis/infografia_construccion.py → figuras/infografias/construccion.{svg,png}
import os
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, Polygon
from matplotlib import font_manager as fm

AQUI = os.path.dirname(os.path.abspath(__file__)); RAIZ = os.path.dirname(AQUI)
OUT = os.path.join(RAIZ, "figuras", "infografias"); os.makedirs(OUT, exist_ok=True)
for f in fm.findSystemFonts():
    if "Inter" in f: fm.fontManager.addfont(f)
plt.rcParams["font.family"] = "Inter" if any("Inter" in f.name for f in fm.fontManager.ttflist) else "DejaVu Sans"
AZUL, VERDE, VERDE_OSC, NARANJA, AMARILLO, CREMA, TINTA, GRIS = "#2689B7", "#ABBB10", "#7C8710", "#E57813", "#F1B115", "#F5F4EF", "#22223B", "#888888"

PASOS = [
    ("1", "Evaluar", "Plataforma LEEDUCA:\ntres sesiones por curso\nde 4 a 11 años", "Cap. 3", "M2 · M6 · M13 · M15", AZUL),
    ("2", "Ordenar", "Brutos → tabla larga\núnica y diccionario\nmaestro de tareas", "Cap. 4", "M8 · M22", AZUL),
    ("3", "Depurar", "Incidencias, detectores\nde incongruencias y\ncondición Preleo", "Cap. 4 · 23", "M18 · detectores QC", NARANJA),
    ("4", "Medir bien", "Fiabilidad, validez,\nestructura y\ninvarianza", "Cap. 5 · 6", "M1 · M2 · M14 · M17", VERDE_OSC),
    ("5", "Comparar", "Baremos por momento,\ncambio relevante y\nlos dos ciclos lectores", "Cap. 7–18 · Anexo A", "M3 · M4+M7 · M11 · M26", VERDE_OSC),
    ("6", "Decidir y ayudar", "Certeza, cribado,\nseguimiento e\ninforme al centro", "Cap. 19–22 · Anexo F", "M5 · M10 · M19 · M25", AMARILLO),
]
fig = plt.figure(figsize=(13, 6.2), dpi=150); fig.patch.set_facecolor("white")
T = fig.add_axes([0, 0, 1, 1]); T.set_xlim(0, 130); T.set_ylim(0, 62); T.axis("off")
T.text(3, 58.3, "Cómo se construye el manual LEEDUCA", fontsize=20, fontweight="bold", color=AZUL, va="center")
T.text(3, 54.2, "Seis pasos de la evaluación en el aula a la decisión. Cada paso tiene su capítulo y los estudios de la Agenda que lo desarrollan.",
       fontsize=10.8, color=TINTA, va="center")
w, gap, x0, y0, h = 19.2, 1.9, 3, 20, 25
for k, (num, tit, txt, cap, est, col) in enumerate(PASOS):
    x = x0 + k * (w + gap)
    T.add_patch(FancyBboxPatch((x, y0), w, h, boxstyle="round,pad=0,rounding_size=1.6", facecolor=CREMA, edgecolor=col, lw=2))
    T.add_patch(FancyBboxPatch((x, y0 + h - 7.2), w, 7.2, boxstyle="round,pad=0,rounding_size=1.6", facecolor=col, edgecolor=col))
    T.add_patch(FancyBboxPatch((x, y0 + h - 7.2), w, 3.0, boxstyle="square,pad=0", facecolor=col, edgecolor=col))
    tc = TINTA if col == AMARILLO else "white"
    T.text(x + 1.6, y0 + h - 3.6, num, fontsize=17, fontweight="bold", color=tc, va="center")
    T.text(x + 5.0, y0 + h - 3.6, tit, fontsize=11.5, fontweight="bold", color=tc, va="center")
    T.text(x + w / 2, y0 + 11.3, txt, fontsize=9, color=TINTA, ha="center", va="center", linespacing=1.35)
    T.text(x + w / 2, y0 + 3.2, cap, fontsize=8.5, color=col if col != AMARILLO else "#B07A10", fontweight="bold", ha="center", va="center")
    # estudios debajo
    T.add_patch(FancyBboxPatch((x + 1, 11.2), w - 2, 5.2, boxstyle="round,pad=0,rounding_size=2.4", facecolor="white", edgecolor=GRIS, lw=0.9, ls=(0, (2, 1.5))))
    T.text(x + w / 2, 13.8, est, fontsize=8.6, color=TINTA, ha="center", va="center")
    if k < len(PASOS) - 1:
        xa = x + w + 0.25
        T.add_patch(Polygon([[xa, y0 + h / 2 + 2.2], [xa + gap - 0.5, y0 + h / 2], [xa, y0 + h / 2 - 2.2]], closed=True, facecolor=GRIS, edgecolor=GRIS))
T.text(3, 18.3, "Estudios de la Agenda", fontsize=8.5, color=GRIS, fontweight="bold", va="center")
# barra transversal
T.add_patch(FancyBboxPatch((3, 2.6), 124, 5.6, boxstyle="round,pad=0,rounding_size=2.8", facecolor=VERDE, edgecolor=VERDE))
T.text(65, 5.4, "En todos los pasos: datos agregados y seudonimizados · scripts reproducibles · decisiones registradas · referencia SIN Preleo",
       fontsize=9.6, color=TINTA, ha="center", va="center", fontweight="bold")
for ext in ("svg", "png"):
    fig.savefig(os.path.join(OUT, f"construccion.{ext}"), dpi=200 if ext == "png" else None, facecolor="white")
print("OK")
