"""Ajusta los enlaces de los capítulos en inglés (*.en.qmd, fichas/marcos/*.en.md).
- Enlace a un capítulo con versión inglesa -> su .en.qmd
- Enlace a un capítulo solo en español -> URL de la versión española + «(*in Spanish*)»
Uso: python3 traduccion/enlaces_en.py   (desde la raíz del repositorio)"""
import re, os, glob, posixpath
URL = "https://abarejo.github.io/leeduca-manual/"
files = [f for f in glob.glob("**/*.en.qmd", recursive=True) + glob.glob("fichas/marcos/*.en.md") if not f.startswith(("_book", "_freeze"))]
pat = re.compile(r"\[([^\]]+)\]\(([^)\s]+?\.qmd)(#[^)\s]*)?\)(\s*\(\*in Spanish\*\))?")
for f in files:
    base = posixpath.dirname(f) if not f.startswith("fichas/marcos/") else "fichas/auto"   # los marcos se insertan en fichas/auto
    s = open(f, encoding="utf-8").read()
    def rep(m):
        text, tgt, anc, tag = m.group(1), m.group(2), m.group(3) or "", m.group(4)
        if tgt.startswith(("http", "/")) or tgt.endswith((".en.qmd", "summary-en.qmd")): return m.group(0)
        path = posixpath.normpath(posixpath.join(base, tgt))
        en = path[:-4] + ".en.qmd"
        if os.path.exists(en):
            return f"[{text}]({posixpath.relpath(en, base)}{anc})"
        return f"[{text}]({URL}{path[:-4]}.html{anc}) (*in Spanish*)"
    s2 = pat.sub(rep, s)
    if s2 != s:
        open(f, "w", encoding="utf-8").write(s2); print("updated", f)
