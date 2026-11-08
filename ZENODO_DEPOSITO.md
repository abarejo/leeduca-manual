# Depósito en Zenodo: lista de comprobación (versión de consulta 0.9)

Preparado el 22/10/2026. El depósito se hará la semana siguiente, cuando el grupo acuerde la autoría.

1. **Autoría.** Acordarla con J. L. Luque y actualizar `creators` en `.zenodo.json`, `authors` en `CITATION.cff` y el apartado «Autoría» de `index.qmd`. Para cada persona: nombre, afiliación y ORCID.
2. **Marco teórico.** Borradores de los capítulos 1 ([uso responsable](uso-responsable.qmd)) y 2 ([marco teórico](marco-teorico.qmd)) del 08/11/2026. Falta la revisión del grupo y los datos «Por confirmar» del capítulo 1 (consentimiento, comité de ética, formación de evaluadores, responsable del tratamiento).
3. **Congelar la versión.** En `index.qmd`, cambiar el aviso a «Versión de consulta 0.9 (fecha)» y añadir cómo citarla con el DOI reservado.
4. **Conectar el repositorio con Zenodo** (una sola vez): en zenodo.org → GitHub → activar `abarejo/leeduca-manual`. Cada *release* de GitHub crea un DOI.
5. **Crear la *release*** `v0.9` en GitHub. Zenodo lee `.zenodo.json` y archiva el código fuente del libro. Subir además, a mano, el PDF o el ZIP del libro renderizado (`_book`) como archivo del registro.
6. **Comprobar** que el DOI aparece en `CITATION.cff` y en la presentación del libro.
7. **Licencia:** CC BY 4.0 (decidido el 22/10/2026).
