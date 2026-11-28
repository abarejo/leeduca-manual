# Versión en inglés del manual

Decisión del grupo (08/11/2026; ampliada el 27/11/2026): el manual tendrá una versión en inglés que se construye poco a poco, empezando por lo que necesitan los revisores de los artículos y los colaboradores internacionales.

## Cómo está montado

- **Un solo repositorio, dos libros.** El español es el perfil por defecto (`_quarto-es.yml`); el inglés es el perfil `en` (`_quarto-en.yml`). Lo común (formato, bibliografía, ejecución) está en `_quarto.yml`.
- **Cada capítulo traducido tiene su pareja:** `definicion.qmd` → `definicion.en.qmd`. El inglés solo lista los capítulos ya traducidos.
- **Las cifras no se traducen, se calculan.** Los bloques de código R son los mismos que en español; solo cambian los textos de las figuras y tablas. `R/libro.R` detecta los archivos `*.en.qmd` y usa punto decimal.
- **Enlaces a capítulos aún no traducidos:** van a la página española con la marca *(in Spanish)*. Cuando se traduzca el capítulo, se cambia el enlace por el `.en.qmd`.
- **Glosario cerrado:** `traduccion/glosario_es_en.csv`. Todo término de la tabla se traduce siempre igual. Si hace falta uno nuevo, se añade primero al glosario. El libro inglés lo publica como apéndice.
- **Ortografía:** inglés americano (estilo APA).

## Generar en local

```
quarto render                 # español → _book/
quarto render --profile en    # inglés  → _book/en/
```

GitHub Actions genera los dos y publica el inglés en `https://abarejo.github.io/leeduca-manual/en/`.

## Al cambiar un capítulo en español ya traducido

1. Cambiar el `.qmd` español.
2. Trasladar el cambio al `.en.qmd` y actualizar su línea de estado (*Translated from the Spanish version of …*).
3. Si no da tiempo, poner en el `.en.qmd` el aviso `The Spanish version has changed since this translation` y anotarlo en la tabla de estado de `index.en.qmd`.

## Plan de traducción

| Capa | Capítulos | Estado |
|---|---|---|
| 1 (revisores y colaboradores) | Presentación, Technical summary, Definición, Plataforma, Estudio normativo, Método psicométrico, Resultados globales, Riesgo, Límites, Catálogo, ficha de velocidad (VEL), fichas de clase A, Cómo se construye cada ficha, Decisiones metodológicas, Preleo y Leetrea | **Hecha** (27/11/2026) |
| 2 | Marco teórico, DIS-ESP, Conciencia fonológica, Cognición numérica, Woodcock, Baremos, Agrupación, Decisiones, fichas de clase B y C | En curso: fichas piloto LET, RAN, CF523 y TECLE; baremo por trimestre, baremo por nivel lector, criterio Woodcock, tramos del informe y uso responsable; marco teórico, DIS-ESP, agrupación, conciencia fonológica, cognición numérica, tareas por edad y cribado de dos riesgos hechos (28/11/2026). Faltan las fichas B y C |
| No se traduce | Novedades, Avances, Agenda, Consultas, Colaborar, Estructura | — |

Las fichas automáticas no se traducen a mano: `R/ficha_auto_en.R` es la versión inglesa de `R/ficha_auto.R` (mismos datos, textos en inglés) y cada ficha tiene su `fichas/auto/<TAREA>.en.qmd`. Los marcos teóricos compartidos están en `fichas/marcos/<id>.en.md` (traducidos: los siete de las tareas de clase A). **Si se cambia `ficha_auto.R`, hay que trasladar el cambio a `ficha_auto_en.R`.**

**Qué no se traduce en esta capa** (se muestra en español y marcado): las consignas y constructos del catálogo que no tienen versión inglesa, la configuración leída de la plataforma, los nombres de hoja y columna de las tablas completas y los rótulos de las figuras guardadas como imagen (PNG). Las secciones «Preguntas para el grupo» son internas y no se traducen.

**Enlaces:** después de añadir o traducir un capítulo, ejecutar `python3 traduccion/enlaces_en.py`, que apunta cada enlace a la versión inglesa si existe y, si no, a la española con la marca *(in Spanish)*.
