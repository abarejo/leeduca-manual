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
| 1 (revisores y colaboradores) | Presentación, Technical summary, Definición, Plataforma, Estudio normativo, Método psicométrico, Resultados globales, Riesgo, Límites, Catálogo, fichas de clase A | Presentación, summary y Definición hechos (27/11/2026) |
| 2 | Marco teórico, DIS-ESP, Conciencia fonológica, Cognición numérica, Woodcock, Baremos, Agrupación, Decisiones | Pendiente |
| No se traduce | Novedades, Avances, Agenda, Consultas, Colaborar, Estructura | — |

Las fichas automáticas no se traducen a mano: se traducirán la plantilla de `R/ficha_auto.R` y los nombres de tarea del diccionario (ya hay nombres en inglés revisados en `catalogo_tareas_meta_v1.csv`).
