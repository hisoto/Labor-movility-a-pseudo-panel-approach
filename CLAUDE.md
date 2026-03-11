# CLAUDE.md — Labor Mobility: A Pseudo-Panel Approach

## Objetivo del proyecto
Replicar y extender el análisis de cohortes de **Duval-Hernández & Orraca Romano (2009)**
(IZA DP No. 4371) usando datos de la **ENOE 2005–2025**.

El paper original usa ENEU 1987–2002 + ENE 2002–2004 + ENOE 2005–2009 y se restringe
a zonas urbanas. Esta extensión cubre solo ENOE (2005 en adelante) y por decisión del
investigador **no aplica filtro urbano** por el momento.

---

## Estructura de directorios

```
inputs/
  enoe_zip/                      # ZIPs crudos descargados del INEGI (uno por trimestre)
outputs/
  catalogo_enoe_sdem_coe1.rds    # catálogo de rutas de archivos ENOE
  panel_cohortes.rds             # pseudo-panel sintético de cohortes (output central)
  efectos_apc.rds                # lista con 36 modelos (6 variables × 2 géneros × 3 educaciones)
  figuras/                       # PDFs y PNGs de efectos APC
scripts/
  000_enoe_descarga.R            # descarga ZIPs del INEGI con reintentos
  000_enoe_zip.R                 # descomprime ZIPs descargados
  001_catalogo.R                 # catálogo maestro de archivos SDEM/COE1 por trimestre
  002_sdem.R                     # series de tiempo por educación × género (Figs 1–2)
  003_pseudo_panel.R             # construye panel sintético de cohortes (output central)
  004_figuras_perfiles_brutos.R  # perfiles brutos del ciclo de vida (Figs 3–4)
  005_APC_estimacion.R           # estima modelo APC con normalización Deaton
  006_figuras.R                  # genera figuras PDF de efectos α / κ / τ por variable
  007_fig_educacion_cohorte.R    # años de educación por cohorte (Fig A-1)
  theme_conasami.R               # tema ggplot2 compartido (Noto Sans, paleta CONASAMI)
referencias/
  APC Mexico cide.pdf            # paper de referencia (Duval-Hernández 2009)
  Deaton-Analysys.pdf            # método APC, sec. 2.7 pp. 122–133
```

Scripts numerados `000_`, `001_`, … indican orden de ejecución.

---

## Estrategia empírica

### Justificación del pseudo-panel

El pseudo-panel asume que el comportamiento *promedio* de una cohorte en cualquier punto
del tiempo está bien aproximado por los individuos de esa cohorte que aparecen en la
muestra, aunque no sean los mismos individuos de encuestas previas
(Duval-Hernández & Orraca Romano 2009, sec. 3.1).

Ventaja sobre un panel verdadero en el contexto mexicano: los paneles disponibles son
de corta duración y tienen altas tasas de desgaste (*attrition*). Cada encuesta trimestral
se trata como una sección cruzada independiente — no se usa la dimensión rotativa del
panel ENOE (Duval-Hernández & Orraca Romano 2009, sec. 3.1).

Dos características del enfoque de cohorte son relevantes (Deaton 1997, sec. 2.7):
- Los datos de cohorte **no revelan la distribución conjunta** de una variable en dos
  periodos, solo sus marginales: no se puede saber si quienes eran pobres antes siguen
  siéndolo.
- Los efectos fijos de cohorte **no pueden eliminarse por diferenciación**, porque cada
  año se observan individuos distintos de la misma cohorte. La promediación dentro de
  celdas reduce el error de medición respecto a datos individuales.
- Advertencia (Deaton 1997): el procedimiento es riesgoso con pocos periodos, donde es
  difícil separar tendencia de ciclo.

### Modelo APC (Deaton, 1997)

Variable dependiente en **log-odds** de la tasa de participación:

```
ln(p_ct / (1 - p_ct)) = θ + α_a + κ_c + τ_t + ε_ct
```

| Componente | Significado |
|-----------|-------------|
| `α_a` | Efecto edad — perfil de ciclo de vida |
| `κ_c` | Efecto cohorte — tendencia generacional |
| `τ_t` | Efecto tiempo — ciclo económico (⊥ tendencia lineal) |

### Identificación: normalización de Deaton

La colinealidad exacta emerge de la identidad `a_ct = c + t` (Deaton 1997, eq. 2.91) —
la edad de la cohorte c en el año t es exactamente la suma de ambos — lo que implica
`A·s_a = C·s_c + Y·s_y` (eq. 2.92): identidad lineal exacta entre las tres matrices de
dummies, donde los vectores s son secuencias aritméticas {0,1,2,3,...}.

Normalización impuesta (Deaton 1997, eq. 2.94): `s_y'·ψ = 0`
— los efectos de año son ortogonales a una tendencia lineal.

- Restricción equivalente: `Σ τ_t = 0` **y** `Σ t·τ_t = 0`
- Consecuencia: todo crecimiento tendencial se atribuye a edad y cohorte; `τ_t` captura
  **únicamente fluctuaciones cíclicas** (componente no lineal del periodo)

Nota de identificación (Bell 2020): el problema afecta **únicamente los componentes
lineales** de APC. Los efectos discretos y no lineales son identificables sin supuestos.
La normalización de Deaton equivale a fijar `β_P_lineal = 0` — un supuesto fuerte pero
explícito. Todo intento de "resolver" el problema sin supuestos explícitos (Intrinsic
Estimator, Hierarchical APC) en realidad impone supuestos ocultos.

#### Procedimiento de ortogonalización (paso a paso)
1. Crear las T dummies de periodo originales: `d_1, d_2, …, d_T`
2. Regresionar **cada** dummy `d_t` contra una constante y una tendencia lineal
   (`t = 1, 2, …, T`)
3. Tomar los **residuos** de esas regresiones → dummies transformadas `d*_t`, que por
   construcción satisfacen `Σ d*_t = 0` y `Σ t·d*_t = 0`
4. Estimar el modelo APC usando dummies de edad + dummies de cohorte + las `d*_t`
   (en lugar de las dummies originales de periodo)
5. El modelo queda identificado porque las `d*_t` han sido purgadas del componente
   tendencial que causaba la colinealidad exacta con edad y cohorte

Fórmula de transformación (Deaton 1997, eq. 2.95):

```
d*_t = d_t − [(t−1)·d_2 − (t−2)·d_1]
```

donde `d_t` es la dummy original del periodo t (= 1 si año = t, 0 en otro caso).
Implementado en `005_APC_estimacion.R` vía proyección matricial `M_B = I − B(B'B)⁻¹B'`
con `B = [1, t]`, que es algebraicamente equivalente.

### Estimación
- **WLS** ponderado por `n_obs` (observaciones no ponderadas de la celda)
- Dummies completas para cada valor de edad, cohorte y periodo
- Variable dependiente: `log(p / (1 - p))` donde p es la tasa de la celda
- La predicción de las tasas se recupera como `exp(ŷ) / (1 + exp(ŷ))`
- Con los mismos regresores en todas las ecuaciones, SUR reduce a OLS ecuación por
  ecuación — cada tasa se puede estimar por separado (Duval-Hernández 2009, nota 4)

### Interpretación de los efectos estimados

Convenciones de graficación (Duval-Hernández & Orraca Romano 2009, notas 11–12):

- **Efectos edad (α_a)**: se grafican para la cohorte nacida en **1956**. Los perfiles
  de otras cohortes son desplazamientos paralelos del perfil graficado.
- **Efectos cohorte (κ_c)**: se grafican a la edad de **42 años**. Los perfiles de
  otras edades son desplazamientos paralelos.
- **Efectos periodo (τ_t)**: se presentan como desviaciones respecto a su media.
  No representan el efecto periodo completo — solo su componente ortogonal a cualquier
  tendencia lineal (Bell 2020). Capturan fluctuaciones cíclicas, no niveles absolutos.

### Elasticidades cíclicas (análisis secundario)

Fuente: Duval-Hernández & Orraca Romano (2009), sec. 4.3 y Tabla 1.

Regresión OLS del logaritmo del componente cíclico de participación contra el logaritmo
del PIB trimestral real, el PIB tendencial (predicción lineal), y dummies estacionales.
Equivalente a dos etapas:
1. Obtener los residuos cíclicos del log-PIB respecto a tendencia lineal.
2. Regresionar el log de las tasas de participación contra el ciclo del PIB y dummies
   estacionales.

Se reportan coeficientes contemporáneos y rezagados del ciclo del PIB. El coeficiente
del log-PIB es la **elasticidad** de la tasa de participación respecto al ciclo económico.

---

## Definición de cohortes

| Dimensión      | Valores / Regla                                      |
|----------------|------------------------------------------------------|
| Año nacimiento | `anio_tri - eda` (año calendario del trimestre menos edad) |
| Género         | Hombre / Mujer                                       |
| Educación      | Básica (0–6 años), Intermedia (7–12), Superior (>12) |
| Edad           | 20–70 años                                           |
| Celda mínima   | `n_obs >= 100` observaciones no ponderadas           |

El umbral de 20 años evita errores de clasificación educativa (después de esa edad
muy pocos individuos cambian de grupo educativo).

---

## Variables dependientes (5 tasas)

| Variable        | Numerador                        | Denominador      | Fuente    |
|-----------------|----------------------------------|------------------|-----------|
| `tasa_part`     | PEA                              | Población total  | Paper     |
| `tasa_desocu`   | Desocupados                      | PEA              | Paper     |
| `formal`        | Asalariados con SS               | Ocupados         | Paper     |
| `informal`      | Asalariados sin SS               | Ocupados         | Paper     |
| `auto_empleo`   | Empleadores + cuenta propia      | Ocupados         | Paper     |
| `sobreocupado`  | Asalariados con `hrsocup > 40`   | Asalariados      | Extensión |

Las primeras 5 tasas corresponden exactamente al paper original
(Duval-Hernández & Orraca Romano 2009, sec. 3.2). `sobreocupado` es una extensión
propia del investigador — **no aparece en ninguno de los tres papers de referencia**.

Los shares de empleo son exhaustivos:
`formal + informal + auto_empleo + no_remunerado = 1`

---

## Definiciones de variables ENOE

### Filtros de muestra
- `r_def == 0` — entrevista completa
- `c_res %in% c(1, 3)` — residente habitual presente o ausente
- `eda %in% 20:70` — rango de edad de análisis
- Filtro urbano (`ur == 1`): **NO aplicado** (decisión pendiente de revisión)

### Clasificación laboral
```r
# Formal: asalariado con seguridad social
formal      <- pos_ocu == 1 & seg_soc == 1

# Informal: asalariado sin seguridad social (incluye seg_soc == 3)
informal    <- pos_ocu == 1 & seg_soc %in% c(2, 3)

# Autoempleo: empleadores + cuenta propia
auto_empleo <- pos_ocu %in% c(2, 3)

# No remunerado (para exhaustividad de shares)
no_remun    <- pos_ocu == 4
```

### Variables de clasificación
| Variable   | Descripción |
|-----------|-------------|
| `ur`       | 1 = urbano, 2 = rural |
| `t_loc`    | 1=100k+, 2=15–99k, 3=2.5–14k, 4=<2.5k hab. |
| `clase1`   | 1 = PEA, 2 = PNEA |
| `clase2`   | 1 = ocupado, 2 = desocupado (dentro de PEA) |
| `pos_ocu`  | 0=N/A, 1=asalariado, 2=empleador, 3=cuenta propia, 4=sin pago |
| `seg_soc`  | 1=con SS, 2=sin SS, 3=otro |
| `anios_esc`| Años de escolaridad (base para grupos educativos) |
| `fac`/`fac_tri` | Factor de expansión (según tramo del ENOE) |

---

## Convenciones de código R

- Pipe nativa `|>` (no `%>%`)
- `pacman::p_load()` para cargar paquetes
- Scripts inician con `rm(list = ls()); gc()` y `options(scipen = 999)`
- `snake_case` para variables y funciones
- Separadores de sección: `# ── nombre ───`
- Outputs intermedios como `.rds`; datos crudos como `.csv` o `.dta`
- WLS con `lm(..., weights = n_obs)`

---

## Notas metodológicas importantes

1. **Colinealidad APC**: `edad = año - año_nacimiento`, por eso se requiere la
   normalización de Deaton para identificar los tres efectos.

2. **seg_soc == 3**: se asigna a informal (n pequeño; el paper original opera igual).

3. **Perfiles graficados** (Duval-Hernández 2009, notas 11–12): el paper grafica los
   efectos edad para la cohorte nacida en **1956** y los efectos cohorte a la edad de
   **42**. Los perfiles de otras cohortes/edades son desplazamientos paralelos.

4. **Interpretación de τ_t** (Bell 2020): el efecto periodo estimado no es el efecto
   periodo completo — es solo su componente ortogonal a cualquier tendencia lineal.
   Mide desviaciones cíclicas, no niveles absolutos.

5. **Análisis de elasticidades cíclicas** (Tabla 1 del paper): regresión del componente
   cíclico de la participación contra el ciclo del PIB (log GDP sin tendencia lineal),
   con dummies estacionales. Se reportan coeficientes contemporáneos y rezagados.

6. **SUR vs. OLS ecuación por ecuación** (Duval-Hernández 2009, nota 4): al tener los
   mismos regresores en todas las ecuaciones, SUR reduce a OLS individual — se puede
   estimar cada tasa por separado.

7. **`sobreocupado` (extensión propia)**: tasa de asalariados (`pos_ocu == 1`) que
   trabajan más de 40 horas (`hrsocup > 40`), calculada sobre el total de asalariados
   ocupados. El denominador es distinto al de `formal`/`informal`/`auto_empleo` (que
   usan todos los ocupados). Esta variable se estima en el mismo modelo APC y aparece
   como la sexta entrada en `efectos_apc.rds` — de ahí "6 variables" en la descripción
   del output.
