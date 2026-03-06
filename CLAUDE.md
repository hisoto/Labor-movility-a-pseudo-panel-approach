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
  enoe_zip/          # ZIPs crudos descargados del INEGI (uno por trimestre)
outputs/
  catalogo_enoe_sdem_coe1.rds   # catálogo de rutas de archivos ENOE
  panel_cohortes.rds             # pseudo-panel sintético de cohortes (output central)
scripts/
  000_enoe_descarga.R            # descarga de ZIPs del INEGI
  borrador_003_APC.R             # construcción del pseudo-panel (script principal)
referencias/
  APC Mexico cide.pdf            # paper de referencia (Duval-Hernández 2009)
  Deaton-Analysys.pdf            # método APC, sec. 2.7 pp. 122–133
```

Scripts numerados `000_`, `001_`, … indican orden de ejecución.

---

## Estrategia empírica

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
- Restricción: `Σ τ_t = 0` **y** `Σ t·τ_t = 0`
- Consecuencia: todo crecimiento tendencial se atribuye a edad y cohorte; `τ_t` captura
  únicamente fluctuaciones cíclicas

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

### Estimación
- **WLS** ponderado por `n_obs` (observaciones no ponderadas de la celda)
- Dummies completas para cada valor de edad, cohorte y periodo
- Variable dependiente: `log(p / (1 - p))` donde p es la tasa de la celda
- La predicción de las tasas se recupera como `exp(ŷ) / (1 + exp(ŷ))`

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

| Variable       | Numerador            | Denominador   |
|----------------|----------------------|---------------|
| `tasa_part`    | PEA                  | Población total |
| `tasa_desocu`  | Desocupados          | PEA           |
| `formal`       | Asalariados con SS   | Ocupados      |
| `informal`     | Asalariados sin SS   | Ocupados      |
| `auto_empleo`  | Empleadores + cuenta propia | Ocupados |

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

3. **Perfiles graficados**: el paper original grafica los efectos edad para la cohorte
   nacida en 1956 y los efectos cohorte a la edad de 42. Los perfiles de otras
   cohortes/edades son desplazamientos paralelos.

4. **Análisis de elasticidades cíclicas** (Tabla 1 del paper): regresión del componente
   cíclico de la participación contra el ciclo del PIB (log GDP sin tendencia lineal),
   con dummies estacionales. Se reportan coeficientes contemporáneos y rezagados.

5. **SUR vs. OLS ecuación por ecuación**: al tener los mismos regresores en todas las
   ecuaciones, SUR reduce a OLS individual — se puede estimar cada tasa por separado.
