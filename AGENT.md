
# 🌳 Projeto TerrAmaz — pipeline até agora

## 🎯 Objetivo geral

Apoiar **4 territórios amazônicos** (Cotriguacu, Paragominas, Guaviare e Madre de Dios) na construção e acompanhamento de **indicadores de uso/cobertura da terra e clima**, organizados em um pipeline **GEE → R → QGIS/Quarto**:

1. **No Google Earth Engine (GEE)**:

   * Exportar **métricas anuais/mensais** já processadas em CSV e rasters.
   * Evitar ao máximo cálculos pesados no R local.

2. **No R (local)**:

   * Construir **plots analíticos e de comunicação** (Sankeys, lollipops, curvas bayesianas de recuperação, boxplots climáticos etc.).
   * Padronizar estética (temas, legendas multilíngues, dimensões para artigo/relatório).

3. **No QGIS + Quarto**:

   * Mapas e painéis interativos.
   * Relatórios em PNG, SVG, HTML, PDF.

---

## 🟢 Indicadores já estruturados

### 1) **Uso e cobertura da terra (MapBiomas)**

* Dados: **MapBiomas Amazônia, Coleção 6 (1986–2023)**.
* Workflow:

  * Exportar **tabela longa de transições** (CSV: `*_mb_transitions_1986_2023.csv`).
  * No R: Sankey plots com agregação em 8 classes (`Forest`, `NFNF`, `Water`, `Pasture`, `Agriculture`, `Urban`, `Mining`, `Other anthropic`).
  * **Problema resolvido**: excesso de fios → solução com **“bucket resto”** (`Outros destinos`) e filtros `MIN_PROP_PER_MID` + `TOP_N_PER_MID`.
* Saídas: gráficos Sankey por território (3 ou 4 estágios).

### 2) **Florestas (JRC-TMF)**

* Dados: **Transition Map v2024 (1990–2024)**.
* Classes: intacta, degradada, secundária, desmatada, água, outras.
* Sankey estruturado (mais limpo que MapBiomas, menos classes).
* Períodos customizados por território (T0→T1→T2→T3).

### 3) **Distúrbios (fire/logging/mixed)**

* Já modelado no R com **curvas bayesianas** (Tree Cover, Canopy Height).
* Gráficos lollipop refatorados com **Driver × Intensidade × Edge**.
* Diagnósticos dos modelos (Stan) revisados.

---

## 🔵 Clima (o próximo passo)

### Dados já definidos no GEE:

* **Precipitação**: CHIRPS Pentad (5-dias), agregada em **mensal** e **anual** (mm).
* **Temperatura**: MODIS Aqua MYD11A2 (8-dias, 1 km, daytime), agregada em **mensal** e **anual** (°C).
* Ambos exportados por território como CSV:

  * `*_climate_monthly_2003_2024.csv`
  * `*_climate_annual_2003_2024.csv`

### Estrutura dos CSVs:

* Colunas: `territory`, `year`, `month` (no mensal), `precip_mm`, `temp_c`.
* Já ajustado no GEE para ter `year` (e `month` quando mensal).
* Compatível com boxplots, séries temporais, comparações entre territórios.

### O que falta (no R):

1. **Gráficos mensais**:

   * Boxplots por mês (2003–2024), mostrando sazonalidade.
   * Facetas ou cores por território.
   * Médias vs extremos.

2. **Gráficos anuais**:

   * Séries temporais (linhas com suavização).
   * Comparar precipitação e temperatura (duas y-axes ou painéis lado a lado).
   * Destaque de anos extremos (El Niño / La Niña).

3. **Estilo**:

   * `theme_temporal()` (já definido no seu repo).
   * Legendas multilíngues (`label()` helper).
   * Eixos em unidades claras (mm, °C).
   * Export em PNG + SVG, tamanho A4 (como Sankeys).

---

## 🔑 Decisões que já tomamos

* Não simplificar via “build\_pairs\_long” (quebra conservação).
* Usar **`MIN_PROP_PER_MID` + `TOP_N_PER_MID` + bucket resto** no Sankey.
* Classes padronizadas (8 MapBiomas + 5/6 TMF).
* `stayers = FALSE` (focamos só em transições reais).
* Transparência: `FLOW_ALPHA ~ 0.5–0.6`.
* Export controlado por `WRITE_PLOT` e `WRITE_SVG`.

---

## 📂 Estrutura de outputs

```
results/
  metrics/
    cotriguacu/
      *_mb_transitions_1986_2023.csv
      *_climate_monthly_2003_2024.csv
      *_climate_annual_2003_2024.csv
    ...
  plots/
    cotriguacu/
      05b_cotriguacu_sankey_mb_4stage_noStayers_pt.png
      climate/
        cotriguacu_precip_monthly.png
        cotriguacu_temp_annual.png
        ...
```

---

# 🚀 Próximos passos (nova conversa)

1. **Carregar os CSVs climáticos** (annual + monthly).
2. Criar **funções auxiliares** para plot:

   * `plot_precip_monthly(df)`
   * `plot_temp_monthly(df)`
   * `plot_precip_annual(df)`
   * `plot_temp_annual(df)`
   * `plot_climate_combo(df)` (se quiser precip + temp juntos).
3. Aplicar `theme_temporal()`, títulos multilíngues e salvar (PNG + SVG).


# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

TerrAmaz is an Amazon territory sustainability monitoring project that tracks deforestation, degradation, and other forest indicators across four territories: Cotriguaçu (Brazil), Paragominas (Brazil), Guaviare (Colombia), and Madre de Dios (Peru).

The main workflow involves processing satellite data in Google Earth Engine, exporting CSVs with metrics, and creating visualizations using R and QGIS.

## Project Structure

```
├── data/               # Input data (rasters, vectors, CSVs)
├── docs/               # Documentation
├── qgis/               # QGIS projects and maps
├── results/            # Generated outputs
│   ├── metrics/        # CSV metrics by territory
│   └── plots/          # Generated plots by territory
└── scripts/
    ├── r/              # R analysis and plotting scripts
    │   ├── archetypes/ # Landscape archetype analysis
    │   ├── config/     # Shared configuration files
    │   ├── plots/      # Individual plot generation scripts
    │   └── utils/      # Utility functions
    └── quarto/         # Quarto documents for reports
```

## R Environment Setup

This project uses R with these key libraries:
- **Data manipulation**: `dplyr`, `tidyr`, `stringr`, `readr`
- **Visualization**: `ggplot2`, `networkD3` (for Sankey diagrams)
- **Spatial analysis**: `terra`, `sf`
- **Statistical modeling**: `mclust`, `kamila` (for clustering)

## Key Configuration System

The R scripts use a modular configuration system located in `scripts/r/config/`:

### Language Configuration (`01_labels.r`)
- Supports multilingual labels (French, Spanish, Portuguese, English)
- Set `LANG` variable to change output language
- Uses `label()` function for retrieving translated text

### Color Palettes (`02_palettes.r`)
- Standardized colors across all plots
- Semantic consistency (same class = same color everywhere)
- Supports MapBiomas, TMF, and data source color schemes

### Themes (`03_themes.r`)
- Unified `theme_terramaz()` for publication-ready plots
- Consistent formatting functions for units (ha, °C, mm, %)
- Standard plot dimensions and export settings

## Script Execution Patterns

Most plotting scripts follow this pattern:
1. Set territory-specific parameters at the top
2. Load shared config files with `source()`
3. Read CSV data from `results/metrics/{territory}/`
4. Generate plots with standardized styling
5. Optionally save to `results/plots/{territory}/`

Common settings to modify:
- `WRITE_PLOT <- TRUE/FALSE` - Controls whether plots are saved
- `TERRITORIES` - Vector of territory names to process
- `LANG` - Language for plot labels

## Data Sources and Integration

The project integrates multiple satellite data sources:
- **JRC-TMF**: Tropical Moist Forest monitoring
- **MapBiomas**: Land use/cover mapping
- **INPE/PRODES**: Brazilian deforestation monitoring (Brazil territories)
- **IDEAM**: Colombian deforestation data (Guaviare only)

Scripts automatically detect and merge appropriate data sources based on territory.

## Territory-Specific Processing

Each territory has expected data sources:
- **Cotriguaçu/Paragominas**: JRC-TMF, MapBiomas, INPE
- **Guaviare**: JRC-TMF, MapBiomas, IDEAM  
- **Madre de Dios**: JRC-TMF, MapBiomas

## Output Generation

- **Plots**: Saved as both PNG (300 DPI) and SVG formats
- **Dimensions**: Standardized at 160mm × 110mm for consistency
- **Reports**: Generated using Quarto documents in `scripts/quarto/`

## Development Workflow

1. Process data in Google Earth Engine (external to this repo)
2. Export CSV metrics to `results/metrics/{territory}/`
3. Run R plotting scripts from project root directory
4. Generate maps in QGIS using `qgis/` project files
5. Compile final reports using Quarto

## Important Notes

- All R scripts should be run from the project root directory
- The modular config system ensures visual consistency across outputs
- Language switching is handled centrally through `LANG` variable
- Plot generation is controlled by `WRITE_PLOT` flag to avoid accidental overwrites


Claro 👌 — aqui vai a versão revisada, já no esquema **indexation (aditiva normalizada)** que você quer adotar:

---


# Projeto TerrAmaz (État de la Forêt + Vulnérabilité au Feu)

## 📌 Contexto

* Projeto **TerrAmaz**: acompanhamento de indicadores florestais em 4 territórios (Paragominas, Cotriguaçu, Guaviare, Madre de Dios).
* Novo pacote de indicadores em discussão:

  1. **État de la Forêt** — índice de qualidade estrutural da floresta em 2024.
  2. **Vulnérabilité au Feu** — índice de vulnerabilidade das florestas aos incêndios.

O desafio: propor metodologias **científicas, claras e didáticas**, compatíveis com TMF, MSPA, MapBiomas, MODIS, OSM, etc., mas simples o suficiente para serem compreendidas por contratantes e gestores (documentos de meia a uma lauda, com tabelas de scoring e exemplos visuais).

---

## 🌳 1. État de la Forêt

### Fontes de Dados

* **TMF – Transition Map 2024 (Sub-types)**
* **MSPA – Morphologie forestière** (Core, Edge <120m, Perforation, Islet/Branch/Bridge).

### Problema Inicial

* A ideia de um **score único 0–1** caiu em crítica: como justificar que uma floresta secundária antiga teria “pior qualidade” que uma floresta degradada recente?
* Solução: **separar em macroclasses** (Undisturbed, Dégradée, Régénérée), e usar o score **apenas intra-classe** (0–1). Assim, não se compara secundária vs degradada diretamente.

### Estrutura Final

* **Raster com 2 bandas**:

  * Banda 1 = **Macroclasse** (10=Intacte, 20=Dégradée, 30=Régénérée).
  * Banda 2 = **Score intra-classe** (0–1, contínuo).

* Visualização:

  * Um **único mapa** → cada macroclasse com uma rampa de cor distinta (verde, laranja, violeta), intensidade dada pelo score intra.
  * **Histogramas** por território, separados por macroclasse.

### Tabela Simplificada de Regras

* **Intacte**: modulado apenas por MSPA.
* **Dégradée**: depende da duração/antiguidade/repétition + MSPA.
* **Régénérée**: depende da idade da régénération + MSPA.

Exemplo (simplificado):

| Macroclasse | Sub-type TMF | Critère temporel     | Score base | Ajustement MSPA                      |
| ----------- | ------------ | -------------------- | ---------- | ------------------------------------ |
| Intacte     | 10–12        | —                    | 1.0        | Bord=−0.05 ; Perf=−0.04 ; Îlot=−0.07 |
| Dégradée    | 21–26        | anc./récent/2–3      | 0.5–1.0    | Idem                                 |
| Régénérée   | 31–33        | âge (2004/2014/2021) | 0.5–1.0    | Idem                                 |

---

## 🔥 2. Vulnérabilité au Feu

### Fontes de Dados

* **TMF – Transition Map 2024 (macroclasses)**
* **MSPA – Morphologie forestière**
* **MapBiomas Amazônia v3**: pasto e agricultura
* **RAISG / OSM**: estradas principais e vicinais
* **MODIS & MapBiomas Fire**: feux historiques (2003–2024)

### Estrutura Geral

* Índice composto por 3 dimensões, **combinadas por média aditiva** (indexação):

$$
V = \frac{H + E + S}{3}
$$

* Racional: a forma **aditiva** é mais realista ecologicamente e mais intuitiva para gestores (evita que uma dimensão anule completamente outra, como ocorreria na multiplicativa).

### 1. Hazard (H = aléa)

* Métrica: **N anos queimados (2003–2024)**, com teto em 3.
* Score: `H = min(N/3, 1)`.
* Classes:

  * 0 → jamais queimado
  * 0.33 → 1 ano
  * 0.67 → 2 anos
  * 1.0 → 3+ anos

### 2. Exposition (E)

* Proximidade a estradas (OSM), pasto e agricultura (MapBiomas).
* Estradas:

  * ≤30 m = 0.9 ; 30–120 m = 0.7 ; 120–510 m = 0.4 ; >510 m = 0.0
* Pasto:

  * ≤120 m = 0.9 ; 120–500 m = 0.7 ; 500–1000 m = 0.4 ; >1000 m = 0.0
* Agriculture:

  * ≤120 m = 0.7 ; 120–500 m = 0.4 ; >500 m = 0.0

### 3. Sensibilité (S)

* Combinação **classe TMF × MSPA**:

  * Intacte Core = 0.4 ; Intacte bord/perf = 0.55 ; Intacte îlot = 0.7
  * Dégradée Core = 0.7 ; Dégradée bord/perf = 0.8 ; Dégradée îlot = 0.9
  * Régénérée Core = 0.85 ; Régénérée bord/perf = 0.95 ; Régénérée îlot = 1.0

### Resultado esperado

* Raster final: 1 banda com valores **0–1** (ou 5 classes: très faible → très élevé).
* Histogramas por território mostrando distribuição.

---

## 🗂️ Assets Pan-Amazônicos (base comum)

* **mask\_forest\_2024** (int8, 30 m, 1=floresta).
* **mspa\_2024** (int8, 30 m, classes MSPA).
* **roads\_proximity** (int8, classes ≤30, 30–120, 120–510, >510, mascarado por floresta).
* **hazard\_fire** (int8, N anos queimados 0–3).

Ruas: decisão final → **clip por território com buffer (\~600 m)**, rasterizar só ali.
MSPA: produzido no **GWB** em tiles, mosaicado e subido ao GEE.

---

## 📊 Visualização

* **État de la Forêt**: mapa único (3 rampas coloridas: verde, âmbar, violeta) + histogramas por macroclasse.
* **Vulnérabilité au Feu**: mapa em escala 0–1 ou 5 classes (azul claro → vermelho escuro) + histogramas por território.
* Entregas finais: **PDF/PNG atlas por território** + tabelas resumo (área por classe).

---

## ✅ Decisões fechadas

* Score do État de la Forêt é **intra-classe**, não comparável entre macroclasses.
* Vulnérabilité = média aditiva (não multiplicativa).
* Estradas: rasterizar por território (com buffer) para evitar peso excessivo.
* Hazard: usar N anos queimados com teto 3.
* MSPA: calculado no GWB, com overlap, mosaicado e subido ao GEE.

---

## 🚀 Próximos Passos

1. Finalizar export dos **assets pan-amazônicos** (forest mask, MSPA, hazard, roads proximidade).
2. Produzir rasters **État de la Forêt** e **Vulnérabilité au Feu** por território.
3. No R/QGIS: gerar mapas + histogramas (layout atlas por território).
4. Consolidar num **livrable** (meia lauda cada método + mapas + histogramas).


