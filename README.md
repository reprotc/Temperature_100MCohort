## Overview

This repository contains the main analysis code and figures for the manuscript  
**"Ambient temperature and non-accidental mortality: a nationwide space-time case-crossover study within the 100 Million Brazilian Cohort."**

- **[Main analysis](./1_main_analysis_br.R)**: Script used to estimate the association between ambient temperature and non-accidental mortality across Brazilian municipalities using a space-time case-crossover design.

- **[Exposure-response curves](./2_exposure_response_curves_byRegion.R)**: Script to generate region-specific exposure-response curves.

- **[Heatmap visualization](./3_heatmap_cold_heat.R)**: Script to create a heatmap showing the percentage increase in mortality risk associated with heat and cold temperatures across subgroups and regions.

The environmental data processing was conducted by **Danielson J.D. Neves** (danielson.neves@fiocruz.br), based on the  
**[Brazilian Daily Weather Gridded Data (BDWGD)](https://github.com/AlexandreCandidoXavier/BR-DWGD)**, which integrates information from 1,252 meteorological stations nationwide.

