# Spatial & Structural Admixture Analysis: Primula (R)

## Project Overview
This semester-long research project engineered a multi-stage biostatistics pipeline in R to evaluate the population genetics, spatial distribution, and ancestral sub-structures of the plant species *Primula*. The analysis transitions raw sequencing datasets through rigorous quality control, multivariate dimensionality reduction, and geographic metadata integration to evaluate genetic diversity gradients across changing latitudes.

## Key Technical & Biostatistical Features
* **Data Quality Control & Outlier Detection:** Parsed individual and locus-level sequencing depth profiles; utilized quantile distributions to identify and strip extreme right-skewed coverage anomalies, stabilizing dataset integrity for downstream modeling.
* **Multivariate Dimensionality Reduction:** Converted dense Variant Call Format (`.vcf`) files into Genomic Data Structure (`.gds`) matrices via `SNPRelate` to extract eigenvalues and project variance across Principal Component axes (PC1 & PC2).
* **Demographic Vector Assembly:** Applied conditional text substring manipulations to isolate regional cohort factors, structuring data arrays into ordered categorical variables to map distinct population clusters.
* **Spatial & Heterozygosity Correlation:** Computed expected vs. observed heterozygosity indices ($H_S$) and evaluated genetic diversity trajectories against geographic metadata using linear regression models.
* **Admixture Composition Modeling:** Evaluated structural clustering configurations at an optimization threshold of $K=7$ to isolate ancestral coefficients and map population sub-structures.
* **Multi-Panel Data Layouts:** Consolidated spatial maps, PCA variance scatters, latitude regressions, and violin density distributions into a unified publication-ready visual array using `patchwork`.

## Portfolio Visualization
![Primula Population Analysis Portfolio PDF](combination_plot.pdf)

## Technologies Used
* **Language:** R
* **Key Packages:** `SNPRelate`, `ggplot2`, `dplyr`, `reshape2`, `sf`, `maps`, `patchwork`, `janitor`

