# Replication Package – Digital Readiness Regimes

### This repository contains the replication materials for the paper:
### “Digital readiness regimes and upward mobility: The role of governance and skills beyond connectivity”

### Overview
This project investigates cross-country differences in digital development by introducing the concept of digital readiness regimes, understood as multidimensional configurations of:
* digital infrastructure,
* human capital,
* innovation capacity,
* and regulatory quality.

Rather than relying on composite indices, the analysis adopts a configuration-based approach, identifying clusters of countries with similar capability profiles and examining their persistence and mobility over time.

### Objectives of the replication code

The replication files reproduce the main empirical steps of the paper:

#### Data construction
* assembling a country–year panel (2010–2023),
* merging indicators from multiple international sources,
* harmonizing country names and handling missing data.

#### Clustering analysis
* standardization of variables,
* identification of digital readiness regimes using k-means clustering,
* validation using silhouette scores and robustness checks.

#### Regime dynamics (STATA)
* construction of Markov transition matrices,
* estimation of persistence and mobility across regimes.

#### Econometric analysis (STATA)
* multinomial logit models of regime membership,
* robustness checks (ordered logit, transition models, alternative specifications),
* computation of average marginal effects.

#### Visualization (R)
* generation of figures (cluster plots, boxplots, transition heatmaps),
* visualization of marginal effects and model results,
* preparation of publication-ready graphs.


### Key methods

* Unsupervised learning: k-means clustering (baseline: k = 3)
* Dimensional standardization: pooled z-scores
* Dynamic analysis: first-order Markov transition matrix
* Econometric models:
* * multinomial logit (baseline),
* * ordered logit (robustness),
* * binary transition models

### Data sources

The analysis combines macro-level indicators from:
* World Bank (WDI)
* ITU (ICT indicators, regulatory tracker)
* UNCTAD (FTRI index)
* UN Comtrade (trade structure)

All variables are documented in the Appendix of the paper.

### Reproducibility notes

Clustering inputs are standardized using pooled z-scores.
Missing values are handled only for clustering via interpolation and median imputation.
Regression models are estimated on non-imputed data.
Results may vary slightly depending on software versions and random initialization in clustering.

### Notes

The regimes (Leaders, Followers, Outliers) are analytical constructs, not normative rankings.
Results should be interpreted as associations, not causal effects.
The code is designed for transparency and extensibility (e.g. alternative clustering methods).
