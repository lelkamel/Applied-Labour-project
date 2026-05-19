# Applied-Labour-project
# 📁 Repository Overview

This repository contains the source code and outputs for a study estimating the effect of vocational training on stable re-employment following mass layoffs due to firm closures in France.

---

## 📦 Data Sources

All analyses rely on the **ForCE** dataset (Formation, Chômage et Emploi), millésime **2024 S2**, assembled by the DARES. The following administrative databases are used:

| Database | Description |
|---|---|
| **MMO** | *Mouvements de Main-d'Oeuvre* — private-sector employment contracts derived from DSN |
| **REE** | *Répertoire des Entreprises et des Établissements* — firm creation and closure registry |
| **BREST** | *Base Régionalisée des Stagiaires de la Formation Professionnelle* — PIC-funded training enrollments |
| **FH – P2** | France Travail training spells for indemnified job seekers |
| **FH – DE** | *Fichier Historique* — individual unemployment spell histories |

---

## 🗂️ Repository Structure
│
├── 📂 code/
│   ├── 📄 1_detection_faillite_downsizing.R
│   ├── 📄 2_base_individuelle_dernier_contrat.R
│   ├── 📄 3_formation.R
│   ├── 📄 4_variable_interet.R
│   ├── 📄 5_var_controle.R
│   └── 📄 6_dml.R
│
└── 📂 figures/
    └── 🖼️ propensity_score_overlap.png

---

## 📜 Code Description

### `1_detection_faillite_downsizing.R`
Identifies firms that underwent **genuine closures** (liquidation) or **large-scale downsizing** events over the observation period, using the REE as the primary source. Implements a cross-validation procedure against MMO employment dynamics to exclude mergers and acquisitions from the sample.

### `4_base_individuelle_dernier_contrat.R`
Constructs the **individual-level analytical sample** from the MMO database. Starting from the universe of contracts in firms identified as having closed, this script:
- Retains the last contract held prior to displacement for each worker
- Applies sample quality filters (minimum tenure, contract type, termination motive)

### `6_formation.R`
Merges training participation records from **BREST** and **FH table P2** with the analytical sample. Constructs the binary **treatment variable** $D_i$, equal to 1 if the displaced worker enrolled in at least one vocational training program after displacement.

### `7_variable_interet.R`
Constructs the **outcome variable** from the MMO post-displacement contract database. Stable re-employment is defined as the first post-displacement contract of at least 180 days, observed within a 24-month window following displacement — extended by the duration of any training spell occurring between displacement and the first stable job.

### `8_var_controle.R`
Builds the **covariate matrix** $X$ used in the DML estimation, drawing on both MMO and FH sources. Covariates include pre-displacement individual characteristics (age, gender, education, nationality, marital status, number of children), contract characteristics (PCS, tenure, working time), geographic controls, and prior unemployment experience. Also implements the **propensity score overlap check**.

### `9_dml.R`
Estimates the **Average Treatment Effect** (ATE) of vocational training on stable re-employment using the **Double/Debiased Machine Learning Interactive Regression Model** (IRM-DML) of Chernozhukov et al. (2018). Considers $J = 4$ candidate learners (Lasso, Ridge, Random Forest, XGBoost) for both nuisance functions $g(d, X)$ and $p(X)$, selected by minimizing cross-fitted RMSE across all $J^2 = 16$ combinations. Final estimation uses 5-fold cross-fitting.

---

## 📊 Figures

### `propensity_score_overlap.png`
Kernel density plot of the estimated propensity score $\hat{p}(X_i) = \hat{\mathbb{P}}(D_i = 1 \mid X_i)$ separately for trainees and non-trainees, used to verify the overlap assumption underlying the IRM-DML identification strategy.

---

## 📋 Main Result

> The IRM-DML estimate of the ATE of vocational training on stable re-employment within 24 months is $\hat{\tau} = -0.0034$ (s.e. $= 0.0031$, $p = 0.268$), not statistically distinguishable from zero. Random Forest is selected as the best-performing learner for both nuisance functions.

---
