# Anchor_River_Chinook

This repository contains stock assessment information and the Spawner-Recruit analysis for Anchor River Chinook salmon.

The network location for this repository is S:\RTS\Reimer\Anchor_River_Chinook.

The most recent update the the Spawner-Recruit analysis can be viewed at ....

Files/Folders in this repository are:

-   ANC_CHIN_BASSR_JAGS_UPDATE_22_RED_21-46AR_MODEL.jag: Jags model code for the analysis used in the 2019 SR analysis update. This code omitted most of the early stock assessment information associated with this stock.

-   ANC_CHIN_BASSR_JAGS_UPDATE_22_RED_21-46AR_MODEL.jag: Jags model code used for the 2022 analysis update. This code uses all available stock assessment information and mimics the original analysis presented in FMS07-05. Both of these models represent a subset of models considered when the SR analysis was reconsidered in 2016 and follow the naming convention used at that time.

-   \functions: Various helper functions used to tabulate and plot SR analysis results.

-   \script: The R script used to conduct the 2022 SR update.

-   \output: posteriors from the 2022 SR analysis update

-   \data: Currently houses an R data set with statewide Chinook BEG's for horsetail and OYP rugs. Ideally used to house SR datasets.

-   \markdown: A quarto file summarizing the 2022 SR analysis update. This file outputs to \docs so that it can be published to GItHub pages. .nojekyll and _quarto.yml are support files for this infrastructure. Also contains a quarto file creating materials for the 2023 LCI EG report.
