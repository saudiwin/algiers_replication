# README

This Github repository contains code and data to reproduce the results in "Taxes in the Time of Revolution: An Experimental Test of the Rentier State during Algeria's Hirak" (World Politics 2024) by Robert Kubinec and Helen V. Milner.

This repository is a mirror of the Dataverse repository, although the Dataverse repository contains the full model objects, while this repository does not include all saved model objects due to size. These however can be downloaded from this Google Drive folder https://drive.google.com/drive/folders/1r_KBEBzUDtTT5APPC4RKYjNQks8BimFY?usp=drive_link and put into the `data/` folder to reproduce the paper without needing to estimate all the Bayesian models (which can take some time).

A brief description of the files in the repository is below. The paper file is `world_politics_accepted.tex` in the root directory. The supplementary information is in the `appendix.tex`, also in the root directory. Note that the code will need to be run to produce figures/tables before the Latex file will compile. All plots are saved in the `plots/` subdirectory and tables are saved in the `tables/` subdirectory. The code assumes that the working directory is set to the root directory for the repository. 

Note to run the full code, you will need to install `idealstan`, which is not available on CRAN. You must first install package `remotes` (which is available on CRAN), and then run the following code in the R terminal:

```
remotes::install_github("saudiwin/idealstan")
```

To fully estimate the SES index mentioned in the paper, you will have to have a working version of `cmdstanr` installed as well. However, as the repository comes with a fitted `idealstan` model (`data/ses_mod.rds`), you do not need to do so to reproduce the results.

## Code Files Description

1. `check_exp_script.R` This is the main script file that fits Bayesian models, including the SES index mentioned in the paper, and also produces tables and figures in the paper.

2. `review_sim.R`: this script implements a Monte Carlo simulation of potential null results as reported in the supplementary information.

3. `produce_tables.R`: produce some tables of model coefficients for the supplementary information

4. `survey_descriptives.R`: produce descriptives table for main paper

5. `mrp_vignette.Rmd`: code from Lauren Kennedy showing how to use `rstanarm` for MRP adjustment

6. `islam_exp.js`: Javascript code used in the Qualtrics survey for randomization of treatments

