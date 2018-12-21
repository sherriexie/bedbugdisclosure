# Dynamics of bed bug infestations and control under disclosure policies

This repository contains scripts that can be used to recreate the analyses and figures presented in the manuscript titled *Dynamics of bed bug infestations and control under disclosure policies: Short-term costs lead to long-term savings*. A description of the repository's contents are provided below.

## Script files

All scripts can be found in the **code** folder. For scripts to run correctly, it is important to: (1) install all necessary packages (listed within the library function in the first few lines of code) and (2) set the working directory to the location of the downloaded repository.

### Calculate cost and prevalence while varying *p* and *s*
- **costmatrix.R** - Runs multiple simulations to calculate disclosure cost and year-end prevalence over a range of values for *p* and *s*. The output of costmatrix.R can be found in **output_costmatrix**.
- **costmatrix_renters.R** - Same as costmatrix but calculates disclosure cost from the perspective of renters. The output of costmatrix.R can be found in **output_costmatrix_renters**.

### Figures
- **figure_barplot.R** - plots Figure 2
- **figure_cost.R** - plots Figure 3 from the output of costmatrix.R
- **figure_prevalence.R** - plots Figure 4 from the output of costmatrix.R

### Supplemental tables and figures
- **stable_twopopmodel.R** - outputs values used to populate Table S1
- **sfigure_components.R** - plots Figure S1
- **sfigure_relativeprevalencereduction.R** - plots Figure S2
- **sfigure_discountedcosts.R** - plots Figure S3
- **sfigure_renters_barplot.R** - plots Figure S4
- **sfigure_costtorenters.R** - plots Figure S5 from the output of costmatrix_renters.R
- **sfigure_sensitivitytoparameters.R** - plots Figures S6-S9
- **sfigure_intermarketmigration.R** - plots Figure S10
- **sfigure_googletrends.R** - plots Figure S11
- **sfigure_breakevenpoint.R** - plots Figure S12
- **sfigure_ddm_barplot.R** - plots Figure S13
- **sfigure_ddm_props.R** - plots Figure S14

### Shiny figures
- **shinyapp_fig_costplots.R** - makes the plots used in the app's landlord cost animation 
- **shinyapp_fig_rentercostplots.R** - makes the plots used in the app's renter cost animation
- **shinyapp_fig_prevalenceplots.R** - makes the plots used in the app's prevalence animation

### User-defined functions
*Contains the user-defined functions sourced by the other scripts.*
- **functions.R** - contains user-defined functions used in the primary analysis that is presented in the main text of the manuscript
- **functions_extra.R** - contains additional user-defined functions for sub- and sensitivity analyses that are presented in the Supporting Information

### Plot time-course of bed bug spread
*The output of these scripts are not used in the manuscript, but are a good place to start to get an understanding of the models.*
- **plotodes.R** - plots the number of units in each class (Sr, Ir, etc.) for a single simulation, with user controls to change parameter values. Note: time unit for parameter values is in days.
- **plotodes_years.R** - same as plotodes.R except time unit for parameters is in years.
- **plotodes_imm.R** - same as plotodes.R with the addition of intermarket migration parameters (i.e. *i* and *e*). Note: time unit for parameter values is in days.
- **plotodes_ddm.R** - same as plotodes.R but modified to approximate a discrete disclosure period. Note: time unit for parameter values is in days.

## Other files

### Shiny app
R scripts and image files used to create the [R Shiny web application](https://bedbugdisclosure.shinyapps.io/shinyapp/ "R Shiny | Modeling Bed Bug Disclosure")
associated with this manuscript can be found in the **shinyApp** folder. The app can also be run locally with **code/shinyapp_local.R**

### Figures
Figures in the main text of the manuscript can be found in the **figures** folder, and those in the Supplemental Information can be found in the **figures_supplement** folder. All figures were either directly output from R or were output from R and then imported into keynote for formatting.

### Data
Data from Google Trends presented in Figure S7 can be found in the **data** folder.
