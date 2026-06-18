# RAPPORT <img src="www/logo.png" align="right" height="120" /> 

**Relatedness Analysis and Population Planning, Organisation, Reporting and Tracking**

RAPPORT is an open-source R Shiny application for breeding program management 
in closed or semi-closed animal populations. Users upload their own pedigree 
and phenotypic data, and the application provides an integrated environment 
for genetic diversity monitoring, breeding stock evaluation, production planning, 
and mate selection — with all outputs adapting to whatever traits are present 
in the data.

## Features

- **Flexible data input**  Upload pedigree and phenotypic data (Excel), with 
automatic key column detection, robust date parsing, and pedigree QC reporting. 
Any user-defined phenotypic traits present in the data file are carried through 
to all downstream modules.
- **Population overview**  Demographic summaries (litters, dams, sires), birth 
statistics, sex ratios, and individual animal lookup with family browsing.
- **Genetic diversity monitoring**  Inbreeding coefficients, effective 
population size (Nₑ), COI trends with weighted linear regression and ΔF 
rate projection, and mean kinship among breeders vs. the population.
- **Breeder selection**  Candidate ranking by mean kinship with 
population-level tertile banding, combined with user-defined mandatory and 
preferred phenotypic selection goals.
- **Ranked mating**  Mate recommendations filtered and ranked by maximum COI 
increase over parental average, focusing on the rate of inbreeding accumulation 
rather than absolute COI. User-defined phenotypic criteria are applied as 
additional hard filters.
- **Management tools**  Population growth projections, dam breeding window 
planning, progeny planning with correction rate estimation and breeding stock
recruitment tracking.

## Documentation

- [User and Methods Vignette](https://BirgitDeboutte.github.io/RAPPORT/RAPPORT_vignette.html) — computational methods, module descriptions, and interpretation guidance
- [User Guide](https://BirgitDeboutte.github.io/RAPPORT/RAPPORT_user_guide.html) — step-by-step instructions for using the application


## Installation and usage

### Option 1 — Run directly from GitHub

```r
# Install shiny if needed
install.packages("shiny")

# Launch RAPPORT
shiny::runGitHub("RAPPORT", "BirgitDeboutte")
```

### Option 2 — Clone and run locally

```bash
git clone https://github.com/BirgitDeboutte/RAPPORT.git
```

Then in R:

```r
shiny::runApp("path/to/RAPPORT")
```

### Dependencies

RAPPORT requires the following R packages. Install any missing packages before running:

```r
install.packages(c(
  "shiny", "shinyjs", "shinyFeedback", "shinyWidgets",
  "bslib", "bsicons",
  "dplyr", "tidyr", "readxl", "openxlsx",
  "DT", "thematic",
  "pedigree", "kinship2",
  "ggplot2", "plotly", "lubridate",
  "DiagrammeR", "DiagrammeRsvg", "rsvg",
  "sortable", "ragg"
))
```

## Test data

The `data/` folder contains an anonymised example dataset (pedigree and data file) 
that can be used to explore the application. Upload these files manually through
the Data Input tab to get started.

## License

This project is licensed under the GPL-3.0 License — see the [LICENSE](LICENSE) file for details.

## Contact

For questions or feedback, please open an [issue](https://github.com/BirgitDeboutte/RAPPORT/issues) on GitHub.
