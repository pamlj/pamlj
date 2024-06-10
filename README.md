# PAMLj power analysis for linear models in jamovi


Version 0.2.*

<center>
<img width="300" src="https://pamlj.github.io/commons/pics/ui.png" class="img-responsive" alt="">
</center>
<br>

Estimation power parameters (required sample size, posthoc power, minimal detectable effect size and required alpha) for  General Linear Models.


* Correlation
* Regression
* T-Tests (independent, paired and one sample,  and equivalence tests)
* Proportions (independent, paired, one sample)
* ANOVA
* Partial eta-squared based analysis
* Eta-squared and $R^2$ based analysis
* Standardized coefficients based analysis
* F-test for factorial designs from partial eta-squared (between, within and mixed)
* F-test for factorial designs from cells means and sd (between, within and mixed)


# Docs and help

More informations can be found at [PAMLj page](https://pamlj.github.io/)

# Install in jamovi

Please install [jamovi](https://www.jamovi.org/download.html) and run it. Select the jamovi modules library and install PAMLj from there


## From GitHub

In your R script (or Rstudio) simply issue 

```
library(jmvtools)
devtools::install_github("pamlj/pamlj")

```

## From source


You will first need to download [jamovi](https://www.jamovi.org/download.html). 

You can clone this repository and compile the module within R with 

```
library(jmvtools)

jmvtools::install()

```


# Programmatic name

```
paste(paste(LETTERS[c(16,1,13,12)],collapse =""),paste(letters[10]),sep="")

```
