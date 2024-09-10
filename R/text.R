### here we define the text shown in the analysis output panel. 
### Each analysis has two classes (possibly equal), "caller" and "mode"
### INFO contains the text to be shown based on the "caller" class (like glm, factorial, correlation, etc)
### INFO2 contains the text to be shown based on the "mode" class (like peta, eta, beta, etc)
### If the analysis does not have a "mode" (like correlation), the mode is ignored


common_init <- "<p> Please select the aim of the analysis:</p>
                <ul>
                <li> <b> Calculate N</b> computes the required sample size given the <b> Target effect size</b> and <b> Minimum desire power</b>  </li>          
                <li> <b> Calculate Power</b> computes the achievable power given the <b> Target effect size</b> and <b> N (Sample size)</b>  </li>          
                <li> <b> Calculate Effect Size</b> computes the minimally-detectable effect size given the <b> N (Sample size)</b> and <b> Minimum desired power</b>  </li>          
                </ul>
                <p> In all cases, you can set the required Type I error rate (significance cut-off)</b>
                "
factorial_init <- "<p> Please select the aim of the analysis:</p>
                  <ul>
                  <li> <b> Calculate N</b> computes the required sample size given the <b> Target effect size</b> and <b> Minimum desire power</b>  </li>          
                  <li> <b> Calculate Power</b> computes the achievable power given the <b> Target effect size</b> and <b> N (Sample size)</b>  </li>          
                  </ul>
                  <p> In all cases, you can set the required Type I error rate (significance cut-off)</b>
                  "
mediation_init <- "<p> Please select the aim of the analysis:</p>
                  <ul>
                  <li> <b> Calculate N</b> computes the required sample size given the standardized coefficients and <b> Minimum desire power</b>  </li>          
                  <li> <b> Calculate Power</b> computes the achievable power given the standardized coefficients and the <b> N (Sample size)</b>  </li>          
                  </ul>
                  <p> The coefficients (<b1> X to M1 effect (a1)</b>, <b> M1 to Y effect (b1)</b> etc.) are the standardized coefficients (beta).
                  The effect size <b> ME </b> is the completely standardized effect size and it is equal to product of the coefficients involved in the indirect effect.
                  </p>

                  <p> In all cases, you can set the required Type I error rate (significance cut-off)</p>
                  "

INFO<-list()

INFO[["correlation"]] <- common_init
INFO[["glm"]]         <- common_init
INFO[["ttest"]]       <- common_init
INFO[["proportions"]] <- common_init
INFO[["factorial"]]   <- factorial_init
INFO[["mediation"]]   <- mediation_init


INFO2<-list()

INFO2[["peta"]]<-"
             <div>
             <p> Set the <b> Model degrees of freedom</b>. 
              If the model degrees of freedom are not easy to compute, please use the 
             <b> Model definition </b> panel to help you out. <p>
             </div>
             "
INFO2[["eta"]]<-"
             <div>
             <p> In all cases, set the expected <b> R-squared </b> for the full model. 
             For models with only one independent variable the R-squared is calculated
             as the square of the beta coefficients.<p>
             <p> Set the <b> Model degrees of freedom</b>. 
              If the model degrees of freedom are not easy to compute, please use the 
             <b> Model definition </b> panel to help you out. <p>
             </div>
              "
INFO2[["beta"]]<-"
             <div>
             <p> Please notice that in case of multiple regression, the predictors are <b> assumed to be uncorrelated </b>. If correlated predictors
             are expected, please insert the correlations among covariates in the <b>Correlations panel </b> or use the <b>Partial Eta-squared</b> interface.</p>
             <p> In all cases, you can set the required Type I error rate and whether the test will be carried out two-tailed or one-tailed.</b></p>
             <p> For models with more than one independent variable, set the expected <b> R-squared </b> for the full model. 
             For models with only one independent variable the R-squared is calculated
             as the square of the beta coefficients.<p>
             <p> Set the <b> Model degrees of freedom</b>. 
              If the model degrees of freedom are not easy to compute, please use the 
             <b> Model definition </b> panel to help you out. <p>
             </div>
             "
INFO2[["propind"]]<-"
                   <div>
                   In all cases, you can also decide whether the test will be carried out two-tailed or one-tailed.
                   </div>
                   <div>
                   The analysis is carried out based on binomial distribution and arcsine transformation.
                   </div>
                   <div>
                   The default effect size is the odd of the proportions in input (P1/(1-P1)/(P2/(1-P2)). In the <b> Options </b> panel
                   one can choose to use the proportion differences (P1-P2) or the relative risk (P1/P2). Results are equivalent.
                   </div>
                   " 
INFO2[["propone"]]<-"
                   <div>
                   In all cases, you can also decide whether the test will be carried out two-tailed or one-tailed.
                   </div>
                   <div>
                   The analysis is carried out based on binomial distribution and arcsine transformation.
                   </div>
                   <div>
                   The default effect size is the odd of the proportion in input (P1/(1-P1)/(P2/(1-P2)). In the <b> Options </b> panel
                   one can choose to use the proportion differences (P1-P2) or the relative risk (P1/P2). Results are equivalent.
                   </div>
                   " 
INFO2[["proppaired"]]<-"
                   <div>
                   In all cases, you can also decide whether the test will be carried out two-tailed or one-tailed.
                   </div>
                   <div>
                   The analysis is carried out based on  McNemar paired comparison of proportions approximate power calculation.
                   The default effect size is the odd of the proportion in input (P12/P21). P12 is the <b> smaller proportion
                   of discordant pairs </b> and P21 is the largest propotion of discordant pairs.
                   <div>
                   In the <b> Options </b> panel
                   one can choose to use the proportion differences (P12-P21). Results are equivalent.
                   </div>
                   " 
INFO2[["facmeans"]]<-"
             <div>
             <p> Please list in the datasheet at least one <b>factor</b> and the data column containing the groups means and the group standard deviations.
                 If more than one factor is defined, the <b>means</b> and <b>standard deviations</b> should correspond to the means of the cells
                 resulting from the combinations of the factor levels.
              </p>
             </div>
             "
INFO2[["medsimple"]]<-" 
             <div>
             </ul>
              <p> The coefficients <b> X to Mediatior effect (a)</b> and <b> Mediatior to Y effect (b)</b> are the standardized coefficients (beta).
               The effect size <b> ME </b> is the completely standardized effect size and it is equal to <b> a * b </b>.
               <b> X to Y effect (c') </b> is the standardized coefficient relating X to Y while keeping constant the mediation.
               </p>

              <p> In all cases, you can set the required Type I error rate (significance cut-off)</p>
             </div>
             "
INFO2[["medcomplex"]]<-"
             <div>
             </ul>
              <p> The coefficients (<b> X to M1 effect (a1)</b>, <b> M1 to Y effect (b1)</b> etc.) are the standardized coefficients (beta).
               The effect size <b> ME </b> is the completely standardized effect size and it is equal to product of the coefficients involved in the indirect effect.
               </p>

              <p> In all cases, you can set the required Type I error rate (significance cut-off)</p>
             </div>
             "


LINKS<-list()

LINKS[["peta"]]<-"https://pamlj.github.io/glm_peta.html"
LINKS[["beta"]]<-"https://pamlj.github.io/glm_beta.html"
LINKS[["eta"]]<-"https://pamlj.github.io/glm_eta.html"
LINKS[["correlation"]]<-"https://pamlj.github.io/correlation.html"
LINKS[["propind"]]<-"https://pamlj.github.io/prop_ind.html"
LINKS[["propone"]]<-"https://pamlj.github.io/prop_one.html"
LINKS[["proppaired"]]<-"https://pamlj.github.io/prop_paired.html"
LINKS[["facpeta"]]<-"https://pamlj.github.io/factorial_peta.html"
LINKS[["facmeans"]]<-"https://pamlj.github.io/factorial_means.html"
LINKS[["ttestind"]]<-"https://pamlj.github.io/ttest_ind.html"
LINKS[["ttestpone"]]<-"https://pamlj.github.io/ttest_one.html"
LINKS[["ttestpaired"]]<-"https://pamlj.github.io/ttest_paired.html"
LINKS[["mediation"]]<-"https://pamlj.github.io/mediation.html"
