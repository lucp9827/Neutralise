Neutralise: an open source initiative for neutral comparison of
two-sample tests
================

# 1. Load required packages and Installation steps

## Github installation

Code to install *Neutralise*, with authentication code for permission.
(Authentication code needs be deleted when the resporitory is set
public)

``` r
library(devtools)

#install_github("lucp9827/Neutralise",auth_token="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",force=TRUE)

library(Neutralise)
```

## Initialise local directory structure

Download ‘NeutraliseFiles.zip’ from
‘<https://github.com/lucp9827/NeutraliseFiles>’. Unzip the
‘NeutraliseFiles’ file and save the path to this unzipped
NeutraliseFiles folder.

``` r
# Example:
path = "C:\\Users\\lucp9827\\Desktop\\R_codes & Data\\NeutraliseFiles"
```

NeutraliseFiles contains 4 directories;

1.  **Data**: This Folder contains the available data generator
    functions.
2.  **Settings**: This Folder contains the settings for the existing
    data generators. If the settings have been simulated, the setting
    files change from an R-data file extension to an R workspace.
3.  **Methods**: This Folder contains the available statistical methods.
4.  **Results**: This folder contains the result of all combinations of
    statistical methods and simulation scenarios available, and a text
    file ‘Finished’ with information on the simulation runs already
    done, and an R-file describing the status of all methods, data
    generators and settings, and a Local folder for the results of the
    *single* operating mode.

# 2. Demonstration Neutralise

We will demonstrate two operating modes of Neutralise; **single** and
**all**. First, we demonstrate the operating mode *single*, which allows
a user to evaluate a single statistical method under a single simulation
scenario. Second, we demonstrate the operating mode *all*, which allows
the user to evaluate a new statistical method under all scenario’s
available on Github and compare the results to other statistical methods
on Github. It’s also possible to evaluate all statistical methods
available on Github under a new simulation scenario.

## 2.1 Operating mode: *Single*

Suppose that you want to evaluate a method on a simulation scenario, but
you are still in an “experimental stage” of your research and so you do
not (yet) want to add your method or data generator method to the
system. Then you can run the Neutralise in “single” mode. For example,
we have a new method (here Anderson Darling).

``` r
AD_Asymp<-function(db) {
  results<-kSamples::ad.test(db$y[db$group==1],db$y[db$group==2],
                   method="asymptotic")
  return(list(
    stat=results$ad[1,1],
    p.value=results$ad[1,3]
  ))
}
```

and we will use the following data generator

``` r
ExpLocShift<-function(n1=10,n2=10,parameters=c(0,1)) {
  delta<-parameters[1]
  rate1<-parameters[2]
  y<-c(rexp(n1,rate = rate1),
       rexp(n2,rate = rate1)+delta)
  db<-data.frame(y=y, group=rep(c(1,2),c(n1,n2)))
  return(db)
}
```

and the following settings

``` r
settings<-data.frame(
  delta=c(0,1,2,3),
  rate1=c(1,0.5,0.25,0.125),
  null=c(1,0,0,0)
)
```

Then we can run the following code. This will NOT add the new method and
data generator to the *Methods* and *Data* directories, the results will
be saved to the directory Results/Local so that the results are not
mixed with the other results. However, the format in which the results
are saved is the same as for the “all” mode. The power results are also
in the object “res” so that the user does not necessarily has to go to
the Results/Local directory.

``` r
res<-Neutralise(path,Test=AD_Asymp,
                Data.Generator = ExpLocShift,
                settings=settings)
```

## 2.2 Operating mode: *All*

To run all combinations of statistical methods and simulation scenarios
(i.e. combination of data generator and settings) available on Github
the following code can be used and the results will be saved in the
*Results* directory. This function can be used to reproduce results of
previous simulations. Note: To reproduce results it’s important to have
(1) an ‘empty’ *Results* directory and (2) to reinitialise the files
*Finished.txt* and *neutralise_status.Rfile*. Also (3) add the specific
settings RData-files that you want to reproduce into the *Settings*
directory.

``` r
# Code to reinitilize the files *Finished.txt* and *neutralise_status.Rfile (dont run if you don't want to reproduce all results)

#Initialise_Neutralise(path)

# Run Neutralise main function 

res<-Neutralise(path)
```

Suppose that we now have some new additional settings to be run for a
data generator that already exists. Then we add a new settings R-file to
the directory *Settings* and run the neutralise function again. We’ll
first demonstrate how a new settings R-file should be constructed.

### Adding a new setting to the settings directory

Make sure the R-file for the new setting begins with the same name as
the existing Data Generator R-file (in *Data* directory) for which the
setting is developed and ends with ‘\_settings.R’.

``` r
# Add setting for the 'Normal data generator' in the  Settings directory

settings<-data.frame(
  delta=c(0,0.5,1.5,2.5),
  sd=c(1,3,3,3),
  null=c(1,0,0,0)
)

# The setting will be saved in the Settings directory
save(settings,
       file=paste(path,"/Settings/Normal_settings.R",sep=""))

# Run the Neutralise main function

res<-Neutralise(path)
```

    ## Warning in remove("settings"): object 'settings' not found

Suppose that we now have an new statistical method to be run for the
data generators that already exists. Then we’ll add a new method’s
R-file to the directory *Methods* and run the code again. We’ll first
demonstrate how a new method’s R-file should be constructed, this ‘code
chunck’ should be saved by the user as ‘KS.R’ in the *Methods*
directory.

### Adding a new statistical test to the methods directory

Statistical tests can be added to the system as an R file with a single
function. This function must be named **Test** and specify one argument
**bd**, which is a data frame with two columns. One column must be named
**y** and contains the *n\~1\~ + n\~2\~* sample observations, and the
other column must be named **group** and contains values 1 and 2,
referring to the groups the sample obserations come from. The R-file
must contain a header that gives a **name** and a **description** of the
method and references to the literature if appropriate. Also, add 1
empty line at the end of the R-file.

``` r
# NAME
# Asymptotic Kolmogorov-Smirnov test
# DESCRIPTION
# Two sample Kolmogorov-Smirnov test . P-values based on asymptotic approximation
# REFERENCES
# Kolmogorov, A. 1933. Sulla Determinazione Empirica di una Legge di Distributione. Giornale dell'Istituto Italiano degli Attuari, 4: 1-11.
# Smirnov, H. 1939. Sur les Ecarts de la Courbe de Distribution Empirique. Recueil MathematiqueMatematiceskii Sbornik), : 3-26. N.S. 6
# END

Test<-function(db) {
    results<-ks.test(db$y[db$group==1],db$y[db$group==2],
                     alternative="two.sided")
    return(list(
      stat=results$statistic,
      p.value=results$p.value
    ))
}
```

After saving the new method in the *Methods* directory as an R-file, we
can run the main neutralise function. The results of the new simulations
can be found in the *Results* directory.

``` r
res<-Neutralise(path)
```

## 3. Upload code or results to Github

This initiative is set up as an open science collaboration, and we hope
that many users will make use of it and return the code and results to
enhance this initiative. It just takes 5 steps! New users to github can
find a quickstart guide of Github on the following website:
<https://docs.github.com/en/get-started/quickstart/hello-world>

*Step 1*: Go to github.com/login and sign in

*Step 2*: Go to <https://github.com/lucp9827/NeutraliseFiles>

*Step 3*: Fork (copy) the repository ‘NeutraliseFiles’ to your own
account.
(<https://docs.github.com/en/get-started/quickstart/fork-a-repo>)

*Step 4*: Commit changes made to ‘NeutraliseFiles’ folders: Add new
method, data generator or setting file to the correct directories.
Commit (add) additional results as zip file.

*Step 5*: Create pull request to merge changes (commits) with the
original NeutraliseFiles.

All the pull request will be checked by the coreteam and if validated
they will be added to the repository.

## 4. Evaluate - report

## Report results - Power

``` r
#load(paste(path,'\\Results\\NeutraliseStatus.RData',sep=''))

library(dplyr)

# All methods & data generation methods that have been 'neutralised'

methods = All_Neutralised(path)
data.gen = All_Neutralised(path, type='data')

method1=methods[2] # CVM
method2=methods[5] # "TTest_VarUnequal"

# Power comparison between two methods - general 

Graph1 = Power_QQ(path,method1,method2,alpha=0.05)
```

    ## TTest_VarUnequal wins over CVM in 36.1% of the 36 scenarios

``` r
Graph1$graph
```

![](Demonstration_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Power Comparison between a fixed method (e.g. CVM) and all other methods - general 

Graphs = list()
for (m in methods){
  if(m != method1){
  Graphs[[m]] = Power_QQ(path,m,method1,alpha=0.05)$graph
  print("")}
}
```

    ## CVM wins over AD in 36.1% of the 36 scenarios[1] ""
    ## CVM wins over KS in 88.9% of the 36 scenarios[1] ""
    ## CVM wins over TTest_VarEqual in 44.4% of the 36 scenarios[1] ""
    ## CVM wins over TTest_VarUnequal in 55.6% of the 36 scenarios[1] ""
    ## CVM wins over WMW_Asymp in 52.8% of the 36 scenarios[1] ""

``` r
ggarrange(plotlist=Graphs)
```

![](Demonstration_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
# Power comparison between two methods - specified Data Generator = Logistic

Graph1b = Power_QQ(path,method1,method2,alpha=0.05,data='Logistic')
```

    ## TTest_VarUnequal wins over CVM in 16.7% of the 12 scenarios

``` r
Graph1b$graph
```

![](Demonstration_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
Graphsb = list()
for (m in methods){
  if(m != method1){
  Graphsb[[m]] = Power_QQ(path,m,method1,alpha=0.05,data='Logistic')$graph
  print("")}}
```

    ## CVM wins over AD in 66.7% of the 12 scenarios[1] ""
    ## CVM wins over KS in 100% of the 12 scenarios[1] ""
    ## CVM wins over TTest_VarEqual in 66.7% of the 12 scenarios[1] ""
    ## CVM wins over TTest_VarUnequal in 83.3% of the 12 scenarios[1] ""
    ## CVM wins over WMW_Asymp in 66.7% of the 12 scenarios[1] ""

``` r
ggarrange(plotlist=Graphsb)
```

![](Demonstration_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->

``` r
# Power comparison between two methods - per data generation method 
Graph2 = Power_QQ(path,methods[2],methods[5],alpha=0.01,group=TRUE)
```

    ## Logistic: TTest_VarUnequal wins over CVM in 25% of the 12 scenarios
    ## Normal2Var: TTest_VarUnequal wins over CVM in 50% of the 12 scenarios
    ## Normal: TTest_VarUnequal wins over CVM in 58.3% of the 12 scenarios
    ## TTest_VarUnequal wins over CVM in 44.4% of the 36 scenarios

``` r
Graph2$graph
```

![](Demonstration_files/figure-gfm/unnamed-chunk-11-5.png)<!-- -->

``` r
Graphs2 = list()
for (m in methods){
  if(m != method1){
  Graphs2[[m]] = Power_QQ(path,m,method1,alpha=0.05,group=TRUE)$graph
  print("")}}
```

    ## Logistic: CVM wins over AD in 66.7% of the 12 scenarios
    ## Normal2Var: CVM wins over AD in 16.7% of the 12 scenarios
    ## Normal: CVM wins over AD in 25% of the 12 scenarios
    ## CVM wins over AD in 36.1% of the 36 scenarios[1] ""
    ## Logistic: CVM wins over KS in 100% of the 12 scenarios
    ## Normal2Var: CVM wins over KS in 75% of the 12 scenarios
    ## Normal: CVM wins over KS in 91.7% of the 12 scenarios
    ## CVM wins over KS in 88.9% of the 36 scenarios[1] ""
    ## Logistic: CVM wins over TTest_VarEqual in 66.7% of the 12 scenarios
    ## Normal2Var: CVM wins over TTest_VarEqual in 50% of the 12 scenarios
    ## Normal: CVM wins over TTest_VarEqual in 16.7% of the 12 scenarios
    ## CVM wins over TTest_VarEqual in 44.4% of the 36 scenarios[1] ""
    ## Logistic: CVM wins over TTest_VarUnequal in 83.3% of the 12 scenarios
    ## Normal2Var: CVM wins over TTest_VarUnequal in 33.3% of the 12 scenarios
    ## Normal: CVM wins over TTest_VarUnequal in 50% of the 12 scenarios
    ## CVM wins over TTest_VarUnequal in 55.6% of the 36 scenarios[1] ""
    ## Logistic: CVM wins over WMW_Asymp in 66.7% of the 12 scenarios
    ## Normal2Var: CVM wins over WMW_Asymp in 50% of the 12 scenarios
    ## Normal: CVM wins over WMW_Asymp in 41.7% of the 12 scenarios
    ## CVM wins over WMW_Asymp in 52.8% of the 36 scenarios[1] ""

``` r
ggarrange(plotlist=Graphs2)
```

![](Demonstration_files/figure-gfm/unnamed-chunk-11-6.png)<!-- -->

``` r
# Power comparison between two methods - per data generation method and a specified setting parameter (delta = 0.5 --> difference in means of 0.5)

db=data.frame(delta=0.5) # important that the specified parameter is exactly the same as in the setting file 
Graph3 = Power_QQ(path,methods[2],methods[5],alpha=0.05,group=TRUE,par.fix=db)
```

    ## Logistic: TTest_VarUnequal wins over CVM in 25% of the 4 scenarios
    ## Normal2Var: TTest_VarUnequal wins over CVM in NaN% of the 0 scenarios
    ## Normal: TTest_VarUnequal wins over CVM in 25% of the 4 scenarios
    ## TTest_VarUnequal wins over CVM in 25% of the 8 scenarios

``` r
Graph3$graph
```

![](Demonstration_files/figure-gfm/unnamed-chunk-11-7.png)<!-- -->

``` r
Graphs3 = list()
for (m in methods){
  if(m != method1){
  Graphs3[[m]] = Power_QQ(path,m,method1,alpha=0.05,group=TRUE,par.fix=db)$graph
  print("")}}
```

    ## Logistic: CVM wins over AD in 75% of the 4 scenarios
    ## Normal2Var: CVM wins over AD in NaN% of the 0 scenarios
    ## Normal: CVM wins over AD in 25% of the 4 scenarios
    ## CVM wins over AD in 50% of the 8 scenarios[1] ""
    ## Logistic: CVM wins over KS in 100% of the 4 scenarios
    ## Normal2Var: CVM wins over KS in NaN% of the 0 scenarios
    ## Normal: CVM wins over KS in 100% of the 4 scenarios
    ## CVM wins over KS in 100% of the 8 scenarios[1] ""
    ## Logistic: CVM wins over TTest_VarEqual in 75% of the 4 scenarios
    ## Normal2Var: CVM wins over TTest_VarEqual in NaN% of the 0 scenarios
    ## Normal: CVM wins over TTest_VarEqual in 50% of the 4 scenarios
    ## CVM wins over TTest_VarEqual in 62.5% of the 8 scenarios[1] ""
    ## Logistic: CVM wins over TTest_VarUnequal in 75% of the 4 scenarios
    ## Normal2Var: CVM wins over TTest_VarUnequal in NaN% of the 0 scenarios
    ## Normal: CVM wins over TTest_VarUnequal in 75% of the 4 scenarios
    ## CVM wins over TTest_VarUnequal in 75% of the 8 scenarios[1] ""
    ## Logistic: CVM wins over WMW_Asymp in 75% of the 4 scenarios
    ## Normal2Var: CVM wins over WMW_Asymp in NaN% of the 0 scenarios
    ## Normal: CVM wins over WMW_Asymp in 75% of the 4 scenarios
    ## CVM wins over WMW_Asymp in 75% of the 8 scenarios[1] ""

``` r
ggarrange(plotlist=Graphs3)
```

![](Demonstration_files/figure-gfm/unnamed-chunk-11-8.png)<!-- -->

## Report results - Type 1 error

``` r
# All methods & data generation methods that have been 'neutralised'

methods = All_Neutralised(path)
data.gen = All_Neutralised(path, type='data')

method1=methods[2]

# Boxplot of type  error for a specified method - boxplot: over all data generators & scenarios - Every printed row is a specific setting scenario for a data generator. In the Results folder for the specified method(in combination with a data generator), you can find the specifics for all the scenario parameters

Graph1_type1error = Boxplot_TypeI(path,method1)
```

    ## Logistic with a total sample size of 20:
    ## CVM has on average a type I error rate of 1.15% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## CVM has on average a type I error rate of 1.45% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## CVM has on average a type I error rate of 1.32% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## CVM has on average a type I error rate of 1.65% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.67% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.32% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.9% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.45% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## CVM has on average a type I error rate of 5.79% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## CVM has on average a type I error rate of 1.96% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.3% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.22% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.12% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.14% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ## On average (over all scenarios) :CVM has a type I error rate not larger than 0.05 + 0.02 in 79.2% of the scenarios.

``` r
Graph1_type1error
```

    ## Warning: Removed 5 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 5 rows containing missing values (geom_point).

![](Demonstration_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
Graphs_type1error= list()
for (m in methods){
  
  Graphs_type1error[[m]] = Boxplot_TypeI(path,m)
}
```

    ## Logistic with a total sample size of 20:
    ## AD has on average a type I error rate of 1.23% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## AD has on average a type I error rate of 1.27% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## AD has on average a type I error rate of 1.1% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## AD has on average a type I error rate of 1.32% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## AD has on average a type I error rate of 0.54% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## AD has on average a type I error rate of 0.31% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## AD has on average a type I error rate of 1.33% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## AD has on average a type I error rate of 0.5% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## AD has on average a type I error rate of 6.23% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## AD has on average a type I error rate of 3.66% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## AD has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## AD has on average a type I error rate of 0.13% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## AD has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## AD has on average a type I error rate of 0.15% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## AD has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## AD has on average a type I error rate of 0.17% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## AD has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## AD has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## AD has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## AD has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## AD has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## AD has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## AD has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## AD has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## AD has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## AD has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## AD has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## AD has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ## On average (over all scenarios) :AD has a type I error rate not larger than 0.05 + 0.02 in 79.2% of the scenarios.
    ## Logistic with a total sample size of 20:
    ## CVM has on average a type I error rate of 1.15% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## CVM has on average a type I error rate of 1.45% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## CVM has on average a type I error rate of 1.32% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## CVM has on average a type I error rate of 1.65% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.67% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.32% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.9% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.45% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## CVM has on average a type I error rate of 5.79% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## CVM has on average a type I error rate of 1.96% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.3% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.22% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.12% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.14% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ## On average (over all scenarios) :CVM has a type I error rate not larger than 0.05 + 0.02 in 79.2% of the scenarios.
    ## Logistic with a total sample size of 20:
    ## KS has on average a type I error rate of 0.4% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## KS has on average a type I error rate of 0.7% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## KS has on average a type I error rate of 0.9% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## KS has on average a type I error rate of 0.88% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## KS has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## KS has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## KS has on average a type I error rate of 0.63% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## KS has on average a type I error rate of 0.34% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## KS has on average a type I error rate of 4.96% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## KS has on average a type I error rate of 1.49% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## KS has on average a type I error rate of 0.34% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## KS has on average a type I error rate of 0.18% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## KS has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## KS has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## KS has on average a type I error rate of 0.12% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## KS has on average a type I error rate of 0.14% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## KS has on average a type I error rate of 0.03% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## KS has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## KS has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## KS has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## KS has on average a type I error rate of 0.03% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## KS has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## KS has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## KS has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## KS has on average a type I error rate of 0.02% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## KS has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## KS has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## KS has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## KS has on average a type I error rate of 0.02% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## KS has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## KS has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## KS has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## KS has on average a type I error rate of 0.02% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## KS has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## KS has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## KS has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ## On average (over all scenarios) :KS has a type I error rate not larger than 0.05 + 0.02 in 87.5% of the scenarios.
    ## Logistic with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 1.25% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 1.32% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 1.15% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 1.47% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.32% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.29% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.38% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.34% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.22% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.33% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.14% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.18% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.14% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.13% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ## On average (over all scenarios) :TTest_VarEqual has a type I error rate not larger than 0.05 + 0.02 in 100% of the scenarios.
    ## Logistic with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 1.1% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 1.05% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 1.03% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 1.25% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.32% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.25% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.32% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.36% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.31% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.26% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.28% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.35% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.17% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.18% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.14% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ## On average (over all scenarios) :TTest_VarUnequal has a type I error rate not larger than 0.05 + 0.02 in 100% of the scenarios.
    ## Logistic with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 1.07% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 1.15% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 1.57% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 1.05% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.27% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.28% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.43% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.41% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.3% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.35% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.15% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.12% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ## On average (over all scenarios) :WMW_Asymp has a type I error rate not larger than 0.05 + 0.02 in 100% of the scenarios.

``` r
ggarrange(plotlist = Graphs_type1error)
```

    ## Warning: Removed 5 rows containing non-finite values (stat_boxplot).
    ## Removed 5 rows containing missing values (geom_point).

    ## Warning: Removed 5 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 5 rows containing missing values (geom_point).

    ## Warning: Removed 3 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 3 rows containing missing values (geom_point).

![](Demonstration_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
# Boxplot of type  error for a specified method - boxplot: per data generator

Graph2_type1error = Boxplot_TypeI(path,method1,panel="distribution")
```

    ## Logistic with a total sample size of 20:
    ## CVM has on average a type I error rate of 1.15% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## CVM has on average a type I error rate of 1.45% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## CVM has on average a type I error rate of 1.32% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## CVM has on average a type I error rate of 1.65% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.67% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.32% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.9% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.45% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## CVM has on average a type I error rate of 5.79% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## CVM has on average a type I error rate of 1.96% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.3% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.22% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.12% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.14% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ## On average (over all scenarios) :CVM has a type I error rate not larger than 0.05 + 0.02 in 79.2% of the scenarios.

``` r
Graph2_type1error
```

    ## Warning: Removed 5 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 5 rows containing missing values (geom_point).

![](Demonstration_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
Graphs2_type1error= list()
for (m in methods){
  
  Graphs2_type1error[[m]] = Boxplot_TypeI(path,m,panel="distribution")
}
```

    ## Logistic with a total sample size of 20:
    ## AD has on average a type I error rate of 1.23% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## AD has on average a type I error rate of 1.27% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## AD has on average a type I error rate of 1.1% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## AD has on average a type I error rate of 1.32% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## AD has on average a type I error rate of 0.54% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## AD has on average a type I error rate of 0.31% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## AD has on average a type I error rate of 1.33% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## AD has on average a type I error rate of 0.5% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## AD has on average a type I error rate of 6.23% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## AD has on average a type I error rate of 3.66% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## AD has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## AD has on average a type I error rate of 0.13% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## AD has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## AD has on average a type I error rate of 0.15% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## AD has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## AD has on average a type I error rate of 0.17% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## AD has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## AD has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## AD has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## AD has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## AD has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## AD has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## AD has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## AD has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## AD has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## AD has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## AD has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## AD has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ## On average (over all scenarios) :AD has a type I error rate not larger than 0.05 + 0.02 in 79.2% of the scenarios.
    ## Logistic with a total sample size of 20:
    ## CVM has on average a type I error rate of 1.15% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## CVM has on average a type I error rate of 1.45% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## CVM has on average a type I error rate of 1.32% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## CVM has on average a type I error rate of 1.65% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.67% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.32% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.9% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.45% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## CVM has on average a type I error rate of 5.79% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## CVM has on average a type I error rate of 1.96% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.3% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.22% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.12% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.14% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ## On average (over all scenarios) :CVM has a type I error rate not larger than 0.05 + 0.02 in 79.2% of the scenarios.
    ## Logistic with a total sample size of 20:
    ## KS has on average a type I error rate of 0.4% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## KS has on average a type I error rate of 0.7% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## KS has on average a type I error rate of 0.9% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## KS has on average a type I error rate of 0.88% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## KS has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## KS has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## KS has on average a type I error rate of 0.63% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## KS has on average a type I error rate of 0.34% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## KS has on average a type I error rate of 4.96% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## KS has on average a type I error rate of 1.49% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## KS has on average a type I error rate of 0.34% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## KS has on average a type I error rate of 0.18% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## KS has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## KS has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## KS has on average a type I error rate of 0.12% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## KS has on average a type I error rate of 0.14% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## KS has on average a type I error rate of 0.03% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## KS has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## KS has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## KS has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## KS has on average a type I error rate of 0.03% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## KS has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## KS has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## KS has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## KS has on average a type I error rate of 0.02% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## KS has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## KS has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## KS has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## KS has on average a type I error rate of 0.02% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## KS has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## KS has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## KS has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## KS has on average a type I error rate of 0.02% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## KS has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## KS has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## KS has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ## On average (over all scenarios) :KS has a type I error rate not larger than 0.05 + 0.02 in 87.5% of the scenarios.
    ## Logistic with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 1.25% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 1.32% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 1.15% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 1.47% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.32% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.29% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.38% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.34% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.22% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.33% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.14% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.18% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.14% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.13% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ## On average (over all scenarios) :TTest_VarEqual has a type I error rate not larger than 0.05 + 0.02 in 100% of the scenarios.
    ## Logistic with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 1.1% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 1.05% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 1.03% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 1.25% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.32% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.25% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.32% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.36% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.31% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.26% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.28% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.35% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.17% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.18% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.14% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ## On average (over all scenarios) :TTest_VarUnequal has a type I error rate not larger than 0.05 + 0.02 in 100% of the scenarios.
    ## Logistic with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 1.07% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 1.15% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 1.57% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 1.05% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.27% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.28% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.43% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.41% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.3% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.35% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.15% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.12% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ## On average (over all scenarios) :WMW_Asymp has a type I error rate not larger than 0.05 + 0.02 in 100% of the scenarios.

``` r
ggarrange(plotlist = Graphs2_type1error)
```

    ## Warning: Removed 5 rows containing non-finite values (stat_boxplot).
    ## Removed 5 rows containing missing values (geom_point).

    ## Warning: Removed 5 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 5 rows containing missing values (geom_point).

    ## Warning: Removed 3 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 3 rows containing missing values (geom_point).

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](Demonstration_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->

``` r
# Boxplot of type  error for a specified method - boxplot: per sample size

Graph3_type1error = Boxplot_TypeI(path,method1,panel="n")
```

    ## Logistic with a total sample size of 20:
    ## CVM has on average a type I error rate of 1.15% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## CVM has on average a type I error rate of 1.45% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## CVM has on average a type I error rate of 1.32% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## CVM has on average a type I error rate of 1.65% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.67% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.32% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.9% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.45% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## CVM has on average a type I error rate of 5.79% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## CVM has on average a type I error rate of 1.96% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.3% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.22% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.12% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.14% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ## On average (over all scenarios) :CVM has a type I error rate not larger than 0.05 + 0.02 in 79.2% of the scenarios.

``` r
Graph3_type1error
```

    ## Warning: Removed 5 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 5 rows containing missing values (geom_point).

![](Demonstration_files/figure-gfm/unnamed-chunk-12-5.png)<!-- -->

``` r
Graphs3_type1error= list()
for (m in methods){
  
  Graphs3_type1error[[m]] = Boxplot_TypeI(path,m,panel='n')
}
```

    ## Logistic with a total sample size of 20:
    ## AD has on average a type I error rate of 1.23% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## AD has on average a type I error rate of 1.27% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## AD has on average a type I error rate of 1.1% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## AD has on average a type I error rate of 1.32% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## AD has on average a type I error rate of 0.54% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## AD has on average a type I error rate of 0.31% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## AD has on average a type I error rate of 1.33% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## AD has on average a type I error rate of 0.5% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## AD has on average a type I error rate of 6.23% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## AD has on average a type I error rate of 3.66% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## AD has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## AD has on average a type I error rate of 0.13% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## AD has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## AD has on average a type I error rate of 0.15% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## AD has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## AD has on average a type I error rate of 0.17% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## AD has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## AD has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## AD has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## AD has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## AD has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## AD has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## AD has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## AD has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## AD has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## AD has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## AD has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## AD has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## AD has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ## On average (over all scenarios) :AD has a type I error rate not larger than 0.05 + 0.02 in 79.2% of the scenarios.
    ## Logistic with a total sample size of 20:
    ## CVM has on average a type I error rate of 1.15% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## CVM has on average a type I error rate of 1.45% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## CVM has on average a type I error rate of 1.32% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## CVM has on average a type I error rate of 1.65% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.67% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.32% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.9% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.45% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## CVM has on average a type I error rate of 5.79% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## CVM has on average a type I error rate of 1.96% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.3% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.22% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.12% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.14% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## CVM has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## CVM has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## CVM has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ## On average (over all scenarios) :CVM has a type I error rate not larger than 0.05 + 0.02 in 79.2% of the scenarios.
    ## Logistic with a total sample size of 20:
    ## KS has on average a type I error rate of 0.4% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## KS has on average a type I error rate of 0.7% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## KS has on average a type I error rate of 0.9% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## KS has on average a type I error rate of 0.88% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## KS has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## KS has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## KS has on average a type I error rate of 0.63% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## KS has on average a type I error rate of 0.34% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## KS has on average a type I error rate of 4.96% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## KS has on average a type I error rate of 1.49% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## KS has on average a type I error rate of 0.34% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## KS has on average a type I error rate of 0.18% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## KS has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## KS has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## KS has on average a type I error rate of 0.12% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## KS has on average a type I error rate of 0.14% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## KS has on average a type I error rate of 0.03% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## KS has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## KS has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## KS has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## KS has on average a type I error rate of 0.03% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## KS has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## KS has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## KS has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## KS has on average a type I error rate of 0.02% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## KS has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## KS has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## KS has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## KS has on average a type I error rate of 0.02% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## KS has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## KS has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## KS has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## KS has on average a type I error rate of 0.02% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## KS has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## KS has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## KS has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ## On average (over all scenarios) :KS has a type I error rate not larger than 0.05 + 0.02 in 87.5% of the scenarios.
    ## Logistic with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 1.25% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 1.32% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 1.15% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 1.47% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.32% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.29% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.38% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.34% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.22% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.33% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.04% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.14% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.18% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.14% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.13% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarEqual has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarEqual has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ## On average (over all scenarios) :TTest_VarEqual has a type I error rate not larger than 0.05 + 0.02 in 100% of the scenarios.
    ## Logistic with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 1.1% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 1.05% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 1.03% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 1.25% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.32% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.25% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.32% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.36% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.31% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.26% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.28% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.35% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.17% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.18% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.14% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## TTest_VarUnequal has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## TTest_VarUnequal has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## TTest_VarUnequal has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## TTest_VarUnequal has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ## On average (over all scenarios) :TTest_VarUnequal has a type I error rate not larger than 0.05 + 0.02 in 100% of the scenarios.
    ## Logistic with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 1.07% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 1.15% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 1.57% at the nominal 0.05 level.
    ##  Logistic with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 1.05% at the nominal 0.05 level.
    ## Normal2Var with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.27% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.28% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.43% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.41% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.3% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.35% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal2Var with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.11% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.16% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.15% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.12% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.1% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.09% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.08% at the nominal 0.05 level.
    ## Normal with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 20:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ##  Normal with a total sample size of 40:
    ## WMW_Asymp has on average a type I error rate of 0.07% at the nominal 0.05 level.
    ##  Normal with a total sample size of 200:
    ## WMW_Asymp has on average a type I error rate of 0.06% at the nominal 0.05 level.
    ##  Normal with a total sample size of 110:
    ## WMW_Asymp has on average a type I error rate of 0.05% at the nominal 0.05 level.
    ## On average (over all scenarios) :WMW_Asymp has a type I error rate not larger than 0.05 + 0.02 in 100% of the scenarios.

``` r
ggarrange(plotlist = Graphs3_type1error)
```

    ## Warning: Removed 5 rows containing non-finite values (stat_boxplot).
    ## Removed 5 rows containing missing values (geom_point).

    ## Warning: Removed 5 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 5 rows containing missing values (geom_point).

    ## Warning: Removed 3 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 3 rows containing missing values (geom_point).

![](Demonstration_files/figure-gfm/unnamed-chunk-12-6.png)<!-- -->
