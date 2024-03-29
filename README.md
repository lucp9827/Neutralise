Neutralise: an open source initiative for neutral comparison of
two-sample tests
================

The two-sample problem is one of the earliest problems in statistics: given two samples, the question is whether or not the observations were sampled from the same distribution. Many statistical tests have been developed for this problem, and many tests have been evaluated in simulation studies, but hardly any study has tried to set up a neutral comparison study. In this project, we introduce an open science initiative that potentially allows for neutral comparisons of two-sample tests. It is designed as an open-source R package, a repository (NeutralisFiles), and an online R Shiny app (https://dsi-uhasselt.shinyapps.io/Neutralise/).

In this tutorial we will demonstrate how to use the Neutralise function
for first time users. In section 1, we go through the steps to download
the Neutralise package and define the required packages. In section 2,
we demonstrate the main Neutralise function in two operating modes
(single and all) and explain how to add new methods and/or data generation
methods and settings to the local NeutraliseFiles directory. The third section shows several functions to
visualise the results from Neutralise. And the final section gives a
brief overview on how to add code/results to this initiative. We refer
to the <https://github.com/lucp9827/NeutraliseFiles> for all the results
available in this initiative, an annual report on these results and more
information on the details (how to upload results/code, code format,
bugs,…) of this initiative.

# 1. Load required packages and Installation steps

The following packages are suggested and are needed for full functionality. These packages are needed to use all included methods and data generation methods in NeutraliseFiles. Not installing these will not hinder the installation of Neutralise and its functions, it will however not be able to use the specific function for a method or data generation method in NeurtaliseFiles. Neutralise will recognize this and provide the information in the Issues folder of NeutraliseFiles (see subsection 1.2). 
``` r
knitr::opts_chunk$set(echo = TRUE)

# Install and load required packages
reqpkg = c("remotes","kSamples", "lawstat", "BWStest", "RVAideMemoire", "DescTools", "WRS2", "gld", "gk", "twosamples")
# Install and Load all required packages
for(i in reqpkg)
{
print(i)
tryCatch(
    expr = {
     install.packages(i, dependencies=TRUE)
     library(i, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, character.only=TRUE)
      cat("Successfully installed", i, "\n")
    },
    error = function(e) {
      cat("Error installing", i, ":", conditionMessage(e), "\n")
    }
  )
}
```

## 1.1 Neutralise installation

Code to install *Neutralise* from Github.
Note, the installation will require you to install and/or update required R-packages for the Neutralise framework. These packages are dependencies for the Neutralise package.

``` r
# To install remotes package: install.packages("remotes")
remotes::install_github("lucp9827/Neutralise")

library(Neutralise)
```

## 1.2 Initialise local directory structure

To start working with *Neutralise*, you first need to create or download
a local directory structure that contains all directories mentioned
below.

For this tutorial, you can download ‘**NeutraliseFiles_tutorial.zip**’
from ‘<https://github.com/lucp9827/Neutralise>’. Unzip the
‘**NeutraliseFiles_tutorial**’ file and save the path to this unzipped
folder.

``` r
# Set working directory to (unzipped) NeutraliseFiles
path = ".\\NeutraliseFiles_tutorial"
```

NeutraliseFiles contains the following 5 directories;

1.  **Data**: This Folder contains the available data generator
    functions. (no datasets)

2.  **Settings**: This Folder contains the settings for the existing
    data generators. If the settings have been simulated, the setting
    files change from an R-data file extension to an R workspace.

3.  **Issues**: This Folder contains text files with possible issues in
    the functionality and lay-out of the R-files (Data
    generator/Settings file/ Method file) for the Neutralise function.

4.  **Methods**: This Folder contains the available statistical methods.

5.  **Results**: This folder contains the result of all combinations of
    statistical methods and simulation scenarios available, a text file
    ‘Finished’ with information on the simulation runs already done, an
    R-file describing the status of all methods, a Local folder for
    the results of the *single* operating mode and a Reproduce folder for reproducing all the available results. 

    5.A. The output files (method_datagenerationmethod.txt or method_datagenerationmethod.RData) in the results folder contain the following:
    
          - method = method name
          - distribution = data generation method
          - seed = seed number to reproduce the exact results
          - N = number of simulation runs
          - n1 and n2 = sample size per group
          - delta = location shift between the two groups
          - ... parameters that are specific for the data generation method
          - null = 1 if the scenario is under $H_0$, which is used as indicator variable
          - power0.01 = power results with significance level of 1% + l_CI,U_CI which represent the lower and upper confidence limits. 
          - power0.05 = power results with significance level of 5% + l_CI,U_CI which represent the lower and upper confidence limits. 
          - power0.10 = power results with significance level of 10% + l_CI,U_CI which represent the lower and upper confidence limits.
          - ct_0.10 = the number of times $H_0$ gets rejected in the simulation runs with 10% significance level
          - ct_0.05 = the number of times $H_0$ gets rejected in the simulation runs with 5% significance level
          - ct_0.01 = the number of times $H_0$ gets rejected in the simulation runs with 1% significance level
    
<u>**Important note:**</u> the tutorial version of NeutraliseFiles
contains only the files needed to recreate the examples in this
tutorial. The originial NeutraliseFiles-zip folder contains all files
and results already ran in this initiative, visit
‘<https://github.com/lucp9827/NeutraliseFiles>’ for more information.

# 2. Demonstration Neutralise

We will demonstrate two operating modes of *Neutralise*; **single** and
**all**.

- the operating mode ***single*:** this mode allows a user to evaluate
  <u>a single statistical method</u> under <u>a single simulation
  scenario</u>.

- the operating mode ***all***: this allows the user

  1.  to evaluate a new statistical method under <u>all scenario’s</u>
      present in NeutraliseFiles and compare the results to <u>all other
      statistical methods</u> in NeutraliseFiles.

  2.  to evaluate <u>all statistical methods available</u> in
      NeutraliseFiles under a new simulation scenario.

## 2.1 Operating mode: *Single*

Suppose that you want to evaluate a method on a simulation scenario, but
you are still in an “experimental stage” of your research and you do not
want to add your method or data generator method to the system of
NeutraliseFiles (yet). Then you can run the *Neutralise* function in
“single” mode.

<u>**Important note:**</u> The comments in the following code chunks
specify how the functions should be coded to run *Neutralise* in the
“single” mode.

As an example, we have a new method (here Anderson Darling).

``` r
# Funtion that applies Anderson Darling 
# Input parameter is a data frame 'db' with two columns that contains the simulated data (db$y) and group allocation of the observations (db$group, 1=group1 & 2=group2). 
# Output: list with 2 elements: 1) 'stat', saves the teststatistic for every test. 2) 'p.value', saves the p-value for every test. 
# If the funtcion depends on an other package define the function as 'package::functionname'

AD_Asymp<-function(db) {
  results<-kSamples::ad.test(db$y[db$group==1],db$y[db$group==2],
                   method="asymptotic")
  return(list(
    stat=results$ad[1,1],
    p.value=results$ad[1,3]
  ))
}


```

We will use the following data generator which is based on the
Exponential distribution,

``` r
# Data generator function to simulate observations for 2 groups from the exponential distribution. 
# input parameter 'n1' and 'n2' -> sample size per group: neutralise automatically tests all scenarios for sample sizes: 10-10, 20-20, 100-100, 10-100 
# input parameter 'parameters' -> vector that contains the parameters for the exponential distribution of a specific scenario
# output 'db' contains the simulated data in a data frame. The first column 'y' are the simulated data from the exponential distribution. The second column 'group' defines the group of the observation. 

ExpLocShift<-function(n1=10,n2=10,parameters=c(0,1)) {
  delta<-parameters[1]
  rate1<-parameters[2]
  y<-c(rexp(n1,rate = rate1),
       rexp(n2,rate = rate1)+delta)
  db<-data.frame(y=y, group=rep(c(1,2),c(n1,n2)))
  return(db)
}


```

and the following settings that are specific for the data generator
above. The parameters specified in the code:

- delta is the location shift between two distributions

- rate1 is the rate parameter of the exponential distribution

- null defines if the specific setting is specified to test the type I
  error

``` r
# Settings are saved in a data frame where every parameter for the data generator is defined in a column. The order of the columns must follow the order defined in the data generator function. The last column 'null' is necessary to specify if this setting is specified to test the type I error (1 = type I error).

settings<-data.frame(
  delta=c(0,1,2,3),
  rate1=c(1,0.5,0.25,0.125),
  null=c(1,0,0,0)
)
```

After running the above code chunks, we can run the following code to
run the simulations in the single operating mode.

``` r
# Neutralise function in the single operating mode. 
# Input parameter 'path', path to NeutraliseFiles_tutorial folder
# Input parameter 'Test', name of the function of the method you want to apply
# Input parameter 'Data.Generator', name of the function of the data generator method you want to simulate data from
# Input parameter 'settings', name of the settings object where the scenarios for the data generator are saved
# Input paramter 'N', number of simulations you want to run
# Output object 'res' containt the results of the simulation runs. Refer to the help page of Neutralise for a more detailed description of the results. 

res<-Neutralise(path,
                Test=AD_Asymp,
                Data.Generator = ExpLocShift,
                settings=settings,
                N=1)
```

This will <u>**NOT**</u> add the new method and data generator to the
*Methods* and *Data* directories, but <u>the results will be saved to
the directory Results/Local</u> so that these preliminary results are
not mixed with the other results. However, the format in which the
results are saved is the same as for the “all” operating mode. T<u>he
results are also saved in the object “res”</u> so that the user does not
necessarily has to go to the Results/Local directory.

## 2.2 Operating mode: *All*

To run all combinations of statistical methods and simulation scenarios
(i.e. combination of data generator and settings) available in
NeutraliseFiles_tutorial the following code can be used and the results
will be saved in the *Results* directory.

In this tutorial, we included 3 methods (TTest_VarEqual,
TTest_VarUnequal, WMW_asymp), 1 data generation methods (Normal) and 1
settings file (Normal).

``` r
# The code to intialise is used when you start from an 'empty' Results directory, so no simuations are ran yet. When you start from a filled Results directory you don't need to run the Initialise_Neutralise function.
# Code to initialise/create the files 1) *Finished.txt* which will contain information on the finalised evaluations and 2) *neutralise_status.Rfile*, which will contain information on the status of the different files. Check the help file of Initialise_Neutralise() to get more details. Both files are created in the Results directory.

Initialise_Neutralise(path)

# Run Neutralise main function

# Only the path and the number of simulations are defined, this automatically activates the 'all' operating mode. 

res<-Neutralise(path,N=10000)

# The results are saved in the Results directory.
```

After running the Neutralise function, the setting R-files in the
Settings directory will change to a RData file. If you want to run the
same settings again, you need to change the RData file back to a R File
or save a new setting R-file (see section 2.2.2).

### 2.2.1 Adding a new Data Generation method to the Data directory

If we want to add a new data generation method to the NeutraliseFiles
system, we need to add an R-file to the <u>Data directory</u> that
contains the code to simulate data.

We’ll first demonstrate how a new Data generation method R-file should
be constructed.

First, you add a Description as a comment in the R-file (see following
example). After which you can add the main function to generate the
data. This function may refer to other functions in the same R-file or
to other packages. When there is a package dependency, write the
function in the following way: ‘package::functionname’.

The main function must contain only the following input parameters

- ‘n1’ ,‘n2’ -\> sample size per group: Neutralise automatically tests
  all scenarios for sample sizes: 10-10, 20-20, 100-100, 10-100

- ‘parameters’ -\> vector that has the length of the amount of
  parameters needed to simulate data + 1 (the ‘null’ column, which
  indicates if a setting is specified under the null hypothese-see
  section 2.2.2).

The main function has **only one output, a data frame ’db**’. This data
frame contains the simulated data where the first column ‘y’ are the
simulated data (n<sub>1</sub> + n<sub>2</sub>) . The second column
‘group’ defines the group of the observation (1=group1, 2=group2).

``` r
# DESCRIPTION
# simulation of two normal distributions with unequal variance
Data.Generator<-function(n1=10,n2=10,parameters=c(0,1,2)) {
  delta<-parameters[1]
  sd1<-parameters[2]
  sd2<-parameters[3]
  y<-c(rnorm(n1,sd=sd1),
       rnorm(n2,mean = delta,sd=sd2))
  db<-data.frame(y=y, group=rep(c(1,2),c(n1,n2)))
  return(db)
}


```

Save the code chuck above as an R-file, with the name ‘Normal2Var.R’ in
the Data directory. <u>**Important note:**</u> Before we can run
Neutralise, we need to specify the settings for Normal2Var. This is done
in the following subsection.

### 2.2.2 Adding a new setting to the Settings directory

Suppose that we now have some new additional settings to be run for a
data generator that already exists (The R-file for the data generator is
in the Data directory, ‘Normal2Var’). We will add a new settings R-file
to the directory *Settings* and run the Neutralise function again.

We’ll first demonstrate how a new settings R-file should be constructed:

Settings are saved in a data frame where every parameter for the data
generator is defined in a column. The order of the columns must follow
the order defined in the data generator function. The last column ‘null’
is necessary to specify if this setting is specified to test the type I
error (1 = type I error).

Make sure the R-file for the new setting begins with the same name as
the existing Data Generator R-file (in *Data* directory) for which the
setting is created and ends with ‘\_settings.R’.

``` r
# Add setting for the 'Normal data generator' in the  Settings directory

settings<-data.frame(
  delta=c(0.5,0.5,1.5,2.5),
  sd1=c(1,3,3,3),
  sd2=c(3,1,1,1),
  null=c(0,0,0,0)
)

# The setting will be saved in the Settings directory
save(settings,
       file=paste(path,"/Settings/Normal2Var_settings.R",sep=""))

# Run the Neutralise main function in 'all' mode

res<-Neutralise(path,N=10000)
```

The new results are saved in the Results directory.

### 2.2.3 Adding a new statistical test to the Methods directory

Suppose that we now have an new statistical method we want to analyze on
all the data generators that already exists. We’ll add a new method’s
R-file to the directory *Methods* and run the code again.

We’ll first demonstrate how a new method’s R-file should be constructed,
the following ‘code chunk’ should be saved by the user as ‘KS.R’ in the
*Methods* directory.

Statistical tests can be added to the NeutraliseFiles system as an R
file with a <u>single function</u>. This function must be named **Test**
and specify one argument **db**, which is a data frame with two columns
that is a result of the data generation method (see subsection 2.2.1).
This function may refer to other functions in the same R-file or to
other packages. When there is a package dependency, write the function
in the following way: ‘package::functionname’.

The R-file **must** contain a header that gives a **NAME**,**HYPOTHESIS** and a
**DESCRIPTION** of the method and **REFERENCES** to the literature if
appropriate. Also, at the end of the R-file comments add the ’**END’**
comment. Check the following code chuck for an example.

``` r
# NAME
# Asymptotic Kolmogorov-Smirnov test
# DESCRIPTION
# Two sample Kolmogorov-Smirnov test . P-values based on asymptotic approximation
# HYPOTHESIS
# The null hypothesis states that two independent samples have the same underlying distribution. The alternative hypothesis states that two independent samples have different underlying distributions
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
can run the main Neutralise function. The results of the new simulations
can be found in the *Results* directory.

``` r
res<-Neutralise(path,N=10000)
```

## 3. Evaluate - report

We will use the results of the tutorial (section 2.2) to demonstrate the
different functions to visualize the different results. The Results
directory should contain a folder for every combination of Method (4:
TTest_VarEqual, TTest_VarUnequal, WMW_asymp,KS) and data generation
methods (2:Normal and Normal2Var), thus 8 folders.

These functions can also be used to the global results you can find in
NeutraliseFiles on Github.

### 3.1 Summarize results power & Type 1 error

First, we summarize the results in 4 data files;

1.  Results of the power of methods in a list object per data generation
    method, where the setting parameters are defined.

2.  Results of the power of methods in a data frame, without specific
    setting parameter information.

3.  Results of the type I error of methods in a list object per data
    generation method, where the setting parameters are defined.

4.  Results of the type I error of methods in a data frame, without
    specific setting parameter information.

Depending on the amount of results, this might take a few minutes.

note: The summarized results of the global results of NeutraliseFiles on
Github are also available in the zipfile with all the results, it isn’t
necessary to run the following code if you didn’t add new results to the
NeutraliseFiles.

``` r
# power
summarize_results_power(path)

# Type 1 error control
summarize_results_type1(path)
```

### 3. 2 Report results - Boxplot of Type 1 error

``` r
# All methods & data generation methods that have been 'neutralised'

methods = All_Neutralised(path)
data.gen = All_Neutralised(path, type='data')

method1=methods[2]

# Boxplot of type  error for a specified method - boxplot: over all data generators & scenarios

Graph1_type1error = Boxplot_TypeI(path,method1,alpha=0.05,N=10000)
Graph1_type1error$graph
```

![](Demonstration_files/figure-gfm/unnamed-chunk-13-11.png)<!-- -->

``` r
# Boxplot of type 1 error for a specified method per data generation method

Graph2_type1error = Boxplot_TypeI(path,method1,panel="distribution",N=10000)
Graph2_type1error$graph
```

![](Demonstration_files/figure-gfm/unnamed-chunk-13-22.png)<!-- -->

``` r
# Boxplot of type error for a specified method per sample size (total sample size)

Graph3_type1error = Boxplot_TypeI(path,method1,panel="n",N=10000)
Graph3_type1error$graph
```

![](Demonstration_files/figure-gfm/unnamed-chunk-13-33.png)<!-- -->

``` r
# # Boxplot of type error for a all methods (leave methods argument empty) per data generation method (total sample size)

Graph4_type1error = Boxplot_TypeI(path,method=NULL,panel="distribution",N=10000)
Graph4_type1error$graph
```

![](Demonstration_files/figure-gfm/unnamed-chunk-13-44.png)<!-- -->

### 3.3 Report results - Power

The results of the power are filtered based on type I error control. The
scenarios where a method didn’t control the type I error is filtered out
of the results.

#### 3.3.1 Power-Power plot

``` r
# All methods & data generation methods that have been 'neutralised' + plots will be filtered based on type 1 error control

methods = All_Neutralised(path)
data.gen = All_Neutralised(path, type='data')

method1=methods[3] 
method2=methods[2] 

# Power-power Plot - all data generation methods 

Graph1 = Power_QQ(path,method1,method2,alpha=0.05,N=10000)

Graph1$graph
```

![](Demonstration_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
# Power-power Plot - specified Data Generator = Normal

Graph1b = Power_QQ(path,method1,method2,alpha=0.05,data='Normal',N=10000)

Graph1b$graph
```

![](Demonstration_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
# Power-power Plot - per data generation method 
Graph2 = Power_QQ(path,method1,method2,alpha=0.05,group=TRUE,N=10000)

Graph2$graph
```

![](Demonstration_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->

``` r
# Power-power Plot- per data generation method and a specified setting parameter (delta = 0.5 --> difference in means of 0.5)

db=data.frame(delta=0.5) # important that the specified parameter is exactly the same as in the setting file 

Graph3 = Power_QQ(path,method1,method2,alpha=0.05,group=TRUE,par.fix=db,N=10000)

Graph3$graph
```

![](Demonstration_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->

#### 3.3.2 Power-curve
note: Scenarios that have different parameter settings for the same value of delta will result in power-curves that are not usable. Specify a scenario that is unique by specifying par.fix in the argument of the function. 
``` r
# All methods & data generation methods that have been 'neutralised'

methods = All_Neutralised(path)
data.gen = All_Neutralised(path, type='data')

method=methods[2] # TTest_VarEqual
data=data.gen[1] # Normal


# Power curve - (not clear if scenarios have repetitions of delta, solution see next step-adding specified parameter settings )

Graph1 = Power_curve(path,methods=method,data=data,alpha=0.05,N=10000)

Graph1
```

    ## [[1]]

![](Demonstration_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
# Power curve and a specified setting parameter (sd = 0.5)

db=data.frame(sd=3) # important that the specified parameter is exactly the same as in the setting file 

Graph2 = Power_curve(path,methods=method,data=data,alpha=0.05,par.fix = db,CI=TRUE,N=10000)

Graph2
```

    ## [[1]]

![](Demonstration_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

#### 3.3.3 Power-Power Best methods plot

``` r
# Plot a method vs all other methods or a subset of methods (input name_methods as vector with methods specified)
name_extra='WMW_Asymp'

Best_method_plot(path,name_extra,n=200,alpha=0.05,name_methods=NULL,N=10000)$graph
```

![](Demonstration_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## 4. Upload code and/or results to global NeutraliseFiles on Github

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

For more information refer to
<https://github.com/lucp9827/NeutraliseFiles> or contact us at
leyla.kodalci@uhasselt.be
