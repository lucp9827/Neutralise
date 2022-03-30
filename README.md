---
# Neutralise: an open source initiative for neutral comparison of two-sample tests
---

# 1. Load required packages and Installation steps 
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

# load required packages
library(tfse)
library(zip)
library(tidyverse)
library(kSamples)
library(twosamples)
```

## Github installation 

Code to install *Neutralise*, with authentication code for permission. (Authentication code needs be deleted when the resporitory is set public)

```{r , message = FALSE}
library(devtools)

#install_github("lucp9827/Neutralise",auth_token="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",force=TRUE)

library('Neutralise')
```

## Initialise local directory structure

Download 'NeutraliseFiles.zip' from 'https://github.com/lucp9827/NeutraliseFiles'. Unzip the 'NeutraliseFiles' file and save the path to this unzipped NeutraliseFiles folder.

```{r}
# Example:
path = "C:\\Users\\lucp9827\\Desktop\\Neutralise\\NeutraliseFiles"
```


NeutraliseFiles contains 4 directories; 

1. **Data**: This Folder contains the data generator functions available.
2. **Settings**: This Folder contains the settings for the existing data generators. If the settings have been simulated, the setting files change from an R-file extension to an R workspace.
3. **Methods**: This Folder contains the statistical methods available.
4. **Results**: This folder contains the result of all combinations of statistical methods and simulation scenarios available, and a text file 'Finished' with information on the simulation runs already done, and an R-file describing the status of all methods, data generators and settings, and a Local folder for the results of the *single* operating mode.


# 2. Demonstration Neutralise

We will demonstrate two operating modes of Neutralise; **single** and **all**. First, we demonstrate the operating model *single*, which allows a user to evaluate a single statistical method under a single simulation scenario. Second, we demonstrate the operating mode *all*, which allows the user to evaluate a new statistical method under all scenario's available on Github and compare the results to other statistical methods on Github. It's also possible to evaluate all statistical methods available on Github under a new simulation scenario. 

## 2.1 Operating mode: *Single*

Suppose that you want to evaluate a method on a simulation scenario, but you are still in an "experimental stage" of your research and so you do not (yet) want to add your method or data generator method to the system. Then you can run the Neutralise in "single" mode.
For example, we have a new method (here Anderson Darling). 

```{r}
AD_Asymp<-function(db) {
  results<-ad.test(db$y[db$group==1],db$y[db$group==2],
                   method="asymptotic")
  return(list(
    stat=results$ad[1,1],
    p.value=results$ad[1,3]
  ))
}
```

and we will use the following data generator

```{r}
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

```{r}

settings<-data.frame(
  delta=c(0,1,2,3),
  rate1=c(1,0.5,0.25,0.125),
  null=c(1,0,0,0)
)
```

Then we can run the following code. This will NOT add the new method and data generator to the *Methods* and *Data* directories, and the results will be saved to the directory Results/Local so that the results are not mixed with the other results. However, the format in which the results are saved is the same as for the "all" mode. The power results are also in the object "res" so that the user does not necessarily has to go to the Results/Local directory. 


```{r}
res<-Neutralise(path,Test=AD_Asymp,
                Data.Generator = ExpLocShift,
                settings=settings,
                N=1)
```


## 2.2 Operating mode: *All*

To run all combinations of statistical methods and simulation scenarios (i.e. combination of data generator and settings) available on Github the following code can be used and the results will be saved in the *Results* directory. This function can be used to reproduce results of previous simulations.
Note: To reproduce results it's important to have (1) an 'empty' *Results* directory and (2) to reinitialise the files *Finished.txt* and *neutralise_status.Rfile*. Also (3) add the specific settings R-files that you want to reproduce into the *Settings* directory. 


```{r}

# Code to reinitilize the files *Finished.txt* and *neutralise_status.Rfile (dont run if you don't want to reproduce results)

Initialise_Neutralise(path)

# Run Neutralise main function 
# Delete N 

res<-Neutralise(path, N=2)
```


Suppose that we now have some new additional settings to be run for a data generator that already exists. Then we add a new settings R-file to the      directory *Settings* and run the neutralise function again. We'll first demonstrate how a new settings R-file should be constructed.

### Adding a new setting to the settings directory

Make sure the R-file for the new setting begins with the same name as the existing Data.Generator R-file (in *Data* directory) and ends with '_settings.R'. Also, add an empty line of code to the end of the R-file.


```{r}

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

res<-Neutralise(path, N=2)
```

Suppose that we now have an new statistical method to be run for the data generators that already exists. Then we add a new method R-file to the directory *Methods* and run the code again.
We'll first demonstrate how a new method R-file should be constructed, this 'code chunck' should be saved by the user as 'KS.R' in the *Methods* directory.

### Adding a new statistical test to the methods directory

Statistical tests can be added to the system as an R file with a single function. This function must be named **Test** and specify one argument **bd**, which is a data frame with two columns. One column must be named **y** and contains the *n~1~ + n~2~* sample observations, and the other column must be named **group** and contains values 1 and 2, referring to the groups the sample obserations come from. 
The R-file must contain a header that gives a **name** and a **description** of the method and references to the literature if appropriate. 
 
```{r}
# NAME
# Asymptotic Kolmogorov-Smirnov test
# DESCRIPTION
# Two sample Kolmogorov-Smirnov test . P-values based on asymptotic approximation
# REFERENCES
# Kolmogorov, A. 1933. Sulla Determinazione Empirica di una Legge di Distributione???. Giornale dell'Istituto Italiano degli Attuari, 4: 1-11.
# Smirnov, H. 1939. Sur les Ecarts de la Courbe de Distribution Empirique???. Recueil MathematiqueMatematiceskii Sbornik), : 3-26. N.S. 6
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


After saving the new method in the *Methods* directory as an R-file, we can run the main neutralise function. The results of the new simulations can be found in the *Results* directory.

```{r}
res<-Neutralise(path, N=2)
```


## 3. Evaluate - report


# Report results per data generator

```{r}
# load(paste(path,'\\Results\\neutralise_status.R',sep=''))
# m = neutralise_status$type=='method'
```

Tables summerizing the results:
Graphs of the results:

```{r, echo=FALSE}

```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
