[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/biometry/rLPJGUESS.svg?branch=master)](https://travis-ci.org/biometry/rLPJGUESS)

# rLPJGUESS

A R package that wraps the [LPJ-GUESS model](http://iis4.nateko.lu.se/lpj-guess/)

### Recommended installation

Install the latest stable release from https://github.com/biometry/rLPJGUESS/releases.

You can download the binary and install it as follows

```{r}
install.packages("/path/to/binary/rLPJGUESS_1.1.0.tar.gz", repos = NULL, type = "source")
```
Or you can install it directly from the download link

```{r}
library(devtools)
install_url("https://github.com/biometry/rLPJGUESS/releases/download/v1.1.0/rLPJGUESS_1.1.0.tar.gz", 
dependencies = T, build_vignettes = T)
library(rLPJGUESS)
?rLPJGUESS
```

**Note**: rLPJGUESS relies on two packages for parallelization:

- snow (for SOCK cluster, if you use PC/laptop)
- Rmpi (for MPI clusters)

Installing "Rmpi" might be complicated, and it is not strictly required: If you are thinking of using rLPJGUESS on a laptop or workstation, you will be dealing with SOCK clusters and you do not need Rmpi.

### Development release 

If you want to install the current (development) version from this repository, run

```{r}
devtools::install_github(repo = "biometry/rLPJGUESS", subdir = "rLPJGUESS", 
dependencies = T, build_vignettes = T)
```
Below the status of the automatic Travis CI tests on the master branch 

[![Build Status](https://travis-ci.org/biometry/rLPJGUESS.svg?branch=master)](https://travis-ci.org/biometry/rLPJGUESS)


### Build the package yourself 

Clone the package to your computer, and build with hand or Rstudio. If you need help see here http://biometry.github.io/APES/R/R70-PackageDevelopment.html


In Rstudio, the vignette may not be built per default. You will turn this on in your project options, or run 

```{r}
devtools::build_vignettes("rLPJGUESS")
```
### Workflow when using the package: 

#### 1. Define a folder to run the simulations, e.g. 
```{r}
mainDir    <- file.path(getwd(), "LPJrunTest")
```
and place the binary (exectuable) of LPJ-GUESS into this folder (the binary does not come with this package).

#### 2. Provide a valid instruction file that you usually use to define the set-up of LPJ-GUESS to the InferParameterAndDesignList function. 
```{r}
defaultparameters <- InferParameterAndDesignList(list(main = paste0(mainDir,"LPJ_instruction_file.ins")), 
                                                 NameMainFile = "main.ins", NamePftFile = "pft.ins",
                                                 vectorvaluedparams = c("rootdist","eps_mon",
                                                                        "storfrac_mon","photo",
                                                                        "fertdates","fertrate"))
```

This function extracts the parameters, design and driver-files used to run LPJ-GUESS and splits the instruction into a main file containing inputs and a PFT file, which has all the settings. Because we cannot handle vectorvalued parameters at the moment, they have to be provided to this function as well. 

#### 3. Call the adjust template function, which rewrites the templates to new destinations (IMPORTANT: the files need to be in the same directory as the binary, here: LPJrunTest)
```{r}
AdjustTemplates(defaultparameters = defaultparameters$defaultparameters,
                defaultlist = defaultparameters$defaultlist,
                MainTemplateDestination = "./LPJrunTest/main_new.ins",
                PftTemplateDestination = "./LPJrunTest/pft_new.ins",
                NameMainFile = "main.ins", NamePftFile = "pft.ins")
```

#### 4. Generate a set of parameters for the species (GetRunAbleParameters), we want to simulate, here Fagus sylvatica (Fag_syl):
```{r}
parameters <- GetRunAbleParameters(defaultparameters = defaultparameters, PFTs = c("Fag_syl"))
```
#### 5. Change some of these parameters and provide it as matrix with a set of parameters per row and parameter names as colnames (here: new parameter matrix is called parameters_new)

#### 6. Define the settings and input files, we want to use as input for the simulations in a list (the required files depend on the LPJ-Version, here: the list is called LPJsettings)

#### 7. Define the setup (i.e. sequential, parallel, which kind of parallelisation, here a SOCK cluster with numCores cores)
```{r}
LPJsetup <- setupLPJParallel(numCores = numCores, clusterType = "SOCK",
                             mainDir = mainDir)
```

#### 8. Run the simulations with the model
```{r}
results <- runLPJ(x = LPJsetup, parameterList = parameters_new,
                      typeList = typeList, settings = LPJsettings)
```
