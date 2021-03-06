# MTH2401 - Probability and Statistics
>This is meant to be a shared R library for functions directly related to course material. <br><br>
>The purpose of having a separate package is to consolidate frequently used procedures into a common location,<br>
as well as to make some of the data manipulation more user-friendly, by abstracting the programming.

--

## Installation Guide
>1. Open RStudio
>2. Session > Set Working Directory > Choose Directory... (Create a new directory wherever seems best)
>3. Console
>4. Type: ```install.packages("devtools")```
>5. Type: ```library("devtools")```
>6. Type: ```install_github("mithradatha/MTH2401")```
>7. Type: ```library("MTH2401")```

--

## User Guide
> * To see a list of functions in the MTH2401 package, Type: ```ls("package:MTH2401")```
> * To see the documentation related to each function, Type: ```help(<function_name>)```

--

## Additional Details
> * [DevTools](https://cran.r-project.org/web/packages/devtools/index.html) contains many functions that help to make package development easier, including the ability to install R packages directly from github (...hence the use of ```install_github``` function)
> * [Roxygen2](https://cran.r-project.org/web/packages/roxygen2/index.html) has a whole bunch of tools to aid in function/package level documentation
> * I will be adding information on the process of developing new functions, for this package, in the future
