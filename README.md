
<!-- README.md is generated from README.Rmd. Please edit that file -->

# titable

<!-- badges: start -->
<!-- badges: end -->

The `titable` package provides function to efficiently conduct logistic
regression analyses, and summarize them into compact tables. These
tables can be saved in an excel sheet with a nice formatting.

The raw results of logistic regression are also directly accessible in
the form of a data.frame. Thus, enabling, to achieve some usal plots as,
for instance, forest plots.

## Installation

You can install the development version of titable from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("paul-bssr/titable", INSTALL_opts=c("--no-multiarch"))
```

Sometimes JAVA version can cause an issue, generating th efollowing
error message :

    Error : .onLoad failed in loadNamespace() for 'rJava', details:
       call: fun(libname, pkgname)
       error: JAVA_HOME cannot be determined from the Registry
     Error: package or namespace load failed for ‘rJava’

In that case, installing 64 bit Java
([here](https://java.com/en/download/manual.jsp)) should solve the
issue.

## Example

This is a typical use case for the library:

``` r
library(titable)
## Generating a summary table
table <- summary_table(data = wdbc.data,
                       studied_vars = c("radius", "texture", "compactness_quartile"),
                       dependent = "diagnosis",
                       multivariate = list(c("smoothness", "texture"),
                                           c("concavity", "symmetry")),
                       digits = 2,
                       digits_p = 2,
                       p_limit=0.001,
                       verbose=FALSE
                       )
table
#>                 label    levels          B          M
#>                radius Mean (SD) 12.1 (1.8) 17.5 (3.2)
#>               texture Mean (SD) 17.9 (4.0) 21.6 (3.8)
#>  compactness_quartile   1, N(%) 133 (37.4)    9 (4.2)
#>                         2, N(%) 120 (33.7)  22 (10.4)
#>                         3, N(%)  77 (21.6)  65 (30.7)
#>                         4, N(%)   26 (7.3) 116 (54.7)
#>                OR (univariate)                  OR (model 1)
#>      2.81 (2.37-3.42, p<0.001)     4.04 (3.07-5.64, p<0.001)
#>       1.26 (1.2-1.33, p<0.001)     1.33 (1.26-1.41, p<0.001)
#>                              -                             -
#>      2.71 (1.24-6.42, p=0.016)      3.4 (1.4-8.97, p=0.0093)
#>    12.47 (6.16-28.14, p<0.001)   12.69 (5.42-32.87, p<0.001)
#>  65.93 (31.13-155.48, p<0.001) 54.21 (20.66-157.56, p<0.001)
#>               OR (model 2)
#>   2.72 (2.2-3.46, p<0.001)
#>  1.25 (1.16-1.34, p<0.001)
#>                          -
#>   0.77 (0.31-1.96, p=0.57)
#>   0.52 (0.18-1.52, p=0.23)
#>  0.22 (0.05-0.91, p=0.038)

## Saving the result in an excel file
# # Creating an excel file
# save_summary_table(table, filepath="inst/extdata/", filename="test",
#                    sheetname = "test_sheet_1",
#                    title = "Regression logistic study",
#                    subtitle = "This is an interesting study.",
#                    list_variables_renaming = list("compactness_quartile"="Compactness quartile")
#                    )
```
