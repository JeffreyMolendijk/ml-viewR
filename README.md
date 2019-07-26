# ml-viewR
ml-viewR is a visualization package for machine learning analyses. The functions in `mlview` are named according to the package which they are to be used with, whereas generic functions start with "mlview.".

## ropls 
This package contains functions used for visualization of `ropls` objects. 
`mlview` currently supports all models created by `ropls`, including PCA, PLS, OPLS, PLS-DA and OPLS-DA models. 

The `ropls.plot` function can be used to create score (default) and loading plots. This function allows colour by group and the addition of group ellipses. 
Hotelling's T2 ellipse can be toggled on or off. The ggplot output from `ropls.plot` can be further modified if required.

The `ropls.enrich` function performs `fgsea` enrichment on a matrix of variables, variable information and a single numeric comlumn. Furthermore, the column(s) used in the enrichment analysis can be specified. This function returns the `fgsea` output, including enriched groups, enrichment scores and p-values.