# OptFilBov
Package where is nutshell the method used for my thesis at INRA. This package allow to access to the dataset which have been used for data mining.
Modelisation throught 5 prediction model is possible with OptFilBov: linear modele (lm), sliced-inversed regression (sir), randomForest (rf), ridge regression, partial least squares regression (plsr).

Black box analysis is enterprise via sensibility index from the sobol's decomposition of the variation.

Multiobjectifs optimisation is enterprise with the minMax method and the aggregation weight.

To install the package on your machine, you first need to install devtools if you have not previously installed it:
```r
install.package('devtools')
```

Then you can you use devtools to install OptFilBov package from my Github account:
```r 
devtools::install_github('alex-conanec/OptFilBov')
```
