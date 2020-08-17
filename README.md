# Metalearning for robust regression, version 1.0

The code in R performs an optimal method selection (known as meta-learning) over a database of available datasets. Here, various linear regression estimators are compared including several robust ones. The comutationally intensive
method predicts the most suitable method (estimator) for a new dataset.

Feel free to use or modify the code.

## Requirements

You need to install these package of R software: MASS, robustbase, moments, lmtest, glmnet, class, e1071.

## Usage

* The metalearning analysis should be performed in the following order: ReadingData.R, Primary.R, Features.R, Secondary.R. Alternative approaches
to the methods of Secondary.R are included in files RandomForest.R and Regression.R. Alternative validation (i.e. autovalidation instead of cross
validation) is implemented in Autovalidation.R.

## Authors
  * Jan Kalina, The Czech Academy of Sciences, Institute of Computer Science
  * Petra Vidnerová, The Czech Academy of Sciences, Institute of Computer Science
  * Barbora Peštová, The Czech Academy of Sciences, Institute of Computer Science

## Contact

Do not hesitate to contact us (kalina@cs.cas.cz) or write an Issue.

## How to cite

Please consider citing the following:

Kalina J, Vidnerová P (2020): A metalearning study for robust nonlinear regression. Proceedings of the 21st EANN (Engineering Applications of Neural Networks) 2020 Conference, Springer, Cham, pp. 499-510.

## Acknowledgement

This work was supported by the Czech Science Foundation grants GA19-05704S and GA18-23827S.