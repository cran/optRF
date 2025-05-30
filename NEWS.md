# optRF 1.2.1

## Improvements

-   Improved output of `estimate_numtrees` and `estimate_stability` functions

## Bug Fixes

-   Fixed a bug with the plot functions for the `opt_importance_object` and `opt_prediction_object` objects.

# optRF 1.2.0

## New Features

-   Added function `measure_stability` to measure the stability of random forest for a certain data set with a certain number of trees.

## Improvements

-   Adding checks for validity of parameters `alpha`, `number_repetitions` and `rec_thresh`.

## Other

-   Updated `CITATION` file with published manuscript.

# optRF 1.1.0

## New Features

-   Added support for selecting multiple classes in `opt_prediction`.
-   Introduced the `verbose` parameter in `opt_importance` and `opt_prediction` to control message display.
-   Added the `importance` parameter to `opt_importance`, allowing users to specify the importance measure used by `ranger`.
-   Included code for generating the `SNPdata` data set.
-   Added vignettes to guide users on using the package.

## Improvements

-   Replaced the evaluation of random forest using simulated data with out-of-bag (OOB) data.
-   Removed and simplified redundant code to enhance efficiency.
-   Reduced minimum version requirements for imported packages.

## Bug Fixes

-   Fixed a bug in the calculation of `estimate_numtrees`.

## Other

-   Added a `NEWS.md` file to track changes to the package.
-   Added a `CITATION` file.
-   Added a `README.md` file for the Github repository.

# optRF 1.0.1

-   Initial release on CRAN
