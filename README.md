# mfe: Meta-Feature Extractor
[![Travis-CI Build Status](https://travis-ci.org/rivolli/mfe.svg?branch=master)](https://travis-ci.org/rivolli/mfe)

Extracts meta-features from datasets to support the design of recommendation systems based on Meta-Learning (MtL). The meta-features, also called characterization measures, are able to characterize the complexity of datasets and to provide estimates of algorithm performance. The package contains not only the standard characterization measures, but also more recent characterization measures. By making available a large set of meta-feature extraction functions, this package allows a comprehensive data characterization, a deep data exploration and a large number of MtL-based data analysis.


## Measures

In MtL, meta-features are designed to extract general properties able to characterize datasets. The meta-feature values should provide relevant evidences about the performance of algorithms, allowing the design of MtL-based recommendation systems. Thus, these measures must be able to predict, with a low computational cost, the performance of the  algorithms under evaluation. In this package, the meta-feature measures are divided into six groups:

* **General** (`general`) - General information related to the dataset, also known as simple measures, such as number of instances, attributes and classes.
* **Statistical** (`statistical`) - Standard statistical measures to describe the numerical properties of a distribution of data.
* **Discriminant** (`discriminant`) - Measures computed using the discriminant analysis.
* **Information-theoretic** (`infotheo`) - Particularly appropriate to describe discrete (categorical) attributes and thei relationship with the classes.
* **Decision Tree Model-based**  (`model.based`) - Measures desined to extract characteristics like the depth, the shape and size of a Decision Tree model induced from a dataset.
* **Landmarking** (`landmarking`) - Represents the performance of some simple and efficient learning algorithms.

## Installation

The installation process is similar to other packages available on CRAN:

```r
install.packages("mfe")
```

Compile the project:

```r
R CMD INSTALL --no-multiarch --with-keep.source mfe
```

It is possible to install the development version using:

```r
if (!require("devtools")) {
    install.packages("devtools")
}
devtools::install_github("rivolli/mfe")
library("mfe")
```

## Example of use

The simplest way to extract meta-features is using the `metafeatures` method. The method can be called by a symbolic description of the model or by a data frame. The parameters are the dataset and the group of measures to be extracted. To extract all the measures, the parameter "group" needs to be set to "all". A simple example is given next:

```{r}
library(mfe)
data("iris")

## Extract all measures using formula
iris.info <- metafeatures(Species ~ ., iris, groups="all")

## Extract all measures using data frame
iris.info <- metafeatures(iris[,1:4], iris[,5], groups="all")

## Extract general, statistical and information-theoretic measures
iris.info <- metafeatures(Species ~ ., iris, groups=c("general", "statistical", "infotheo"))

## Show the the available groups
ls.metafeatures()
```
Several measures return more than one value. To aggregate the returned values, post processed methods can be used. This method can compute min, max, mean, median, kurtosis, standard deviation, among others (see the `post.processing` documentation for more details). The default methods are the `mean` and the `sd`. Next, it is possible to see an example of the use of this method:

```{r}
## Compute all measures using min, median and max 
iris.info <- metafeatures(Species ~ ., iris, summary=c("min", "median", "max"))
                          
## Compute all measures using quantile
iris.info <- metafeatures(Species ~ ., iris, summary="quantile")
```

## Developer notes

In the current version, the meta-feature extractor supports only classification problems. The authors plan to extend the package to add clustering and regression measures and to support MtL evaluation measures. For more specific information on how to extract each group of measures, please refer to the functions documentation page and the examples contained therein. For a general overview of the `mfe` package, please have a look at the associated vignette.

To cite `mfe` in publications use: Rivolli, A., Garcia, L. P. F., de Carvalho, A. C. P. L. F. (2017). mfe: Meta-Feature Extractor. R package version 0.1.0. http://CRAN.R-project.org/package=mfe


To submit bugs and feature requests, report at [project issues](https://github.com/rivolli/mfe/issues).
