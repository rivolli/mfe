# mfe: Meta-Feature Extractor
[![Travis-CI Build Status](https://travis-ci.org/rivolli/mfe.svg?branch=master)](https://travis-ci.org/rivolli/mfe)
[![codecov](https://codecov.io/gh/rivolli/mfe/branch/master/graph/badge.svg)](https://codecov.io/gh/rivolli/mfe)
[![CRAN](https://www.r-pkg.org/badges/version/mfe)](https://CRAN.R-project.org/package=mfe)

Extracts meta-features from datasets to support the design of recommendation systems based on Meta-Learning (MtL). The meta-features, also called characterization measures, are able to characterize the complexity of datasets and to provide estimates of algorithm performance. The package contains not only the standard, but also more recent characterization measures. By making available a large set of meta-feature extraction functions, this package allows a comprehensive data characterization, a deep data exploration and a large number of MtL-based data analysis.

## Measures

In MtL, meta-features are designed to extract general properties able to characterize datasets. The meta-feature values should provide relevant evidences about the performance of algorithms, allowing the design of MtL-based recommendation systems. Thus, these measures must be able to predict, with a low computational cost, the performance of the  algorithms under evaluation. In this package, the meta-feature measures are divided into six groups:

* **General**: General information related to the dataset, also known as simple measures, such as number of instances, attributes and classes.
* **Statistical**: Standard statistical measures to describe the numerical properties of a distribution of data.
* **Information-theoretic**: Particularly appropriate to describe discrete (categorical) attributes and their relationship with the classes.
* **Model-based**: Measures designed to extract characteristics like the depth, the shape and size of a Decision Tree (DT) model induced from a dataset.
* **Landmarking**: Represents the performance of simple and efficient learning algorithms. Include the subsampling and relative strategies to decrease the computation cost and enrich the relations between these meta-features.
* **Clustering**: Clustering measures extract information about dataset based on external validation indexes. 

## Installation

The installation process is similar to other packages available on CRAN:

```r
install.packages("mfe")
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

The simplest way to extract meta-features is using the `metafeatures` method. The method can be called by a symbolic description of the model or by a data frame. The parameters are the dataset and the group of measures to be extracted. The default parameter is extract all the measures. To extract a specific measure, use the function related with the group. A simple example is given next:

```r
## Extract all measures using formula
metafeatures(Species ~ ., iris)

## Extract all measures using data frame
metafeatures(iris[,1:4], iris[,5])

## Extract general, statistical and information-theoretic measures
metafeatures(Species ~ ., iris, groups=c("general", "statistical", "infotheo"))

## Extract the DT model based measures
model.based(Species ~ ., iris)

## Show the the available groups
ls.metafeatures()
```

Several measures return more than one value. To aggregate the returned values, post processed methods can be used. This method can compute min, max, mean, median, kurtosis, standard deviation, among others (see the `post.processing` documentation for more details). The default methods are the `mean` and the `sd`. Next, it is possible to see an example of the use of this method:

```r
## Extract all measures using min, median and max 
metafeatures(Species ~ ., iris, summary=c("min", "median", "max"))
                          
## Extract all measures using quantile
metafeatures(Species ~ ., iris, summary="quantile")
```

## Developer notes

In the current version, the meta-feature extractor supports only classification problems. The authors plan to extend the package to add clustering and regression measures and to support MtL evaluation measures. For more specific information on how to extract each group of measures, please refer to the functions documentation page and the examples contained therein. For a general overview of the `mfe` package, please have a look at the associated vignette.

To cite `mfe` in publications use: 

* Rivolli, A., Garcia, L. P. F., Soares, C., Vanschoren, J., and de Carvalho, A. C. P. L. F. (2018). Towards Reproducible Empirical Research in Meta-Learning. arXiv:1808.10406


To submit bugs and feature requests, report at [project issues](https://github.com/rivolli/mfe/issues).
