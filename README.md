# mfe: Meta-Features Extractor for Meta-Learning
[![Travis-CI Build Status](https://travis-ci.org/rivolli/mfe.svg?branch=master)](https://travis-ci.org/rivolli/mfe)

The `mfe` package is designed to extract meta-features from datasets. The meta-features can be understood as characterization measures able to describe datasets to support recommendation systems based on Meta-learning (MtL). The package contains the standard and the state of the art characterization measures with the goal to improve the MtL experiments and also guide the complexity dataset understanding.

## Measures

The meta-features are designed to extract general properties of datasets and provide evidences about the performance of algorithms in MtL recomendation systems. These measures must be able to predict, with a low computational cost, the performance of these algorithms. The measures used in MtL can be divided into six groups:

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
    library("devtools")
}
install_github("rivolli/mfe")
library("mfe")
```

## Example of use

The simplest way to extract meta-features is using the `metafeatures` method. The method can be usage by a symbolic description of the model or by a data frame. The parameters are the dataset and the group of measures to be extracted. To extract all the measures, the parameter "group" needs to be set as "all". For instance:

```{r}
library(mfe)
data("iris")

## Extract all measures using formula
iris.info <- metafeatures(Species ~ ., iris, groups="all")

## Extract all measures using data frame
iris.info <- metafeatures(iris[1:4], iris[,5], groups="all")

## Extract general, statistical and information-theoretic measures
iris.info <- metafeatures(Species ~ ., iris, groups=c("general", "statistical", "infotheo"))

## Show the the available groups
ls.metafeatures()
```

Several measures return more than one value. To agregate them, post processed methods can be used. It is possible to compute min, max, mean, median, kurtosis, standard deviation, among others (see the `post.processing` documentation for more details). The default methods are the `mean` and the `sd`. For instance:

```{r}
## Compute all measures using min, median and max 
iris.info <- metafeatures(Species ~ ., iris, summary=c("min", "median", "max"))
                          
## Compute all measures using quantile
iris.info <- metafeatures(Species ~ ., iris, summary="quantile")
```

## Developer notes

In the current version, the meta-feature extractor only support classification problems. In a near future we plan add clustering and regression measures and also support MtL evaluation measures. For more specific information on how to extract each group of measures, please refer to the functions documentation page and the examples contained therein. For a general overview of the **mfe** package, please look up the associated vignette.

To cite **mfe** in publications use:

Rivolli, Adriano and Garcia, Luis P. F. (2017). mfe: Meta-Features Extractor for Meta-Learning. R package version 1.0.0. http://CRAN.R-project.org/package=mfe

