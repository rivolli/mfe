# mfe: Meta-features Extraction for meta-learning

The `mfe` package is designed to extract meta-features from datasets. It 
contains traditional and new characterization measures reported in the 
literature of meta-learning. Its main objective is to be employed as part of the
meta-learning experiments, however it contains a wide range of measures that can 
be used for another purposes.

> *Note*: In this current version, only classification datasets are supported, 
however, we plan support regression datasets in the future versions. 

## Groups of measures
* **General** (`general`) - General information related to the dataset, also known as simple 
measures, such as number of instances, attributes and classes.
* **Statistical** (`statistical`) - Standard statistical measures to describe the numerical 
properties of a distribution of data.
* **Discriminant** (`discriminant`) - Measures computed using the discriminant analysis.
* **Information-theoretic** (`infotheo`) - Particularly appropriate to describe discrete 
(categorical) attributes and thei relationship with the classes.
* **Model-based**  (`model.based`) - Measures desined to extract characteristics like the depth, 
the shape and size of a Decision Tree model induced from a dataset.
* **Landmarking** (`landmarking`) - Represents the performance of some simple and efficient 
learning algorithms.

## Installation
The installation process is similar to other packages available on CRAN:
```r
install.packages("mfe")
```

### Development version
It is possible to install the development version using
```r
devtools::install_github("rivolli/mfe")
```

### Compile the project:
R CMD INSTALL --no-multiarch --with-keep.source mfe

## Extracting meta-features
The simplest way to extract meta-features is using the `metafeatures` method.
You just need to inform which group of measures do you need or use "all" for 
all measures. For instance:
```{r}
library(mfe)
data("iris")

## Extract general, statistical and information-theoretic measures
iris.info <- metafeatures(Species ~ ., iris, c("general", "statistical", "infotheo"))

## Extract all meta-features
iris.info <- metafeatures(iris[1:4], iris$Species)

## List the available groups
ls.metafeatures()
```

Several measures generates more than one values and them can be post processing
using distinct alternatives. It is possible compute the min, max, mean, median,
kurtosis, standard deviation, among others (See the `post.processing` 
documentation for more details). The default is the `mean` and the `sd` however
you can use anothers. For instance:
```{r}
## Summarize multiples meta-features values using min, median and max 
iris.info <- metafeatures(Species ~ ., iris, "statistical", 
                          summary=c("min", "median", "max"))
                          
## Summarize multiples meta-features values using quantile
iris.info <- metafeatures(Species ~ ., iris, "statistical", summary="quantile")
```

Despite the simplicity it is not possible customize the meta-features, for this 
is necessary to use the specific methods. There is a method for each group of 
measure, for instance `mf.general` and `mf.statistical` compute the general and
the statistical measures respectively. To list the measures of these groups use 
`ls.general()` and `ls.statistical()`. The folloing examples illustrate this 
cases:
```{r}
## Extract some statistical meta-features
stat.iris <- mf.statistical(iris[1:4], iris[5], c("correlation", "variance"))

## Extract the histogram of correlation
hist.iris <- mf.statistical(iris[1:4], iris[5], "correlation", "hist")

## Extract some discriminant meta-features
disc.iris <- mf.discriminant(iris[1:4], iris[5], c("cancor", "cancor.fract"))
```

Different from the `metafeatures` method, these methods return a list instead of
a numeric vector. To get all meta-feature values without post processing use `summary=non.aggregated` like this:
```{r}
## Extract all correlation values
cor.iris <- mf.statistical(iris[1:4], iris[5], "correlation", 
                           summary="non.aggregated", by.class=FALSE)
```

Consult the method documentation to details about the variation of each group
of measures.



