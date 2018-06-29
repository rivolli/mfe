## ------------------------------------------------------------------------
library(mfe)

## Extract all measures using formula
iris.info <- metafeatures(Species ~ ., iris)

## Extract all measures using data frame
iris.info <- metafeatures(iris[,1:4], iris[,5])

## Extract general, statistical and information-theoretic measures
iris.info <- metafeatures(Species ~ ., iris, 
                          groups=c("general", "statistical", "infotheo"))

## ------------------------------------------------------------------------
## Compute all measures using min, median and max 
iris.info <- metafeatures(Species ~ ., iris, summary=c("min", "median", "max"))
                          
## Compute all measures using quantile
iris.info <- metafeatures(Species ~ ., iris, summary="quantile")

## ------------------------------------------------------------------------
## Extract two information theoretical measures
stat.iris <- infotheo(Species ~ ., iris, 
                         features=c("attrEnt", "jointEnt"))

## Extract three statistical measures
disc.iris <- statistical(Species ~ ., iris, 
                            features=c("cancor", "cor", "iqr"))

## Extract the histogram for the correlation measure
hist.iris <- statistical(Species ~ ., iris, 
                            features="cor", summary="hist")

## ------------------------------------------------------------------------
## Show the the available groups
ls.metafeatures()

## ------------------------------------------------------------------------
## Show the the available general measures
ls.general()

## Extract all general measures
general.iris <- general(Species ~ ., iris)

## Extract two general measures
general(Species ~ ., iris, features=c("nrAttr", "nrClass"))

## ------------------------------------------------------------------------
## Extract two general measures
general(Species ~ ., iris, features="freqClass", summary=c("min", "max", "sd"))

## ------------------------------------------------------------------------
## Show the the available statistical measures
ls.statistical()

## Extract all statistical measures
stat.iris <- statistical(Species ~ ., iris)

## Extract two statistical measures
statistical(Species ~ ., iris, features=c("cor", "skewness"))

## ------------------------------------------------------------------------
## Extract correlation using instances by classes
statistical(Species ~ ., iris, features="cor", by.class=TRUE)

## Ignore the class attributes
aux <- cbind(class=iris$Species, iris)
statistical(Species ~ ., aux, transform=FALSE)

## ------------------------------------------------------------------------
## Show the the available information theoretical measures
ls.infotheo()

## Extract all information theoretical measures
inf.iris <- infotheo(Species ~ ., iris)

## Extract two information theoretical measures
infotheo(Species ~ ., iris, features=c("normClassEnt", "mutInf"))

## ------------------------------------------------------------------------
## Ignore the discretization process
aux <- cbind(class=iris$Species, iris)
infotheo(Species ~ ., aux, transform=FALSE)

## ------------------------------------------------------------------------
## Show the the available model.based measures
ls.model.based()

## Extract all model.based measures
land.iris <- model.based(Species ~ ., iris)

## Extract three model.based measures
model.based(Species ~ ., iris, features=c("leaves", "nodes"))

## ------------------------------------------------------------------------
## Show the the available landmarking measures
ls.landmarking()

## Extract all landmarking measures
land.iris <- landmarking(Species ~ ., iris)

## Extract two landmarking measures
landmarking(Species ~ ., iris, features=c("naiveBayes", "oneNN"))

## ------------------------------------------------------------------------
## Extract one landmarking measures with folds=2
landmarking(Species ~ ., iris, features="naiveBayes", folds=2)

## Extract one landmarking measures with folds=2
landmarking(Species ~ ., iris, features="naiveBayes", score="kappa")

## ------------------------------------------------------------------------
## Apply several statistical measures as post processing
statistical(Species ~ ., iris, "cor", 
               summary=c("kurtosis", "max", "mean", "median", "min", "sd", 
                         "skewness", "var"))

## Apply quantile as post processing method
statistical(Species ~ ., iris, "cor", summary="quantile")

## Get the default values without summarize them
statistical(Species ~ ., iris, "cor", summary=c())

## ------------------------------------------------------------------------
## Apply histogram as post processing method
statistical(Species ~ ., iris, "cor", summary="hist")

## Apply histogram as post processing method and customize it
statistical(Species ~ ., iris, "cor", summary="hist", bins=5, min=0, max=1)

## Extract all correlation values
statistical(Species ~ ., iris, "cor", summary="non.aggregated")

## ------------------------------------------------------------------------
## Compute the absolute difference between the mean and the median 
my.method <- function(x, ...) abs(mean(x) - median(x))

## Using the user defined post processing method
statistical(Species ~ ., iris, "cor", summary="my.method")

