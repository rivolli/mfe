## ------------------------------------------------------------------------
library(mfe)

## Extract all measures using formula
iris.info <- metafeatures(Species ~ ., iris, groups="all")

## Extract all measures using data frame
iris.info <- metafeatures(iris[,1:4], iris[,5], groups="all")

## Extract general, statistical and information-theoretic measures
iris.info <- metafeatures(Species ~ ., iris, 
                          groups=c("general", "statistical", "infotheo"))


## ------------------------------------------------------------------------
## Compute all measures using min, median and max 
iris.info <- metafeatures(Species ~ ., iris, summary=c("min", "median", "max"))
                          
## Compute all measures using quantile
iris.info <- metafeatures(Species ~ ., iris, summary="quantile")

## ------------------------------------------------------------------------
## Extract two infotheo measures
stat.iris <- mf.infotheo(Species ~ ., iris, 
                         features=c("attrEnt", "jointEnt"))

## Extract three statistical measures
disc.iris <- mf.statistical(Species ~ ., iris, 
                            features=c("cancor", "cor", "iqr"))

## Extract the histogram for the correlation measure
hist.iris <- mf.statistical(Species ~ ., iris, 
                            features="cor", summary="hist")

## ------------------------------------------------------------------------
## Show the the available groups
ls.metafeatures()

## ------------------------------------------------------------------------
## Show the the available general measures
ls.general()

## Extract all general measures
general.iris <- mf.general(Species ~ ., iris, features="all")

## Extract two general measures
mf.general(Species ~ ., iris, features=c("nrAttr", "nrClass"))

## ------------------------------------------------------------------------
## Extract two general measures
mf.general(Species ~ ., iris, features="propClass", 
           summary=c("min", "max", "sd"))

## ------------------------------------------------------------------------
## Show the the available statistical measures
ls.statistical()

## Extract all statistical measures
stat.iris <- mf.statistical(Species ~ ., iris, features="all")

## Extract two statistical measures
mf.statistical(Species ~ ., iris, features=c("cor", "skewness"))


## ------------------------------------------------------------------------
## Extract correlation using instances by classes
mf.statistical(Species ~ ., iris, features="cor", by.class=TRUE)

## Ignore the class attributes
aux <- cbind(class=iris$Species, iris)
mf.statistical(Species ~ ., aux, transform=FALSE)

## ------------------------------------------------------------------------
## Show the the available infotheo measures
ls.infotheo()

## Extract all infotheo measures
inf.iris <- mf.infotheo(Species ~ ., iris, features="all")

## Extract two infotheo measures
mf.infotheo(Species ~ ., iris, 
            features=c("normClassEnt", "mutInf"))


## ------------------------------------------------------------------------
## Show the the available model.based measures
ls.model.based()

## Extract all model.based measures
land.iris <- mf.model.based(Species ~ ., iris, features="all")

## Extract three model.based measures
mf.model.based(Species ~ ., iris, features=c("leaves", "nodes"))

## ------------------------------------------------------------------------
## Show the the available landmarking measures
ls.landmarking()

## Extract all landmarking measures
land.iris <- mf.landmarking(Species ~ ., iris, features="all")

## Extract two landmarking measures
mf.landmarking(Species ~ ., iris, features=c("nb", "nn"))

## ------------------------------------------------------------------------
## Extract one landmarking measures with folds=2
mf.landmarking(Species ~ ., iris, features="nb", folds=2)

## Extract one landmarking measures with folds=2
mf.landmarking(Species ~ ., iris, features="nb", score="kappa")

## ------------------------------------------------------------------------
## Apply several statistical measures as post processing
mf.statistical(Species ~ ., iris, "cor", 
               summary=c("kurtosis", "max", "mean", "median", "min", "sd", 
                         "skewness", "var"))

## Apply quantile as post processing method
mf.statistical(Species ~ ., iris, "cor", summary="quantile")

## Get the default values without summarize them
mf.statistical(Species ~ ., iris, "cor", summary=c())

## ------------------------------------------------------------------------
## Apply histogram as post processing method
mf.statistical(Species ~ ., iris, "cor", summary="hist")

## Apply histogram as post processing method and customize it
mf.statistical(Species ~ ., iris, "cor", 
               summary="hist", bins=5, min=0, max=1)

## Extract all correlation values
mf.statistical(Species ~ ., iris, "cor", summary="non.aggregated")

## ------------------------------------------------------------------------
## Compute the absolute difference between the mean and the median 
my.method <- function(x, ...) abs(mean(x) - median(x))

## Using the user defined post processing method
mf.statistical(Species ~ ., iris, "cor", summary="my.method")

