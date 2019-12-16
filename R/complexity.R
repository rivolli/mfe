#' Complexity meta-features
#'
#' The complexity group is a set of measures to characterize the complexity of 
#' classification problems based on aspects that quantify the linearity of the 
#' data, the presence of informative feature, the sparsity and dimensionality 
#' of the datasets.
#'
#' @family meta-features
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param features A list of features names or \code{"all"} to include all them.
#'  The supported values are described in the details section. (Default: 
#'  \code{"all"})
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{post.processing} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class
#' @param ... Not used.
#' 
#' @details
#'  The following features are allowed for classification problems:
#'  \describe{
#'    \item{"C1"}{Entropy of class proportions.}
#'    \item{"C2"}{Multi-class imbalance ratio.}
#'    \item{"F1"}{Fisher's discriminant ratio.}
#'    \item{"F1v"}{The directional-vector Fisher's discriminant ratio.}
#'    \item{"F2"}{Overlapping of the per-class bounding boxes.}
#'    \item{"F3"}{Maximum individual feature efficiency.}
#'    \item{"F4"}{Collective feature efficiency.}
#'    \item{"L1"}{Distance of erroneous instances to a linear classifier.}
#'    \item{"L2"}{Training error of a linear classifier.}
#'    \item{"L3"}{Nonlinearity of a linear classifier.}
#'    \item{"LSC"}{Local-Set cardinality average.}
#'    \item{"N1"}{Fraction of points lying on the class boundary.}
#'    \item{"N2"}{Average intra/inter class nearest neighbor distances.}
#'    \item{"N3"}{Leave-one-out error rate of the 1-nearest neighbor algorithm.}
#'    \item{"N4"}{Nonlinearity of the one-nearest neighbor classifier.}
#'    \item{"T1"}{Fraction of maximum covering spheres on data.}
#'    \item{"T2"}{Average number of samples per dimension.}
#'    \item{"T3"}{Average intrinsic dimensionality per number of examples.}
#'    \item{"T4"}{Intrinsic dimensionality proportion.}
#'  }
#'  Also it is possible to ask for a subgroup of features:
#'  \describe{
#'     \item{"balance"}{Include the measures C1 and C2.}
#'     \item{"dimensionality"}{Include the measures T2, T3 and T4.}
#'     \item{"linearity"}{Include the measures L1, L2 and L3.}
#'     \item{"neighborhood"}{Include the measures N1, N2, N3, N4, T1 and LSC.}
#'     \item{"network"}{Include the measures Density, ClsCoef and Hubs.}
#'     \item{"overlapping"}{Include the measures F1, F1v, F2, F3 and F4.}
#'  }
#' @return A list named by the requested meta-features.
#'
#' @references
#'  Ana C. Lorena, Luis P. F. Garcia, Jens Lehmann, Marcilio C. P. Souto, and 
#'  Tin Kam Ho. 2019. How Complex Is Your Classification Problem?: A Survey on 
#'  Measuring Classification Complexity. ACM Comput. Surv. 52, 5.
#'
#'  Lorena, A. C., Maciel, A. I., de Miranda, P. B. C., Costa, I. G., and 
#'  Prudencio, R. B. C. (2018). Data complexity meta-features for regression 
#'  problems. Machine Learning, 107(1):209-246.
#'
#'  Ho, T., and Basu, M. (2002). Complexity measures of supervised 
#'  classification problems. IEEE Transactions on Pattern Analysis and 
#'  Machine Intelligence, 24(3):289-300.
#'
#' @examples
#' ## Extract all metafeatures
#' complexity(Species ~ ., iris)
#'
#' ## Extract some metafeatures
#' complexity(iris[30:120, 1:4], iris[30:120, 5], c("F1", "F2", "linearity"))
#' @export
complexity <- function(...) {
  UseMethod("complexity")
}

#' @rdname complexity
#' @export
complexity.default <- function(x, y, features="all", 
                               summary=c("mean", "sd"), ...) {
  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }
  
  if(is.data.frame(y)) {
    y <- y[, 1]
  }
  
  y <- as.factor(y)
  if(min(table(y)) < 2) {
    stop("number of examples in the minority class should be >= 2")
  }
  
  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }
  
  if(features[1] == "all") {
    features <- ls.complexity()
  }
  
  if (any(features %in% ls.complexity.groups("class"))) {
    features <- unique(c(features, unlist(sapply(features, 
                                                 ls.complexity.groups))))
  }
  features <- match.arg(features, ls.complexity(), TRUE)
  colnames(x) <- make.names(colnames(x), unique=TRUE)
  
  groups <- names(which(sapply(ls.complexity.groups("class"), 
                 function(x) any(features %in% ls.complexity.groups(x)))))

  do.call(c, lapply(groups, function(group) {
    fmethod <- get(group, asNamespace("ECoL"))
    measures <- intersect(features, ls.complexity.groups(group))
    subgroups <- do.call(fmethod, list(x=x, y=y, measures=measures, 
                                       summary="return", ...))
    sapply(names(subgroups), function(measure){
      post.processing(subgroups[[measure]], summary, 
                      measure %in% ls.complexity.multiples(), ...)
    }, simplify = FALSE)
  }))[features]
}

#' @rdname complexity
#' @export
complexity.formula <- function(formula, data, features="all", 
                               summary=c("mean", "sd"), ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }
  
  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }
  
  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL
  
  complexity.default(modFrame[-1], modFrame[1], features, summary, ...)
}

#' List the complexity meta-features
#'
#' @return A list of complexity meta-features names
#' @export
#'
#' @examples
#' ls.complexity()
ls.complexity <- function() {
  sort(unlist(lapply(ls.complexity.groups("class"), ls.complexity.groups)))
}

ls.complexity.multiples <- function() {
  c(c("F1", "F1v", "F2", "F3", "F4"), #overlapping
    c("N2", "N3", "N4", "T1"), #neighborhood
    c("L1", "L2", "L3"), #linearity
    c(), #dimensionality
    c(), #balance
    c("Hubs") #network
  )
}

ls.complexity.groups <- function(type) {
  
  switch(type,
         class = {
           c("overlapping", "neighborhood", "linearity", "dimensionality",
             "balance", "network")
         }, regr = {
           c("correlation", "linearity", 
             "smoothness", "dimensionality")
         },
         overlapping = {
           c("F1", "F1v", "F2", "F3", "F4")
         },
         neighborhood = {
           c("N1", "N2", "N3", "N4", "T1", "LSC")
         },
         linearity = {
           c("L1", "L2", "L3")
         },
         dimensionality = {
           c("T2", "T3", "T4")
         },
         balance = {
           c("C1", "C2")
         },
         network = {
           c("Density", "ClsCoef", "Hubs")
         }
  )
}
