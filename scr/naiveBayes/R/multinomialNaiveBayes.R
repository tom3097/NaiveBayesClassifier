##' Multinomial naive bayes classifier for bag of words representation.
##'
##' @name multinomialNaiveBayes

library(ramify)

#'@title multinomialNaiveBayes
#'@description
#'Multinomial naive bayes classifier implementation. This function creates an instance of multinomialNaiveBayes
#'class and calculates apriori class probabilities as well as conditional probabilities. Uses Laplace smoothing
#'to ensure, that each conditional probability is greater than 0.
#'
#' @param formula Describes the name of the column, which will be used as a label.
#' @param data Dataframe that contains training data.
#' @return An instance of multinomialNaiveBayes class trained on the given data.
#' @export
multinomialNaiveBayes <- function(formula, data) {
  stopifnot(inherits(formula, "formula"))
  stopifnot(is.data.frame(data))
  
  m <- match.call(expand.dots = FALSE)
  m[[1L]] <- quote(stats::model.frame)
  m <- eval(m, parent.frame())
  Terms <- attr(m, "terms")
  Y <- model.extract(m, "response")
  X <- m[,-attr(Terms, "response"), drop = FALSE]
  
  aprioriProbs <- table(Y) / sum(table(Y))
  labels <- as.numeric(levels(sort(unique(Y)))[sort(unique(Y))])
  classProbs <- lapply(labels, function(x) {
    clsData <- X[Y == x,]
    clsData[1:length(clsData)] <- lapply(clsData[1:length(clsData)],function(x) {
      as.numeric(as.character(x))
    })
    return(
      (colSums(clsData) + 1) / (sum(clsData) + ncol(X))
    )
  })
  classProbsMat <- matrix(data = unlist(classProbs), nrow = length(unique(Y)), byrow = TRUE)
  colnames(classProbsMat) <- colnames(X)
  rownames(classProbsMat) <- sort(unique(Y))
  conditionalProbs <- as.data.frame(classProbsMat)

  return(
    structure(
      list(
        atrNames = names(X),
        aprioriProbs = aprioriProbs,
        conditionalProbs = conditionalProbs
      ),
      class = "multinomialNaiveBayes"
    )
  )
}

#'@title print.multinomialNaiveBayes
#'@description
#'Prints an instance of multinomialNaiveBayes class. This includes printing apriori class probabilities
#'as well as printing conditional probabilities.
#'
#' @param object An instance of multinomialNaiveBayes class to be printed.
#' @export
print.multinomialNaiveBayes <- function(x, ...) {
  cat("\nMultinomial Naive Bayes for bag of words classification\n")
  cat("\nA-priori probabilities:\n")
  print(x$aprioriProbs)
  cat("\nConditional probabilities:\n")
  print(x$conditionalProbs)
  print(nrow(x$conditionalProbs))
  print(ncol(x$conditionalProbs))
}

#'@title predict.multinomialNaiveBayes
#'@description
#'Predicts labels for the given data using previously trained classifier.
#'
#' @param object An instance of multinomialNaiveBayes class.
#' @param data Dataframe that contains data to be classified.
#' @return Vector of labels for the given data.
#' @export
predict.multinomialNaiveBayes <- function(object, data) {
  stopifnot(is.data.frame(data))
  
  aprioriProbsMat <- matrix(data = object$aprioriProbs, nrow = nrow(data),
                            ncol = size(object$aprioriProbs), byrow = TRUE)
  scores <- matrix(data = 1, nrow = nrow(data), ncol = size(object$aprioriProbs))
  
  for (atrName in object$atrNames) {
    atrData <- as.numeric(levels(data[[atrName]]))[data[[atrName]]]
    dataMat <- matrix(atrData, nrow = nrow(data), ncol = size(object$aprioriProbs))
    probsMat <- t(matrix(object$conditionalProbs[[atrName]], nrow = size(object$aprioriProbs),
                         ncol = nrow(data)))
    
    atrScore <- dataMat * log(probsMat)
    scores <- scores + atrScore
  }

  scores <- scores + log(aprioriProbsMat)
  Y <- argmax(scores)
  
  return(Y)
}