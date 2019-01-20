##' Bernoulli naive bayes classifier for bag of words representation.
##'
##' @name bernoulliNaiveBayes

library(ramify)

#'@title bernoulliNaiveBayes
#'@description
#'Bernoulli naive bayes classifier implementation. This function creates an instance of bernoulliNaiveBayes
#'class and calculates apriori class probabilities as well as conditional probabilities. Uses Laplace smoothing
#'to ensure, that each conditional probability is greater than 0.
#'
#' @param formula Describes the name of the column, which will be used as a label.
#' @param data Dataframe that contains training data.
#' @return An instance of bernoulliNaiveBayes class trained on the given data.
#' @export
bernoulliNaiveBayes <- function(formula, data) {
  stopifnot(inherits(formula, "formula"))
  stopifnot(is.data.frame(data))
  
  m <- match.call(expand.dots = FALSE)
  m[[1L]] <- quote(stats::model.frame)
  m <- eval(m, parent.frame())
  Terms <- attr(m, "terms")
  Y <- model.extract(m, "response")
  X <- m[,-attr(Terms, "response"), drop = FALSE]
  
  aprioriProbs <- table(Y) / sum(table(Y))
  conditionalProbs <- lapply(names(X), function(atrName) {
    atrLevels <- levels(X[[atrName]])
    stopifnot(length(atrLevels) == 2)
    stopifnot(atrLevels[1] == "0")
    stopifnot(atrLevels[2] == "1")
    return(
      (table(Y, X[[atrName]]) + 1) / (rowSums(table(Y, X[[atrName]])) + 2)
    )
  })
  names(conditionalProbs) <- names(X)
  
  return(
    structure(
      list(
        atrNames = names(X),
        aprioriProbs = aprioriProbs,
        conditionalProbs = conditionalProbs
      ),
      class = "bernoulliNaiveBayes"
    )
  )
}

#'@title print.bernoulliNaiveBayes
#'@description
#'Prints an instance of bernoulliNaiveBayes class. This includes printing apriori class probabilities
#'as well as printing conditional probabilities.
#'
#' @param object An instance of bernoulliNaiveBayes class to be printed.
#' @export
print.bernoulliNaiveBayes <- function(object, ...) {
  cat("\nBernoulli Naive Bayes for bag of words classification\n")
  cat("\nA-priori probabilities:\n")
  print(object$aprioriProbs)
  cat("\nConditional probabilities:\n")
  for (i in c(1: length(object$conditionalProbs))) {
    cat(paste("\nWord:", names(object$conditionalProbs)[[i]]))
    print(object$conditionalProbs[[i]])
  }
}

#'@title predict.bernoulliNaiveBayes
#'@description
#'Predicts labels for the given data using previously trained classifier.
#'
#' @param object An instance of bernoulliNaiveBayes class.
#' @param data Dataframe that contains data to be classified.
#' @return Vector of labels for the given data.
#' @export
predict.bernoulliNaiveBayes <- function(object, data) {
  stopifnot(is.data.frame(data))

  aprioriProbsMat <- matrix(data = object$aprioriProbs, nrow = nrow(data),
                            ncol = length(object$aprioriProbs), byrow = TRUE)
  scores <- matrix(data = 1, nrow = nrow(data), ncol = length(object$aprioriProbs))
  
  for (atrName in object$atrNames) {
    dataMat <- as.matrix(as.numeric(levels(data[[atrName]]))[data[[atrName]]])
    probs0Mat <- t(as.matrix(object$conditionalProbs[[atrName]][, "0"]))
    probs1Mat <- t(as.matrix(object$conditionalProbs[[atrName]][, "1"]))
    
    atrScore <- dataMat %*% probs1Mat + (1 - dataMat) %*% probs0Mat
    scores <- scores * atrScore
  }
  
  scores <- scores * aprioriProbsMat
  Y <- argmax(scores)
  
  return(Y)
}