# Multinomial Naive Bayess classifier implementation


multinomialNaiveBayess <- function(formula, data) {
  stopifnot(inherits(formula, "formula"))
  stopifnot(is.data.frame(data))
  
  m <- match.call(expand.dots = FALSE)
  m[[1L]] <- quote(stats::model.frame)
  m <- eval(m, parent.frame())
  Terms <- attr(m, "terms")
  Y <- model.extract(m, "response")
  X <- m[,-attr(Terms, "response"), drop = FALSE]
  
  aprioriProbs <- table(Y) / sum(table(Y))
  classProbs <- lapply(sort(unique(Y)), function(x) {
    clsData <- X[Y == x,]
    print(colSums(clsData))
    return(
      (colSums(clsData) + 1) / (sum(clsData) + ncol(X))
    )
  })
  classProbsMat <- matrix(data = unlist(classProbs), nrow = length(unique(Y)), byrow = TRUE)
  colnames(classProbsMat) <- colnames(X)
  rownames(classProbsMat) <- sort(unique(Y))
  conditionalProbs <- as.data.frame(classProbsMat)
  print(rowSums(conditionalProbs))
  
  return(
    structure(
      list(
        aprioriProbs = aprioriProbs,
        conditionalProbs = conditionalProbs
      ),
      class = "multinomialNaiveBayess"
    )
  )
}

print.multinomialNaiveBayess <- function(x, ...) {
  cat("\nMultinomial Naive Bayess for bag of words classification\n")
  cat("\nA-priori probabilities:\n")
  print(x$aprioriProbs)
  cat("\nConditional probabilities:\n")
  print(x$conditionalProbs)
}

predict.multinomialNaiveBayess <- function(object, data) {
  stopifnot(is.data.frame(data))
  
  aprioriProbsMat <- matrix(data = object$aprioriProbs, nrow = nrow(data), ncol = size(object$aprioriProbs), byrow = TRUE)
  scores <- matrix(data = 1, nrow = nrow(data), ncol = size(object$aprioriProbs))
  
  for (atrName in names(data)) {
    dataMat <- matrix(data[[atrName]], nrow = nrow(data), ncol = size(object$aprioriProbs))
    probsMat <- t(matrix(object$conditionalProbs[[atrName]], nrow = size(object$aprioriProbs),
                         ncol = nrow(data)))
    atrScore = probsMat ^ dataMat
    scores <- scores * atrScore
  }
  
  scores <- scores * aprioriProbsMat
  print(scores)
  Y <- argmax(scores)

  return(Y)
}



multinomialBOWTrain <- data.frame("dog" = c(2,4,1,4,1,2,0,6,2,1),
                                "cat" = c(3,5,2,5,7,2,8,2,1,4),
                                "head" = c(5,5,2,2,6,2,3,7,3,5),
                                "neck" = c(4,8,1,5,4,3,8,7,2,3),
                                "bear" = c(5,2,1,5,3,6,7,4,3,2),
                                "age" = c(5,2,6,3,2,6,5,3,2,7),
                                "myclass" = c(1,3,2,3,1,3,2,2,3,1))
multinomialBOWTest <- data.frame("dog" = c(3,4),
                                 "cat" = c(0,5),
                                 "head" = c(1,4),
                                 "neck" = c(2,2),
                                 "bear" = c(10,0),
                                 "age" = c(0,5))
print(multinomialBOWTrain)

mnb <- multinomialNaiveBayess(myclass ~., multinomialBOWTrain)
print(mnb)

y <- predict(bnb, multinomialBOWTest)
print(y)
