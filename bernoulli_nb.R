# Bernoulli Naive Bayess classifier implementation

library(ramify)

bernoulliNaiveBayess <- function(formula, data) {
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
    return(
      (table(Y, X[[atrName]]) + 1) / (rowSums(table(Y, X[[atrName]])) + 2)
    )
  })
  names(conditionalProbs) <- names(X)
  
  return(
    structure(
      list(
        aprioriProbs = aprioriProbs,
        conditionalProbs = conditionalProbs
      ),
      class = "bernoulliNaiveBayess"
    )
  )
}

print.bernoulliNaiveBayess <- function(x, ...) {
  cat("\nBernoulli Naive Bayess for bag of words classification\n")
  cat("\nA-priori probabilities:\n")
  print(x$aprioriProbs)
  cat("\nConditional probabilities:\n")
  for (i in c(1: length(x$conditionalProbs))) {
    cat(paste("\nWord:", names(x$conditionalProbs)[[i]]))
    print(x$conditionalProbs[[i]])
  }
}

predict.bernoulliNaiveBayess <- function(object, data) {
  stopifnot(is.data.frame(data))
  
  aprioriProbsMat <- matrix(data = object$aprioriProbs, nrow = nrow(data), ncol = size(object$aprioriProbs), byrow = TRUE)
  scores <- matrix(data = 1, nrow = nrow(data), ncol = size(object$aprioriProbs))
  
  for (atrName in names(data)) {
    dataMat <- as.matrix(data[[atrName]])
    probs1Mat <- t(as.matrix(object$conditionalProbs[[atrName]][, "1"]))
    probs0Mat <- t(as.matrix(object$conditionalProbs[[atrName]][, "0"]))
    
    atrScore <- dataMat %*% probs1Mat + (1 - dataMat) %*% probs0Mat
    scores <- scores * atrScore
  }
  
  scores <- scores * aprioriProbsMat
  Y <- argmax(scores)
  
  return(Y)
}


bernoulliBOWTrain <- data.frame("dog" = c(0,0,1,0,1,0,1,1,0,1),
                          "cat" = c(1,1,0,1,1,0,0,0,1,0),
                          "head" = c(1,0,1,1,1,0,0,0,1,1),
                          "neck" = c(1,0,0,0,1,0,0,1,1,0),
                          "bear" = c(1,0,1,1,0,1,0,0,0,0),
                          "age" = c(0,0,0,0,1,1,1,1,0,0),
                          "myclass" = c(1,2,2,3,1,3,2,2,3,1))

bernoulliBOWTest <- data.frame("dog" = c(0,1),
                              "cat" = c(0,1),
                              "head" = c(1,1),
                              "neck" = c(0,0),
                              "bear" = c(1,1),
                              "age" = c(1,0))

print(bernoulliBOW)

bnb <- bernoulliNaiveBayess(myclass ~., bernoulliBOWTrain)
print(bnb)

y <- predict(bnb, bernoulliBOWTest)
print(y)
