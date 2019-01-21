#FIXME: this package should load while loading naiveBayes package as a dependency
if (!require("ramify")) {
  install.packages("ramify", repo=NULL, type="source")
}
if (!require("naiveBayes")) {
  install.packages("naiveBayes", repo=NULL, type="source")
}
if (!require("e1071")) {
  install.packages("e1071", repo=NULL, type="source")
}

library(naiveBayes)
library(e1071)

source("utils/metrics.R")


main <- function() {
  args <- commandArgs(trailingOnly=TRUE)
  if (length(args) != 1) {
  	cat("Please provide one required parameter (no more)")
  	return(-1)
  }

  load(paste0("../data/processed/", args))

  cat("Testing MULTINOMIAL naive bayes\n\n")

  cat("e1071 naive bayes:\n")
  startTime <- Sys.time()
  naiveBayesModel <- naiveBayes(Y ~., data=bagOfWords, laplace = 1)
  NBPredictions <- predict(naiveBayesModel,bagOfWords)
  endTime <- Sys.time()
  cat(paste("Accuracy:", accuracyScore(NBPredictions, bagOfWords$Y), "\n"))
  cat(paste("Duration:", endTime - startTime, "\n"))

  cat("\n")

  cat("custom naive bayes:\n")
  startTime <- Sys.time()
  mnb <- multinomialNaiveBayes(Y ~., bagOfWords)
  mnbPredictions <- predict(mnb, bagOfWords)
  endTime <- Sys.time()
  cat(paste("Accuracy:", accuracyScore(mnbPredictions, bagOfWords$Y), "\n"))
  cat(paste("Duration:", endTime - startTime, "\n"))
}

main()