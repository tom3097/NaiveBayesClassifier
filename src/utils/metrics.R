# Calculates accuracy for the given set of predicted labels and ground truth labels.
accuracyScore <- function(predLabels, trueLabels) {
  labTable <- table(predLabels, trueLabels)
  diagTable <- diag(nrow(labTable)) * labTable
  
  return(sum(diagTable) / sum(labTable))
}