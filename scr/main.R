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
  print("Hello World")
}

main()