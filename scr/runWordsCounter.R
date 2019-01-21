source("transformer/transformer.R")

main <- function() {
  args <- commandArgs(trailingOnly=TRUE)
  if (length(args) < 1 || length(args) > 2) {
  	cat("Please provide one or two required parameter (no more)")
  	return(-1)
  }

  dataset <- args[1]
  removeHeaders <- FALSE
  if (length(args) == 2 && args[2] == "removeHeaders") {
  	removeHeaders <- TRUE
  }

  freqs <- wordsFreq(paste0("../data/raw/", dataset), removeHeaders)
  write.csv(freqs, file = paste0("../data/transitional/", dataset, ".csv"))
}

main()