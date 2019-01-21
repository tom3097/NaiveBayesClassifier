source("transformer/transformer.R")

main <- function() {
  args <- commandArgs(trailingOnly=TRUE)
  if (length(args) < 2 || length(args) > 3) {
  	cat("Please provide two or three required parameter (no more)")
  	return(-1)
  }

  dataset <- args[1]
  dictSize <- as.numeric(args[2])
  removeHeaders <- FALSE
  if (length(args) == 3 && args[3] == "removeHeaders") {
    removeHeaders <- TRUE
  }

  wordsFreq <- wordsDict(paste0("../data/transitional/", dataset, ".csv"), dictSize)
  bagOfWordsBinary <- transToBOW(paste0("../data/raw/", dataset), wordsFreq$word, TRUE, removeHeaders)

  abbr <- ""
  if (dataset == "20_newsgroups") {
    abbr <- "20NG"
  }
  if (dataset == "C50") {
    abbr <- "C50"
  }
  if (dataset == "emotions") {
    abbr <- "EMO"
  }

  save(bagOfWordsBinary, file = paste0("../data/processed/", abbr, "_", dictSize, "_B_.Rda"));
}

main()