library(tm);
library(qdap);
library(SnowballC)
library(stringr);

# Applies tm_map transformations.
tmMapTransform <- function(data) {
  words <- Corpus(VectorSource(data));
  
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x));
  words <- tm_map(words, toSpace, "/");
  words <- tm_map(words, toSpace, "@");
  words <- tm_map(words, toSpace, "\\|");
  
  words <- tm_map(words, content_transformer(tolower));
  words <- tm_map(words, removeNumbers);
  words <- tm_map(words, removePunctuation);
  words <- tm_map(words, stripWhitespace);
  words <- tm_map(words, removeWords, stopwords("en"));
  words <- tm_map(words, stemDocument, language = "english");
  
  dtm <- TermDocumentMatrix(words);
  matrix <- as.matrix(dtm);
  frequence <- sort(rowSums(matrix),decreasing=TRUE);
  
  dataFrame <- data.frame(word = names(frequence), freq = frequence);
  
  return(dataFrame)
}

# Calculates words frequency for the given dataset.
wordsFreq <- function(dirPath, removeHeaders = FALSE) {
  resultFrame <-data.frame(word='', freq=0)
  
  categories <- list.files(dirPath);
  for(category in categories) {
    categoryPath <- paste(dirPath, toString(category), sep = "/");
    categoryFiles <- list.files(categoryPath);
    for(catFile in categoryFiles)
    {
      filePath <- paste(categoryPath, toString(catFile), sep = "/");
      
      data <- readLines(filePath);
            
      if (removeHeaders == TRUE) {
        flag <- FALSE;
        dirtyData <- data
        data <- character();
        iter <- 0;
        
        for(line in dirtyData) {
          
          if(!isTRUE(flag) && (!str_detect(line, ""))) {
            flag <- TRUE;
          }
          
          if(!isTRUE(flag)) {
            next;
          }
          else {
            iter <- iter + 1;
            data[iter] <- line;
          }
        }
      }
      
      dataFrame <- tmMapTransform(data)
      
      resultFrame <- merge(x = resultFrame, y = dataFrame, by = "word", all = TRUE)
      resultFrame[is.na(resultFrame)] <- 0
      freq <- resultFrame$freq.x + resultFrame$freq.y;
      resultFrame <- data.frame(word = resultFrame$word, freq = freq)
    }
  }
  
  freqs <- resultFrame[rev(order(resultFrame$freq)), ]
  return(freqs)
}