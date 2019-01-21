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
    for(catFile in categoryFiles) {
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

# Generates words dictionary with the given size based on the given frequency file.
wordsDict <- function(file, N) {
  wordsFreq <- read.csv(file = file, header = TRUE, sep = ",")
  wordsFreq <- wordsFreq[, 2:3]
  
  dict <- wordsFreq[1:N, ]
  return(dict)
}

# Transforms given dataset to bag of words using given dictionary.
transToBOW <- function(dirPath, words, binaryFreq, removeHeaders = FALSE) {
  bagOfWords <- data.frame(matrix(ncol = length(words)+1, nrow = 0));
  classID <- 1;
  
  categories <- list.files(dirPath);
  for(category in categories) {
    categoryPath <- paste(dirPath, toString(category), sep = "/");
    categoryFiles <- list.files(categoryPath);
    for(catFile in categoryFiles) {
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
      
      freqVec <- c()
      i <- 1
      for(w in words) {
        if(w %in% dataFrame$word) {
          freqVec[i] <- dataFrame[w, ]$freq;
          if (binaryFreq == TRUE) {
            freqVec[i] <- 1
          }
        }
        else {
          freqVec[i] <- 0;
        }
        i <- i + 1;
      }
      freqVec[i] <- classID;
      bagOfWords[nrow(bagOfWords)+1, ] <- freqVec;
    }
    
    classID <- classID + 1;
  }
  
  words <- c(levels(words)[words], 'Y')
  colnames(bagOfWords) <- words;
  
  for (column  in colnames(bagOfWords)) {
    bagOfWords[, column] <- as.factor(bagOfWords[, column]);
  }
  
  return(bagOfWords)
}