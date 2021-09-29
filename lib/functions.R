f.word_count=function(str){
  library(stringr)
  return(str_count(str, '\\w+'))
}


create_wc <- function(data){
  text <- data$tokenized_txt
  docs <- Corpus(VectorSource(text))
  
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  
  dtm <- TermDocumentMatrix(docs) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  df <- data.frame(word = names(words),freq=words)
  
  set.seed(1234)
  wordcloud(words = df$word, freq = df$freq, min.freq = 10,
            max.words=150, random.order=FALSE, rot.per=0.35,
            colors=brewer.pal(8, "Dark2"), scale = c(3.5, 0.25))
}