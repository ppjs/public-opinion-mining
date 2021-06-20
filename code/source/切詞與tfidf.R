library(jiebaR)
library(tmcn)
library(tm)
library(Matrix)
# library(googleLanguageR)
# gl_auth("thesis-29eb6ba4f72a.json")

stopwords <- read.table("../../source/停用詞-繁體中文.txt")
stopwords <- stopwords$V1 %>% as.character()

clean_tm <- function(corpus){
  tm <- Corpus(DataframeSource(corpus), readerControl = list(language = "NA"))
  tm <- tm_map(tm, removePunctuation)
  tm <- tm_map(tm, removeNumbers)
  tm <- tm_map(tm, content_transformer(function(x){
    gsub("[A-Za-z0-9]", "", x)
  }))
  tm <- tm_map(tm, content_transformer(function(x){
    gsub("[^\u4e00-\u9fa5]", " ", x)
  }))
  return(tm)
}
att_object <- read_csv("../../source/att_obeject.csv")
cut_jiebar <- function(corpus){
  cut = worker()
  tagger = worker("tag")
  new_user_word(tagger, c(att_object$att_object, "柯文哲", "黃國昌"))
  corpus$words <- ""
  for(i in 1:nrow(corpus)){
    words <- tagger <= corpus$message[i]
    # words <- list(words[names(words) %in% c("n","nr","nr1","nr2","nrj","nrf","ns","nsf","nt","nz","nl","ng")])
    words <- list(words)
    corpus[i,]$words <- words
  }
  corpus$text <- NULL
  names(corpus)[names(corpus) == 'id'] <- 'doc_id'
  names(corpus)[names(corpus) == 'words'] <- 'text'
  tm <- clean_tm(corpus)
  tdm <- TermDocumentMatrix(tm,
                            control = list(wordLengths = c(1, Inf), stopwords=stopwords))
  # tdm <- DocumentTermMatrix(tm,
  #                           control = list(wordLengths = c(1, Inf), stopwords=stopwords))
  
  return(tdm)
}

jiebar_for_lda <- function(corpus){
  cut = worker()
  tagger = worker("tag")
  new_user_word(tagger, c(att_object$att_object, "柯文哲", "黃國昌"))
  corpus$words <- ""
  for(i in 1:nrow(corpus)){
    words <- segment(corpus$message[i], tagger)
    # words <- list(words[names(words) %in% c("n","nr","nr1","nr2","nrj","nrf","ns","nsf","nt","nz","nl","ng")])
    words <- list(words)
    corpus[i,]$words <- words
  }
  corpus$text <- NULL
  names(corpus)[names(corpus) == 'id'] <- 'doc_id'
  names(corpus)[names(corpus) == 'words'] <- 'text'
  tm <- clean_tm(corpus)
  tdm <- DocumentTermMatrix(tm,
                            control = list(wordLengths = c(1, Inf), stopwords=stopwords))
  
  return(tdm)
}

cut_google <- function(corpus){
  corpus$words <- ""
  for(i in 1:nrow(corpus)){
    nlp_result <- gl_nlp(corpus$message[i],
                         nlp_type = "analyzeSyntax",
                         language = "zh")
    tokens <- nlp_result$tokens[[1]]
    words <- tokens[tokens$tag=="NOUN",]$content
    words <- tokens$content
    corpus[i,]$words <- list(words)
  }
  names(corpus)[names(corpus) == 'id'] <- 'doc_id'
  names(corpus)[names(corpus) == 'words'] <- 'text'
  tm <- clean_tm(corpus)
  tdm <- TermDocumentMatrix(tm,
                            control = list(wordLengths = c(1, Inf), stopwords=stopwords))
  # tdm <- DocumentTermMatrix(tm,
  #                           control = list(wordLengths = c(1, Inf), stopwords=stopwords))
  return(tdm)
}

cut_center <- function(corpus){
  corpus$words <- ""
  for(i in 1:nrow(corpus)){
    id <- corpus$pid[i]
    word_df <- read_csv(paste("../核電粉專/all_words/",id,".txt.csv",sep=""),
                        col_types = c(word = col_character(),
                                      tag = col_character()))
    word_df <- word_df[grepl("N",word_df$tag) & nchar(word_df$tag)<=4,]
    if(length(word_df$word)>0){
      corpus$words[i] <-  list(word_df$word)
    }
  }
  names(corpus)[names(corpus) == 'id'] <- 'doc_id'
  names(corpus)[names(corpus) == 'words'] <- 'text'
  tm <- clean_tm(corpus)
  tdm <- TermDocumentMatrix(tm, 
                            control = list(wordLengths = c(1, Inf), stopwords=stopwords))
  return(tdm)
}

tfidf <- function(tdm){
  tf <- apply(tdm, 2, sum) # term frequency
  idf <- function(word_doc){ log2( (length(word_doc)+1) / nnzero(word_doc) ) }
  idf <- apply(tdm, 1, idf)
  doc.tfidf <- as.matrix(tdm)
  for(i in 1:nrow(tdm)){
    for(j in 1:ncol(tdm)){
      doc.tfidf[i,j] <- (doc.tfidf[i,j] / tf[j]) * idf[i]
    }
  }
  return(doc.tfidf)
}

find_keyword <- function(doc.tfidf, threshold){
  output <- c()
  for(i in 1:ncol(doc.tfidf)){
    tmp <- doc.tfidf[, i][doc.tfidf[, i] > threshold] %>% sort(decreasing = TRUE)
    if(length(tmp)==0) next
    else{
      output <- rbind(output,
                      cbind(names(tmp)[1:length(tmp)]))
    }
    # if(length(tmp) > 10) {
    #   output <- rbind(output, 
    #                   # cbind(names(tmp[tmp>1]), tmp[tmp>1]))
    #                   # cbind(names(tmp)[1:5], tmp[1:5]))
    #                   cbind(names(tmp)[1:10]))
    # }else{
      # output <- rbind(output,
      #                 # cbind(names(tmp[tmp>1]), tmp[tmp>1]))
      #                 # cbind(names(tmp)[1:5], tmp[1:5]))
      #                 cbind(names(tmp)[1:length(tmp)]))
    # }
  }
  tfidf_freq <- data.frame(table(output)) %>% arrange(desc(Freq)) %>% transform(
    output = as.character(output)
  )
  return(tfidf_freq)
}

