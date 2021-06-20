library(topicmodels)
library(ldatuning)
library(tidytext)
source("切詞與tfidf.R")

getTopicNumber <- function(dtm){
  result <- FindTopicsNumber(
    dtm,
    topics = seq(from = 4, to = 15, by = 1),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 77),
    mc.cores = 2L,
    verbose = TRUE
  )
  return(result)
}

getTopicTerms <- function(dtm, topic_number){
  lda <- LDA(dtm, k = topic_number, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  top_terms <- topics  %>% # take the topics data frame and..
    filter(nchar(term)>1) %>%
    group_by(topic) %>% # treat each topic as a different group
    top_n(10, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta)
  return(top_terms)
}
