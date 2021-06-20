library(tidyverse)
source("../function/lda.R")

df <- read_csv("../../../human_coding/ptt_post.csv") %>% select(pID, text) %>% rename(id=pID, message=text) %>% data.frame()
df <- read.csv("../../../human_coding/ptt_comment.csv") %>% select(cID, text) %>% rename(id=cID, message=text) %>% transform(message=as.character(message))%>% data.frame()
df <- read.csv("../../../human_coding/fb_post.csv") %>% select(pid, text) %>% rename(id=pid, message=text) %>% transform(message=as.character(message))%>% data.frame()
df <- rbind(
  read.csv("../../../human_coding/fb_comment.csv") %>% select(cid, text) %>% rename(id=cid, message=text),
  read.csv("../../../human_coding/fb_reply.csv") %>% select(rid, text) %>% rename(id=rid, message=text)
  ) %>% 
  transform(message=as.character(message)) %>% data.frame()

df <- df %>% filter(grepl("[\u4e00-\u9fa5]", message))

dtm <- jiebar_for_lda(df)
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm   <- dtm[rowTotals> 0, ]

result <- getTopicNumber(dtm)
FindTopicsNumber_plot(result)

top_terms <- getTopicTerms(dtm, 8)
top_terms %>% # take the top terms
  mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
  ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
  geom_col(show.legend = FALSE) + # as a bar plot
  facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
  labs(x = NULL, y = "Beta") + # no x label, change y label 
  coord_flip() + # turn bars sideways
  theme(text = element_text(family = "AdobeFanHeitiStd-Bold"))

