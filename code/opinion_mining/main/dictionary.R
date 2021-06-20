library(tidyverse)
library(jiebaR)
source("../function/dictionary.R")

cut = worker()
new_user_word(cut, c(att_objects$att_object, "柯文哲", "黃國昌"))

ptt_post <- read_csv("../../../human_coding/ptt_post.csv")
ptt_comment <- read_csv("../../../human_coding/ptt_comment.csv")
fb_post <- read_csv("../../../human_coding/fb_post.csv")
fb_comment <- read_csv("../../../human_coding/fb_comment.csv") %>% 
  select(text, coding_score)
fb_reply <- read_csv("../../../human_coding/fb_reply.csv") %>% 
  select(text, coding_score)

df <- fb_post %>% 
  select(pid, text) %>%
  mutate(
    single_score = 0,
    single_record = "",
    pos_ao_score = 0,
    neg_ao_score = 0,
    neu_ao_score = 0,
    multi_score = 0,
    multi_record = ""
  )

for(i in 1:nrow(df)){
  print(i)
  tokens <- segment(df$text[i], cut) %>% 
    filter_segment(stopwords) %>% gsub("[^\u4e00-\u9fa5]", "", .)
  #單一態度物件
  result <- get_sentiment_score(tokens, "single_att", 5)
  df$single_score[i] <- result$single_score
  df$single_record[i] <- result$single_record
  #價性態度物件
  result <- get_sentiment_score(tokens, "valanced_att", 5)
  df$pos_ao_score[i] <- result$pos_ao_score
  df$neg_ao_score[i] <- result$neg_ao_score
  df$neu_ao_score[i] <- result$neu_ao_score
  df$multi_score[i] <- result$multi_score
  df$multi_record[i] <- result$multi_record
}

df <- rbind(read_csv("../../training_data/fb_comment_dict.csv") %>% select(-cid),
            read_csv("../../training_data/fb_reply_dict.csv") %>% select(-rid))
df <- read_csv("../../training_data/ptt_comment_dict.csv")
df <- read_csv("../../training_data/ptt_post_dict.csv")
df <- read_csv("../../training_data/fb_post_dict.csv")
set.seed(1001)
n <- nrow(df)
t_idx <- sample(seq_len(n), size = round(0.7 * n))
df <- df[ - t_idx,]
(single_f1 <- get_f1(df, "single"))
(multi_f1 <- get_f1(df, "valanced"))

ggplot(data = df) +
    geom_point(
      mapping = aes(x = coding_score, y = multi_score)
    ) + xlab("人工編碼") + ylab("價性態度物件") +
    theme(text = element_text(family = "AdobeFanHeitiStd-Bold"))
ggsave("../../../multi_fb_comment.png")

create_op_words(df)
for(i in 1:nrow(df)){
  print(i)
  tokens <- segment(df$text[i], cut) %>% 
    filter_segment(stopwords) %>% gsub("[^\u4e00-\u9fa5]", "", .)
  # tokens <- df$tokens[i] %>% unlist()
  result <- get_sentiment_score_correct(tokens, "single_att", 5)
  df$single_score[i] <- result$single_score
  df$single_record[i] <- result$single_record
  result <- get_sentiment_score_correct(tokens, "valanced_att", 5)
  df$pos_ao_score[i] <- result$pos_ao_score
  df$neg_ao_score[i] <- result$neg_ao_score
  df$neu_ao_score[i] <- result$neu_ao_score
  df$multi_score[i] <- result$multi_score
  df$multi_record[i] <- result$multi_record
}
(single_f1 <- get_f1(df, "single"))
(multi_f1 <- get_f1(df, "valanced"))
