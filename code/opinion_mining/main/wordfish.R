source("../../source/wordfish_1.3.r")
source("../../source/切詞與tfidf.R")

ptt_post <- read_csv("../../../human_coding/ptt_post.csv") %>% mutate(
  stance = ifelse(coding_score>0, 1, ifelse(coding_score<0, -1, 0))
)
ptt_comment <- read_csv("../../../human_coding/ptt_comment.csv") %>% mutate(
  stance = ifelse(coding_score>0, 1, ifelse(coding_score<0, -1, 0))
)
fb_comment <- read_csv("../../../human_coding/fb_comment.csv")
fb_reply <- read_csv("../../../human_coding/fb_reply.csv")
fb_cr <- rbind(
  fb_comment %>% select(cid, text, coding_score) %>% rename_(id="cid"),
  fb_reply %>% select(rid, text, coding_score) %>% rename_(id="rid")
  ) %>% 
  mutate(
    stance = ifelse(coding_score>0, 1, ifelse(coding_score<0, -1, 0))
  )

df <- ptt_post %>% select(pID, text) %>% 
  rename_(id="pID", message="text") %>% data.frame()
df <- ptt_comment %>% select(cID, text) %>% 
  rename_(id="cID", message="text") %>% data.frame()
df <- fb_cr %>% select(id, text) %>% 
  rename_(id="id", message="text") %>% data.frame()
tdm <- cut_jiebar(df)
input_matrix <- as.matrix(tdm)
input_matrix <- input_matrix[,colSums(input_matrix)!=0]
# tf_idf <- tfidf(tdm)
# input_matrix <- tf_idf
result <- wordfish(input_matrix, dir=c(39,7), writeout = TRUE)
result <- wordfish(input_matrix, dir=c(3,10), writeout = TRUE)
result <- wordfish(input_matrix, dir=c(20,26), writeout = TRUE) 

ptt_post$word_fish <- result$documents[,1]
ggplot(data = ptt_post) +
  geom_point(
    mapping = aes(x = coding_score, y = word_fish)
  )

x <- round(result$documents[,1],1)
result_stance <- ifelse(x>0, 1,
                        ifelse(result$documents[,1]<0, -1, 0))
orig_stance <- fb_cr$stance[colSums(input_matrix)!=0]

cm <- table(orig_stance, result_stance)
cm
(pre_pos <- cm[3,3] / sum(cm[,3]))
(rec_pos <- cm[3,3] / sum(cm[3,]))
(f1_pos <- 2*pre_pos*rec_pos/(pre_pos+rec_pos))
(pre_neg <- cm[1,1] / sum(cm[,1]))
(rec_neg <- cm[1,1] / sum(cm[1,]))
(f1_neg <- 2*pre_neg*rec_neg/(pre_neg+rec_neg))
(pre_neu <- cm[2,2] / sum(cm[,2]))
(rec_neu <- cm[2,2] / sum(cm[2,]))
(f1_neu <- 2*pre_neu*rec_neu/(pre_neu+rec_neu))
(f1_pos+f1_neg+f1_neu)/3

