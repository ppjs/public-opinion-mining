att_objects <- read_csv("../../source/att_obeject.csv")
stopwords <- read.table("../../source/停用詞-繁體中文.txt")
stopwords <- stopwords$V1 %>% as.character()
stopwords <- stopwords[!grepl("[^\u4e00-\u9fa5]", stopwords)]
#台大情緒字典
dict <- read_csv("../../source/opinion_word_utf8.csv", col_names = FALSE)
names(dict) <- c("word", "score", "postag", "neutag", "negtag", "nonop", "nonword")
dict <- dict %>% 
  filter(!(nonword==1 | nonop==1)) %>% 
  filter(score!=0) %>% 
  filter(!word %in% stopwords)

detect_sentiment <- function(tokens){
  if(length(tokens)==0) return(list(0, ""))
  op_count <- 0
  total_score <- 0
  total_record <- ""
  for(i in seq_along(tokens)){
    if(tokens[i] %in% dict$word){
      op_count <- op_count + 1
      op_word <- tokens[i]
      op_score <- dict %>% filter(word == op_word) %>% .$score
      op_record <- op_word
      if((i-1)>0){
        if(tokens[(i-1)]=="不"){
          op_score <- op_score*(-1)
          op_record <- str_c(op_record, "*", "不")
        }
      }
      total_score <- total_score + op_score
      total_record <- ifelse(total_record=="",
                             str_c(total_record, op_record),
                             str_c(total_record, "+", op_record))
    }
  }
  return(list(total_score, total_record, op_count))
}

get_sentiment_score <- function(tokens, tell_att, n){
  pos_ao_score <- 0
  neg_ao_score <- 0
  neu_ao_score <- 0
  output_score <- 0
  output_record <- ""
  output_count <- 0
  if(tell_att=="single_att"){
    subjects <- att_objects$att_object[att_objects$stance==0]
    subject_inds <- which(tokens %in% subjects)
    for(a_ind in subject_inds){
      s_ind <- ifelse((a_ind-n) < 1, 1, (a_ind-n))
      e_ind <- ifelse((a_ind+n) > length(tokens), length(tokens), (a_ind+n))
      ao_tokens <- tokens[s_ind:e_ind]
      tmp_result <- detect_sentiment(ao_tokens)
      output_score <- output_score + tmp_result[[1]]
      if(tmp_result[[1]]!=0){
        output_record <- str_c(output_record, "/", 
                               tokens[a_ind], "/", tmp_result[[2]])
        output_count <- output_count + tmp_result[[3]]
      }
    }
    if(output_count!=0) output_score <- output_score/output_count
    return(list(single_score=output_score,
                single_record=output_record))
  }else{
    subjects <- att_objects$att_object
    subject_inds <- which(tokens %in% subjects)
    for(a_ind in subject_inds){
      s_ind <- ifelse((a_ind-n) < 1, 1, (a_ind-n))
      e_ind <- ifelse((a_ind+n) > length(tokens), length(tokens), (a_ind+n))
      ao_tokens <- tokens[s_ind:e_ind]
      tmp_result <- detect_sentiment(ao_tokens)
      if(att_objects$stance[att_objects$att_object==tokens[a_ind]]==1){
        pos_ao_score <- pos_ao_score + tmp_result[[1]]
      }else if(att_objects$stance[att_objects$att_object==tokens[a_ind]]==(-1)){
        neg_ao_score <- neg_ao_score + tmp_result[[1]]
      }else{
        neu_ao_score <- neu_ao_score + tmp_result[[1]]
      }
      if(tmp_result[[1]]!=0){
        output_record <- str_c(output_record, "/", 
                               tokens[a_ind], "/", tmp_result[[2]])
        output_count <- output_count + tmp_result[[3]]
      }
    }
    output_score <- pos_ao_score - neg_ao_score + neu_ao_score
    if(output_count!=0) output_score <- output_score/output_count
    return(list(pos_ao_score=pos_ao_score,
                neg_ao_score=neg_ao_score,
                neu_ao_score=neu_ao_score,
                multi_score=output_score,
                multi_record=output_record))
  }
}

get_f1 <- function(df, tell){
  d <- df %>% select(single_score, multi_score, coding_score) %>% 
    transform(
      single_score = ifelse(single_score>0, 1, ifelse(single_score<0, -1, 0)),
      multi_score = ifelse(multi_score>0, 1, ifelse(multi_score<0, -1, 0)),
      coding_score = ifelse(coding_score>0, 1, ifelse(coding_score<0, -1, 0))
    )
  if(tell=="single"){
    cm <- table(d$coding_score, d$single_score)
  }else{
    cm <- table(d$coding_score, d$multi_score)
  }
  (pre_pos <- cm[3,3] / sum(cm[,3]))
  (rec_pos <- cm[3,3] / sum(cm[3,]))
  (f1_pos <- 2*pre_pos*rec_pos/(pre_pos+rec_pos))
  (pre_neg <- cm[1,1] / sum(cm[,1]))
  (rec_neg <- cm[1,1] / sum(cm[1,]))
  (f1_neg <- 2*pre_neg*rec_neg/(pre_neg+rec_neg))
  (pre_neu <- cm[2,2] / sum(cm[,2]))
  (rec_neu <- cm[2,2] / sum(cm[2,]))
  (f1_neu <- 2*pre_neu*rec_neu/(pre_neu+rec_neu))
  return(list(cm = cm,
              sup_f1 = f1_pos,
              unsup_f1 = f1_neg,
              nonop_f1 = f1_neu,
              avg_f1 = (f1_pos+f1_neg+f1_neu)/3))
}

create_op_words <- function(d){
  bad <- d %>% filter(coding_score>0&multi_score<0)
  opinion_words <- create_opinion_words(bad)
  
  d_opinion_words <- opinion_words %>% 
    filter(att_object %in% att_objects$att_object[att_objects$stance==(0)])
  bad_neu_op_words <<- d_opinion_words %>% select(op_word, freq) %>% 
    group_by(op_word) %>% 
    summarize(
      count = sum(freq)
    ) %>% rowwise() %>% mutate(
      stance = ifelse(dict$score[dict$word == op_word]>0, 1, -1)
    ) %>% arrange(desc(count))
  
  d_opinion_words <- opinion_words %>% filter(att_object %in% att_objects$att_object[att_objects$stance==(-1)])
  bad_neg_op_words <<- d_opinion_words %>% select(op_word, freq) %>% 
    group_by(op_word) %>% 
    summarize(
      count = sum(freq)
    ) %>% rowwise() %>% mutate(
      stance = ifelse(dict$score[dict$word == op_word]>0, 1, -1)
    ) %>% arrange(desc(count))
}

create_opinion_words <- function(dd){
  opinion_words <- data.frame(att_object=character(),
                              op_word=character(),
                              freq=integer())
  for(record in dd$multi_record){
    if(is.na(record)) next
    array <- record %>% str_c(collapse = "") %>% str_split(pattern = "\\+|/") %>% sapply("[")
    array <- array[array!=""]
    for(w in array){
      w <- w %>% gsub("[\\*].+", "", .)
      if(w %in% att_objects$att_object){
        tag <- w
        next
      }else{
        n <- opinion_words %>% filter(att_object==tag & op_word==w)
        if(nrow(n)==0){
          opinion_words <- opinion_words %>% add_row(
            att_object = tag,
            op_word = w,
            freq = 1
          )
        }else{
          opinion_words <- opinion_words %>% mutate(
            freq = ifelse(att_object==tag & op_word==w, freq + 1, freq)
          )
        }
      }
    }
  }
  return(opinion_words)
}

detect_sentiment_correct <- function(att_stance, tokens){
  if(length(tokens)==0) return(list(0, ""))
  op_count <- 0
  total_score <- 0
  total_record <- ""
  for(i in seq_along(tokens)){
    if(tokens[i] %in% dict$word){
      op_count <- op_count + 1
      op_word <- tokens[i]
      op_score <- dict %>% filter(word == op_word) %>% .$score
      op_record <- op_word
      if((i-1)>0){
        if(tokens[(i-1)]=="不"){
          op_score <- op_score*(-1)
          op_record <- str_c(op_record, "*", "不")
        }
      }
      if(att_stance=="neg"){
        if(op_word %in% bad_neg_op_words$op_word){
          op_score <- op_score*(-1)
        }
      }
      if(att_stance=="neu"){
        if(op_word %in% bad_neu_op_words$op_word){
          op_score <- op_score*(-1)
        }
      }
      total_score <- total_score + op_score
      total_record <- ifelse(total_record=="",
                             str_c(total_record, op_record),
                             str_c(total_record, "+", op_record))
    }
  }
  return(list(total_score, total_record, op_count))
}

get_sentiment_score_correct <- function(tokens, tell_att, n){
  pos_ao_score <- 0
  neg_ao_score <- 0
  neu_ao_score <- 0
  output_score <- 0
  output_record <- ""
  output_count <- 0
  if(tell_att=="single_att"){
    subjects <- att_objects$att_object[att_objects$stance==0]
    subject_inds <- which(tokens %in% subjects)
    for(a_ind in subject_inds){
      s_ind <- ifelse((a_ind-n) < 1, 1, (a_ind-n))
      e_ind <- ifelse((a_ind+n) > length(tokens), length(tokens), (a_ind+n))
      ao_tokens <- tokens[s_ind:e_ind]
      tmp_result <- detect_sentiment_correct("neu", ao_tokens)
      output_score <- output_score + tmp_result[[1]]
      if(tmp_result[[1]]!=0){
        output_record <- str_c(output_record, "/", 
                               tokens[a_ind], "/", tmp_result[[2]])
        output_count <- output_count + tmp_result[[3]]
      }
    }
    if(output_count!=0) output_score <- output_score/output_count
    return(list(single_score=output_score,
                single_record=output_record))
  }else{
    subjects <- att_objects$att_object
    subject_inds <- which(tokens %in% subjects)
    for(a_ind in subject_inds){
      s_ind <- ifelse((a_ind-n) < 1, 1, (a_ind-n))
      e_ind <- ifelse((a_ind+n) > length(tokens), length(tokens), (a_ind+n))
      ao_tokens <- tokens[s_ind:e_ind]
      if(att_objects$stance[att_objects$att_object==tokens[a_ind]]==1){
        tmp_result <- detect_sentiment_correct("pos", ao_tokens)
        pos_ao_score <- pos_ao_score + tmp_result[[1]]
      }else if(att_objects$stance[att_objects$att_object==tokens[a_ind]]==(-1)){
        tmp_result <- detect_sentiment_correct("neg", ao_tokens)
        neg_ao_score <- neg_ao_score + tmp_result[[1]]
      }else{
        tmp_result <- detect_sentiment_correct("neu", ao_tokens)
        neu_ao_score <- neu_ao_score + tmp_result[[1]]
      }
      if(tmp_result[[1]]!=0){
        output_record <- str_c(output_record, "/", 
                               tokens[a_ind], "/", tmp_result[[2]])
        output_count <- output_count + tmp_result[[3]]
      }
    }
    output_score <- pos_ao_score - neg_ao_score + neu_ao_score
    if(output_count!=0) output_score <- output_score/output_count
    return(list(pos_ao_score=pos_ao_score,
                neg_ao_score=neg_ao_score,
                neu_ao_score=neu_ao_score,
                multi_score=output_score,
                multi_record=output_record))
  }
}