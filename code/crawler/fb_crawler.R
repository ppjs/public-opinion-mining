library(readxl)
library(httr)
library(tidyverse)
library(xlsx)

getPost_by_postID <- function(postID, token){

  url <- paste('https://graph.facebook.com/v3.1/', postID, '?fields=id,message,created_time,comments{message,created_time,comments}', '&access_token=', token, sep='')
  post <- content(GET(url))
  result <- tryCatch({
    p_message <- post$message
    comments <- post$comments
    while(!is.null(comments$data)){
      for(com in comments$data){
        comID <- com$id
        message <- com$message
        time <- com$created_time
        parentID <- pid
        comment_output <<- rbind(comment_output,
                                 cbind(cid, comID, message, time, parentID))
        replies <- com$comments
        while(!is.null(replies$data)){
          for(rep in replies$data){
            repID <- rep$id
            message <- rep$message
            time <- rep$created_time
            parentID <- cid
            reply_output <<- rbind(reply_output,
                                  cbind(rid, repID, message, time, parentID))
            rid <<- rid + 1
          }
          next_page <- replies$paging
          if(!is.null(next_page$'next')){
            res <- GET(next_page$'next')
            replies <- content(res)
          }else break
        }
        cid <<- cid + 1
      }
      next_page <- comments$paging
      if(!is.null(next_page$'next')){
        res <- GET(next_page$'next')
        comments <- content(res)
      }else break
    }
  }, error = function(e){
    return(NULL)
  })
  return(p_message)
}

df <- read_excel("../output_fb/title_v1.xlsx")
df$message <- ""
df$pid <- 1:nrow(df)

comment_output <- c()
reply_output <- c()

cid <- 1
rid <- 1

token <- "1131200690349794|WBfnAJ6n5AzCBELIG8G3AYM6eXw"

for(i in 1:nrow(df)){
  pid <- df$pid[i]
  postID <- df$post_id[i]
  message <- getPost_by_postID(postID, token)
  if(!is.null(message)){
    df$message[i] <- message
  }
  if(i / 10 == 0) Sys.sleep(3)
}

comment_output <- data.frame(comment_output)
reply_output <- data.frame(reply_output)
write.xlsx(df, "../output_fb/post.xlsx", row.names = FALSE)
write.xlsx(comment_output, "../output_fb/comment.xlsx", row.names = FALSE)
write.xlsx(reply_output, "../output_fb/reply.xlsx", row.names = FALSE)
