library(readxl)
library(tidyverse)
library(rvest)

sheetNames <- excel_sheets("../output_ptt/title_v2.xlsx")
df <- c()
for(sheet in sheetNames){
  df <- read_excel("../output_ptt/title_v2.xlsx", sheet = sheet) %>%
    rbind(df, .)
}
df$ID <- c(1:nrow(df))
df$time <- ""
df$user_id <- ""
df$post <- ""

com_output <- c()
for(i in 1:nrow(df)){
  print(i)
  url <- df$url[i]
  if(df$board[i]=="Gossiping"){
    result <- tryCatch({
      gossiping.session <- html_session(url)
      gossiping.form <- gossiping.session %>%
        html_node("form") %>%
        html_form()
      content <- submit_form(session = gossiping.session, 
                             form = gossiping.form, submit = "yes")
    }, warning = function(e){
      return(NULL)
    }, error = function(e){
      return(NULL)
    }
    )
  }else{
    result <- tryCatch({
      content <- read_html(url)
    }, warning = function(e){
      return(NULL)
    }, error = function(e){
      return(NULL)
    }
    )
  }
  if(!is.null(result)){
    author <- html_nodes(content, xpath = "//div[@class='article-metaline']")[1] %>%
      html_nodes(xpath = "span[@class='article-meta-value']") %>% 
      html_text() %>% strsplit(" ") %>% sapply("[", 1)
    time <- html_nodes(content, xpath = "//div[@class='article-metaline']")[3] %>%
      html_nodes(xpath = "span[@class='article-meta-value']") %>% 
      html_text()
    post <- html_nodes(content, xpath = "//div[@id='main-content']/text()") %>%
      html_text() %>% paste(collapse = "。")
    df$user_id[i] <- author
    df$time[i] <- time
    df$post[i] <- post
    # comments <- html_nodes(content, ".push")
    # if(length(comments)!=0){
    #   for(com in comments){
    #     tag <- html_nodes(com, ".push-tag") %>% html_text()
    #     user_id <- html_nodes(com, ".push-userid") %>% html_text()
    #     comment <- html_nodes(com, ".push-content") %>%
    #       html_text() %>% sub(": ", "", .)
    #     post_id <- df$ID[i]
    #     com_output <- rbind(com_output,
    #                         cbind(post_id, tag, user_id, comment))
    #   }
    # }
  }
}
