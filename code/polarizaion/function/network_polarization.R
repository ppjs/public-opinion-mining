library(igraph)
library(devtools)
library(isnar)

create_edge_fb <- function(d1, d2){
  
  edge_fb <- merge(d1 %>% select(parentID, nodeID), 
                   d2 %>% select(cid, nodeID), by.x = "parentID", by.y = "cid") %>%
    rename_(n1 = "nodeID.x", n2 = "nodeID.y") %>% select(n1, n2)
  
  return(edge_fb)
}

create_edge_ptt <- function(d1, d2){
  edge_ptt <-  merge(d1 %>% select(parentID, nodeID), d2 %>% select(pid, nodeID), by.x = "parentID", by.y = "pid") %>%
    rename_(n1 = "nodeID.x", n2 = "nodeID.y") %>% select(n1, n2)
  
  return(edge_ptt)
}

measure_segregation <- function(d_child, d_parent, d_attr, spam, platform, index){
  
  output <- c()
  serial <- 1
  
  start_date <- d_child$date %>% min()
  end_date <- d_child$date %>% max()
  start_d <- start_date
  end_d <- start_date
  
  while(end_d < end_date){
  
    print(start_d)
    
    if(start_d+spam>end_date) end_d <- end_date else end_d <- start_d+spam
    
    cur_d_child <- d_child %>% filter(date>=start_d, date<=end_d)
    
    if(platform=="fb"){
      edge <- create_edge_fb(cur_d_child, d_parent)
    }else{
      edge <- create_edge_ptt(cur_d_child, d_parent)
    }
    
    nID <- c(edge$n1, edge$n2) %>% unique()
    cur_attr <- d_attr %>% filter(nodeID %in% nID)
    
    if(nrow(cur_attr)==0){
      output <- rbind(output,
                      c(serial, str_c(start_d, "~", end_d), NA))
      serial <- serial + 1
      start_d <- end_d + 1
      next
    }
    
    mm_result <- create_mm(edge, cur_attr)
    mm0 <- mm_result[[1]]
    mm1 <- mm_result[[2]]
    d_graph <- graph_from_data_frame(edge, directed = TRUE, vertices = cur_attr)
    
    if(index=="E-I-index"){
      cur_ind <- ei(d_graph, 'stance')
    }
    if(index=="Assortativity-Coefficient"){
      cur_ind <- assortativity(d_graph, V(d_graph)$stance)
    }
    if(index=="Gupta-Anderson-and-Mays'Q"){
      sum_GAM <- 0
      K <- nrow(mm1)
      for(i in 1:K){
        tmp <- mm1[i, i] / sum(mm1[i, ])
        sum_GAM <- sum_GAM + tmp
      }
      cur_ind <- (sum_GAM-1)/(K-1)
    }
    if(index=="odds-ratio"){
      mgg1 <- sum(diag(mm1))
      mgh0 <- 0
      for(i in 1:nrow(mm0)){
        mgh0 <- mgh0 + sum(mm0[i,]) - mm0[i,i]
      }
      mgg0 <- sum(diag(mm0))
      mgh1 <- 0
      for(i in 1:nrow(mm1)){
        mgh1 <- mgh1 + sum(mm1[i,]) - mm1[i,i]
      }
      cur_ind <- (mgg1*mgh0)/(mgg0*mgh1)
    }
      
    output <- rbind(output,
                    c(serial, str_c(start_d, "~", end_d), cur_ind))
    serial <- serial + 1
    start_d <- end_d + 1
  }
  
  output <- data.frame(output, stringsAsFactors = FALSE)
  names(output) <- c("serial", "date", "index")
  output <- output %>% transform(
    serial = as.numeric(serial),
    index = as.numeric(index)
  ) %>% arrange(serial)
  return(output)
}
  
create_mm <- function(edge, attr){
  
  attr$index <- c(1:nrow(attr))
  n <- matrix(0, nrow = nrow(attr), ncol = nrow(attr))
  for(i in 1:nrow(edge)){
    n1 <- edge$n1[i]
    n2 <- edge$n2[i]
    x <- attr[attr$nodeID==n1,]$index
    y <- attr[attr$nodeID==n2,]$index
    n[x,y] <- 1
    n[y,x] <- 1
  }
  mm1 <- matrix(0, nrow=length(unique(attr$stance)), ncol=length(unique(attr$stance)))
  mm0 <- matrix(0, nrow=length(unique(attr$stance)), ncol=length(unique(attr$stance)))
  for(i in 1:nrow(mm1)){
    for(j in 1:nrow(mm1)){
      if(i==1){
        vi <- (-1)
      }else if(i==2){
        vi <- 0
      }else{
        vi <- 1
      }
      if(j==1){
        vj <- (-1)
      }else if(j==2){
        vj <- 0
      }else{
        vj <- 1
      }
      ver1 <- attr[attr$stance==vi,]$index
      ver2 <- attr[attr$stance==vj,]$index
      m1_sum <- 0
      m0_sum <- 0
      for(x in ver1){
        for(y in ver2){
          if(x!=y){
            m1_sum <- m1_sum + n[x,y]
            m0_sum <- m0_sum + (1-n[x,y])
          }
        }
      }
      if(i==j){
        mm1[i,j] <- m1_sum/2
        mm0[i,j] <- m0_sum/2
      }
      else{
        mm1[i,j] <- m1_sum
        mm0[i,j] <- m0_sum
      }
    }
  }
  return(list(mm0, mm1))
}

network_result <- function(result_fb, result_fb_nu_sup, result_fb_nu_unsup, result_fb_news, result_ptt, compare_number){
  if(compare_number==2){
    result_combine <- merge(result_fb, result_ptt, by = c("serial", "date")) %>% 
      rename_(fb = "index.x", ptt = "index.y")
    dat <- result_combine %>% melt(id.vars = c("serial", "date")) %>% arrange(serial)
    
    dat <- dat[!is.infinite(dat$value),]
    t_result <- t.test(value~variable, data = dat)
  }else{
    result_combine <- merge(result_fb_nu_sup, result_fb_nu_unsup, by = "serial") %>% rename_(fb_nu_sup = "index.x", fb_nu_unsup = "index.y") %>% 
      merge(result_fb_news, by="serial") %>% rename_(fb_news = "index") %>%
      select(serial, fb_nu_sup, fb_nu_unsup, fb_news) %>%
      merge(result_ptt, by="serial") %>% rename_(ptt = "index") %>% 
      select(serial, fb_nu_sup, fb_nu_unsup, fb_news, ptt, date)
    dat <- result_combine %>% melt(id.vars = c("serial", "date")) %>% arrange(serial)
    dat <- dat[!is.infinite(dat$value),]
    t_result <- aov(value~variable, data = dat)
  }
  return(list(dat, t_result))
}

