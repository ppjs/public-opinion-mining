

spectrum_polarization <- function(df, cycle, index){
  
  output <- c()
  serial <- 1
  
  df <- df %>% arrange(date)
  start_date <- df$date %>% min()
  end_date <- df$date %>% max()
  start_d <- start_date
  end_d <- start_date
  while(end_d < end_date){
    if(start_d+cycle>end_date) end_d <- end_date else end_d <- start_d+cycle
    cur_df <- df %>% filter(date>=start_d & date<=end_d)
    if(index=="spread"){
      cur_ind <- max(cur_df$opinion_score, na.rm = TRUE) - min(cur_df$opinion_score, na.rm = TRUE)
    }
    if(index=="dispersion"){
      cur_ind <- 2*var(cur_df$opinion_score)
    }
    bin_opscore <- cut(cur_df$opinion_score, seq(-1, 1, 0.05)) %>% table
    if(index=="coverage"){
      cur_ind <- length(bin_opscore[bin_opscore!=0])/length(bin_opscore)
    }
    if(index=="regionalization"){
      cur_ind <- ifelse(bin_opscore[1]==0, 1, 0)
      for(i in 2:(length(bin_opscore)-1)){
        if(bin_opscore[i]>0 & bin_opscore[(i+1)]==0) cur_ind <- cur_ind + 1
      }
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

polarization_result <- function(result_fb, result_fb_nu_sup, result_fb_nu_unsup, result_fb_news, result_ptt, compare_number){
  
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
