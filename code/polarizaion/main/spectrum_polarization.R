library(tidyverse)
library(lubridate)
library(reshape)
source("../function/spectrum_polarization.R")

fb_comment <- read_csv("../../output_polarization/fb_comment.csv")
fb_reply <- read_csv("../../output_polarization/fb_reply.csv")
df <- rbind(fb_comment %>% select(time, opinion_score, page_cate),
            fb_reply %>% select(time, opinion_score, page_cate))
df_fb <- df %>% 
  mutate(date=date(time)) %>% 
  filter(date>=as.Date("2017-8-30"))
df_fb_nu_sup <- df %>% 
  filter(page_cate=="核電類-正") %>%
  mutate(date=date(time)) %>% 
  filter(date>=as.Date("2017-8-30"))
df_fb_nu_unsup <- df %>% 
  filter(page_cate=="核電類-反") %>%
  mutate(date=date(time)) %>% 
  filter(date>=as.Date("2017-8-30"))
df_fb_news <- df %>% 
  filter(page_cate=="新聞類") %>%
  mutate(date=date(time)) %>% 
  filter(date>=as.Date("2017-8-30"))

ptt_post <- read_csv("../../output_polarization/ptt_post.csv")
ptt_comment <- read_csv("../../output_polarization/ptt_comment.csv")
df <- rbind(ptt_post %>% select(time, opinion_score),
            ptt_comment %>% select(time, opinion_score))
df_ptt <- df %>% mutate(date = as.Date(time, "%a %b %d %H:%M:%S %Y"))

index <- "spread"
spam <- 7
result_fb <- spectrum_polarization(df_fb, spam, index)
result_fb_nu_sup <- spectrum_polarization(df_fb_nu_sup, spam, index)
result_fb_nu_unsup <- spectrum_polarization(df_fb_nu_unsup, spam, index)
result_fb_news <- spectrum_polarization(df_fb_news, spam, index)
result_ptt <- spectrum_polarization(df_ptt, spam, index)

result <- polarization_result(result_fb, result_fb_nu_sup, result_fb_nu_unsup, 
                              result_fb_news, result_ptt, 2)
result <- polarization_result(result_fb, result_fb_nu_sup, result_fb_nu_unsup, 
                              result_fb_news, result_ptt, 4)

data <- result[[1]]
ggplot(data = data, aes(x = serial, y = value)) +
  geom_point(
    mapping = aes(color = variable, shape = variable)
  ) +
  geom_line(
    mapping = aes(group = variable, color = variable, linetype = variable)
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab(index)
ggsave("../../../img/regionalization_4.png")

result[[2]]
summary(result[[2]])
DescTools::ScheffeTest(result[[2]])
mean(data$value[data$variable=="fb_nu_sup"], na.rm = TRUE)
mean(data$value[data$variable=="fb_nu_unsup"], na.rm = TRUE)
mean(data$value[data$variable=="fb_news"], na.rm = TRUE)


serial_date <- data %>% select(serial, date) %>% unique() %>% mutate(
  start_date = strsplit(date, "~") %>% sapply("[", 1),
  end_date = strsplit(date, "~") %>% sapply("[", 2)
)
df <- df_fb_nu_unsup %>% mutate(
  serial = 0
)
for(i in 1:nrow(serial_date)){
  df$serial[df$date>=as.Date(serial_date$start_date[i]) & df$date<=as.Date(serial_date$end_date[i])] <- serial_date$serial[i]
}
df <- df %>% filter(serial !=0) %>% transform(
  serial = as.factor(serial)
)
ggplot(data = df, aes(x = serial, y = opinion_score)) +
  geom_boxplot(
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylim(-1, 1)
ggsave("../../../fb_unsup.png")  
