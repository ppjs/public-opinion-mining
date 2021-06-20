library(tidyverse)
library(lubridate)
library(reshape)
source("../function/network_polarization.R")

fb_comment <- read_csv("../../output_polarization/fb_comment.csv")
fb_reply <- read_csv("../../output_polarization/fb_reply.csv") %>% mutate(date=date(time)) %>% filter(date>=as.Date("2017-8-30"))
attr_fb <- read_csv("../../output_polarization/fb_attr.csv")
ptt_post <- read_csv("../../output_polarization/ptt_post.csv")
ptt_comment <- read_csv("../../output_polarization/ptt_comment.csv") %>% mutate(date = as.Date(time, "%a %b %d %H:%M:%S %Y"))
attr_ptt <- read_csv("../../output_polarization/ptt_attr.csv")


spam <- 7
index <- "Gupta-Anderson-and-Mays'Q"
result_ptt <- measure_segregation(ptt_comment, ptt_post, attr_ptt, spam, "ptt", index)
result_fb <- measure_segregation(fb_reply, fb_comment, attr_fb, spam, "fb", index)
result_fb_news <- measure_segregation(fb_reply %>% filter(page_cate=="新聞類"), 
                                      fb_comment, attr_fb, spam, "fb", index)
result_fb_nu_sup <- measure_segregation(fb_reply %>% filter(page_cate=="核電類-正"), 
                                        fb_comment, attr_fb, spam, "fb", index)
result_fb_nu_unsup <- measure_segregation(fb_reply %>% filter(page_cate=="核電類-反"), 
                                          fb_comment, attr_fb, spam, "fb", index)

result <- network_result(result_fb, result_fb_nu_sup, result_fb_nu_unsup, result_fb_news, result_ptt, 2)
result <- network_result(result_fb, result_fb_nu_sup, result_fb_nu_unsup, result_fb_news, result_ptt, 4)

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
ggsave("../../../img/Gupta-Anderson-and-Mays'Q_4.png")

result[[2]]
summary(result[[2]])
DescTools::ScheffeTest(result[[2]])
mean(data$value[data$variable=="fb_nu_sup"], na.rm = TRUE)
mean(data$value[data$variable=="fb_nu_unsup"], na.rm = TRUE)
mean(data$value[data$variable=="fb_news"], na.rm = TRUE)
