library(e1071)
library(glmnet)

fb_post <- read_csv("../../../machine_learning/fb_post_train.csv")

#F1+F2+F3+F5
df <- fb_post %>% select(comment, page_category, title_nu, 
                         pos_word, neg_word, 
                         核四:億元,
                         topic1:topic7,
                         stance) %>% na.omit()
#F1+F2+F3+F4+F5
df <- fb_post %>% select(comment, page_category, title_nu, 
                         pos_word, neg_word, 
                         核四:億元,
                         topic1:topic7,
                         pos_att:multi_score,
                         stance) %>% na.omit()


df <- df %>% transform(
  page_category = factor(page_category),
  title_nu = factor(title_nu),
  stance = factor(stance)
)
df[,6:70] <- lapply(df[,6:70], factor)

set.seed(1001)
n <- nrow(df)
t_idx <- sample(seq_len(n), size = round(0.7 * n))
traindata <- df[t_idx,]
testdata <- df[ - t_idx,]
tune.model = tune(svm,
                  stance~.,
                  data=traindata,
                  kernel="radial", # RBF kernel function
                  range=list(cost=10^(-1:2), gamma=c(.5,1,2))# 調參數的最主要一行
)
tune.model$best.model
model <- svm(stance~. , traindata, cost = 10, gamma = 0.1, cross = 10)
train.pred = predict(model, traindata)
test.pred = predict(model, testdata)
cm <- table(traindata$stance, train.pred)
cm <- table(testdata$stance, test.pred)
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

#F1+F2+F3+F5
df <- fb_post %>% select(comment, page_category, title_nu, 
                         pos_word, neg_word, 
                         核四:億元,
                         topic1:topic7,
                         stance) %>% na.omit() %>% 
  mutate(
    page_category.pos = ifelse(page_category=="核電類-正", 1, 0),
    page_category.neg = ifelse(page_category=="核電類-負", 1, 0),
    page_category.news = ifelse(page_category=="新聞類", 1, 0)
  ) %>% select(-page_category)

#F1+F2+F3+F4+F5
df <- fb_post %>% select(comment, page_category, title_nu, 
                         pos_word, neg_word, 
                         核四:億元,
                         topic1:topic7,
                         pos_att:multi_score,
                         stance) %>% na.omit() %>% 
  mutate(
    page_category.pos = ifelse(page_category=="核電類-正", 1, 0),
    page_category.neg = ifelse(page_category=="核電類-負", 1, 0),
    page_category.news = ifelse(page_category=="新聞類", 1, 0)
  ) %>% select(-page_category)

df[] <- lapply(df, as.numeric)

set.seed(1001)
n <- nrow(df)
t_idx <- sample(seq_len(n), size = round(0.7 * n))
traindata <- df[t_idx,]
testdata <- df[ - t_idx,]

ridge = glmnet(x = as.matrix(traindata %>% select(-stance)), 
               y = as.matrix(traindata %>% select(stance)), 
               alpha = 0,
               family = "multinomial")
cv.ridge = cv.glmnet(x = as.matrix(traindata %>% select(-stance)), 
                     y = as.matrix(traindata %>% select(stance)), 
                     alpha = 0,  # ridge
                     family = "multinomial")
best.ridge.lambda = cv.ridge$lambda.min
ridge.test = predict(ridge, 
                     s = best.ridge.lambda, 
                     newx = as.matrix(testdata %>% select(-stance)),
                     type = "class")
# table(testdata$stance, ridge.test)
cm <- table(testdata$stance, ridge.test)
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
