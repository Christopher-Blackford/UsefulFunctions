###Confusion matrix
library(caret)

if(!dir.exists("./images")){dir.create("./images")}


obs <- sort(rep((0:12), times = 20))

pred <- round(rnorm(length(obs), mean = obs, sd = 2))
pred

pred <- gsub(pred, pattern = paste(-100:-1, collapse = "|"), replacement = 0)
pred <- gsub(pred, pattern = paste(13:100, collapse = "|"), replacement = 12)

#pred <- as.factor(pred)
pred


df <- data.frame(obs = obs, pred = pred)

df$obs <- as.factor(df$obs)
df$pred <- as.factor(df$pred)

str(df)

new <- confusionMatrix(df$obs, df$pred)
new



x <- table(pred, obs)

cm <- confusionMatrix(df$obs, df$pred)

write.csv(cm[2]$table, "C:/Users/Chris/Dropbox/DSM presentation/conmatrix.csv")


