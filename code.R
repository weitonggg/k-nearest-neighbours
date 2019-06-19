library(dplyr)
library(dummies)
library(class)

# load data
heart <- read.csv("heart.csv")
colnames(heart) <- c("age", "sex", "chest_pain", "bp_rest",
                     "cholesterol", "bloodsugar_high", "ecardio_rest",
                     "max_heartrate", "exer_angina", "oldpeak", "slope",
                     "major_ves", "thal", "target")

str(heart) # factors stored as numeric
summary(heart) # no NA values
# arrange by feature class
heart <- heart %>% select(age, bp_rest, cholesterol,
                          max_heartrate, oldpeak, major_ves, 
                          sex, chest_pain, bloodsugar_high, ecardio_rest,
                          exer_angina, slope, thal, target)

# convert class for factors with more than 2 categories (for dummy coding)
factor_cols <- c("chest_pain", "ecardio_rest", "slope", "thal")
for (fac in factor_cols){
  col_num <- which(colnames(heart) == fac)
  heart[,col_num] <- as.factor(heart[,col_num])
}

# dummy coding
tempheart <- dummy.data.frame(heart[,c(8,10,12,13)])
heart <- heart[,-c(8,10,12,13)]
heart <- cbind(heart, tempheart)
for (i in 7:24){ # convert all factors to numeric for standardisation
  heart[,i] <- as.numeric(as.character(heart[,i]))
}
scaled_heart <- as.data.frame(scale(heart[,-10]))
scaled_heart <- cbind(scaled_heart, target = heart$target)

# separate into training-validate-test: 70-20-10
set.seed(100)
indices <- sample(1:3, size = nrow(heart), prob = c(0.7, 0.2, 0.1), replace = TRUE)
training <- scaled_heart[indices == 1, ]
validate <- scaled_heart[indices == 2, ]
testing <- scaled_heart[indices == 3, ]

set.seed(35)
error_rate <- c()
for (i in 1:10){
  pred <- knn(train = training[,-24], test = validate[,-24],
              cl = training$target, k = i)
  error_num <- nrow(validate) - sum(pred == validate$target)
  error_rate <- c(error_rate, (error_num/nrow(validate) * 100))
}
plot(error_rate, x = 1:10, type = "b", xlab = "k", ylab = "Validation error rate (%)")
# min error rate at k = 8

# build classifier using k = 8
knn_pred <- knn(train = training[,-24], test = testing[,-24],
                cl = training$target, k = 8)
# confusion matrix
table("knn" = knn_pred, "labels" = testing$target)
# accuracy 
sum(knn_pred == testing$target)
