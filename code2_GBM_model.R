# GBM
library(dplyr)
library(caret)
library(klaR)
library(apaTables)


tt.f <- read.csv("test_features_2013-03-07.csv")

tn.f <- read.csv("train_features_2013-03-07.csv")

tn.s <- read.csv("train_salaries_2013-03-07.csv")
# I already exemained data in for Linear regression so I will skip this step
# Add salary to the train table
tn <- merge(tn.f, tn.s, by = "jobId")
tn %>% head

# -----------------------GBM- Gradient Boosting Machine-----------------------------------------------------
library(data.table)
require(gbm)
library(gbm)
library(mlbench)

tn %>% dim

st.time <- Sys.time()
m.boost=gbm(salary ~ jobType+degree+ major+yearsExperience+industry +milesFromMetropolis, data = tn[,-1 ],distribution = "gaussian", n.trees = 1000,
            shrinkage = 0.01, interaction.depth = 4)
m.boost
# time was about 30 sec for 100 trees but Time difference of 12.05476013 mins for 1000 trees without CV
end.time <- Sys.time()
taken.time <- end.time-st.time
taken.time
summary(m.boost, las=2)
# make predictions
best.iter <- gbm.perf(m.boost)
m.boost
# var  rel.inf
# jobType                         jobType 41.98204
# yearsExperience         yearsExperience 18.86314
# milesFromMetropolis milesFromMetropolis 12.73673
# industry                       industry 11.62670
# degree                           degree 11.12100
# major                             major  3.67040

# another trial with 500 trees, cv.folds =5 and interaction.depth = 2
st.time <- Sys.time()
m2.boost <- gbm(salary ~jobType+ degree+ yearsExperience + milesFromMetropolis, distribution = "gaussian", data =tn[,-1], cv.folds=5, n.trees = 500, interaction.depth = 2 )
end.time <- Sys.time()
taken.time <- end.time-st.time
taken.time
#Time difference of 4.568511 mins 500 trees
summary(m2.boost, las=2)
best.iter2 <- gbm.perf(m2.boost, method="cv")
print(best.iter2)

#prediction on GBM of the first model, the varienca explained bigger
f.predict <- predict(m.boost,tt.f[,-1],best.iter)
f.predict

salary.gbm<- round(f.predict)

# make data frame for saving submission csv file
tt.gbm <- data.frame(tt.f$jobId, salary.gbm)
names(tt.gbm) <- c("jobId", "salary")
tt.gbm %>% head
#      jobId          salary
# 1 JOB1362685407687    111
# 2 JOB1362685407688     92
# 3 JOB1362685407689    172
# 4 JOB1362685407690    104
# 5 JOB1362685407691    119
# 6 JOB1362685407692    152
tt.gbm %>% summary
write.csv(tt.gbm, "test_salaries2_GBM.csv")
#---------------------------------------------------------end of GBM------------------------------
# I compare with train salary and count min max accuracy

pl4 <- data.frame(tn$salary, salary.gbm)
names(pl4) <- c("actual", "predic")

#AVG - test mean square error
mse_test_value_GBM <-((pl4$actual-pl4$predic)^2)
mse_test_value_GBM
#[1] 2560.537354  less then in linear model

#min_max_accuracy 71.2%  ###########################################
min_max_acc2 <- mean (apply(pl4, 1,min) / apply(pl4, 1,max))
min_max_acc2
# [1] 0.7147162863

#####################################  try split tn for test and train to achive accuracy of the model
#separating training and test data
set.seed(123)
smp_size <- floor(0.90 * nrow(tn))

train_ind <- sample(seq_len(nrow(tn)), size = smp_size, replace = FALSE  )

train <- tn[train_ind, ]
test <- tn[-train_ind, ]

st.time <- Sys.time()
m.boost_train=gbm(salary ~jobType+degree+ major+yearsExperience+industry +milesFromMetropolis, distribution = "gaussian", data =train[,-1], cv.folds=5,n.trees = 1000,                        shrinkage = 0.01, interaction.depth = 4)
m.boost_train
# gbm(formula = salary ~ jobType + degree + major + yearsExperience +
#       industry + milesFromMetropolis, distribution = "gaussian",
#     data = train[, -1], n.trees = 1000, interaction.depth = 4,
#     shrinkage = 0.01, cv.folds = 5)
# A gradient boosted model with gaussian loss function.
# 1000 iterations were performed.
# The best cross-validation iteration was 1000.
# There were 6 predictors of which 6 had non-zero influence.
# var   rel.inf

# Time difference of 29.87811 mins
end.time <- Sys.time()
taken.time <- end.time-st.time
taken.time
#Time difference of 10.98412 mins for 1000t
m.boost_train %>% summary
summary(m.boost_train, las=2)
m.boost_train
#rrsult for 1000T with that vars
#                                     var      rel.inf
# jobType                         jobType 42.048304992
# yearsExperience         yearsExperience 18.937273496
# milesFromMetropolis milesFromMetropolis 12.657834616
# industry                       industry 11.623071581
# degree                           degree 11.051343879
# major                             major  3.682171437
# make predictions
best.iter <- gbm.perf(m.boost_train)
best.iter

f.predict_mytest <- predict(m.boost_train,test[,c(-1,-9)],best.iter)
f.predict_mytest

salary_mytest.gbm<- round(f.predict_mytest)

#make data frame for saving submission csv file
df.gbm <- data.frame(test$salary, salary_mytest.gbm)
df.gbm %>% head
names(df.gbm) <- c("actual", "new")
df.gbm %>% summary
plot(sample(nrow(m.boost_tr),100))
library(forecast)
accuracy(df.gbm$actual,df.gbm$new)
#                ME        RMSE      MAE          MPE        MAPE
# Test set 0.01089 19.14154879 15.51071 0.3048894325 13.37741449

# statistics for GBM


#min_max_accuracy   ###########################################
min_max_acc_gbm <- mean (apply(df.gbm, 1,min) / apply(df.gbm, 1,max))
min_max_acc_gbm
#[1] 0.8765882  very nice result! Its higher then Lin Mondel

#staistics by forecast library
install.packages("forecast")
library(forecast)
accuracy(df.gbm$actual, df.gbm$new)
#             ME     RMSE      MAE       MPE     MAPE
# Test set 0.01089 19.14155 15.51071 0.3048894 13.37741

#plot prediction red line is actual model prediction
plot(df.gbm[sample(nrow(df.gbm),1000),], col = "light blue", main = "GBM model prediction on the subset test")
abline(lm( df.gbm$actual ~ df.gbm$new  , data=df.gbm),  col = "blue")
#
abline(lm(df.gbm$actual ~ df.gbm$new  + 0),  col="red")
