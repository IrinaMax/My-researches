library(dplyr)
library(caret)
library(klaR)
library(apaTables)
library(forecast)

tt.f <- read.csv("test_features_2013-03-07.csv")
tt.f  %>% summary
tt.f %>% head

tn.f <- read.csv("train_features_2013-03-07.csv")
tn.f %>% summary
tn.f %>% head


tn.s <- read.csv("train_salaries_2013-03-07.csv")
tn.s %>% head
tn.s %>% summary()

# looking for NA
sapply(tt.f, function(x) sum(is.na(x)))
hist(as.numeric(tt.f$companyId))
plot(tn.f[sample(nrow(tn.f), 50),])

sapply(tn.f, function(x) sum(is.na(x)))


sapply(tn.s, function(x) sum(is.na(x)))

# library("VIM")
# aggr(tt.f)

tn <- merge(tn.f, tn.s, by = "jobId")
tn %>% head
tn %>% dim
hist(tn$salary)
t.test(tn$salary)
# Corralation matrix to find out what variables better to use for my model
# I do not need jobID coz it is not relative for sure
tn.data <- data.matrix(tn[ , 2:length(tn)])
cor.mat <- round(cor(tn.data),2)
cor.mat
#                        companyId jobType degree major industry yearsExperience milesFromMetropolis salary
# companyId                   1    0.00   0.00  0.00     0.00            0.00                 0.0   0.00
# jobType                     0    1.00  -0.02 -0.02     0.00            0.00                 0.0  -0.23
# degree                      0   -0.02   1.00  0.37     0.00            0.00                 0.0  -0.23
# major                       0   -0.02   0.37  1.00     0.00            0.00                 0.0  -0.26
# industry                    0    0.00   0.00  0.00     1.00            0.00                 0.0   0.09
# yearsExperience             0    0.00   0.00  0.00     0.00            1.00                 0.0   0.38
# milesFromMetropolis         0    0.00   0.00  0.00     0.00            0.00                 1.0  -0.30
# salary                      0   -0.23  -0.23 -0.26     0.09            0.38                -0.3   1.00

#  linear reg model using 6 variables
start.time <- Sys.time()
m1 <- lm(salary ~ jobType+degree+ major+yearsExperience+industry +milesFromMetropolis, tn)
m1 %>% summary()
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# Call:
#   lm(formula = salary ~ jobType + degree + major + yearsExperience +
#        industry + milesFromMetropolis, data = tn)
#
# Residuals:
#   Min       1Q   Median       3Q      Max
# -180.563  -14.178   -0.452   13.267   94.757
#
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)
# (Intercept)           134.868442   0.123157 1095.097   <2e-16 ***
#   jobTypeCFO             -9.802960   0.078567 -124.772   <2e-16 ***
#   jobTypeCTO             -9.788102   0.078460 -124.752   <2e-16 ***
#   jobTypeJANITOR        -62.358359   0.082600 -754.943   <2e-16 ***
#   jobTypeJUNIOR         -49.795332   0.078532 -634.080   <2e-16 ***
#   jobTypeMANAGER        -29.867090   0.078448 -380.723   <2e-16 ***
#   jobTypeSENIOR         -39.785754   0.078329 -507.930   <2e-16 ***
#   jobTypeVICE_PRESIDENT -19.945669   0.078431 -254.308   <2e-16 ***
#   degreeDOCTORAL         10.030247   0.066207  151.499   <2e-16 ***
#   degreeHIGH_SCHOOL      -5.718382   0.099718  -57.346   <2e-16 ***
#   degreeMASTERS           4.997457   0.066212   75.477   <2e-16 ***
#   degreeNONE             -9.402883   0.099729  -94.284   <2e-16 ***
#   majorBUSINESS           7.698873   0.114700   67.122   <2e-16 ***
#   majorCHEMISTRY          1.080359   0.114526    9.433   <2e-16 ***
#   majorCOMPSCI            4.040752   0.114767   35.208   <2e-16 ***
#   majorENGINEERING       10.604011   0.114662   92.480   <2e-16 ***
#   majorLITERATURE        -3.620469   0.114620  -31.587   <2e-16 ***
#   majorMATH               5.142328   0.115055   44.694   <2e-16 ***
#   majorNONE              -4.955860   0.114697  -43.208   <2e-16 ***
#   majorPHYSICS            2.326222   0.114753   20.271   <2e-16 ***
#   yearsExperience         2.010102   0.002719  739.371   <2e-16 ***
#   industryEDUCATION      -9.990091   0.073361 -136.177   <2e-16 ***
#   industryFINANCE        21.142806   0.073354  288.228   <2e-16 ***
#   industryHEALTH          6.247261   0.073370   85.148   <2e-16 ***
#   industryOIL            21.309748   0.073367  290.453   <2e-16 ***
#   industrySERVICE        -4.980364   0.073384  -67.867   <2e-16 ***
#   industryWEB            12.110486   0.073311  165.193   <2e-16 ***
#   milesFromMetropolis    -0.399509   0.000679 -588.374   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 19.61 on 999972 degrees of freedom
# Multiple R-squared:  0.7435,	Adjusted R-squared:  0.7435
# F-statistic: 1.074e+05 on 27 and 999972 DF,  p-value: < 2.2e-16
#  Time difference of 3.459355 secs

# make prediction based on the linear model m1 for test tt.f
salary.new <- predict(m1, tt.f[,-1])
salary.new %>% head
salary.n<- round(salary.new)

#make data frame for saving submission csv file
tt.salary <- data.frame(tt.f$jobId, salary.n)
tt.salary %>% head
names(tt.salary) <- c("jobId", "salary")
tt.salary %>% summary
write.csv(tt.salary, "test_salaries.csv")


#_________________________________________________end linear mod______________________________________

# I dont have data real data for text table so I can compare only with train salary and achieve min max accuracy
pl <- data.frame(tn$salary, salary.new)
names(pl) <- c("actual", "predic")
pl %>% head

min_max_acc <- mean (apply(pl, 1,min) / apply(pl, 1,max))
min_max_acc
## [1] 0.7117415

## Also good to check all Analys of Varianse with apaTables.
library(apaTables)
apa.aov.table(lm(salary ~ jobType+degree+ major+yearsExperience+industry +milesFromMetropolis, tn[,-1]),table.number = 1)
# Table 1
#
# ANOVA results using salary as the dependent variable
#
#
# Predictor                  SS     df           MS          F             p partial_eta2 CI_90_partial_eta2
# (Intercept)      461073909.97      1    461073909.97 1199238.09 .000
# jobType           385797445.26      7    55113920.75  143349.50 .000          .50         [.50, .50]
# degree             18373425.44      4     4593356.36   11947.17 .000          .05         [.04, .05]
# major              11908573.07      8     1488571.63    3871.73 .000          .03         [.03, .03]
# yearsExperience   210179494.80      1    210179494.80  546669.96 .000          .35         [.35, .35]
# industry          130131922.85      6    21688653.81   56411.48 .000          .25         [.25, .25]
# milesFromMetropolis 133098216.91      1   133098216.91  346184.09 .000          .26         [.26, .26]
# Error            384461603.71     999972       384.47
#
# Note: Values in square brackets indicate the bounds of the 90% confidence interval for partial eta-squared
#
apa.cor.table(tn[,-1],table.number = 3,show.conf.interval = F)

# Table 3
#
# Means, standard deviations, and correlations
#
#
# Variable               M      SD    1     2
# 1. yearsExperience     11.99  7.21
#
# 2. milesFromMetropolis 49.53  28.88 .00
#
# 3. salary              116.06 38.72 .38** -.30**
#
#
#   Note. * indicates p < .05; ** indicates p < .01.
# M and SD are used to represent mean and standard deviation, respectively.

apa.reg.table(lm(salary ~ jobType+degree+ major+yearsExperience+industry +milesFromMetropolis, tn),table.number = 4)

# Table 4
#
# Regression results using salary as the criterion
#
#
# Predictor        b         b_95%_CI sr2 sr2_95%_CI             Fit
# (Intercept) 134.87** [134.63, 135.11]
# jobTypeCFO  -9.80**   [-9.96, -9.65] .00 [.00, .00]
# jobTypeCTO  -9.79**   [-9.94, -9.63] .00 [.00, .00]
# jobTypeJANITOR -62.36** [-62.52, -62.20] .15 [.15, .15]
# jobTypeJUNIOR -49.80** [-49.95, -49.64] .10 [.10, .10]
# jobTypeMANAGER -29.87** [-30.02, -29.71] .04 [.04, .04]
# jobTypeSENIOR -39.79** [-39.94, -39.63] .07 [.07, .07]
# jobTypeVICE_PRESIDENT -19.95** [-20.10, -19.79] .02 [.02, .02]
# degreeDOCTORAL  10.03**    [9.90, 10.16] .01 [.01, .01]
# degreeHIGH_SCHOOL  -5.72**   [-5.91, -5.52] .00 [.00, .00]
# degreeMASTERS   5.00**     [4.87, 5.13] .00 [.00, .00]
# degreeNONE  -9.40**   [-9.60, -9.21] .00 [.00, .00]
# majorBUSINESS   7.70**     [7.47, 7.92] .00 [.00, .00]
# majorCHEMISTRY   1.08**     [0.86, 1.30] .00 [.00, .00]
# majorCOMPSCI   4.04**     [3.82, 4.27] .00 [.00, .00]
# majorENGINEERING  10.60**   [10.38, 10.83] .00 [.00, .00]
# majorLITERATURE  -3.62**   [-3.85, -3.40] .00 [.00, .00]
# majorMATH   5.14**     [4.92, 5.37] .00 [.00, .00]
# majorNONE  -4.96**   [-5.18, -4.73] .00 [.00, .00]
# majorPHYSICS   2.33**     [2.10, 2.55] .00 [.00, .00]
# yearsExperience   2.01**     [2.00, 2.02] .14 [.14, .14]
# industryEDUCATION  -9.99**  [-10.13, -9.85] .00 [.00, .00]
# industryFINANCE  21.14**   [21.00, 21.29] .02 [.02, .02]
# industryHEALTH   6.25**     [6.10, 6.39] .00 [.00, .00]
# industryOIL  21.31**   [21.17, 21.45] .02 [.02, .02]
# industrySERVICE  -4.98**   [-5.12, -4.84] .00 [.00, .00]
# industryWEB  12.11**   [11.97, 12.25] .01 [.01, .01]
# milesFromMetropolis  -0.40**   [-0.40, -0.40] .09 [.09, .09]
#                                                                R2 = .744**
#                                                                95% CI[.71,.74]
#
#
# Note. * indicates p < .05; ** indicates p < .01.
# A significant b-weight indicates the semi-partial correlation is also significant.
# b represents unstandardized regression weights;
# sr2 represents the semi-partial correlation squared.
# Square brackets are used to enclose the lower and upper limits of a confidence interval.

#______________________________________________End of ANOVA__________________________________

# Why I choose the linear model? I check it out on train with existing salary.
# I split the merged table "tn" with salary to train & test to check out the accurasy for my model compare with existing salary of mytest
set.seed(123)
smp_size <- floor(0.90 * nrow(tn))

train_ind <- sample(seq_len(nrow(tn)), size = smp_size, replace = FALSE  )

train <- tn[train_ind, ]
test <- tn[-train_ind, ]
test
start.time <- Sys.time()
lm.mytrain <- lm(salary ~ jobType+degree+ major+yearsExperience+industry +milesFromMetropolis, train[,-1])
lm.mytrain %>% summary()
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# Call:
#   lm(formula = salary ~ jobType + degree + major + yearsExperience +
#        industry + milesFromMetropolis, data = tn)
#
# Residuals:
#   Min       1Q   Median       3Q      Max
# -180.563  -14.178   -0.452   13.267   94.757
#
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)
# (Intercept)           134.868442   0.123157 1095.097   <2e-16 ***
#   jobTypeCFO             -9.802960   0.078567 -124.772   <2e-16 ***
#   jobTypeCTO             -9.788102   0.078460 -124.752   <2e-16 ***
#   jobTypeJANITOR        -62.358359   0.082600 -754.943   <2e-16 ***
#   jobTypeJUNIOR         -49.795332   0.078532 -634.080   <2e-16 ***
#   jobTypeMANAGER        -29.867090   0.078448 -380.723   <2e-16 ***
#   jobTypeSENIOR         -39.785754   0.078329 -507.930   <2e-16 ***
#   jobTypeVICE_PRESIDENT -19.945669   0.078431 -254.308   <2e-16 ***
#   degreeDOCTORAL         10.030247   0.066207  151.499   <2e-16 ***
#   degreeHIGH_SCHOOL      -5.718382   0.099718  -57.346   <2e-16 ***
#   degreeMASTERS           4.997457   0.066212   75.477   <2e-16 ***
#   degreeNONE             -9.402883   0.099729  -94.284   <2e-16 ***
#   majorBUSINESS           7.698873   0.114700   67.122   <2e-16 ***
#   majorCHEMISTRY          1.080359   0.114526    9.433   <2e-16 ***
#   majorCOMPSCI            4.040752   0.114767   35.208   <2e-16 ***
#   majorENGINEERING       10.604011   0.114662   92.480   <2e-16 ***
#   majorLITERATURE        -3.620469   0.114620  -31.587   <2e-16 ***
#   majorMATH               5.142328   0.115055   44.694   <2e-16 ***
#   majorNONE              -4.955860   0.114697  -43.208   <2e-16 ***
#   majorPHYSICS            2.326222   0.114753   20.271   <2e-16 ***
#   yearsExperience         2.010102   0.002719  739.371   <2e-16 ***
#   industryEDUCATION      -9.990091   0.073361 -136.177   <2e-16 ***
#   industryFINANCE        21.142806   0.073354  288.228   <2e-16 ***
#   industryHEALTH          6.247261   0.073370   85.148   <2e-16 ***
#   industryOIL            21.309748   0.073367  290.453   <2e-16 ***
#   industrySERVICE        -4.980364   0.073384  -67.867   <2e-16 ***
#   industryWEB            12.110486   0.073311  165.193   <2e-16 ***
#   milesFromMetropolis    -0.399509   0.000679 -588.374   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 19.61 on 999972 degrees of freedom
# Multiple R-squared:  0.7435,	Adjusted R-squared:  0.7435
# F-statistic: 1.074e+05 on 27 and 999972 DF,  p-value: < 2.2e-16

# Time difference of 3.996008 secs, R-squered pretty hight with said the model is good and all point will be close to the perfect prediction line.

# prediction
s.new <- predict(lm.mytrain, test[,c(-1, -9)])
s.new %>% head
s.new<- round(s.new)
# make data frame for look accuracy of statistics
df.lm <- data.frame(test$salary, s.new)
df.lm %>% head
names(df.lm) <- c("actual", "new")
df.lm %>% summary

# statistics
RSS <- c(crossprod(lm.mytrain$residuals))
RSS
MSE <- RSS / length(lm.mytrain$residuals)
MSE
RMSE <- sqrt(MSE)
RMSE
#[1] 19.60797  And look the Residual Mean Squared Error  is nicely low with said about the model is working good
summary(lm.mytrain$model)
lm.mytrain %>% summary

#__________________________________________accuracy _____________________________________

min_max_acc_lm <- mean (apply(df.lm, 1,min) / apply(df.lm, 1,max))
min_max_acc_lm
#[1] 0.8736889  Min_Max_Accuracy is also pretty high

# I also can use library(forecast) to look on the all statisitc together
accuracy_lm <- accuracy(df.lm$actual,df.lm$new, threshold = 0.5)
accuracy_lm
#               ME     RMSE      MAE        MPE     MAPE
#Test set -0.01137 19.60818 15.84963 -0.2499639 13.83658


# Good to proof the model accuracy with plotting predicted salary vs actual and count accurasy
plot(df.lm[sample(nrow(df.lm),1000),],col = "pink")
abline(lm( df.gbm$actual ~ df.gbm$new  , data=df.gbm),  col = "black")
# the red line will show the perfect line of 45degree where is for perfect case
abline(lm(df.gbm$actual ~ df.gbm$new  + 0),  col="red")

# ___________________________END_______________________________________
