dforig <- data.frame(read.csv("~/Downloads/NCDBData.csv"))
library(iai)
library(survival)
dforig <- dforig[!is.na(dforig$Surgtype)]
dforig <- dforig[df$KPS >= 60]
df <- dforig[,(colnames(dforig) %in% c("SEX", "Racenew", "AGE", "size2", "ki", "mgmt", "codeletion", "Locationmerge", "KPS", "histonew", "CDCC_TOTAL_BEST"))]
df$SEX <- as.factor(df$SEX)
df$Racenew <- as.factor(df$Racenew)
df$AGE <- as.numeric(df$AGE)
df$Surgtype <- as.ordered(df$Surgtype)
df$size2 <- as.numeric(df$size2)
df$ki <- as.numeric(df$ki)
df$mgmt <- as.factor(df$mgmt)
df$codeletion <- as.factor(df$codeletion)
df$Locationmerge <- as.factor(df$Locationmerge)
df$KPS <- as.ordered(df$KPS)
df$histonew <- as.factor(df$histonew)
df$CDCC_TOTAL_BEST <- as.ordered(df$CDCC_TOTAL_BEST)
df <- df[!is.na(df$Surgtype)]
df <- df[df$KPS >= 60]
dfsupra = df[df$Locationmerge == 0 | df$Locationmerge == 1 | df$Locationmerge == 2 | df$Locationmerge == 3 | df$Locationmerge == 7 | df$Locationmerge == 8 | df$Locationmerge == 10]
dfinfra = df[df$Locationmerge == 4 | df$Locationmerge == 5 | df$Locationmerge == 9]
times <- dforig$DX_LASTCONTACT_DEATH_MONTHS
died <- dforig$PUF_VITAL_STATUS == 1

split <- iai::split_data("survival", df, died, times, seed = 12345)
train_X <- split$train$X
train_died <- split$train$deaths
train_times <- split$train$times
test_X <- split$test$X
test_died <- split$test$deaths
test_times <- split$test$times

grid <- iai::grid_search(
  iai::optimal_tree_survival_learner(
    random_seed = 1,
    missingdatamode = "separate_class",
    minbucket = 15,
  ),
  max_depth = 1:4,
)
iai::fit(grid, train_X, train_died, train_times,
         validation_criterion = "harrell_c_statistic")
iai::get_learner(grid)

pred_curves <- iai::predict(grid, test_X)
t <- 10
pred_curves[1][t]

iai::score(grid, train_X, train_died, train_times,
           criterion = "harrell_c_statistic")

