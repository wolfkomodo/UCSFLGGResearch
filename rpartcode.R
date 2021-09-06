df <- data.frame(read.csv("~/Downloads/NCDBData.csv"))
library(rpart)
library(survival)

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

fit <- rpart(Surv(DX_LASTCONTACT_DEATH_MONTHS, PUF_VITAL_STATUS) ~ SEX + Racenew + AGE + Surgtype + size2 + ki + mgmt + codeletion + Locationmerge + KPS + histonew + CDCC_TOTAL_BEST, data = df, control = rpart.control(minsplit =1,minbucket=1, cp=0))

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE,
     main="Classification Tree for LGG")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree
post(fit, file = "c:/tree.ps",
     title = "Classification Tree for LGG")

# prune the tree 
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree for Kyphosis")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "c:/ptree.ps",
     title = "Pruned Classification Tree for Kyphosis")