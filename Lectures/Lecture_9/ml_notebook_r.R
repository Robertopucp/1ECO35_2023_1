################  Laboratorio 9 (LM) ############################
## Curso: Laboratorio de R y Python ###########################
## @author: Roberto Mendoza 

# clean environment variables
rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

# additional options
options(scipen = 999)      # No scientific notation

# Hide warnnings 

options(warn=-1)

# Library


# Load libraries ----

library(pacman) 

p_load(
  hdm, # high dimention metric model
  glmnet, # linear machine learning models (lasso, elasticnet, ridge)
  randomForest, # random forest
  rpart, # plot random trees
  nnet,
  gbm, #  boosting 
  rpart.plot,
  xtable,  # export table in latex format
  tidyverse,
  caret
)



# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# A Simple Case Study using Wage Data from 2015


# Load data ------------------------

load("../../data/wage2015_subsample_inference.Rdata")
dim(data)

# select regressors 

Z <- subset(data,select=-c(lwage,wage)) 
colnames(Z)

# wage histogram (asimetric)

hist(data$wage, xlab= "hourly wage",
     main="Empirical wage distribution from the US survey data", breaks= 35)


# split data into train and test 

set.seed(1234)
training <- sample(nrow(data), nrow(data)*(3/4), replace=FALSE)

# sample without replacement 

data_train <- data[training,]  # data trainning
data_test <- data[-training,]  # data for test 

# Models (basic and flexible) ----------------------

# regression specificaction

X_basic <-  "sex + exp1 + exp2+ shs + hsg+ scl + clg + mw + so + we + occ2+ ind2"
X_flex <- "sex + exp1 + exp2 + shs+hsg+scl+clg+occ2+ind2+mw+so+we + (exp1+exp2+exp3+exp4)*(shs+hsg+scl+clg+occ2+ind2+mw+so+we)"
formula_basic <- as.formula(paste("lwage", "~", X_basic))
formula_flex <- as.formula(paste("lwage", "~", X_flex))

# Model matrix

model_X_basic_train <- model.matrix(formula_basic,data_train)
model_X_basic_test <- model.matrix(formula_basic,data_test)
p_basic <- dim(model_X_basic_train)[2]

model_X_flex_train <- model.matrix(formula_flex,data_train)
model_X_flex_test <- model.matrix(formula_flex,data_test)
p_flex <- dim(model_X_flex_train)[2]

Y_train <- data_train$lwage
Y_test <- data_test$lwage


### OLS -----------------

# ols (basic model)
fit.lm.basic <- lm(formula_basic, data_train)
fit.lm.basic

# Compute the Out-Of-Sample Performance
yhat.lm.basic <- predict(fit.lm.basic, newdata=data_test)
cat("The mean squared error (MSE) using the basic model is equal to" ,
    mean((Y_test-yhat.lm.basic)^2)) # MSE OLS (basic model)  


MSE.lm.basic <- summary(lm((Y_test-yhat.lm.basic)^2~1))$coef[1:2]
MSE.lm.basic

R2.lm.basic <- 1-MSE.lm.basic[1]/var(Y_test)
cat("The R^2 using the basic model is equal to",
    R2.lm.basic) # MSE OLS (basic model) 


# ols (flexible model)
fit.lm.flex <- lm(formula_flex, data_train) 

# Compute the Out-Of-Sample Performance

yhat.lm.flex <- predict(fit.lm.flex, newdata=data_test)
MSE.lm.flex <- summary(lm((Y_test-yhat.lm.flex)^2~1))$coef[1:2]

R2.lm.flex <- 1-MSE.lm.flex[1]/var(Y_test)
cat("The R^2 using the flexible model is equal to",
    R2.lm.flex) # MSE OLS (flexible model) 



### Lasso, Ridge and Elastic Net ---------------

# Basic model 

fit.rlasso  <- rlasso(formula_basic, data_train, post=FALSE)
fit.rlasso.post <- rlasso(formula_basic, data_train, post=TRUE) # Post-lasso

yhat.rlasso   <- predict(fit.rlasso, newdata=data_test)
yhat.rlasso.post   <- predict(fit.rlasso.post, newdata=data_test)

MSE.lasso <- summary(lm((Y_test-yhat.rlasso)^2~1))$coef[1:2]
MSE.lasso.post <- summary(lm((Y_test-yhat.rlasso.post)^2~1))$coef[1:2]

R2.lasso <- 1-MSE.lasso[1]/var(Y_test)
R2.lasso.post <- 1-MSE.lasso.post[1]/var(Y_test)
cat("The R^2 using the basic model is equal to",R2.lasso,
    "for lasso and",R2.lasso.post,"for post-lasso") # R^2 lasso/post-lasso (basic model) 

#---------------------------------------------------------------#

# Flexible model 

fit.rlasso.flex  <- rlasso(formula_flex, data_train, post=FALSE)
fit.rlasso.post.flex <- rlasso(formula_flex, data_train, post=TRUE)
yhat.rlasso.flex   <- predict(fit.rlasso.flex, newdata=data_test)
yhat.rlasso.post.flex   <- predict(fit.rlasso.post.flex, newdata=data_test)

MSE.lasso.flex <- summary(lm((Y_test-yhat.rlasso.flex)^2~1))$coef[1:2]
MSE.lasso.post.flex <- summary(lm((Y_test-yhat.rlasso.post.flex)^2~1))$coef[1:2]

R2.lasso.flex <- 1-MSE.lasso.flex[1]/var(Y_test)
R2.lasso.post.flex <- 1-MSE.lasso.post.flex[1]/var(Y_test)
cat("The R^2 using the flexible model is equal to",R2.lasso.flex,"for lasso and",
    R2.lasso.post.flex,"for post-lasso") # R^2 lasso/post-lasso (flexible model)

#-------------------------------------------------------------------------------#

# Cross-validation

# default arguments: nfolds = 10, standarize = TRUE

# By default cv.glmnet standarize predictors 

fit.lasso.cv   <- cv.glmnet(model_X_basic_train, Y_train, family="gaussian",
                            alpha=1 ) # alpha =1 for lasso

fit.ridge   <- cv.glmnet(model_X_basic_train, Y_train, family="gaussian", 
                         alpha=0 ) # alpha = 0 for ridge

fit.elnet   <- cv.glmnet(model_X_basic_train, Y_train, family="gaussian",
                         alpha=.5 ) # alpha = 0.5 fir elastic net


yhat.lasso.cv   <- predict( fit.lasso.cv , newx = model_X_basic_test)
yhat.ridge   <- predict(fit.ridge, newx = model_X_basic_test)
yhat.elnet   <- predict(fit.elnet, newx = model_X_basic_test)


MSE.lasso.cv <- summary(lm((Y_test-yhat.lasso.cv)^2~1))$coef[1:2]
MSE.ridge <- summary(lm((Y_test-yhat.ridge)^2~1))$coef[1:2]
MSE.elnet <- summary(lm((Y_test-yhat.elnet)^2~1))$coef[1:2]


R2.lasso.cv <- 1-MSE.lasso.cv[1]/var(Y_test)
R2.ridge <- 1-MSE.ridge[1]/var(Y_test)
R2.elnet <- 1-MSE.elnet[1]/var(Y_test)

# R^2 using cross-validation (basic model) 
cat("R^2 using cross-validation for lasso, ridge and elastic net in the basic model:",
    R2.lasso.cv,R2.ridge,R2.elnet)


# ----------------------------------------------------------------- #

fit.lasso.cv.flex   <- cv.glmnet(model_X_flex_train, Y_train,
                                 family="gaussian", alpha=1)
fit.ridge.flex   <- cv.glmnet(model_X_flex_train, Y_train,
                              family="gaussian", alpha=0)
fit.elnet.flex   <- cv.glmnet(model_X_flex_train, Y_train, 
                              family="gaussian", alpha=.5)

yhat.lasso.cv.flex    <- predict(fit.lasso.cv.flex , newx = model_X_flex_test)
yhat.ridge.flex    <- predict(fit.ridge.flex , newx = model_X_flex_test)
yhat.elnet.flex    <- predict(fit.elnet.flex , newx = model_X_flex_test)

MSE.lasso.cv.flex  <- summary(lm((Y_test-yhat.lasso.cv.flex )^2~1))$coef[1:2]
MSE.ridge.flex  <- summary(lm((Y_test-yhat.ridge.flex )^2~1))$coef[1:2]
MSE.elnet.flex  <- summary(lm((Y_test-yhat.elnet.flex )^2~1))$coef[1:2]

R2.lasso.cv.flex  <- 1-MSE.lasso.cv.flex [1]/var(Y_test)
R2.ridge.flex  <- 1-MSE.ridge.flex [1]/var(Y_test)
R2.elnet.flex  <- 1-MSE.elnet.flex [1]/var(Y_test)

# R^2 using cross-validation (flexible model) 
cat("R^2 using cross-validation for lasso, ridge and elastic net in the flexible model:",
    R2.lasso.cv.flex,R2.ridge.flex,R2.elnet.flex)

MSE.lasso.cv.flex[1]
MSE.lasso.post.flex[1]


# The performance of the lasso regression with cross-validated 
# penalty is quite similar to the performance of lasso using
# a theoretical based choice of the tuning parameter.



## No-linear models ----------------------------

#### Random tress -----------------


# tree
fit.trees <- rpart(formula_basic, data_train, cp = 0)
prp(fit.trees, leaf.round=1, space=2, yspace=2,
    split.space=2, shadow.col = "gray", trace = 1) # plotting the tree

# cp: complexity parameter for controlling the size of the tree

# Add penalization to prune tree

fit.trees <- rpart(formula_basic, data_train, cp = 0.001)
prp(fit.trees, leaf.round=1, space=2, yspace=2,
    split.space=2, shadow.col = "gray", trace = 1) # plotting the tree


plotcp(fit.trees)

fit.trees$cptable

which.min(fit.trees$cptable[,"xerror"])


# best alpha

bestcp<- fit.trees$cptable[which.min(fit.trees$cptable[,"xerror"]),"CP"]
bestcp

### Prune Tree -------------

fit.prunedtree <- prune(fit.trees, cp=bestcp)


plot1 <- rpart.plot(fit.prunedtree,leaf.round=1, space=1, 
                    yspace=1.5, split.space=1,
                    shadow.col = "gray", type =4,
                    branch = 1, box.palette="GnBu", cex = 0.7)


plot1 <- rpart.plot(fit.prunedtree,type =3, 
                    fallen = TRUE, leaf.round=1, 
                    extra = 100, branch = 0.1, box.palette="RdBu",cex = 0.7)


# ---------------------------------------------------------- #

yhat.pt <- predict(fit.prunedtree,newdata=data_test)

MSE.pt <- summary(lm((Y_test-yhat.pt)^2~1))$coef[1:2]
R2.pt  <- 1-MSE.pt[1]/var(Y_test)

# R^2 of the pruned tree
cat("R^2 of the pruned tree:",R2.pt)


#### Random forest and boosted trees 

## Applying the methods
# random forest
fit.rf       <- randomForest(formula_basic, ntree=2000, 
                             nodesize=5, data=data_train, mtry = sqrt(p_basic))


# ntree = number of tree
# matry = number of covariable randm¿omly selected in each tree

# for tuning: adjust input "mtry" to change the number of variables randomly sampled as candidates at each split

# boosting
fit.boost   <- gbm(formula_basic, data=data_train, 
                   distribution= "gaussian", bag.fraction = .5, 
                   interaction.depth=2, n.trees=1000, shrinkage=.01)

# distribution: distribution of outcome variable
# ntrees: fit total 1000 trees
# shrinkage: learning rate to control overffiting 
# bag.fraction: fraction of observation to use in each tree


best.boost  <- gbm.perf(fit.boost, plot.it = FALSE) # cross-validation

## Evaluating the methods
yhat.rf       <- predict(fit.rf, newdata=data_test) # prediction
yhat.boost    <- predict(fit.boost, newdata=data_test, n.trees=best.boost)

MSE.rf       = summary(lm((Y_test-yhat.rf)^2~1))$coef[1:2]
MSE.boost    = summary(lm((Y_test-yhat.boost)^2~1))$coef[1:2]
R2.rf  <- 1-MSE.rf[1]/var(Y_test)
R2.boost  <- 1-MSE.boost[1]/var(Y_test)

# printing R^2
cat("R^2 of the random forest and boosted trees:",R2.rf,R2.boost)


summary(fit.boost)


#  Summarize table -------------------------

table<- matrix(0, 15, 3)
table[1,1:2]   <- MSE.lm.basic
table[2,1:2]   <- MSE.lm.flex
table[3,1:2]   <- MSE.lasso
table[4,1:2]   <- MSE.lasso.post
table[5,1:2]   <- MSE.lasso.flex
table[6,1:2]   <- MSE.lasso.post.flex
table[7,1:2]   <- MSE.lasso.cv
table[8,1:2]   <- MSE.ridge
table[9,1:2]   <- MSE.elnet
table[10,1:2]   <- MSE.lasso.cv.flex
table[11,1:2]  <- MSE.ridge.flex
table[12,1:2]  <- MSE.elnet.flex
table[13,1:2]  <- MSE.rf
table[14,1:2]  <- MSE.boost
table[15,1:2]  <- MSE.pt



table[1,3]   <- R2.lm.basic
table[2,3]   <- R2.lm.flex
table[3,3]   <- R2.lasso
table[4,3]   <- R2.lasso.post
table[5,3]   <- R2.lasso.flex
table[6,3]   <- R2.lasso.post.flex
table[7,3]   <- R2.lasso.cv
table[8,3]   <- R2.ridge
table[9,3]   <- R2.elnet
table[10,3]   <- R2.lasso.cv.flex
table[11,3]  <- R2.ridge.flex
table[12,3]  <- R2.elnet.flex
table[13,3]  <- R2.rf
table[14,3]  <- R2.boost
table[15,3]  <- R2.pt




colnames(table)<- c("MSE", "S.E. for MSE", "R-squared")
rownames(table)<- c("Least Squares (basic)","Least Squares (flexible)", "Lasso", "Post-Lasso","Lasso (flexible)","Post-Lasso (flexible)", 
                    "Cross-Validated lasso", "Cross-Validated ridge","Cross-Validated elnet","Cross-Validated lasso (flexible)","Cross-Validated ridge (flexible)","Cross-Validated elnet (flexible)",  
                    "Random Forest","Boosted Trees", "Pruned Tree")
tab <- xtable(table, digits =3)

print(tab,type="latex") # set type="latex" for printing table in LaTeX

















