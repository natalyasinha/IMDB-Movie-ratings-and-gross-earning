library(tidyverse)
library(magrittr)
library(dplyr)


## Change it to your current location

My.Location <- ""
setwd(My.Location)

# Load data created at the last of the previous step
movie.color.nonmissing <- read_csv('movie_color_nonmissing.csv')
movie.color <- read_csv('movie_color.csv')

########## transformation ###############
movie.color.nonmissing$gross <- log(movie.color.nonmissing$gross)
movie.color.nonmissing$budget <- log(movie.color.nonmissing$budget)

summary(movie.color.nonmissing)
quantile(movie.color.nonmissing$gross, c(.25, .50,  .75, .90,.95, .99))

cols <- c('facenumber_in_poster',
          'language',
          'country',
          'content_rating',
          'title_year',
          'aspect_ratio',
          'Genre.N',
          'short.genre',
          'director_rating',
          'actor1_rating',
          'actor2_rating'
)

movie.color.nonmissing[,cols] <- lapply(movie.color.nonmissing[,cols], factor)
movie.color.nonmissing %>%
  mutate_each_(funs(factor(.)),cols)
str(movie.color.nonmissing)


keep_var<-c('num_critic_for_reviews',
            'duration',
            'gross',
            #'movie_title',
            'num_voted_users',
            #'facenumber_in_poster',
            #'movie_imdb_link',
            'num_user_for_reviews',
            #'language',
            #'country',
            'content_rating',
            'budget',
            #'title_year',
            'imdb_score',
            'aspect_ratio',
            #'Genre.N',
            'short.genre',
            'director_rating',
            'actor1_rating',
            'actor2_rating')

movie.color.nonmissing <- movie.color.nonmissing[keep_var]
str(movie.color.nonmissing)

# Sampling to create training and validation data
set.seed(123)
d <- sort(sample(nrow(movie.color.nonmissing), nrow(movie.color.nonmissing)*0.7))
dev<-movie.color.nonmissing[d,]
val<-movie.color.nonmissing[-d,]

# Dependent variable equation
All.predictor.var <- colnames(subset( movie.color.nonmissing, select = -gross ))
yColumn <- "gross"
formula <- paste(yColumn,paste(All.predictor.var,collapse=' + '),sep=' ~ ')
str(movie.color.nonmissing)

#Linear Regression model 

library(MASS)

pairs(~gross+budget+imdb_score,data=movie.color.nonmissing, 
      main="Simple Scatterplot Matrix")



m0<-lm(gross ~ budget + short.genre + director_rating + actor1_rating + num_critic_for_reviews , data=dev)
summary(m0)
predictions <- predict(m0, dev)
# summarize accuracy
rmse <- sqrt(mean((dev$gross - predictions)^2))
print(rmse)

m1<-lm(formula, data=dev)
?stepAIC
summary(m1)
# make predictions
predictions <- predict(m1, dev)
# summarize accuracy
rmse <- sqrt(mean((dev$gross - predictions)^2))
print(rmse)

# Multiple R-squared:  0.5476,	Adjusted R-squared:  0.5403 
# F-statistic: 75.41 on 33 and 2056 DF,  p-value: < 2.2e-16


stepAIC(m1, direction='backward', criterion='AIC')

?step
null=lm(gross~1, data=dev)
full=lm(gross~., data=dev)
step(null, scope=list(lower=null, upper=full), direction="forward")

step(null, scope = list(upper=full), data=dev, direction="both")
step(full, data=dev, direction="backward")

m2<-lm(formula = gross ~ budget + num_voted_users + content_rating + 
         short.genre + director_rating + num_critic_for_reviews + 
         aspect_ratio + imdb_score + actor1_rating + num_user_for_reviews, data = dev)
summary(m2)
# make predictions - Training data
predictions <- predict(m2, dev)
# summarize accuracy
rmse <- sqrt(mean((dev$gross - predictions)^2))
print(rmse)

# make predictions - Test data
predictions <- predict(m2, val)
# summarize accuracy
rmse <- sqrt(mean((val$gross - predictions)^2))
print(rmse)


##When comparing models fitted by maximum likelihood to the same data, the smaller the AIC or BIC, 
## the better the fit.The theory of AIC requires that the log-likelihood has been maximized
# install.packages("leaps")
library(leaps)
leaps=regsubsets(gross ~.,data=dev, nbest=10,nvmax=10)
plot(leaps, scale="adjr2")
plot(leaps, scale="bic")

regfit.full=regsubsets(gross ~.,data=dev,nvmax=10,method="forward")
summary(regfit.full)
reg.summary=summary(regfit.full)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(reg.summary$cp)
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],pch=20,col="red")


## Support Vector Machine 
# Fit the training data with the radila kernel and Y/gamma = 1 and cost = 1
library(e1071)
svmfit=svm(gross ~., data=dev, kernel="radial",  gamma=1, cost=1)
plot(svmfit, dat[train,])
summary(svmfit)
# make predictions - Training data
predictions <- predict(svmfit, dev)
# summarize accuracy
rmse <- sqrt(mean((dev$gross - predictions)^2))
print(rmse)

# make predictions - Test data
predictions <- predict(svmfit, val)
# summarize accuracy
rmse <- sqrt(mean((val$gross - predictions)^2))
print(rmse)


# Perform a cross - validation using the tuning parameter to identify the best choice available with varying 
# cost and Y for SVM using a radial kernel 
set.seed(1)
tune.out=tune(svm, gross ~., data=dev, kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
# make predictions - Training data
predictions <- predict(tune.out$best.model, dev)
# summarize accuracy
rmse <- sqrt(mean((dev$gross - predictions)^2))
print(rmse)

# make predictions - Test data
predictions <- predict(tune.out$best.model, val)
# summarize accuracy
rmse <- sqrt(mean((val$gross - predictions)^2))
print(rmse)

## Neural Network

# load the package
library(nnet)
# fit model
fit <- nnet(gross ~., dev, size=12, maxit=500, linout=T, decay=0.01)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, dev, type="raw")
# summarize accuracy
rmse <- sqrt(mean((dev$gross - predictions)^2))
print(rmse)

# make predictions - Test data
predictions <- predict(fit, val)
# summarize accuracy
rmse <- sqrt(mean((val$gross - predictions)^2))
print(rmse)

## MARS - Multivariate Adaptive Regression Splines
# install.packages("earth")

# here pmethod is the pass method (forward or backward) nprune is the used to set the maximum number 
# of terms. Nfold is the number #of folds in cross validation.


library(earth)
# fit model
fit <- earth(gross ~., dev)
fit2 <- earth(gross ~., dev,pmethod="backward",nprune=20, nfold=10)
# summarize the fit
summary(fit)
summary(fit2)
# make predictions using fit
predictions <- predict(fit, dev)
rmse <- sqrt(mean((dev$gross - predictions)^2))
print(rmse)
# make predictions - Test data
predictions <- predict(fit, val)
rmse <- sqrt(mean((val$gross - predictions)^2))
print(rmse)

# make predictions using fit2
predictions <- predict(fit2, dev)
rmse <- sqrt(mean((dev$gross - predictions)^2))
print(rmse)
# make predictions - Test data
predictions <- predict(fit2, val)
rmse <- sqrt(mean((val$gross - predictions)^2))
print(rmse)

### Random Forest 
install.packages("miscTools")
library(randomForest)
library(miscTools)
library(ggplot2)

rf <- randomForest(gross ~., dev, ntree=20)
importance(rf)
varImpPlot(rf,type=2)

# importance(rf,type=1)
# make predictions
predictions <- predict(rf, dev)
# summarize accuracy
rmse <- sqrt(mean((dev$gross - predictions)^2))
print(rmse)
(r2 <- rSquared(dev$gross, dev$gross - predict(rf, dev)))

# make predictions - Test data
predictions <- predict(rf, val)
# summarize accuracy
rmse <- sqrt(mean((val$gross - predictions)^2))
print(rmse)



(r2 <- rSquared(val$gross, val$gross - predict(rf, val)))
# [1] 0.6192
(mse <- mean((val$gross - predict(rf, val))^2))
# [1] 1.745427

p <- ggplot(aes(x=actual, y=pred),
            data=data.frame(actual=val$gross, pred=predict(rf, val)))
p + geom_point() +
  geom_abline(color="red") +
  ggtitle(paste("RandomForest Regression in R r^2=", r2, sep=""))

# run the party implementation
install.packages("party")
library(party)
cf1 <- cforest(gross~.,data=dev,control=cforest_unbiased(ntree=20))
varimp(cf1)
varimp(cf1,conditional=TRUE)

# Add fit lines
# abline(lm(mpg~wt), col="red") # regression line (y~x) 
# lines(lowess(wt,mpg), col="blue") # lowess line (x,y)