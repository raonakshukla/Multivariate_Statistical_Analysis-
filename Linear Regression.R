#Importing glmnet for regression
install.packages('glmnet')
install.packages('pls')
library(glmnet)
library(pls)
UN_reg <-cbind(gdp[,2:13], lifeExp[,13])
colnames(UN_reg)[13]<- "Life.Exp"
UN_reg_log <- cbind(log(gdp[,2:13]), lifeExp[,13])
colnames(UN_reg_log)[13]<- "Life.Exp"
independent<- as.matrix(log(UN_reg[,1:12]))
dependent<- as.matrix(UN_reg[13])

#Fitting a linear model(Ordinary Least Square Method)
model1 <- lm(Life.Exp~.,data = UN_reg)
summary(model1)
plot(model1,pages=1)

model1_log <- lm(Life.Exp~.,data = UN_reg_log)
summary(model1_log)
plot(model1_log,pages=1)

#Fitting PCR Regression
library(pls)
model2 <- pcr(Life.Exp~.,data = UN_reg_log,ncomp=5, validation = "CV",scale = TRUE)
summary(model2)
plot(RMSEP(model2), legendpos = "topright")
head(coef(model2))


#Fitting a Lasso Regression
set.seed(164)
model3 <- glmnet(independent, dependent, alpha=1)
plot(model3, xvar='lambda') 
lambdas <- 10^seq(3,-2,by=-0.1)
cv_fit <- cv.glmnet(independent, dependent, alpha = 1, lambda = lambdas)
plot(cv_fit)
cv_fit$lambda.min
cv_fit$lambda.1se
print(cv_fit)
coef(glmnet(independent, dependent, alpha=1, lambda=cv_fit$lambda.1se))

#Fitting a Ridge Regression
set.seed(164)
model4 <- glmnet(independent, dependent, alpha=0)
plot(model4, xvar='lambda') 
lambdas <- 10^seq(3,-2,by=-0.1)
cv_fit1 <- cv.glmnet(independent, dependent, alpha = 0, lambda = lambdas)
plot(cv_fit1)
cv_fit1$lambda.min
cv_fit1$lambda.1se
print(cv_fit1)
coef(glmnet(independent, dependent, alpha=0, lambda=cv_fit$lambda.1se))


