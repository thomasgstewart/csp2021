#######################################################
## File:	relaxo examples.R
## Purpose: Perform (relaxed) lasso & crossvalidate
##
## Author: 	J. Blume
#######################################################

# https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
# https://stackoverflow.com/questions/24148805/how-can-i-force-cv-glmnet-not-to-drop-one-specific-variable
# adaptive lasso example: http://ricardoscr.github.io/how-to-adaptive-lasso.html

#######################################################
## Packages
#######################################################

library(glmnet)
library(lars)
library(relaxo)
library(corrplot)
# library(boot)

#######################################################
## Data
#######################################################

data(diabetes)

y <- diabetes$y ; length(y)
x <- diabetes$x ; dim(x)

rhomat=cor(cbind("y"=c(y),x))

## Correlation Plot
corrplot(rhomat, method="circle",tl.srt=45,tl.col="black",diag=F) #,addCoef.col = "black")
corrplot(rhomat, method="number",tl.srt=45,tl.col="black",diag=F)

#######################################################
## Lasso, ridge, and elastic net via GLMNET 
#######################################################

## Lasso
lasso.1 <- glmnet(x,y,alpha=1,family="gaussian")	
plot(lasso.1,label=TRUE)

## Ridge
lasso.0 <- glmnet(x,y,alpha=0,family="gaussian")
plot(lasso.0,label=TRUE)

## Elastic Net
lasso.5 <- glmnet(x,y,alpha=0.5,family="gaussian")	
plot(lasso.5,label=TRUE)

#######################################################
## Add variable names in right margin of plot
#######################################################

plot(lasso.1)
vn=names(x[1,])
vnat=coef(lasso.1) 
vnat=vnat[-1,ncol(vnat)] # remove the intercept, and get the coefficients at the end of the path 
axis(4, at=vnat,line=-.5,label=vn,las=1,tick=FALSE, cex.axis=0.8)

#######################################################
## Crossvalidation 
#######################################################

## Lasso
l1.cv <- cv.glmnet(x,y,alpha=1,family="gaussian")
summary(l1.cv)
plot(l1.cv)  # top axis is number of variables in model

cbind(coef(l1.cv,s="lambda.min"),coef(l1.cv,s="lambda.1se"))

plot(lasso.1,label=TRUE,xvar="lambda")
abline(v=log(l1.cv$lambda.min),lty=2,lwd=0.5)
abline(v=log(l1.cv$lambda.1se),lty=2,lwd=0.5)

#######################################################
## Force variable(s) to remain in model 
#######################################################

## variable order
cbind(names(x[1,]),seq(dim(x)[[2]]))

## Set penalty vector (1=normal lasso, 0=Keep, huge=Drop)
keep=rep(1,dim(x)[[2]])
keep[3]=0					# Force BMI [3rd column in X] in model

lasso.bmi <- glmnet(x,y,penalty.factor=keep)	
lb.cv <- cv.glmnet(x,y,penalty.factor=keep)	

plot(lasso.bmi,label=TRUE,xvar="lambda")
abline(v=log(lb.cv$lambda.min),lty=2,lwd=0.5)
abline(v=log(lb.cv$lambda.1se),lty=2,lwd=0.5)

## Check Lasso against OLS
coef(lasso.bmi,s=max(lasso.bmi$lambda))
lm(y~x[,3])

coef(lasso.bmi,s=min(lasso.bmi$lambda))
lm(y~x) 

## Pick best cross-validated
coef(lasso.bmi,s=log(lb.cv$lambda.1se))

#######################################################
## Relaxed Lasso (Lasso for selection; OLS for Fit)
#######################################################

b <- coef(l1.cv,s="lambda.min")
colnames(b) <- "lasso.min"
subset <- colnames(x)[which(b!=0)-1]
dropped <- setdiff(colnames(x),subset)

ols.1 <- lm(y~x[,subset])
summary(ols.1)		# this is a relaxed lasso 
b.ols <- as(c(coef(ols.1),rep(0,length(dropped))),"sparseMatrix")
rownames(b.ols)=c("(Intercept)",subset,dropped)

relaxo.1 <- relaxo(scale(x),scale(y)) 
plot(relaxo.1,xvar="lambda")

rl1.cv <- cvrelaxo(scale(x),scale(y),k=10) 
print(rl1.cv$lambda) 

here <- max(which(relaxo.1$lambda==rl1.cv$lambda))
B <- c(mean(y),relaxo.1$beta[here,]*sd(y)/sd(x) )	# unstandardized coefs

c(rl1.cv$lambda,l1.cv$lambda.min)

cbind(b,'relaxo'=B,'ols'=b.ols[match(rownames(b),rownames(b.ols))])

# relaxo does not take lasso arguments like penalty.factor, so
# easest to just fit lasso, CV, then fit OLS model at cv.lambda

#######################################################
## Relaxo extras (phi is relaxiation parameter)
#######################################################

## Plot relaxed lasso paths at cv.phi
print(rl1.cv$phi) 
plot(relaxo(scale(x),scale(y),phi=c(0,rl1.cv$phi,0.5,1)))

## Compute fitted values and plot them versus actual values 
fitted.values <- predict(rl1.cv) 
plot(fitted.values,y) 
abline(c(0,1))

## R^2 for models
cor(fitted.values,y)
sqrt(summary(ols.1)$r.squared)

sqrt(summary(lm(y~x))$r.squared)
sqrt(summary(lm(y~x[,1]))$r.squared)

#######################################################
## Bootstrap Lasso
#######################################################

boots=500
b.betas <- matrix(NA,nrow=boots,ncol=dim(x)[[2]]+1)

for (i in 1:boots) {
	select <- sample(length(y),replace=TRUE)
	l1.cv.boot <- cv.glmnet(x[select,],y[select],alpha=1,family="gaussian")
	b.betas[i,]=as.vector(coef(l1.cv.boot,s="lambda.min"))
	if(i %% 50==0) {cat(paste0("iteration: ", i, "\n"))}
}

colMeans(b.betas)
hist(b.betas[,2])
hist(b.betas[,2],breaks=80)	


####
###
##
#