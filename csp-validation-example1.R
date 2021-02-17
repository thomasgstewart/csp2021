## EXAMPLE 1
## Super simple logistic regression model
require(rms)
Hmisc::getHdata(sex.age.response)
head(sex.age.response)
m1 <- lrm(response ~ rcs(age,3)*sex, data = sex.age.response, x=TRUE, y=TRUE)
v1 <- validate(m1, B=100)


## EXAMPLE 2
## From Frank Harrell's Regression Modeling Strategies, Chapter 12
## http://hbiostat.org/doc/rms.pdf
Hmisc::getHdata(titanic3)
dd <- datadist(titanic3)
options(datadist = "dd")
f1 <- lrm(survived~sex*pclass*rcs(age, 4) +rcs(age ,4)*(sibsp), data=titanic3)
p  <- Predict(f1, age , sex , pclass , sibsp=0, fun=plogis)
ggplot(p)
ggplot(Predict(f1, sibsp , age=c(10 ,15 ,20 ,50), conf.int=FALSE))
f1 <- update(f1, x=TRUE , y=TRUE)
v1 <- validate(f1, B=100)
cal <- calibrate(f1, B=200)
par(mfrow = c(1,1))
plot(cal , subtitles=FALSE)

