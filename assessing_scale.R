
# read in dataset
dat<-read.csv("data/data_caerphilly_full.csv", sep = ";")
str(dat)

# make factors out of character variables
dat<- dat |> mutate(mi = factor(mi, levels=c(0, 1), labels=c("No", "Yes")),
                    socclass = factor(socclass, levels=c("I", "II", "IIINM", "IIIM", "IV", "V")), 
                    diabetes=factor(diabetes, levels=c("No/uncertain", "Yes")), 
                    smoking=factor(smoking, levels=c("Never smoked", " Ex>5 years", "Ex 1-4 years" ,"<15 per day", ">15 per day")),
                    bmicat = factor(bmicat, levels=c("Underweight", "Normal", "Overweight", "Obese")),
                    cursmoke = factor(cursmoke, levels=c("No", "Yes")))
str(dat)
head(dat,10)


# Linearity

# fibrin
# define a factor variable representing quintils of fibrin
hist(dat$fibrin)
quint <- quantile(dat$fibrin, seq(0,1,0.2))
dat$fibrin_q <- cut(dat$fibrin, quint, include.lowest = TRUE)
# a numeric variable representing means within quintiles
qmed <- tapply(dat$fibrin, dat$fibrin_q, median)
dat$fibrin_qm <- qmed[dat$fibrin_q]
table(dat$fibrin_q, dat$fibrin_qm)
# run regressions
temp0<-glm(mi ~ fibrin , data=dat, family=binomial(link="logit"))
dat$fit0_lin <- temp0$linear.predictors
dat$fit0 <- temp0$fitted.values
temp1<-glm(mi ~ fibrin_qm , data=dat, family=binomial(link="logit"))
summary(temp1)
dat$fit1_lin <- temp1$linear.predictors
dat$fit1 <- temp1$fitted.values
temp2<-glm(mi ~ fibrin_q  , data=dat, family=binomial(link="logit"))
summary(temp2)
dat$fit2_lin <- temp2$linear.predictors
dat$fit2 <- temp12fitted.values

# LR test
anova(temp1, temp2, test="Chisq") # We can keep the restriction and assume linearity


# Plots
# logit scale
ggplot(data=dat, aes(x=fibrin)) +
  geom_vline(xintercept = quint, linetype="dotted",  color = "blue") +
  geom_point(aes(y=fit1_lin), col="red") +
  geom_line(aes(x=fibrin_qm, y=fit1_lin), col="red") +
  geom_point(aes(y=fit2_lin), col="blue", alpha = 1/10) +
  geom_line(aes(x=fibrin_qm, y=fit2_lin), col="blue") +



# probability scale
ggplot(data=dat, aes(x=fibrin, y=as.numeric(mi=="Yes")))+
  geom_point() + 
  geom_smooth(method="loess") + 
  geom_line(aes(y=fit0, x=fibrin), color="red")





set.seed(12345)
n <- 500
dat<-data.frame(x=rnorm(n))
dat$xs <- dat$x^2
dat$logit <- -1.5+ 0.3*dat$x + 0.3* dat$xs
dat$odds <- exp(dat$logit)
dat$prob <- dat$odds/(1+dat$odds)
dat$y <- as.integer(runif(n)<=dat$prob)


# x
# define a factor variable representing quintils of x
hist(dat$x)
quint <- quantile(dat$x, seq(0,1,0.20))
dat$x_q <- cut(dat$x, quint, include.lowest = TRUE)
# a numeric variable representing means within quintiles
qmed <- tapply(dat$x, dat$x_q, median)
dat$x_qm <- qmed[dat$x_q]
table(dat$x_q, dat$x_qm)
# run regressions
temp<-glm(y ~ x + xs , data=dat, family=binomial(link="logit"))
summary(temp)
dat$fit_lin <- temp$linear.predictors
dat$fit <- temp$fitted.values
temp0<-glm(y ~ x , data=dat, family=binomial(link="logit"))
summary(temp0)
dat$fit0_lin <- temp0$linear.predictors
dat$fit0 <- temp0$fitted.values
temp1<-glm(y ~ x_qm , data=dat, family=binomial(link="logit"))
summary(temp1)
dat$fit1_lin <- temp1$linear.predictors
dat$fit1 <- temp1$fitted.values
temp2<-glm(y ~ x_q  , data=dat, family=binomial(link="logit"))
summary(temp2)
dat$fit2_lin <- temp2$linear.predictors
dat$fit2 <- temp2$fitted.values


# LR test
anova(temp1, temp2, test="Chisq") # We can keep the restriction and assume linearity


# Plots
# logit scale
ggplot(data=dat, aes(x=x_qm,)) +
  geom_vline(xintercept = quint, linetype="dotted",  color = "blue") +
  geom_line(aes(y=fit1_lin), col="red") +
  geom_line(aes(y=fit2_lin), col="blue") +
  theme_void()


ggplot(data=dat, aes(x=x)) +
  geom_vline(xintercept = quint, linetype="dotted",  color = "blue") +
  geom_point(aes(y=fit1_lin), col="red", alpha = 1/10) +
  geom_line(aes(x=x_qm, y=fit1_lin), col="red") +
  geom_point(aes(y=fit2_lin), col="blue", alpha = 1/10) +
  geom_line(aes(x=x_qm, y=fit2_lin), col="blue") +
  theme_void()


# probability scale
ggplot(data=dat, aes(x, y))+
  geom_point(alpha = 1/10) + 
  geom_smooth(method="loess", col="grey", se=FALSE) + 
  geom_line(aes(y=fit0, x=x), color="red") +
  geom_line(aes(y=fit, x=x), color="blue") +
  geom_line(aes(y=prob, x=x), color="black") +
  theme_minimal()





