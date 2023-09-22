
n<-1000
X<-mvrnorm(n=n, mu=c(0,0), Sigma=cbind(c(1, 0.98), c(0.98, 1)))
y<-1+X[,1]+X[,2]+rnorm(n)
dat<-data.frame(y, X)
reg<-lm(y~X1+X2, data=dat)
summary(reg)
vif(reg)

reg2<-lm(y~(x3=X1+X2), data=dat)
summary(reg2)
