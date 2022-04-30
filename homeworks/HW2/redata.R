Data <- read.table("C:\\Users\\Home\\Documents\\UCD\\Math 6388\\HW2\\RealEstateSales.txt",header=T)
attach(Data)
dim(Data)

	index.training <- sample(1:521,347)

	index.test <- seq(1,521,1)[!is.element(1:521, index.training)]

	dat.train <- Data[index.training,]
	dat.test <- Data[index.test,]

	dim(dat.train)
	dim(dat.test)
	
	
X.tr = as.matrix(cbind(dat.train[,2:12]))
Y.tr = as.matrix(cbind(dat.train[,1]))
X.te = as.matrix(cbind(dat.test[,2:12]))
Y.te = as.matrix(cbind(dat.test[,1]))
dim(X.te)
dim(X.tr)

pr.price = lm(Y.tr ~ X.tr)
summary(pr.price)

X = as.matrix(cbind(X.tr[,1:5],X.tr[,7:11]))
pr.price = lm(Y.tr ~ X)
summary(pr.price)

X = as.matrix(cbind(X[,1:2],X[,4:10]))
pr.price = lm(Y.tr~ X)
summary(pr.price)

X = as.matrix(cbind(X[,1:2],X[,4:9]))
pr.price = lm(Y.tr~ X)
summary(pr.price)

X = as.matrix((X[,1:7]))
pr.price = lm(Y.tr~ X)
summary(pr.price)

X = cbind(X[,1:2],X[,4:7])
pr.price = lm(Y.tr~ X)
names(pr.price)
summary(pr.price)
summary(reg.class.test)
X.te=cbind(dat.test[,2:3],dat.test[,8:11])
reg.class.test <- predict.lm(pr.price,newdata=X.te)
reg.class.test <- 1*(reg.class.test > 0.5)
Y.test <- 1*(test[,1]==2)
table(Y.test, reg.class.test)
dim(X.te)
dim(X)

Bayes.reg.out <- MCMCregress(price~ area+lotsize)
summary(Bayes.reg.out)
plot(Bayes.reg.out)
X.te = cbind(dat.test[,2],dat.test[,11])
=]ol.,,.pr.price = lm(price~ area+lotsize)
summary(pr.price)
prd.price <- predict.lm(pr.price,newdata=dat.test[2:12], interval="prediction")
reg.class.test <- predict.lm(y.lm,newdata=dat.test[2:12], interval="prediction")
conf.price
length(dat.test[,1])
dim(conf.price)
m.t = mean((conf.price-dat.test[,1]))
m = mean(Data[,1])
error.rate = m.t/m


##############################################################################

Variables = names(dat.train)[2:12]
for (i in 1:11)
{
	pr.price = lm(Y.tr ~ X.tr[,i])
	cat("\nResults using predictor: ",Variables[i],"\n",sep="")
	print(summary(pr.price)$coeff)
}
X = as.matrix(X.tr[,1])
X2 = as.matrix(X.tr[,2:11]

for (i in 2:11)
{
	pr.price = lm(Y.tr ~ X+X.tr[,i])
	cat("\nResults using predictor: Area + ",Variables[i],"\n",sep="")
	print(summary(pr.price)$coeff)
}

X = as.matrix(cbind(X.tr[,1],X.tr[,8]))
X2 = as.matrix(cbind(X.tr[,2:7],X.tr[,9:11]))
Variables = c(names(dat.train)[3:8],names(dat.train)[10:12])
for (i in 1:9)
{
	pr.price = lm(Y.tr ~ X+X2[,i])
	cat("\nResults using predictor: Area + Quality + ",Variables[i],"\n",sep="")
	print(summary(pr.price)$coeff)
}

X = as.matrix(cbind(X.tr[,1],X.tr[,7],X.tr[,8]))
X2 = as.matrix(cbind(X.tr[,2:6],X.tr[,9:11]))
Variables = c(names(dat.train)[3:7],names(dat.train)[10:12])
for (i in 1:8)
{
	pr.price = lm(Y.tr ~ X+X2[,i])
	cat("\nResults using predictor: Area + Quality + Age + ",Variables[i],"\n",sep="")
	print(summary(pr.price)$coeff)
}

X = as.matrix(cbind(X.tr[,1],X.tr[,7],X.tr[,8],X.tr[,9],))
X2 = as.matrix(cbind(X.tr[,2:6],X.tr[,10:11]))
Variables = c(names(dat.train)[3:7],names(dat.train)[11:12])
for (i in 1:7)
{
	pr.price = lm(Y.tr ~ X+X2[,i])
	cat("\nResults using predictor: Area + Quality + Age + Style + ",Variables[i],"\n",sep="")
	print(summary(pr.price)$coeff)
}

X = as.matrix(cbind(X.tr[,1],X.tr[,7],X.tr[,8],X.tr[,9],X.tr[,10]))
X2 = as.matrix(cbind(X.tr[,2:6],X.tr[,11]))
Variables = c(names(dat.train)[3:7],names(dat.train)[12])
for (i in 1:6)
{
	pr.price = lm(Y.tr ~ X+X2[,i])
	cat("\nResults using predictor: Area + Quality + Age + Style + Lot Size + ",Variables[i],"\n",sep="")
	print(summary(pr.price)$coeff)
}

X = as.matrix(cbind(X.tr[,1:2],X.tr[,7:10]))
X2 = as.matrix(cbind(X.tr[,3:6],X.tr[,11]))
Variables = c(names(dat.train)[4:7],names(dat.train)[12])
for (i in 1:5)
{
	pr.price = lm(Y.tr ~ X+X2[,i])
	cat("\nResults using predictor: Area + Quality + Age + Style + Lot Size + # of Bedrooms + ",Variables[i],"\n",sep="")
	print(summary(pr.price)$coeff)
}

pr.price = lm(Y.tr ~ X)
summary(pr.price)
confint(pr.price)
reg.class.test <- predict.lm(pr.price, newdata=X.te)
reg.class.test <- 1*(reg.class.test > 0.5)
Y.test <- 1*(test[,1]==2)
table(Y.test, reg.class.test)

####################################################################

X = as.matrix(cbind(X[,1:2],X[,4:7]))
pr.price = lm(price~ area+bed+age+qu+style+lotsize)
summary(pr.price)

X.te=(cbind(dat.test[,2:3],dat.test[,8:11]))
prd.price <- predict.lm(pr.price,newdata=dat.test[2:12], interval="prediction")
conf.price <- predict.lm(pr.price,newdata=dat.test[2:12], interval="confidence")

conf.price
dat.test[,1]

m.t = mean(conf.price-dat.test[,1])
m = mean(Data[,1])
error.rate = m.t/m

#####################################################################################

library(leaps)

price.predic = leaps(x=X.tr, y=Y.tr, method="r2")
cbind(price.predic$which, round(price.predic$r2,2))

pr.price = lm(price~ area+age+qu+style+lotsize)
prd.price <- predict.lm(pr.price,newdata=dat.test[2:12], interval="prediction")
conf.price <- predict.lm(pr.price,newdata=dat.test[2:12], interval="confidence")

conf.price
dat.test[,1]

m.t = mean(conf.price-dat.test[,1])
m = mean(Data[,1])
error.rate = (m.t/m)

###################################################################################
library(stats)
g = lm(price ~ area+bed+bath+ac+grsize+pool+age+qu+style+lotsize+hgway)
g = lm(Y.tr ~ X.tr[,1]+X.tr[,2]+X.tr[,3]+X.tr[,4]+X.tr[,5]+X.tr[,6]+X.tr[,7]+X.tr[,8]+X.tr[,9]+X.tr[,10]+X.tr[,11])

summary(g)
gg = step(g, direction="both", k=2, )
summary(gg)
names(Data)
plot(conf.price <- predict.lm(g,newdata=dat.test[2:12], interval="confidence"))
m.t = mean(conf.price-dat.test[,1])
m = mean(Data[,1])
error.rate = (m.t/m)
plot(conf.price[,1],dat.test[,1])


Bayes.reg.out <- MCMCregress(Y.tr ~ X.tr[,1:2]+X.tr[,7:10])
summary(Bayes.reg.out)
plot(Bayes.reg.out)