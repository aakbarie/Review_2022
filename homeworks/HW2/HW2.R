Data <- read.table("C:\\Users\\aesfahani\\Documents\\Personal\\personalitydata.txt",header=T)
attach(Data)
dim(Data)
	index.training <- sample(1:231, 154)
	index.training

	index.test <- seq(1,231,1)[!is.element(1:231, index.training)]
	index.test

	dat.train <- Data[index.training,]
	dat.test <- Data[index.test,]

	dim(dat.train)
	dim(dat.test)

X.tr = as.matrix(cbind(dat.train[,1:10]))
X.tr = as.matrix(cbind(X.tr,dat.train[,12:13]))
dim(X.tr)
X.te = as.matrix(cbind(dat.test[,1:10]))
X.te = as.matrix(cbind(X.te,dat.test[,12:13]))
dim(X.te)
X.tr = scale(X.tr,scale=T)
X.te = scale(X.te,scale=T)

ans = lm(dat.train[,11]~ X.tr)
w = as.matrix(abs(residuals(ans)))
w.bdi = lm(dat.train[,11]~X.tr, weights=w)

summary(w.bdi)



summary(ans)

ans = lm(dat.train[,11]~ dat.train[,1]+dat.train[,2]+dat.train[,3]+dat.train[,4]+
				dat.train[,5]+dat.train[,6]+dat.train[,7]+dat.train[,8]+
				dat.train[,10]+dat.train[,12]+dat.train[,13])
summary(ans)

ans = lm(dat.train[,11]~ dat.train[,1]+dat.train[,2]+dat.train[,3]+dat.train[,4]+
				dat.train[,5]+dat.train[,6]+dat.train[,8]+
				dat.train[,10]+dat.train[,12]+dat.train[,13])
summary(ans)

ans = lm(dat.train[,11]~ dat.train[,1]+dat.train[,2]+dat.train[,3]+
				dat.train[,5]+dat.train[,6]+dat.train[,8]+
				dat.train[,10]+dat.train[,12]+dat.train[,13])
summary(ans)

ans = lm(dat.train[,11]~ dat.train[,1]+dat.train[,5]+dat.train[,6]+dat.train[,8]+
				dat.train[,10]+dat.train[,12]+dat.train[,13])
summary(ans)

ans = lm(dat.train[,11]~ dat.train[,5]+dat.train[,6]+dat.train[,8]+
				dat.train[,10]+dat.train[,12]+dat.train[,13])
summary(ans)

ans = lm(dat.train[,11]~ dat.train[,5]+dat.train[,8]+
				dat.train[,10]+dat.train[,12]+dat.train[,13])
summary(ans)

ans = lm(dat.train[,11]~ dat.train[,5]+dat.train[,8]+ dat.train[,12]+dat.train[,13])
summary(ans)

ans = lm(dat.train[,11]~ dat.train[,5]+ dat.train[,12]+dat.train[,13])
summary(ans)
