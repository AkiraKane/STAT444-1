rm()
setwd("~/Desktop/Data Mining & Machine Learning/hw4/")

# Problem 1
X = as.matrix(read.csv("author_training.csv")[1:69])
Y = as.factor(read.csv("author_training.csv")[,70])

X_ts = as.matrix(read.csv("author_testing.csv")[,1:69])
Y_ts = as.factor(read.csv("author_testing.csv")[,70])

trdat = data.frame(Y,X)
trfdat = data.frame(as.factor(Y),X)
tsdat = data.frame(as.factor(Y_ts),X_ts)
names(trdat)[1] = Yf
#k means clustering

ss = svd(X)
u = ss$u; v = ss$v; d = ss$d

k = 4
km = kmeans(trdat,centers = k)
plot(u[,1]*d[1],u[,2]*d[2],col = km$cluster,pch = 16)
cens = km$center
points(cens %*% v[,1],cens%*%v[,2],col = 1:k, pch = 16,cex = 2)

#hierarchical clustering
#complete linkage
com.hclust = hclust(dist(t(X)),method="complete")
plot(com.hclust,cex=.7)

#single linkage
sing.hclust = hclust(dist(t(X)),method="single")
plot(sing.hclust,cex=.7)

#average linkage
ave.hclust = hclust(dist(t(X)),method="average")
plot(ave.hclust,cex=.7)

#bi-clustering via clusterdendograms

sv = svd(t(X));
U = sv$u
V = sv$v
D = sv$d
Z = t(X)%*%V;
ord = order(abs(V[,2]),decreasing=TRUE)
x = as.matrix(X[ord[1:150],])

heatmap(x)

#Problem 2

n1 = length(Y)
sum(Y)/n1
n2 = length(Y_ts)
sum(Y_ts)/n2
X = as.matrix(log(1 + X))
X_ts = as.matrix(log(1+X_ts))
colnames(X) = nams
X = scale(X)/sqrt(n1-1)
X_ts = scale(X_ts)/sqrt(n2-1)

#Classification Tree
require('rpart')
#Build a tree - medium CP - Gini Index
ans = rpart(Y~.,data = as.data.frame(X),method="class",cp=.01,parms=list(split="gini"))
plot(ans,margin=.05)
text(ans,use.n=TRUE)
summary(ans)
checkfunc = function(y1,y2){
    count = 0
    for (i in 1:length(y1)){
        if (y1[i] == y2[i]){
            count = count+1
        }
    }
    return (count/length(y1))
}
# prediction
yhat = predict(ans,newdata = as.data.frame(X_ts),type = 'class')
yh = apply(as.matrix(yhat),1,which.max) - 1
tserr = sum(yhat != Y_ts)/length(Y_ts)
tserr #0.2420635

#Bagging
require('rpart')

B = 100
alpha = .001
n = nrow(trdat)

ybag = rep(0,nrow(tsdat))
for(i in 1:B){
    bsam = sample(1:n,n,replace=TRUE)
    btrdat = trfdat[bsam,]
    ans = rpart(ytr~.,data=btrdat,method="class",cp=alpha)
    yh = predict(ans,newdata=tsdat)
    ybag = ybag + yh/B
}
yh2 = apply(as.matrix(ybag),1,which.max) - 1
tserr = sum(yh!= as.factor(Y_ts))/length(Y_ts)
tserr

#Boosting
sample(1:10,10,replace=TRUE)


#boostrapping & trees
n = nrow(trdat)

alpha = .01

bsam = sample(1:n,n,replace=TRUE)
btrdat = trfdat[bsam,]
ans = rpart(ytr~.,data=btrdat,method="class",cp=alpha,parms=list(split="gini"))
yb = predict(ans,newdata = as.data.frame(X_ts),type = "class")
tserr = sum(yb != Y_ts)/length(Y_ts)
tserr#0.1349206
plot(ans,margin=.05)
text(ans,use.n=TRUE)

#Random Forests
require(randomForest)

fitr = randomForest(Y~.,data=as.data.frame(X), mtry = 69)
yf = predict(fitr,newdata = as.data.frame(X_ts),type = 'class')
summary(fitr)
plot(fitr)
tserr = sum(yf != Y_ts)/length(Y_ts)
tserr #0.1031746

