#libraries required
library(glmnet)
library(ncvreg)

#load in gene expression data
gene_X = read.csv("~/Desktop/Data Mining & Machine Learning/hw1/X_SRBCT.csv")
gene_Y = read.csv("~/Desktop/Data Mining & Machine Learning/hw1/Y_SRBCT.csv")
gene_data = cbind(gene_Y,gene_X)

gene_X = as.matrix(gene_X);
gene_X = scale(gene_X,center = TRUE, scale = FALSE)

gene_Y = as.numeric(gene_Y[,1]);
gene_Y = gene_Y - mean(gene_Y)

#Elastic Net Paths
fitel = cv.glmnet(x=gene_X,y=gene_Y,family="gaussian",alpha=.165)
coef(fitel)
fitel2 = glmnet(x=gene_X,y=gene_Y,family="gaussian",alpha=.17)
plot(fitel2,col=1:82,main="Elastic Net alpha=.18")
legend("topright",legend=names(gene_data)[1:82],col=1:82,lty=rep(1,10),cex=.75)

#Lasso Paths
fitl <- cv.glmnet(x = gene_X, y = gene_Y, family = "gaussian", alpha = 1)
coef(fitl)
plot(fitl,col =1:82)
legend("topright",legend = names(gene_data)[1:82],col = 1:82,lty = rep(1,82),cex = .8)





