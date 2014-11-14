#libraries required
library(glmnet)
library(ncvreg)
library(leaps)
library(MASS)
library(cvTools)
library(DAAG)

#load in prostate data
m = read.csv("~/Desktop/prostate.data",header = TRUE,sep="\t")
m_backup = m
m = as.matrix(m)
#m = scale(m,center = TRUE, scale = TRUE )

#get train and test sets
train = m[m[,11]==TRUE,]
train_backup = m_backup[m_backup[,11]==TRUE,]
train_backup = train_backup[,-11]
Y_t = as.numeric(train[,10]);
Y_t = Y_t - mean(Y_t)
X_t = as.matrix(train[,-10]);
X_t = as.matrix(X_t[,-10]);
X_t = scale(X_t,center = TRUE,scale = TRUE)
test = m[m[,11]==FALSE,]
test = as.data.frame(test)
y_t = as.numeric(test[,10]);
y_t = y_t - mean(y_t);
x_t = as.matrix(test[,-10]);
x_t = as.matrix(x_t[,-10]);
x_t = scale(x_t,center = TRUE,scale = TRUE)

#least squares, ridge regression, lasso, adaptive lasso,SCAD, elastic net,

lam = 1
betals = solve(t(X_t)%*%X_t)%*%t(X_t)%*%Y_t
betar = solve(t(X_t)%*%X_t + diag(rep(lam/2*nrow(X_t),9)))%*%t(X_t)%*%Y_t
fitl = glmnet(x=X_t,y=Y_t,family="gaussian",lambda=lam,alpha=1)
fital = glmnet(x=X_t,y=Y_t,family="gaussian",lambda=lam,alpha=1,penalty.factor=1/abs(betals))
fitel = glmnet(x=X_t,y=Y_t,family="gaussian",lambda=lam,alpha=.5)
fitscad = ncvreg(X_t,Y_t,family="gaussian",penalty="SCAD",lambda=lam)
fitr = glmnet(x=X_t,y=Y_t,family="gaussian",lambda=lam,alpha=0)

#best subset,forward step-wise, backward step-wise
fitbsub = regsubsets(x=X_t,y=Y_t)
summary(fitbsub)

fit0 = lm(Y_t~1, data = train_backup)
fitf = stepAIC(fit0,direction = "forward",data = train_backup,k = log(nrow(train_backup)))
summary(fitf)

fit = lm(Y_t~X_t,data = train_backup)
fitb = stepAIC(fit,direction = "backward",data = train_backup, k = log(nrow(train_backup)))
summary(fitb)


#Looking at principal components
svdx = svd(X_t)
#PC regression
betapcr = diag(svdx$d)%*%t(svdx$u)%*%Y_t

#PLS regression
plsfunc = function(x,y)
{
    p = ncol(x); n = nrow(x);
    M = t(x)%*%y
    Z = NULL; V = NULL; P = NULL;
    for(k in 1:p)
    {
        svdm = svd(M)
        z = x%*%svdm$u
        z = z*as.numeric(1/sqrt(t(z)%*%z))
        V = cbind(V,svdm$u)
        p = t(x)%*%z/as.numeric(t(z)%*%z)
        P = cbind(P,p);
        Z = cbind(Z,z);
        M = M - P%*%solve(t(P)%*%P)%*%t(P)%*%M;
    }
    return(list(Z=Z,V=V))
}

plsx = plsfunc(X_t,Y_t)
betapls = t(plsx$Z)%*%Y_t

#Using cross Validation with 10 folds 
fitl_cv = cv.glmnet(x=X_t,y=Y_t,family="gaussian",alpha=1);
fital_cv = cv.glmnet(x=X_t,y=Y_t,family="gaussian",alpha=1,penalty.factor=1/abs(betals));
fitel_cv = cv.glmnet(x=X_t,y=Y_t,family="gaussian",alpha=.5);
fitr_cv = cv.glmnet(x=X_t,y=Y_t,family="gaussian",alpha=0);
fitscad_cv = cv.ncvreg(X_t,Y_t,family="gaussian",penalty="SCAD",lambda=1)
cof_fitl = coef(fitl_cv);
cof_fital = coef(fital_cv);
cof_fitel = coef(fitel_cv);
cof_fitr = coef(fitr_cv);
cof_fitscad = coef(fitscad)

#Calculate the training error and test error
terrfunc = function(fit,x_t,y)
{
    n = 67
    y_t = predict(fit,x_t)
    terr = sum((y_t-y)^2)/n
    return(terr)
}

test_err_l = terrfunc(fitl,x_t,y_t)
train_err_l = terrfunc(fitl,X_t,Y_t)
test_err_el = terrfunc(fitel,x_t,y_t)
train_err_el = terrfunc(fitel,X_t,Y_t)
test_err_al = terrfunc(fital,x_t,y_t)
train_err_al = terrfunc(fitel,X_t,Y_t)
test_err_r = terrfunc(fitr,x_t,y_t)
train_err_r = terrfunc(fitel,X_t,Y_t)
fitlm = lm(Y_t~X_t)
predict.lm(fitlm,test,se.fit= TRUE)




    



