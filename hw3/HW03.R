rm()
setwd("C:/Users/Yinsen/Desktop/Term 3/STAT640/HW03")

packages <- c("e1071", "caret", "doParallel", "parallel", "plyr")
packages.list <- packages %in% rownames(installed.packages())
if (sum(!packages.list) != 0)
    install.packages(packages[!packages.list])
sapply(packages, require, character.only = T)

X = as.matrix(read.csv("spam_X.csv"))
Y = as.factor(read.csv("spam_Y.csv")[,1])

set.seed(2015)
trainIndex = createDataPartition(Y, p = .8, list = F, times = 1)
# CV subset
Xtrain = X[trainIndex,]
Ytrain = Y[trainIndex] # 3682
# Model assessment subset
Xtest = X[-trainIndex,]
Ytest = Y[-trainIndex] # 919



# Question a-e
myCV <- function(X, Y, K, C){
    set.seed(1)
    kFolds = createFolds(Y, k = K)
    MErr = lapply(C, function(Cidx){
        print(c("Current Parameter", Cidx))
        
        # use parallel to boost CV speed
        result = mclapply(kFolds, mc.cores = 15, function(testIdx){
            model = svm(X[-testIdx,], Y[-testIdx], kernel =  "linear", cost = Cidx, probability = T)
            pred = predict(model, X[testIdx,], decision.values = T, probability = T)
            DecValue = attr(pred, which = "dec")
            ProbValue = attr(pred, which = "prob")
            
            # calculate Error
            Err = sum(pred!=Y[testIdx]) / length(testIdx)
            BErr = -mean(as.numeric(pred != "0")*log(ProbValue[,"0"]) + 
                             (1-as.numeric(pred != "0"))*(1-log(ProbValue[,"0"])))
            HErr = mean(pmax(0,1 - sign(DecValue) * DecValue))
            list(Err = Err, BErr = BErr, HErr = HErr)
        })
        matrix(unlist(result), nrow = 3)        
    })
    print(MErr)
    MErr
}

C = 2^(-8:8)
K = 5
my5cv1 = myCV(Xtrain, Ytrain, K, C)
save(my5cv1, file = "my5cv1.Rdata")

K = 10
my10cv1 = myCV(Xtrain, Ytrain, K, C)
save(my10cv1, file = "my10cv1.Rdata")


# plot CV error 
load("my10cv1.Rdata")
Err = matrix(sapply(my10cv1, function(x) x[1,]), ncol = 1)
BErr = matrix(sapply(my10cv1, function(x) x[2,]), ncol = 1)
HErr = matrix(sapply(my10cv1, function(x) x[3,]), ncol = 1)
df <- data.frame(err = c(Err, BErr, HErr), C = rep(rep(C, each = 10), 3),
    Type = rep(c("Missclassification Err.", "Binomial Dev.", "Hinge Loss"), each = length(C)*10))
CV10err <- ddply(df, .(C, Type), summarize, sd = sd(err), mean = mean(err), 
    se = sd(err)/sqrt(length(err)), count = length(err))
minCVSE = CV10err[CV10err$mean == as.numeric(tapply(CV10err$mean, CV10err$Type, min)),]
dummy1 <- data.frame(Type = minCVSE$Type, Y = minCVSE$mean, SE = minCVSE$se)
CV10err$select = as.factor(CV10err$mean == as.numeric(tapply(CV10err$mean, CV10err$Type, min)))

pdf(file = "figure3-2.pdf", width = 5, height = 7)
ggplot(CV10err, aes(x = log2(C), y = mean)) + 
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = select), width = .2, lwd = 0.5) +
    geom_line(lwd = .5) + geom_point(size = .2) +
    ggtitle("10 Fold Cross Validation")+
    theme_bw() + theme(legend.position="none") +
    facet_grid(Type ~ ., scale = "free") +
    geom_hline(data = dummy1, aes(yintercept = Y), color = "red") +
    geom_hline(data = dummy1, aes(yintercept = Y + SE), color = "blue") +
    scale_color_manual(values = c("black", "orange"))
dev.off()

load("my5cv1.Rdata")
Err = matrix(sapply(my5cv1, function(x) x[1,]), ncol = 1)
BErr = matrix(sapply(my5cv1, function(x) x[2,]), ncol = 1)
HErr = matrix(sapply(my5cv1, function(x) x[3,]), ncol = 1)
df <- data.frame(err = c(Err, BErr, HErr), C = rep(rep(C, each = 5), 3),
    Type = rep(c("Missclassification Err.", "Binomial Dev.", "Hinge Loss"), each = length(C)*5))
CV5err <- ddply(df, .(C, Type), summarize, sd = sd(err), mean = mean(err), 
    se = sd(err)/sqrt(length(err)), count = length(err))
minCVSE = CV5err[CV5err$mean == as.numeric(tapply(CV5err$mean, CV5err$Type, min)),]
dummy1 <- data.frame(Type = minCVSE$Type, Y = minCVSE$mean, SE = minCVSE$se)
CV5err$select = as.factor(CV5err$mean == as.numeric(tapply(CV5err$mean, CV5err$Type, min)))

pdf(file = "figure3-1.pdf", width = 5, height = 7)
ggplot(CV5err, aes(x = log2(C), y = mean)) + 
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = select), width = .2, lwd = 0.5) +
    facet_grid(Type ~ ., scale = "free") +
    scale_color_manual(values = c("black", "orange")) +
    geom_line(lwd = .5) + geom_point(size = .2) +
    geom_hline(data = dummy1, aes(yintercept = Y), color = "red") +
    geom_hline(data = dummy1, aes(yintercept = Y + SE), color = "blue") +
    theme_bw() + theme(legend.position="none") +
    ggtitle("5 Fold Cross Validation")
dev.off()

# model assessment
myAssess <- function(X, Y, Xt, Yt, C){
    # use parallel to boost CV speed
    result = mclapply(C, mc.cores = 15, function(Cidx){
        model = svm(X, Y, kernel =  "linear", cost = Cidx)
        pred = predict(model, Xt)
        
        # calculate Error
        Acc = 1 - sum(pred!=Yt) / length(Yt)
    })
    print(result)
    result
}

C = c(2^-3, 2^6, 2^8, 2^3, 2^-1, 2^-6, 2^8, 2^5, 2^1, 2^-2)
Assess = myAssess(Xtrain, Ytrain, Xtest, Ytest, C)
save(Assess, file = "Assess.Rdata")

# Question f-h
# use 80% for cross validation purpose and the rest 20% for model assessment
trainIndex = createDataPartition(Y, p = .8, list = F, times = 1)
# CV subset
Xtrain = X[trainIndex,]
Ytrain = Y[trainIndex] # 3682
# Model assessment subset
Xtest = X[-trainIndex,]
Ytest = Y[-trainIndex] # 919



# use parallel to boost CV speed
# linear SVM
cl <- makePSOCKcluster(15)
registerDoParallel(cl)
set.seed(2014)
trCon <- trainControl(method = "CV", number = 10, verboseIter = T)
lingrid = expand.grid(C = 2^(-7:7))
linsvm <- train(Xtrain, Ytrain, trControl = trCon, tuneGrid = lingird, method = "svmLinear")
linsvm
save(linsvm, file = "linsvm.Rdata")
stopCluster(cl)


# svmRadial
cl <- makePSOCKcluster(15)
registerDoParallel(cl)
trCon <- trainControl(method = "CV", number = 10, verboseIter = T)
set.seed(2014)
rdfgrid = expand.grid(C = 2^(-7:7), sigma = 2^(-7:7))
rdfsvm <- train(Xtrain, Ytrain, trControl = trCon, tuneGrid = rdfgrid, method = "svmRadial")
rdfsvm
save(rdfsvm, file = "rdfsvm.Rdata")
stopCluster(cl)


# svmPoly
cl <- makePSOCKcluster(15)
registerDoParallel(cl)
trCon <- trainControl(method = "CV", number = 10, verboseIter = T)
set.seed(2014)
polygrid = expand.grid(degree = 1:10, scale = 2^(-5:5), C = 2^(-5:5))
polysvm <- train(Xtrain, Ytrain, trControl = trCon, tuneGrid = polygrid, method = "svmPoly")
polysvm
save(polysvm, file = "polysvm.Rdata")
stopCluster(cl)


# Model assessment
# Linear SVM
load(file = "linsvm.Rdata")
# optimal parameter C = 0.5
linsvm$bestTune
linsvm$results["7",] # CV Accuracy: 0.9424222
# Refit the model with full CV dataset
linPred = predict(linsvm, newdata = Xtest)
linAcc = sum(linPred == Ytest) / length(Ytest)
pdf(file = "linsvm.pdf", height = 5, width = 8)
plot(linsvm, main = "Linear SVM CV with K = 10")
dev.off()

# Gaussin SVM
load(file = "rdfsvm.Rdata")
rdfsvm$bestTune
rdfsvm$results["152",] # CV Accuracy: 0.9535591
# Refit the model with full CV dataset
rdfPred = predict(rdfsvm, newdata = Xtest)
rdfAcc = sum(rdfPred == Ytest) / length(Ytest)
pdf(file = "rdfsvm.pdf", height = 5, width = 8)
plot(rdfsvm)
dev.off()

# Polynomial SVM
load(file = "polysvm.Rdata")
polysvm$bestTune
polysvm$results["136",] # CV Accuracy: 0.9542982
# Refit the model with full CV dataset
polyPred = predict(polysvm, newdata = Xtest)
polyAcc = sum(polyPred == Ytest) / length(Ytest)
pdf(file = "ploysvm.pdf", height = 10, width = 12)
plot(polysvm, scales = list(relation = 'free'))
dev.off()

acc = c(linAcc, rdfAcc, polyAcc)
names(acc) <- c("Linear Kernel", "Gaussian Kernel", "Polynomial Kernel")
xtable(as.matrix(acc), digits = 3)
