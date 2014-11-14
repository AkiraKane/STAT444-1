#Question 1

#import data
training_set = read.csv('~/Desktop/Data Mining & Machine Learning/hw2/trainzip.csv',header = F);
test_set = read.csv('~/Desktop/Data Mining & Machine Learning/hw2/ziptest.csv',header = F);

training_3 = read.csv('~/Desktop/Data Mining & Machine Learning/hw2/training_3.csv',header = F);
training_8 = read.csv('~/Desktop/Data Mining & Machine Learning/hw2/training_8.csv',header = F);

training_38 = rbind(training_3,training_8);
test_3 = read.csv('~/Desktop/Data Mining & Machine Learning/hw2/test_3.csv',header = F);
test_8 = read.csv('~/Desktop/Data Mining & Machine Learning/hw2/test_t8.csv',header = F);
test_38 = rbind(test_3,test_8)

#Naive Bayes Classifier
require("e1071")
fitB = naiveBayes(x = training_38,y = as.factor(training_38[,1]))
predB = predict(fitB,newdata = test_38 ,type = "class")

#KNN Classifier
require("class")
predK = knn(test=test_38[,-1],train=training_38[,-1],as.factor(training_38[,1]),k=2)

#LDA
require("MASS")
y2 = training_38[,1]
x2 = as.data.frame(training_38[,-1])
z = lda(y2~.,x2)
z_prime_lda = predict(z,test_38[,-1])$class

#QDA
require("MASS")
training_3_new = training_3[1:542,]
training_38_new = rbind(training_3_new,training_8)
y3 = factor(training_38_new[,1])
x3 = as.matrix(training_38_new[,-1])
z2 = qda(x3,y3)
#does not work since the size of group 8 is 167, which is less than 256. qda got into a rank deficiency problem.

#Logistic
library('MASS')
X = as.matrix(log(1+x2))
X = scale(training_set[,-1])/sqrt(length(y2)-1)
rownames(dat)[1:1200] <- paste("foo",1:1200,sep = "")
dat = as.data.frame(y2,x2)
fit = glm(y2~., dat, family = "binomial")

#Regularized logistic
require(LiblineaR)
m = LiblineaR(x2,y2)
predRL = predict(m,test_38[,-1])

#Linear SVM
require('e1071')
z_svm = svm(y = as.factor(y2),x = x2,cost = 100,gamma = 0.01)
z_prime_svm = predict(z_svm,test_38[,-1])


#Kernel SVM(polynomial and radial kernel)
z_svm_kernel_p <- svm(as.factor(y2)~.,as.data.frame(x2),kernel = "polynomial",gamma = 1,degree = 1)
z_prime_kernel_p = predict(z_svm_kernel_p,test_38[,-1])

z_svm_kernel_r <- svm(as.factor(y2)~., as.data.frame(x2),kernel = "radial",gamma = 1,degree = 1)
z_prime_kernel_r = predict(z_svm_kernel_r,test_38[,-1])
# Check test error
checkfunc = function(y1,y2){
    count = 0
    for (i in 1:length(y1)){
        if (y1[i] == y2[i]){
            count = count+1
        }
    }
    return (count/length(y1))
}

y1 = test_38[,1]
checkfunc(y1,predB) #0.8373494
checkfunc(y1,predK) # 0.9638554
checkfunc(y1,z_prime_lda) #0.9518072
checkfunc(y1,z_prime_svm) #0.9156627
checkfunc(y1,z_prime_kernel_p) #0.9668675
checkfunc(y1,z_prime_kernel_r) #0.5
checkfunc(y1,unlist(predRL)) #0.9638554

#Question 2
#multiclass case:

#SVM 1 vs 1:
require('e1071')
y_label = training_set$V1
x_test = test_set[,-1]
y_test = test_set[,1]
z_svm_one = svm(as.factor(y_label)~.,as.data.frame(training_set[,-1]),cost = 100,gamma = 0.01)
z_prime_svm_one = predict(z_svm_one,x_test)
checkfunc(y_test,z_prime_svm_one) #0.9063279

tab <- table(pred = z_prime_svm_one,true = y_test)

#SVM 1 vs all:
z_svm_all = svm(as.factor(y_label)~.,as.data.frame(training_set[,-1]),cross = 10,cost = 100,gamma = 0.01)
z_prime_svm_all = predict(z_svm_all,x_test)
checkfunc(y_test,z_prime_svm_all)


#Question 3
#Import data
ctraining_labels = read.csv('~/Desktop/Data Mining & Machine Learning/hw2/14cancer_training_set_class_labels.ytrain.csv',header = F)
ctraining_data = read.csv('~/Desktop/Data Mining & Machine Learning/hw2/14cancer_training_set_gene_expression.xtrain.csv',header = F)
ctest_labels = read.csv('~/Desktop/Data Mining & Machine Learning/hw2/14cancer_test_set_class_labels.ytrain.csv',header = F)
ctest_data = read.csv('~/Desktop/Data Mining & Machine Learning/hw2/14cancer_test_set_gene_expression.xtest.csv',header = F)

#Subprob-a
#Multinomial ridge regression
fitr = glmnet(x=x_t,y=y_t,family="multinomial",type.multinomial = "grouped",alpha=0)
predR = predict(fitr,t(ctest_data),type = "class")
resultR = as.matrix(predR[1:144,100])
checkfunc(as.vector(resultR),t(ctest_labels))#0.9861111
#Multinomial elastic net
library(glmnet)

fitel = glmnet(x=x_t,y=y_t,family="multinomial",type.multinomial = "grouped",alpha=.2)
predEL = predict(fitel,t(ctest_data),type = "class")
resultEL = as.matrix(predEL[1:144,100])
checkfunc(as.vector(resultEL),t(ctest_labels))#1
#Multinomial lasso
fitl = glmnet(x=x_t,y=y_t,family="multinomial",type.multinomial = "grouped",alpha=1)
predL = predict(fitl,t(ctest_data),type = "class")
resultL = as.matrix(predL[1:144,100])
checkfunc(as.vector(resultL),t(ctest_labels))
#Subprob-b
plot(fitr)
plot(fitel)
plot(fitl)
#Subprob-c
#Naive Bayes Classifier
require("e1071")
y_t = t(ctraining_labels)
x_t = t(ctraining_data)
fitB = naiveBayes(x = as.data.frame(x_t),y = as.factor(y_t))
predB_3 = predict(fitB, newdata = t(ctest_data),type = "class")
checkfunc(predB_3,t(ctest_labels))
#0.9375

#KNN
require("class")
predK_3 = knn(test=t(ctest_data),train=x_t,as.factor(y_t),k=2)
checkfunc(predK_3,t(ctest_labels))
#0.83333

#Linear SVM
require('e1071')
z_svm_3 = svm(y = as.factor(y_t),x = as.data.frame(x_t),cost = 100,gamma = 0.01)
z_prime_svm_3 = predict(z_svm_3,t(ctest_data))
checkfunc(z_prime_svm_3,t(ctest_labels))
#1