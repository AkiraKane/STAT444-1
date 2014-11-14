X = csvread('~/Desktop/X_SRBCT.csv');
Y = csvread('~/Desktop/Y_SRBCT.csv');
beta = mldivide(X,Y);
alpha = Y - X * beta;
RSS = transp(alpha) * alpha


