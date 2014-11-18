%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%ICA and NMF
%lecture 17
tdata = dlmread('digits_X1.csv');
clabs = textread('digits_Y1.csv','%s');

labs=str2num(char(clabs));
j=1;

for i=1:size(labs)
if labs(i)==1
    data(j,:)=tdata(i,:);
    j=j+1;
end
end%plotting some images

colormap('gray')
for i=1:12
    subplot(3,10,i)
    mat = flipud(rot90(reshape(data(i,:),16,16)));
    imagesc(mat)
    axis off
end
%nmf
%addpath('~/matlab/nmf_toolbox_ver1.4')
%[W,H] = nmf(zscore(data),12,'als',1e3,1);
%colormap('gray')
%for i=1:8
    %subplot(3,8,i)
    %mat = flipud(rot90(reshape(H(i,:),16,16)));
    %imagesc(mat)
    %axis off
%end


 %ica
 [S,A,Wh] = fastica(zscore(data),'lastEig',8,'g','tanh','maxNumIterations',1e6);
 colormap('gray')
 for i=1:8
     subplot(3,8,i+8)
     mat = flipud(rot90(reshape(S(i,:),16,16)));
     imagesc(mat)
     axis off
 end

% 
% %pca
 [U,D,V] = svds(zscore(data),8);
 colormap('gray')
 for i=1:8
     subplot(3,8,i+16)
     mat = flipud(rot90(reshape(V(:,i),16,16)));
     imagesc(mat)
     axis off
 end
% 
% 
 d = diag(D); d = d(1:8);
 varex = (d.^2)./sum(d.^2);
 subplot(1,3,1)
 bar(varex)
 xlabel('Component')
 ylabel('% Variance Explained')
 axis tight
 
 cumvarex = 0;
 for i=1:8
     cumvarex(i) = sum(varex(1:i));
 end
 subplot(1,3,2)
 plot(cumvarex)
 axis([1 8 0 1])
 xlabel('Component')
 ylabel('Cumulative % Variance Explained')
 
 subplot(1,3,3)
 plot(1 - cumvarex)
 axis([1 8 0 1])
 xlabel('Component');
 ylabel('1 - Cumulative % Variance Explained')
