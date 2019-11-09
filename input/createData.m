

%% import csv not as table!
subjno=Data95(:,1);
X=Data95(:,[3 5 7 9 11 13]); % kwaad depre droov angst ontsp blij 
Y=Data95(:,[2 4 6 8 10 12]);

save('semiUsefull.mat','X','Y','subjno')

%%


close all
clear all
clc 

load('semiUsefull.mat');

for i =1:size(X,2)
    idx=find(X(:,i)<999);
    X=X(idx,:);
    Y=Y(idx,:);
    subjno=subjno(idx,:);
    
    idx=find(Y(:,i)<999);
    X=X(idx,:);
    Y=Y(idx,:);
    subjno=subjno(idx,:);
end


ui=unique(subjno);
for i =1:length(ui)
    idx=find(subjno==ui(i));
    L(i)=length(idx);
    if L(i)<40
        idxUse=find(subjno~=ui(i));
        X=X(idxUse,:);
        Y=Y(idxUse,:);
        subjno=subjno(idxUse,:);
    end
    
    
end



ui=unique(subjno);
for i =1:length(ui)
    idx=find(subjno==ui(i));
    L(i)=length(idx);      
end

min(L)

subjno2=subjno;
ui=unique(subjno);
add=1;
for i =1:length(ui)
    idx=find(subjno==ui(i));
    subjno2(idx)=add;
    add=add+1;
end

subjno=subjno2;


minAll=ones(1,size(X,2));
maxAll=ones(1,size(X,2))*100;

save('usefull.mat','X','Y','subjno','maxAll','minAll');
MM = load('usefull.mat')
csvwrite('MindMaastrichtEdit



    

