clear all;
clc
close all;

%%Before you run this script, you have to open the Data95.csv file in matlab
%%After opening the file, click on "import numeric matrix" and then on "import selection". 
%%In the workspace you now see Data95. Click on the data and save as Data95.mat.
%%Now you can run this matlab script.
load('Data95.mat')  


idx=(Data95==9999); %find nans and replcae them with NAN 
Data95(idx)=NaN;



indiv=Data95(:,1);
dataY=Data95(:,[2 4 6 8 10 12]);
dataX=Data95(:,[3 5 7 9 11 13]);

%% group center
indivindiv=unique(indiv)

for i =1:length(indivindiv)
    idxTemp=find(indiv==indivindiv(i));
    nanmean(dataX(idxTemp,:))
    dataX(idxTemp,:)= dataX(idxTemp,:)-repmat(nanmean(dataX(idxTemp,:)),length(idxTemp),1);
    nanmean(dataX(idxTemp,:))
end

%%

dataX=[ones(size(dataX,1),1), dataX]; %add intercept
    
%% fit all models and save them (takes a while)

X=dataX;
Z=dataX;

%%With this code you save your results
    for iy=1:size(dataY,2)
        iy
        y=dataY(:,iy);
        lme = fitlmematrix(X,y,Z,indiv,'FitMethod','REML');
       
        
        strName=['Model' num2str(iy) '.mat']
        save(strName,'lme');
        pvalues=lme.Coefficients.pValue
        fixed=lme.Coefficients.Estimate
        random=reshape(lme.randomEffects, 7,95)'+repmat(lme.Coefficients.Estimate',95,1);
        realerror=sqrt(lme.MSE);
        Data_to_txt(['Modelfixed' num2str(iy) '.txt'],fixed)
        Data_to_txt(['Modelpvalues' num2str(iy) '.txt'],pvalues)
        Data_to_txt(['Modelrandom' num2str(iy) '.txt'],random)
        ;
    end
    
  