clear
load OD1_Scores3.mat

respZone = [];

for trial = 1:16
    for subj = 1:20
        if ~isempty(OD1.respZone{trial,subj})
            respZone(trial,subj) = OD1.respZone{trial,subj}(1);
        else
            respZone(trial,subj) = NaN;
        end
    end 
end
% 
[A,B]=find(respZone==1);
Click1PriorHi = zeros(16,20);
Click1PriorMed = zeros(16,20);
Click1PriorLo = zeros(16,20);

Click1PriorHi(respZone==1)=1;
Click1PriorMed(respZone==2)=1;
Click1PriorLo(respZone==3)=1;

HC_Click1PriorHi = sum(Click1PriorHi(:,1:7));
PD_Click1PriorHi = sum(Click1PriorHi(:,8:end));

ranksum(HC_Click1PriorHi,PD_Click1PriorHi)
% 
%     click1CorAll = nansum(click1Cor);
%     click1CorHC = click1CorAll(1:7);
%     click1CorPD = click1CorAll(8:end);
% 
% click1CorAll_Expected = nansum(click1Cor([1:4,9:12],:));
% click1CorAll_Unexpected = nansum(click1Cor([5:8,13:16],:));
% 
% IDs = [1:length(click1Cor) 1:length(click1Cor)]';
% Group = [ones(7,1);2*ones(13,1);ones(7,1);2*ones(13,1)];
% Prior=[ones(20,1);2*ones(20,1)];
% click1Cors = [click1CorAll_Expected';click1CorAll_Unexpected'];
% 
% Data = [IDs Group Prior click1Cors];

