clear
load OD1_tROI2.mat
RT = cell2mat(OD1.RT);
RT_exp = RT([1:4,9:12],:);
RT_unexp = RT([5:8,13:16],:);
meanRT_exp = nanmean(RT_exp);
meanRT_unexp = nanmean(RT_unexp);
meanRT = [meanRT_exp;meanRT_unexp];

meanRTcol = reshape(meanRT,[2*20,1]);

IDnum = 1:20;
IDs = repelem(IDnum,2);
IDs = IDs';

GroupHC = ones(14,1);
GroupPD = 2*ones(26,1);
Group = [GroupHC;GroupPD];

PriorNum = [1,2]';
Prior = repmat(PriorNum,20,1);

Data = [IDs Group Prior meanRTcol];

save OD1meanRT.mat Data