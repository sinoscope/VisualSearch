clear
load OD1_tROI2.mat
deltaRTt1ROI = OD1.deltaRTt1ROI;
deltaRTt1ROIcol = reshape(deltaRTt1ROI,[16*20,1]);

IDnum = 1:20;
IDs = repelem(IDnum,16);
IDs = IDs';

GroupHC = repmat(ones(16,1),7,1);
GroupPD = repmat(2*ones(16,1),13,1);
Group = [GroupHC;GroupPD];

PriorNum = [ones(4,1);2*ones(4,1);ones(4,1);2*ones(4,1)];
Prior = repmat(PriorNum,20,1);

TrialNum = (1:16)';
Trial = repmat(TrialNum,20,1);

Data = [IDs Group Prior Trial deltaRTt1ROIcol];

save OD1deltaRTt1ROI.mat Data