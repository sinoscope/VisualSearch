clear
load OD1_tROI2.mat
Score = cell2mat(OD1.score);
ScoreAll = sum(Score);
ScoreHC = ScoreAll(1:7);
ScorePD = ScoreAll(8:end);

ScoreAll_Expected = sum(Score([1:4,9:12],:));
ScoreAll_Unexpected = sum(Score([5:8,13:16],:));

IDs = [1:length(Score) 1:length(Score)]';
Group = [ones(7,1);2*ones(13,1);ones(7,1);2*ones(13,1)];
Prior=[ones(20,1);2*ones(20,1)];
Scores = [ScoreAll_Expected';ScoreAll_Unexpected'];

Data = [IDs Group Prior Scores];

save OD1Scores.mat Data