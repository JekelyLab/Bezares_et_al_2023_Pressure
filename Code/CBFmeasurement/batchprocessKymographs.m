
%Reading stimulus table
StimulusTableName = "StimCBF_WTCops.csv";
ISTablpath = "./Data/InputTables/";
FullIpath= strcat(ISTablpath, StimulusTableName);
StimTable = readtable( FullIpath, 'ReadVariableNames',true);

FOutdir = './Data/FreqRidges_CiliaBeating/';

%dir read
FIndir = './Data/Kymographs/CLAEHKymographs/CSV-CLAEH/Input/WT/todo/';
PatternFile = strcat(FIndir,'**/CLAEH*');
Files=dir(PatternFile);
ListFileNames = {Files.name};
ListFileDir = {Files.folder};

for m = 1:length(StimTable.Trial_ID)
    Index = find(~cellfun(@isempty,strfind(ListFileNames,StimTable.Trial_ID{m})));
    if(Index > 0)
        Cellsborders = StimTable.Cell_border_start(m);
        coordCells = str2double(split(split(Cellsborders,';'),'-'))
        %Loading file
        Image = csvread(strcat(ListFileDir{Index},"/",ListFileNames{Index}));
        NumSignals = 0;
        for j = 1:length(coordCells)
            BinnedI = binning(coordCells(j,1),coordCells(j,2),Image);
            NewFreq = WaveCalc(BinnedI,StimTable.Cell_border_start(m));
            NumSignals = NumSignals + width(BinnedI);
            if(j == 1)
                OldFreq = NewFreq;
            else
                OldFreq = OldFreq + NewFreq;
            end
        end
        FreqAvg = OldFreq/NumSignals;
        FreqAvg = transpose(FreqAvg);
        %writing freqs to file
        ToName = strsplit(ListFileNames{Index},'.csv');
        FullOutFilename = strcat(FOutdir,'Rfreq_',ToName{1},'.txt');
        csvwrite(FullOutFilename, FreqAvg);
        %plot(FreqAvg, 'green')
    else
        disp(strcat(StimTable.Trial_ID{m}," was not found"));
    end
end
        