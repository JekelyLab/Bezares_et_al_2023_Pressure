
%Reading stimulus table
StimulusTableName = "StimCBF_WTCops.csv";
ISTablpath = "/ebio/ag-jekely/share/Luis/Writing/Pressure_paper/publicRepo/Bezares_et_al_2023_Pressure/Data/InputTables/";
FullIpath= strcat(ISTablpath, StimulusTableName);
StimTable = readtable( FullIpath, 'ReadVariableNames',true);

%dir read
FIndir = '/ebio/ag-jekely/share/Luis/Writing/Pressure_paper/publicRepo/Bezares_et_al_2023_Pressure/Data/Kymographs/CLEHKymographs/CSV-CLAEH/todo/';
PatternFile = strcat(FIndir,'CLAEH*');
Files=dir(PatternFile);
ListFileNames = {Files.name}

for m = 1:length(StimTable.Trial_ID)
    Index = find(~cellfun(@isempty,strfind(ListFileNames,StimTable.Trial_ID{m})));
    if(Index > 0)
        Cellsborders = StimTable.Cell_border_start(m);
        coordCells = str2double(split(split(Cellsborders,';'),'-'))
        %Loading file
        Image = csvread(strcat(FIndir,ListFileNames{Index}));
        NumSignals = 0;
        for j = 1:length(coordCells)
            BinnedI = binning(coordCells(j,1),coordCells(j,2),Image);
            NewFreq = WaveCalc(BinnedI);
            NumSignals = NumSignals + width(BinnedI);
            if(j == 1)
                OldFreq = NewFreq;
            else
                OldFreq = OldFreq + NewFreq;
            end
        end
        FreqAvg = OldFreq/NumSignals;
        plot(FreqAvg, 'green')
    else
        disp(strcat(StimTable.Trial_ID{m}," was not found"));
    end
end
        



for j = 1:length(coordCells)  %loop over the cells to analyse as defined by beginning an end in the X dimension.
    A = Image(:,coordCells(j,1):coordCells(j,2)); %subsetting the column to analyse.
    B = mat2cell(A,repmat(blocksize(1),[1 size(A,1)/blocksize(1)]),...
        repmat(blocksize(2),[1 size(A,2)/blocksize(2)]));  %binning the kymograph by 2, only in the X dimension.
    B = cellfun(@(x) min(x,[],'all'),B);
    % filename2 = "KymoRaw_190521_M1_L1_E002_minbin.csv";
    % csvwrite(filename2,B);
    % signal = csvread(filename2);

    for i = 1:width(B)  % loop over the signal to calculate frequencies and extract the ridge.
        [WT,freqarr,wopt]=wt(B(:,i),fs,'fmin',interval1,'fmax',interval2,'CutEdges','on',...
                'Preprocess',preprocess,'Wavelet',w_type,'f0',f0);
        tfsupp = ecurve(WT,freqarr,wopt);
        if (i == 1) && (j == 1)
            tfsuppOld = tfsupp;
        else 
            tfsuppOld = tfsupp + tfsuppOld;
        end
        totalSignals = totalSignals + 1;
        strcat(num2str(j),'\n',num2str(i));
    end
end
 tfsuppAvg = tfsuppOld/totalSignals;
 %tfsuppAvg = tfsuppOld/19;
 %plot(tfsuppAvg(1,:),'red')
 [iamp,iphi,ifreq] = rectfr(tfsuppAvg,WT,freqarr,wopt,'ridge');
 plot(ifreq, 'red')