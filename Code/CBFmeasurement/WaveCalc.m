function ifreqOld = WaveCalc(signals) 

%defining parameters for MODA.
interval1= 5;
interval2 = 22;
fs = 200;
w_type = 'Lognorm';
preprocess = 'on';
f0 = 1;

    for i = 1:width(signals)  % loop over the signal to calculate frequencies and extract the ridge.
            [WT,freqarr,wopt]=wt(signals(:,i),fs,'fmin',interval1,'fmax',interval2,'CutEdges','on',...
                    'Preprocess',preprocess,'Wavelet',w_type,'f0',f0);
            tfsupp = ecurve(WT,freqarr,wopt);
            [iamp,iphi,ifreq] = rectfr(tfsupp,WT,freqarr,wopt,'ridge');
            if (i == 1)
                ifreqOld = ifreq;
            else 
                ifreqOld = ifreq + ifreqOld;
            end
    end
end