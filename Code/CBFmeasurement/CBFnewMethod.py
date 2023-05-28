#%%

import numpy as np
import pandas
import matplotlib.pyplot as plt
from scipy import fft
from statistics import mean
from os import listdir
from os.path import isfile, join




#%% def processft

# this does the FT of a single function, 'track', and returns the peak frequency in Hz
# meanf - enter a guess for the freq in Hz and it finds the closest peak of the FT to that guess - enter 0 for no bias
def processft(track,fps,sh,meanf = 0):
    ft = fft.fft(track)
    
    # remove peak at f=0 (which is always present)
    # may need to change value depending on the width of the f=0 peak
    ft[0:5] = 0

    freq = fft.fftfreq(len(track), 1)*fps
    
    #default: just take +ve frequency values
    if meanf == 0:
        cutoff = round(len(freq)/2)

    #otherwise: cut off at 1.5x meanf
    else:
        cutoff = round(1.5*np.argmin(abs(freq-meanf)))
        
    # principal frequency is highest peak with f<1.5*meanf
    f1 = freq[np.argmax(abs(ft[:cutoff]))]
    
    # if f1<0.7meanf, remove peak and recalculate
    while f1<=0.7*meanf:
        ft[np.argmax(abs(ft[:cutoff]))] = 0
        f1 = freq[np.argmax(abs(ft[:cutoff]))]
    
    #this calculates the phase of that frequency component
    phase_arr = np.angle(ft)
    fmax_phase = phase_arr[np.argmax(abs(ft[:cutoff]))]
    
    plotcut = min(cutoff*5, round(len(freq)/2))
    # plots and prints
    if sh == 1:
        plt.figure()
        plt.plot(freq[:plotcut], abs(ft[:plotcut]))
        plt.xlabel('frequency/Hz')
        plt.ylabel('|FFT|')
        plt.show()
        print('f1 = ', f1, 'Hz, ', 'T1 = ',1/(f1),'s')
    
    return (f1,fmax_phase)


#%%
def FPSfreqList(t_image,fps):

    
    s = np.shape(t_image)[1]
    
    freqsSec = []
    freqRecord = []
    for j in range(int(num_bins)):
        subim = t_image[fps*j:fps + fps*j, :]
        for i in range(s):
            track = subim[:,i]
            f, phi = processft(track,fps,0, 10)
            freqsSec.append(f)
        freqRecord.append(mean(freqsSec))
    return freqRecord

    
#%% read in text image

INdir = "/ebio/ag-jekely/share/Luis/Writing/Pressure_paper/publicRepo/Bezares_et_al_2023_Pressure/Data/Kymographs/"
Odir = "/ebio/ag-jekely/share/Luis/Writing/Pressure_paper/publicRepo/Bezares_et_al_2023_Pressure/Data/CBF_timecourse/" 
fps = 200
for file  in listdir(INdir):
    image = np.loadtxt(join(INdir,file))
    FreqTcourse = FPSfreqList(image,fps)
    kymo_size = np.shape(image)[0]
    num_bins = kymo_size/fps
    Binrange = list(range(1, int(num_bins) +1))
    df = pandas.DataFrame(FreqTcourse)
    df.columns = ["CBF"]
    df.to_csv(join(Odir,file))
        
    
    # num_bins = kymo_size/fps
    # plt.figure()
    # plt.scatter(range(int(num_bins)),FreqTcourse)

#%%

plt.figure()
plt.scatter(range(s),freqsSec)

#%%

diff = np.array(freqs) - np.array(freqs1)
plt.scatter(range(s),diff)

#%%

plt.figure()
plt.scatter(range(int(num_bins)),freqRecord)
