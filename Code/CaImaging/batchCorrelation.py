#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Dec  9 16:15:25 2022

@author: lab239
"""

'''
Correlation analysis of calcium-imaging movies
Gaspar Jekely, MPI for Developmental Biology, Tübingen Oct 2015
Modified by Csaba Veraszto, MPI for Developmental Biology, Tübingen Dec 2016
Modified by Luis Bezares,University of Exeter, Dec 2022
It is a prerequisite to have 4 folders, the video you are analyzing has to be a square (e.g. 512x512, 128x128), otherwise the script won't work. 

'''

import numpy as np
import matplotlib
matplotlib.use('Agg') # Force matplotlib to not use any Xwindows backend. Important if you run it without needing an X-server.
import matplotlib.pyplot as plt # Import pyplot afterwards the previous line, otherwise you are in trouble
import os, os.path
from os import listdir
from os.path import isfile, join
from PIL import Image
import pandas as pd #Pandas is a must
import seaborn as sns #Only for nice graphs
import scipy
from scipy import stats

df1 = pd.DataFrame() #define panda dataframes



#Set the directory
Dir='/ebio/ag-jekely/share/Luis/Writing/Pressure_paper/Pressure-sensation-in-zooplankton/Data/CaImagingDataAnalysis/CorrelationAnalysis/AllTogehter/' #directory with the working folders
#You can also set the 3 important variables by hand: number_of_cells= number of ROIs (and output images), number_of_cell=number of frames of your video, image_size=size of your square shaped video
videoDir = 'processed_videos/'
TextImDir = 'Text_images/'
IntFileDir = 'Intensity_tables/'
OutfileDir =  'output_images/'
for movie in listdir(str(Dir)+ videoDir):
    df2 = pd.DataFrame() #Also clears the dataframes from data of previous runs
    if( movie[0:1]!='.' ):
        moviename = movie
        print(moviename)
        image = Dir + videoDir + moviename
        im = Image.open(image)
        image_size=im.size[0]
        moviename = moviename.replace(".tif","")
        ListIntFiles = os.listdir(str(Dir)+ IntFileDir)
        indicesIntensity = [i for i, s in enumerate(ListIntFiles) if moviename in s]
        number_of_cells=len(indicesIntensity) #Counts how many ROIs you defined.
        ListTxtImgs = os.listdir(str(Dir)+ TextImDir)
        indicesTxImgs = [i for i, s in enumerate(ListTxtImgs) if moviename in s]
        number_of_files = len(indicesTxImgs) #Counts how many frames your video consisted.
        for k in range(0, number_of_files):  #loop through file list
           df = pd.read_csv(filepath_or_buffer=str(Dir) + TextImDir + ListTxtImgs[indicesTxImgs[k]], header=None, sep='\t')
           df1 = pd.DataFrame()
           df1= df[0]
           for x in range(1, image_size):
               df1=df1.append(df[x])
           df2=pd.concat([df2, df1], axis=1)    
        df2=df2.T #Transpose matrix

        #Import reference signal and create Text images and sns generated images with LUT plots. The for loop will generate 2 images (text + sns) for each ROI. 
        print("Writing image:")
        for m in range(0, number_of_cells):  #loop through file list
               reference = pd.read_csv(filepath_or_buffer=str(Dir)+ IntFileDir + ListIntFiles[indicesIntensity[m]], header=0, sep='\t')
               reference = reference.iloc[0:number_of_files,1] #truncate reference according to the number of files to analyze, also truncate first row "Mean"
               #Normalization by dividng with Max 
               Max= reference.max(axis=0)
               reference=reference.divide(Max)
               Pearson=pd.DataFrame() #Generates a 3rd dataframe, calculates Pearson correlation coeefficients.
               for n in range (0,image_size*image_size-1):
                       y=scipy.stats.pearsonr(df2.iloc[:,n], reference)
                       Pearson = np.append(Pearson,y[0])  #What is this step doing??
                #Printing the image and formatting by braking up the correlation list to a table.
               Pearson_image = np.zeros(image_size) 
               for l in range (0,image_size-1):
                   y=(Pearson[image_size*l:image_size*l+image_size:1]) #Makes it 2D and changes the values
                   if l<1:
                       Pearson_image=y
                   Pearson_image = np.column_stack((Pearson_image, y)) #Final image   
               np.savetxt(str(Dir)+ OutfileDir  + 'Pearson_image_' + ListIntFiles[indicesIntensity[m]], Pearson_image, delimiter=",") #Text image is saved, simply from the variable.
               plt.ioff()
               cmap = sns.diverging_palette(220, 10, as_cmap=True) #Creates the heatmap plot with 2 colors.
               ax = plt.subplots(figsize=(8, 7)) #Creates an image (size is 8 inch by 7 inch)
               sns.set_style("darkgrid", {'font.sans-serif': [u'Arial']})
               ax = sns.heatmap(Pearson_image, cmap=cmap, xticklabels=False, yticklabels=False, vmax=1, vmin=-1, square=True) #Creates the actual image. It makes sure it's a square image.
               plt.savefig(str(Dir)+ OutfileDir + 'Pearson_image_' + ListIntFiles[indicesIntensity[m]].replace(".txt","") + '.png', format='png', dpi=300, bbox_inches='tight', pad_inches=0, transparent=True)        
               plt.clf()
         
        print("Next Please!")

        

#imarray = numpy.array(im)

#Import all text images representing frames of the movie, and create a big dataframe

#Normalization by dividing with Max of each column is not necessary.
#df2Max= df2.max(axis=0)
#df2=df2.divide(df2Max)
#print "Done"







