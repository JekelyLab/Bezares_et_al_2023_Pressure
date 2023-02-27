//Description
//Author: Luis Bezares Calderon, written in May 2017 
//Purpose:Recording intensity profiles of selected ROIs across a list of files. This macro is the same as that called Measure-intensityvaluechanges3.ijm
//Input Data: Registered time-lapse recordings with a green (GCaMP) and red (tdTomato) channel. In this case 
//Important: the macro assumes the videos are already corrected for X-Y shifts. This is done using the Descriptor-based Series registration plugin. Before registering the videos, crop only the frames to be analyzed and that do not include big XY shifts. Use the DIC channel for registration and then apply the calculations to the GCaMP and TdTomato channels.
//Publication:Bezares-Calderon et al, 2018 

macro "MeasureIntensityprofiles"

//Obtaining directory addresses.
{
	if((getBoolean("Choose input and output directories if not select a list of input and output directories")))
	{
	
        inputGCDir= getDirectory("Choose the GC file  directory (where the files are)");
	    G_Ddir = getDirectory("Choose destination directory tables");
	    GJet_Ddir = getDirectory("Choose destination directory JETLUT videos");
	}

 
	// Get all the files in the input directory.
	list=getFileList(inputGCDir);
	Array.sort(list);
	print(list.length);

//This 'for' will apply  the LUT described in Guehmann et al,2015
	 for (k=0; k<list.length; k++)
	{
			print(list[k]); 
			fullpathGC_image=inputGCDir + list[k];
			 open(fullpathGC_image);
			img_title = getTitle();
			img_title = replace(img_title, " ", "_"); // Replace spaces by underscores to avoid problems with file writing
	        print(img_title);

	  
			 r=newArray(256);g=newArray(256);b=newArray(256); 
			for (i=0;i<256;i++) { 
				i4=4*i/256; 
				r[i]=255*minOf(maxOf(minOf(i4-1.5,-i4+4.5),0),1); 
				g[i]=255*minOf(maxOf(minOf(i4-0.5,-i4+3.5),0),1); 
				b[i]=255*minOf(maxOf(minOf(i4+0.5,-i4+2.5),0),1); 
			} 
			setLut(r,g,b); 
			fullpathGC_image=GJet_Ddir + list[k];
			 saveAs("Tiff",fullpathGC_image +"_JETlut");
             rename("videoGC");
              for(i=0;i<nSlices;i++) {
               run("Next Slice [>]");
               wait(10);
             }
			
//This part of the code has the purpose of creating the ROIs of the cells to be measured. Use the reference tdTomato signal for drawing the ROIs.	 
	 do{
	   selectWindow("videoGC");
		setTool("polygon");
		waitForUser("Set ellipse in region for measuring intensity");
		Cell="untitled";
		Dialog.create("CellOfInterest");
		Dialog.addString("Title:",Cell );
		Dialog.show();
		Cell=Dialog.getString();
		img_title=replace(img_title,".tif","");
		print(img_title);
		ROIname=img_title+"_"+Cell;
	    print(ROIname);
	      Roi.setName(ROIname); 
	      roiManager("Add");
		 selectWindow("videoGC");
		 run("Clear Results");
		 run("Plot Z-axis Profile");  //This will create the intensity values in the ROI across the whole video.
		//Plot.showValues();
		selectWindow("Plot Values");
		saveAs("Results",G_Ddir+img_title+"_"+Cell+".txt");
		run("Clear Results");
	selectWindow("videoGC");
	 for(i=0;i<nSlices;i++) {
               run("Next Slice [>]");
               wait(10);
             }
	 Dialog.create("More ROIs");
		Dialog.addCheckbox("Check box if more rois in the present video", false);
		Dialog.show();
     	Satis = Dialog.getCheckbox();
     	print(Satis);
	 }while(Satis);
	 run("Close All");
	}
}

/* 
 * Returns index of first ROI that matches  
 * the given regular expression  written by oburri http://forum.imagej.net/t/selecting-roi-based-on-name/3809/2
*/

