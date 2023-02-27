//export ROIs and text images for correlation analysis in python
//Luis Alberto Bezares Calderón
//Dec 2022



macro "export ROIs and text images for correlation analysis in python"
 
{
	run("Set Measurements...", "  mean redirect=None decimal=3");
	if((getBoolean("Choose input and output directories if not select a list of input and output directories")))
	{
         inputCZIDir = getDirectory("Choose the input video directory (where the videos are)");
         OutIntensityDir=getDirectory("Choose the file directory to store the intensity values tables");
         OutFormatVideos=getDirectory("Choose the file directory to store the cropped videos");
        OutTextImDir=getDirectory("Choose the file directory to store the text image files");	
         
         formatCorr(inputCZIDir,OutIntensityDir,OutFormatVideos,OutTextImDir);
	}
}

function formatCorr(IDir,OTDir,OVid,OXDir)
{
// Get all the files in the input directory.
	list=getFileList(IDir);
	Array.sort(list);
	print(list.length);


	 for (k=0; k<list.length; k++)
	{
			print(list[k]); 
			fullpathInput_image=IDir + list[k];
			open(fullpathInput_image);
			img_title = getTitle();
			img_title = replace(img_title, ".czi", ""); 
			img_title = replace(img_title, ".tif", "");
			rename(img_title);
			print(img_title);
			SquareVideo(img_title);
			Dialog.create("Save Text Images?");
			Dialog.addCheckbox("Check box if you wish to save Text Images", true);
			Dialog.show();
		    TImage = Dialog.getCheckbox();
			if(TImage){
				TextImageStack(img_title,OXDir);
			}
			selectWindow(img_title);
			run("Duplicate...",  "title="+img_title + " duplicate channels=1");
			FullVideoGC=OVid + img_title;
			saveAs("Tiff", FullVideoGC);
			selectWindow(img_title  + ".tif");
			close();
			do{
				ind=SelectROI(img_title);
	 			measure_intensity(img_title,ind,OTDir);
				Dialog.create("More ROIs");
				Dialog.addCheckbox("Check box if you want to measure more ROIs", false);
				Dialog.show();
 				Satis = Dialog.getCheckbox();
 				print(Satis);
			}while(Satis);
			run("Close All");
			
	}	
	//IJ.deleteRows(0, 1000000);

}



function TextImageStack(stack,ODir)
{	
	
	selectWindow(stack);
	Stack.getDimensions(w, h, channels, slices, frames);
	Stack.setChannel(1);  //make sure channel 1 is the Ca2+ indicator channel.
	setSlice(1);
		for (j=0; j<frames; j++) 
		{
			run("Select All");
			dest_filename= stack + "_stack_"+j;
			fullpath_results = ODir + dest_filename;
			saveAs("Text image", fullpath_results);
			if(channels > 1){
				setKeyDown("alt");
			}
			run("Next Slice [>]");
		}
}



function SquareVideo(stack) {
	selectWindow(stack);
	Stack.getDimensions(w, h, channels, slices, frames);
    if(w != h ) { //The video needs to have same w and h to be processed by correlation analysis pipeline,
		WHarray = newArray(w,h);
		Array.getStatistics(WHarray, min, max, mean, stdDev);
		setTool("rectangle");
		run("Specify...", "width=" + min + " height=" + min + " x=0 y=0 slice=1");
		Dialog.create("Square the frame size");
		waitForUser("positiion the square where you want to crop the video");
		run("Crop");
    }
    setSlice(frames);
	run("Duplicate...", "use");
	//run("Brightness/Contrast...");
	run("Enhance Contrast", "saturated=0.35");
    Dialog.create("Erase incomplete frames");
	Dialog.addCheckbox("erase last frame if it is incomplete?", true);
	Dialog.show();
	Erase = Dialog.getCheckbox();
	if(Erase){
		selectWindow(stack);
		setSlice(frames);
		run("Delete Slice");
	}
}

function SelectROI(stack){
	Cell="untitled";
	do {
		Dialog.create("CellOfInterest");
		Dialog.addString("Title:",Cell );
		Dialog.show();
		Cell=Dialog.getString();
		Dialog.create("Draw or search");
		ROIname=stack+"_"+Cell;
		print(ROIname);
		selectWindow(stack);	    
	    Stack.getDimensions(w, h, channels, slices, frames);
	    if(channels > 1){
	    	Stack.setChannel(1);  //make sure channel 1 is the Ca2+ indicator channel.
	    	Property.set("CompositeProjection", "null");  
			Stack.setDisplayMode("grayscale");
	    }
	    Dialog.addCheckbox("Check box if you wish to draw a new ROI, leave unchecked if you want to search for existing ROI", true);
		Dialog.show();
		Draw = Dialog.getCheckbox();
		if(Draw){
			selectWindow(stack);
			setTool("polygon");
			waitForUser("Set ellipse in region for measuring intensity");
		    Roi.setName(ROIname); 
		    roiManager("Add");
			run("Clear Results");
		}
		index=findRoiWithName(ROIname);
		print(index);
	}while(index < 0);  //Only able to exit if ROI is found or if a new ROIis drawn.
	return index;
}

function measure_intensity(stack,idx,OD) { 
// measure mean px values in ROI.
	selectWindow(stack);
	roiManager("select",idx);
	rName = Roi.getName();
	run("Clear Results");
	Stack.setChannel(1);
	setOption("InterpolateLines", true);
	run("Plots...", "width=600 height=340 font=14 draw_ticks list minimum=0 maximum=0 interpolate");
	run("Plot Z-axis Profile");  //This will create the intensity values in the ROI across the whole video.
	selectWindow("Plot Values");
	saveAs("Results",OD + rName +".txt");
	run("Clear Results");
}


/* 
 * Returns index of first ROI that matches  
 * the given regular expression  written by oburri http://forum.imagej.net/t/selecting-roi-based-on-name/3809/2
 */ 
function findRoiWithName(roiName) { 
	nR = roiManager("Count"); 
 
	for (i=0; i<nR; i++) { 
		roiManager("Select", i); 
		rName = Roi.getName(); 
		if (matches(rName, roiName)) { 
			return i; 
		} 
	} 
	return -1; 
} 