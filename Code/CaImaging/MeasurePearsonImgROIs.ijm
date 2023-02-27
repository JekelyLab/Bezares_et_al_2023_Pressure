//export ROIs and text images for correlation analysis in python
//Luis Alberto Bezares Calderón
//Dec 2022



macro "measure corr.val in specified ROIs"
 
{
	run("Set Measurements...", "  mean display redirect=None decimal=3");
	if((getBoolean("Choose input and output directories if not select a list of input and output directories")))
	{
         inputPearsonImgDir = getDirectory("Choose the input text image directory");
         fileROIs=File.openDialog("Choose the .zip ROI file");
         roiManager("Open",fileROIs);
         OutTableDir=getDirectory("Choose the file directory to store the tables");
        
         
         MeasCorr(inputPearsonImgDir,OutTableDir);
	}
	
}

function MeasCorr(IDir,OTDir)
{
	list=getFileList(IDir);
	Array.sort(list);
	print(list.length);

	file=File.open(OTDir + "OutCorrMean.csv");
	
	print(file, "File_name,CellROI_name,Mean_corr\n");
	for (k=0; k<list.length; k++)
	{
			print(list[k]); 
			fullpathInput_image=IDir + list[k];
			run("Text Image... ", "open=" + fullpathInput_image);
			img_title = getTitle();
			Vals=SelectROI(img_title);
			Corr= measure_intensityTXim(img_title,Vals[0]);
			print(file, img_title + "," +  Vals[1]  + "," + Corr +"\n");
			close();
	}
	File.close(file);
			
}

function measure_intensityTXim(stack,idx) { 
// measure mean px values in ROI.
	selectWindow(stack);
	roiManager("select",idx);
	rName = Roi.getName();
	Stack.setChannel(1);
	run("Measure");
	meanCorr =getResult("Mean",nResults-1);
	return meanCorr;
}


function SelectROI(TXimage){
	Cell="untitled";
	NameImage = replace(TXimage, "Pearson_image_", ""); 
	NameImage = replace(NameImage, ".txt", ""); 
	do {
		Dialog.create("CellOfInterest");
		Dialog.addString("Title:",Cell );
		Dialog.show();
		Cell=Dialog.getString();
		Dialog.create("Search cell to calculate MeanCorrCoef.");
		ROIname=NameImage+"_"+Cell;
		print(ROIname);
		selectWindow(TXimage);	    
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
			selectWindow(TXimage);
			setTool("polygon");
			waitForUser("Set ellipse in region for measuring intensity");
		    Roi.setName(ROIname); 
		    roiManager("Add");
			run("Clear Results");
		}
		index=findRoiWithName(ROIname);
		print(index);
	}while(index < 0);  //Only able to exit if ROI is found or if a new ROIis drawn.
	return newArray(index,Cell);
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