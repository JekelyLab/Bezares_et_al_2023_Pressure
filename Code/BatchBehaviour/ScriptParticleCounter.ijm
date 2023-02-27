/////////////////////////////////////////////////////////
// Author:L.A.Bezares-Calderón. 2022
//ImageJ macro to calculate distribution of larvae in chamber across a time-series.     //
/////////////////////////////////////////////////////////


macro "Input-Output dir"
{
	if(getBoolean("Choose an input and output directory otherwise give a text file containing a list of input and output directories"))
	{
		inputDir = getDirectory("Choose the input directory (where the tif projection files are)");
		outputDir = getDirectory("Choose the output directory (where the files should go)");

		ParticleProcessor(inputDir, outputDir);
	}
	else
	{
		fileList = File.openDialog("Open a text file containing a list of input and output directories");
		lines = split(File.openAsString(fileList), "\n");
		for(i = 0; i < lines.length; i++)
		{
			dirs = split(lines[i], " ");
			ParticleProcessor(dirs[0], dirs[1]);
		}
	}
}
function ParticleProcessor(inputDir, outputDir)
{

	// These three parameters can be adjusted
	// depending on video length and frame rate
	

	print (inputDir);
	print (outputDir);

	// Do not show all the calculation steps on the
	// ImageJ user interface, this saves time and memory.

	
	// Get all the files in the input directory.
	list=getFileList(inputDir);
	Array.sort(list);
	print(list.length);

	files = 0;
	NumBins=5;
	Dialog.create("Bin number");
	Dialog.addString("Title:",NumBins);
	Dialog.show();
	NumBins=Dialog.getString();
	setTool("rectangle");

    
	for (k=0; k<list.length; k++)
	{
		setBatchMode(true);
		print(list[k]);
		open(inputDir + list[k]);
		imageTitle = getTitle();
		imageTitle = replace(imageTitle, " .", "_"); // Replace spaces by underscores to avoid problems with file writing
		imageTitle = replace(imageTitle, ".tif", "");
		print(imageTitle);
		if (k ==0){
			setBatchMode(false);
			run("Select All");
			waitForUser("Select Width of rectangle ROI to analyse (do not alter height:");
			Dialog.create("Bin number");
			Dialog.addString("Title:","WindowROI");
			Dialog.show();
			ROIname=Dialog.getString();;
			Roi.setName(ROIname); 
			roiManager("Add");
		}
		index=findRoiWithName(ROIname);
		roiManager("select",index);
		nameWind="WindowROI-"+imageTitle;
		run("Duplicate...", "title="+nameWind);
		Height=getHeight() ; 
		Width=getWidth() ; 
		Offset=Height%NumBins;
		threshold();
		countparticle(NumBins,Height,Width,Offset,nameWind);
		selectWindow("Summary");
		Summaryname="SummaryParticleCount-"+imageTitle+".csv";
		saveAs("Results",outputDir+Summaryname);
		close(Summaryname);
		run("Close All");
	}
}	
	
//Threshold image
function threshold()
{
	setAutoThreshold();
	getThreshold(lower, upper);
	setThreshold(lower, upper);
	run("Convert to Mask", " ");
}


//Choose ROI to analyse.
//Divide image in X number of equally sized ROIs.
function countparticle(NB,H,W,OS,name)
{
	BinH=H/NB;
	BinH=floor(BinH);
	print(BinH);
	
	PosY=0;

	while (PosY < (H-OS)){
		selectWindow(name);
		run("Specify...", "width="+W+ " height="+BinH+" x=0 y="+PosY+OS);
		//Count particles for each second for each ROI.
		run("Analyze Particles...", "size=30-1000 show=Outlines exclude clear summarize");
		PosY=PosY+BinH;
		print(PosY);
	}	
}

/* Returns index of first ROI that matches  
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
//Save results
