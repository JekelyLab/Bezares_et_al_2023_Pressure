macro "VolumeCalc_NIT-AC"
{
	if(getBoolean("Choose an input and output directory otherwise give a text file containing a list of input and output directories"))
	{
		 inputDir = getDirectory("Choose the input directory (where the files are)");
		 NITDir = getDirectory("Choose the NITstack output directory (where the files should go)");
		 AcTubDir =getDirectory("Choose the Ac-Tub  output directory"); 
		 CprCStackDir=getDirectory("Choose the cPRC Stack output directory"); 
		 ResultDir=getDirectory("Choose the Result output directory"); 
		Volcal(inputDir,NITDir,AcTubDir);
	}
	else
	{
		fileList = File.openDialog("Open a text file containing a list of input and output directories");
		lines = split(File.openAsString(fileList), "\n");
		for(i = 0; i < lines.length; i++)
		{
			dirs = split(lines[i], " ");
			extractTracks(dirs[0], dirs[1]);
		}
	}
}

function Volcal(inputDir, outputNDir,outputATDir)
{

	// These three parameters can be adjusted
	// depending on video length and frame rate
	

	print (inputDir);
	print (outputNDir);
	print (outputATDir);

	// Do not show all the calculation steps on the
	// ImageJ user interface, this saves time and memory.


	// Get all the files in the input directory.
	list=getFileList(inputDir);
	Array.sort(list);
	print(list.length);

	// Track the number of files that have been read
	files = 0;
	
	for (k=0; k<list.length; k++)
	{
		print(list[k]);
		open(inputDir + list[k]);
		imageTitle = getTitle();
		
		imageTitleNIT = replace(imageTitle, ".czi", "_NIT");
		imageTitleAcT=replace(imageTitle, ".czi", "_AcT");
		
		run("Duplicate...", "title="+ imageTitleNIT +" duplicate channels=1"); //verify that this is the NIT channel.
		fullpathN_image= outputNDir+imageTitleNIT ;
		selectWindow(imageTitleNIT);
		saveAs("Tiff",fullpathN_image);
		
		selectWindow(imageTitle);
		run("Duplicate...", "title="+ imageTitleAcT +" duplicate channels=2");
		fullpathA_image= outputATDir+imageTitleAcT ;
		saveAs("Tiff",fullpathA_image);
		imageTitleNIT=imageTitleNIT+".tif";
		selectWindow(imageTitleNIT);
		run("Enhance Contrast", "saturated=0.35");
		setTool("rectangle");
		waitForUser("Select ROI that includes cilia");
		Roi.setName(imageTitleNIT); 
     	run("Add to Manager");
		waitForUser("Write down First and Last slice to analyze");
		StackRanges(imageTitleNIT,CprCStackDir,ResultDir);
		run("Close All");
	}
	
	roiManager("Deselect");
	roiManager("Save",ResultDir+"cPRCr_volumeROIs");
}


function StackRanges(NITst,StackDir,RDir)
{
	selectWindow(NITst);
	Stack.getDimensions(w, h, channels, slices, frames);

		FSlice=getNumber("First Slice", 1);
		LSlice=getNumber("Last Slice", slices);
		Substack= NITst + "_cPRCstck_"+"Fs-"+FSlice+"_Ls-"+LSlice;
		Substack=replace(Substack, ".tif", "");
		run("Duplicate...", "title=" + Substack +" duplicate range=" + FSlice + "-" + LSlice);
		selectWindow(Substack);
		fullpathS_image= StackDir+Substack ;
		saveAs("Tiff",fullpathS_image);
		run("Object Counter3D"); // Try to include only te cPRC cilia volume and nothing else. "threshold=47 slice=1 min=1000 max=2807574 new_results particles dot=3 font=12 summary");
		selectWindow("Results from " + Substack+".tif");
		fullpathR= RDir+Substack ;
		saveAs("Text",fullpathR);
		close("Results from " + Substack+".tif");
		selectWindow(Substack+".tif");
		run("Z Project...", "projection=[Max Intensity]");
		saveAs("Tiff",fullpathS_image+"MAXproj");
		
}