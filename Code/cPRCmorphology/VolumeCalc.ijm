
/////////////////////////////////////////////////////////
// This file is best viewed with a monospaced font.    //
/////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////
// ImageJ macro to threshold the cPRC cilia signal and     //
// measure the volume with the "Object Counter3D" plugin.  //
// images obtained from kymographs of the ciliary band.    //
// Author: Luis Alberto Bezares-Calderón                   // 
//												    	   //
/////////////////////////////////////////////////////////////

macro "VolumeCalc_cPRCcil-AC"
{
	if(getBoolean("Choose an input and output directory otherwise give a text file containing a list of input and output directories"))
	{
		 inputDir = getDirectory("Choose the input directory (where the files are)");
		 cPRCcilDir = getDirectory("Choose the cPRCcilstack output directory (where the files should go)");
		 AcTubDir =getDirectory("Choose the Ac-Tub  output directory"); 
		 CprCStackDir=getDirectory("Choose the cPRC Stack output directory"); 
		 ResultDir=getDirectory("Choose the Result output directory"); 
		Volcal(inputDir,cPRCcilDir,AcTubDir);
	}
}

function Volcal(inputDir, outputCilDir,outputATDir)
{

	print (inputDir);
	print (outputCilDir);
	print (outputATDir);

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
		
		imageTitlecPRCcil = replace(imageTitle, ".czi", "_cPRCcil");
		imageTitleAcT=replace(imageTitle, ".czi", "_AcT");
		
		run("Duplicate...", "title="+ imageTitlecPRCcil +" duplicate channels=1"); //verify that this is the cPRCcil channel.
		fullpathN_image= outputCilDir+imageTitlecPRCcil ;
		selectWindow(imageTitlecPRCcil);
		saveAs("Tiff",fullpathN_image);
		
		selectWindow(imageTitle);
		run("Duplicate...", "title="+ imageTitleAcT +" duplicate channels=2");
		fullpathA_image= outputATDir+imageTitleAcT ;
		saveAs("Tiff",fullpathA_image);
		imageTitlecPRCcil=imageTitlecPRCcil+".tif";
		selectWindow(imageTitlecPRCcil);
		run("Enhance Contrast", "saturated=0.35");
		setTool("rectangle");
		waitForUser("Select ROI that includes cilia");
		Roi.setName(imageTitlecPRCcil); 
     	run("Add to Manager");
		waitForUser("Write down First and Last slice to analyze");
		StackRanges(imageTitlecPRCcil,CprCStackDir,ResultDir);
		run("Close All");
	}
	
	roiManager("Deselect");
	roiManager("Save",ResultDir+"cPRCr_volumeROIs");
}


function StackRanges(cPRCcilst,StackDir,RDir)    // Select the slice range that include the volume to measure, threshold and measure the volume.
{
	selectWindow(cPRCcilst);
	Stack.getDimensions(w, h, channels, slices, frames);

		FSlice=getNumber("First Slice", 1);
		LSlice=getNumber("Last Slice", slices);
		Substack= cPRCcilst + "_cPRCstck_"+"Fs-"+FSlice+"_Ls-"+LSlice;
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