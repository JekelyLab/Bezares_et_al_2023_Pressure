
/////////////////////////////////////////////////////////
// This file is best viewed with a monospaced font.    //
/////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////
// ImageJ macro to extract the max.R values from FFT   //
// images obtained from kymographs of the ciliary band.//
// Author: Luis Alberto Bezares-Calderón               // 
//													   //
/////////////////////////////////////////////////////////

macro "CBF_per_second"
{
	if(getBoolean("Choose an input and output directory otherwise give a text file containing a list of input and output directories"))
	{
		 inputDir = getDirectory("Choose the input directory (where the files are)");
		 outputDir = getDirectory("Choose the Kymograph output directory (where the files should go)");
		 measureDir =getDirectory("Choose the CBF measurements output directory"); 
		CBFsec(inputDir, outputDir);
	}
}

function CBFsec(inputDir, outputDir)
{

	print (inputDir);
	print (outputDir);

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
		imageTitle = replace(imageTitle, " ", "_"); // Replace spaces by underscores to avoid problems with file writing
		print(imageTitle);
		FPS =200 ;  //  this parameter has to be manually adjusted according to the recording rate.
		run("8-bit");
		rename("video");
		selectWindow("video");
		waitForUser("Write down intervals video to analyze");
		KimoRanges(imageTitle,FPS);
		close();
	}
	selectWindow("video");
	close();

}

function KimoRanges(Imge,FPS) // This function selects the range to analyse.
{
	selectWindow("video");
	Stack.getDimensions(w, h, channels, slices, frames);
	print(slices);
	LSlice=0;
	do{
		selectWindow("video");
		FSlice=LSlice;
		FSlice=getNumber("First Slice", LSlice +1);
		do{
			LSlice=getNumber("Last Slice", slices);
			check=(LSlice)%FPS;
			print(check);
		}while(check != 0);
		run("Duplicate...", "title=subvideo duplicate range=" + FSlice + "-" + LSlice);
		Dialog.create("Stabilization");
		Dialog.addCheckbox("Check box if you wish to run stabilization of stack", false);
		Dialog.show();
     	Stabil = Dialog.getCheckbox();
		if(Stabil){
			stabilize();
		}
		selectWindow("subvideo");
		KimoN=qualityKimo(FSlice,LSlice);
		FFTfunction(Imge,KimoN,FSlice,LSlice,FPS);
		close("subvideo");
		print(slices);
		print(LSlice);
	}while(LSlice < slices);
}

function stabilize() // This function runs the plugin "image stablizer"
{
	selectWindow("subvideo");
	run("Image Stabilizer", "transformation=Affine maximum_pyramid_levels=1 template_update_coefficient=0.90 maximum_iterations=200 error_tolerance=0.0000001");
}

function qualityKimo(First,Last)  // This function is called to define the kymograph line and to let user adjust its quality.
{
	do{
		selectWindow("subvideo");
		setTool("polyline");
		run("Set Scale...", "distance=0 known=0 pixel=1 unit=pixel");
		waitForUser("Select line for Kymopgraph");
		run("Multi Kymograph", "linewidth=1");
		KymoName="Kymo"+First+"-"+Last;
		selectWindow("Kymograph");
		rename(KymoName);
		waitForUser("Check quality of Kymograph (ensure lines are oriented towards left)");
		Dialog.create("Kymograph Quality");
		Dialog.addCheckbox("Uncheck box if satisfied with Kymograph", true);
		Dialog.show();
     	Satis = Dialog.getCheckbox();
     		if (Satis){
     			selectWindow(KymoName);
     			run("Close");
     		}
	}while(Satis);
	selectWindow("subvideo");
	run("Add to Manager");
	count = roiManager("count");
	roiManager("Select", count-1);
	roiManager("Save",outputDir + "KymoLine_"+ imageTitle+"_"+First +"-"+Last+".roi");
	selectWindow(KymoName);
	run("Duplicate...", "title=Kymo");
	KymRaw=outputDir+"KymoRaw_"+imageTitle+"_"+First +"-"+Last ;
	selectWindow(KymoName);
	saveAs("Tiff",KymRaw);
	close();
	return KymoName;
}

function FFTfunction(ImageName,KymName,First,Last,FPS)  // Calculation of the FFT images from the kymographs. Manually adjust prominence value.
{
	setBatchMode(true);
	selectWindow("Kymo");
	width = getWidth;
  	height = getHeight;
  	prominence=18; // The lower the prominence value the more permissive the search for maxima is. A compromise has to be chosen between sensitvity and specificity.
	setTool("rectangle");
	y=0;
	while (y < height){
		selectWindow("Kymo");
		run("Specify...", "width="+width+ " height="+FPS+" x=0 y="+y);
		run("Duplicate...", "title=SubKymo-"+y);
		if(y>0){
			count = roiManager("count");
			roiManager("Select", count-1);
			}
			else{
				run("Select All");
			}
			selectWindow("SubKymo-"+y);
			run("FFT");
				selectWindow("FFT of SubKymo-"+y);
				print("prominence for " +y+ " is "+prominence);
				run("Find Maxima...", "prominence="+prominence+" exclude output=[Point Selection]");   //Take the R value in the first quadrant (ideally with a Theta of 90° (given that the frequency pattern is aligned vertically)=. If no value appears there you have to adjust the prominence value, usaully you need to reduce it to find local maxima. The points further away from the orgiing represent higher frequencies and they could also be noise.
     	selectWindow("SubKymo-"+y);
     	run("Add to Manager");
     	selectWindow("FFT of SubKymo-"+y);
		run("Measure"); //use following formula:  Freq=1/(R*timeperframe(in sec))
		ResT=measureDir+ImageName+"_Kymo-"+First+"-"+Last+"_-"+y;
		ResT=replace(ResT, ".avi_", "_");
		selectWindow("Results");
		saveAs("Text",ResT);
		selectWindow("Results");
		run("Close");
		selectWindow("FFT of SubKymo-"+y);
		run("Close");
		selectWindow("SubKymo-"+y);
		run("Close");
		y=y+FPS;
	}
	selectWindow("Kymo");
	run("Close");
	setBatchMode(false);
}
