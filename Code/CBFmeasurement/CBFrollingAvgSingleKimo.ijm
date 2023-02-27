macro "CBF_per_second"
{
	if(getBoolean("Choose an input and output directory otherwise give a text file containing a list of input and output directories"))
	{
		 inputDir = getDirectory("Choose the input directory (where the files are)");
		 outputDir = getDirectory("Choose the output directory (where the files should go)");
		 measureDir =getDirectory("Choose the measure directory"); 
		CBFsec(inputDir, outputDir);
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

function CBFsec(inputDir, outputDir)
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

	// Track the number of files that have been read
	files = 0;
	
	for (k=0; k<list.length; k++)
	{
		print(list[k]);
		open(inputDir + list[k]);
		imageTitle = getTitle();
		imageTitle = replace(imageTitle, " ", "_"); // Replace spaces by underscores to avoid problems with file writing
		print(imageTitle);
		FPS =200 ;  //  with 10fps (noise reduction)
		run("8-bit");
		rename("video");
		selectWindow("video");
		qualityKimo();
		// Cut the video into smaller parts and give each part an index.
		// Start with 100 to aviod problems with file sorting
		FFTfunction();
		//selectWindow("video");
		//run("Close");
		close();
	}
	selectWindow("video");
	close();

}

function qualityKimo()
{
	do{
		setTool("polyline");
		run("Set Scale...", "distance=0 known=0 pixel=1 unit=pixel");
		waitForUser("Select line for Kymopgraph");
		run("Multi Kymograph", "linewidth=1");
		waitForUser("Check quality of Kymograph");
		Dialog.create("Kymograph Quality");
		Dialog.addCheckbox("Uncheck box if satisfied with Kymograph", true);
		Dialog.show();
     		Satis = Dialog.getCheckbox();
     		if (Satis){
     			selectWindow("Kymograph");
     			run("Close");
     		}
	}while(Satis);
	selectWindow("video");
	run("Add to Manager");
	count = roiManager("count");
	roiManager("Select", count-1);
	roiManager("Save",outputDir + "KymoLine_"+ imageTitle+".roi");
	selectWindow("Kymograph");
	run("Duplicate...", "title=Kymo");
	KymRaw=outputDir+"KymoRaw_"+imageTitle;
	selectWindow("Kymograph");
	saveAs("Tiff",KymRaw);
	close();
}

function FFTfunction()
{
	selectWindow("Kymo");
	width = getWidth;
  	height = getHeight;
  	prominence=40;
	setTool("rectangle");
	y=0;
	FPS=200;
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
		waitForUser("Adjust width rectangular ROI  to threshold");
		run("Add to Manager");
		run("FFT");
		do{
			selectWindow("FFT of SubKymo-"+y);
			print("prominence for " +y+ " is "+prominence);
			run("Find Maxima...", "prominence="+prominence+" exclude output=[Point Selection]");   //Take the R value in the first quadrant (ideally with a Theta of 90° (given that the frequency pattern is aligned vertically)=. If no value appears there you have to adjust the prominence value, usaully you need to reduce it to find local maxima. The points further away from the orgiing represent higher frequencies and they could also be noise.
			Dialog.create("FFT quality");
			Dialog.addCheckbox("check box if not one maximum in 1st quadrant", false);
			Dialog.show();
     		Satis = Dialog.getCheckbox();
     		if(Satis){
     			prominence=getNumber("New prominence", prominence); 
     		}
     	}while(Satis);
     	selectWindow("FFT of SubKymo-"+y);
		run("Measure"); //use following formula:  Freq=1/(R*timeperframe(in sec))
		ResT=measureDir+"Results_Kymo-"+y;
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
}
