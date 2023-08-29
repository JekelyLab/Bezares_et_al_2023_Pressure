
/////////////////////////////////////////////////////////
// This file is best viewed with a monospaced font.    //
/////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////
// ImageJ macro to extract the intensity profiles of   //
// selected ROIs across a list of files. 		       //
// Author: Luis Alberto Bezares-Calderón               // 
//													   //
/////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////// 
//                                                                     //                                  
// Important: the macro assumes the videos are already                 //
// corrected for X-Y shifts.                                           //
// This is done using the Descriptor-based Series                      //
// registration ImageJ plugin.                                         //
// Use the DIC or tdTomato channel for registration and                //
// then apply the calculations to the GCaMP and TdTomato channels.     //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 

macro "MeasureIntensityprofiles"

//Obtaining directory addresses.
{
	if((getBoolean("Choose input and output directories if not select a list of input and output directories")))
	{
         inputCZIDir = getDirectory("Choose the Input file directory (where the files are)");
         OutJET=getDirectory("Choose the file  directory to store the JET GC stacks");
         OutTableDir=getDirectory("Choose the file directory to store the intensity values tables");
	}

 
	// Get all the files in the input directory.
	list=getFileList(inputCZIDir);
	Array.sort(list);
	print(list.length);

//This 'for' will apply  the LUT described in Guehmann et al,2015
	 for (k=0; k<list.length; k++)
	{
			print(list[k]); 
			fullpathInput_image=inputCZIDir + list[k];
			open(fullpathInput_image);
			imageTitle = getTitle();
			imageTitle = replace(imageTitle, ".czi", ""); 
			imageTitle = replace(imageTitle, ".tif", "");
			rename(imageTitle);
			print(imageTitle);
			Dialog.create("Status registration");
			Dialog.addCheckbox("Check box if you want to register the video to correct for XY shifts", true);
			Dialog.show();
 			Regis = Dialog.getCheckbox();
 			if(Regis)
 			{
 				OutRegDir= getDirectory("Choose the  Reg file  directory (where the registered files will be stored)");
 				do{
        			selectWindow(imageTitle);
        			succ=Registration(imageTitle);
        			selectWindow("registered "+imageTitle);
        			Stack.getDimensions(w, h, channels, slices, frames);
        			if(w != h ) { //The video needs to have same w and h to be processed by correlation analysis pipeline,
        				WHarray = newArray(w,h);
        				Array.getStatistics(WHarray, min, max, mean, stdDev);
        				setTool("rectangle");
        				run("Specify...", "width=" + min + " height=" + min + " x=0 y=0 slice=1");
        				Dialog.create("Square the frame size");
						waitForUser("position the square where you want to crop the video");
						run("Crop");
        			}
        			selectWindow("registered "+imageTitle);
        			setSlice(1);
        			for(i=0;i<frames;i++) {			               
			               setKeyDown("alt");
			               run("Next Slice [>]");
			               wait(10);
       				}
 					Dialog.create("More ROIs");
					Dialog.addCheckbox("Check box if registration was successful", true);
					Dialog.show();
 					Success = Dialog.getCheckbox();
					print(Success);
					if(!Success){
						selectWindow("registered "+imageTitle);
						close();
					}
 			}while(!Success);
			
				if(Success){
					selectWindow("registered "+imageTitle);
					Stack.setDisplayMode("grayscale");
					regtitle="reg_"+imageTitle;
					rename(regtitle);
					fullpathReg_image=OutRegDir + regtitle;
					saveAs("Tiff",fullpathReg_image);
					imageTitle=replace(regtitle, ".tif", ""); 
					rename(imageTitle);
					
				}
			}
		
		run("Duplicate...",  "title="+imageTitle+"_GC duplicate channels=1");
		 r=newArray(256);g=newArray(256);b=newArray(256); 
		for (i=0;i<256;i++) { 
			i4=4*i/256; 
			r[i]=255*minOf(maxOf(minOf(i4-1.5,-i4+4.5),0),1); 
			g[i]=255*minOf(maxOf(minOf(i4-0.5,-i4+3.5),0),1); 
			b[i]=255*minOf(maxOf(minOf(i4+0.5,-i4+2.5),0),1); 
		} 
		setLut(r,g,b); 
		fullpathRegJET_image=OutJET + imageTitle;
		saveAs("Tiff",fullpathRegJET_image +"_JETlut");
        close();
		selectWindow(imageTitle);
        Stack.getDimensions(w, h, channels, slices, frames);
        setSlice(1);
		for(i=0;i<frames;i++) {
           setKeyDown("alt");
           run("Next Slice [>]");
           wait(10);
         }
			
//This part of the code has the purpose of creating the ROIs of the cells to be measured. Use the reference tdTomato signal for drawing the ROIs.	 
	 do{
	   	selectWindow(imageTitle);
		Stack.setChannel(2);  //channel 2 is tdTomato. Adjust if otherwise.
		setTool("polygon");
		waitForUser("Set ellipse in region for measuring intensity");
		Cell="untitled";
		Dialog.create("CellOfInterest");
		Dialog.addString("Title:",Cell );
		Dialog.show();
		Cell=Dialog.getString();
		
		ROIname=imageTitle+"_"+Cell;
	    print(ROIname);
	      Roi.setName(ROIname); 
	      roiManager("Add");
		 run("Clear Results");
		 setOption("InterpolateLines", true);
		 run("Plots...", "width=600 height=340 font=14 draw_ticks list minimum=0 maximum=0 interpolate");
		 run("Plot Z-axis Profile");  //This will create the intensity values in the ROI across the whole video.
		selectWindow("Plot Values");
		saveAs("Results",OutTableDir + imageTitle + "_Tom_" +Cell + ".txt");
		run("Clear Results");
	
	index=findRoiWithName(ROIname);
	 roiManager("select",index);
	 run("Clear Results");
	 selectWindow(imageTitle);
	 Stack.setChannel(1);
	 run("Plot Z-axis Profile");
	 selectWindow("Plot Values");
	 saveAs("Results",OutTableDir + imageTitle + "_GC_" + Cell + ".txt");
	 run("Clear Results");
	 selectWindow(imageTitle);
	 setSlice(1);
	 for(i=0;i<frames;i++) {
           setKeyDown("alt");
           run("Next Slice [>]");
           wait(10);
             }
	Dialog.create("More ROIs");
	Dialog.addCheckbox("Check box if you want to measure more ROIs", false);
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

function Registration(stack) {
	selectWindow(stack);
	Title=getTitle();
	Channel="2";
	Dialog.create("Channel to use for reg (usually GC:1,Tom:2,BF:3, but verify order)");
	Dialog.addString("Title:",Channel);
	Dialog.show();
	Channel=Dialog.getString();
	//run("Descriptor-based series registration (2d/3d + t)", "series_of_images="+Title+" brightness_of=Strong approximate_size=[10 px] type_of_detections=[Minima & Maxima] subpixel_localization=[3-dimensional quadratic fit] transformation_model=[Rigid (2d)] images_are_roughly_aligned number_of_neighbors=3 redundancy=1 significance=3 allowed_error_for_ransac=5 global_optimization=[All against first image (no global optimization)] range=5 choose_registration_channel="+Channel+" image=[Fuse and display] interpolation=[Linear Interpolation]");
	run("Descriptor-based series registration (2d/3d + t)", "series_of_images="+Title+" brightness_of=[Interactive ...] approximate_size=[Interactive ...] type_of_detections=[Interactive ...] subpixel_localization=[3-dimensional quadratic fit] transformation_model=[Rigid (2d)] images_are_roughly_aligned number_of_neighbors=3 redundancy=1 significance=3 allowed_error_for_ransac=5 global_optimization=[All against first image (no global optimization)] range=5 choose_registration_channel=" + Channel + " image=[Fuse and display] interpolation=[Linear Interpolation]");
	return Title;
	
}
