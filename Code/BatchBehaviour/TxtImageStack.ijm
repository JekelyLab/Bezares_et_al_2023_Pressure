
macro "ImgTxtStack"
{
	PathImgs = getDirectory("Choose the Input file directory (where the files are)");
	DirVar = "DirectionDispVert";
	ListDirect = newArray("Upward","Downward");//,"Quiet");
	TrialID = "011020_E001T_2";
	NumTimePoints = 3;
	
	for(i=0; i<lengthOf(ListDirect); i++) {	
		for(j=0; j<NumTimePoints; j++) {	
		corrCount = j + 1 ;
		run("Text Image... ", "open=[" + PathImgs + DirVar + "_" + ListDirect[i] + "_" + TrialID + "_" + "Time-" + corrCount + "]");
		}
		run("Images to Stack", "name="+ ListDirect[i] + " title=" + ListDirect[i] + " use");
	}
	run("Merge Channels...", "c5=" + ListDirect[0] + " c6=" + ListDirect[1] + " create");
	selectImage("Composite");
	rename(DirVar + "_" + TrialID + "_composite");

}