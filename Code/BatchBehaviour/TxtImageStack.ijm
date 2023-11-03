
macro "ImgTxtStack"
{
	PathImgs = getDirectory("Choose the Input file directory (where the files are)");
	DirVar = "DirectionDispVert";
	ListDirect = newArray("Upward","Downward","Quiet");
	TrialID = "011020_E001T_2";
	NumTimePoints = 10;
	
	for(i=0; i<lengthOf(ListDirect); i++) {	
		for(j=0; j<NumTimePoints; j++) {	
		corrCount = j + 1 ;
		run("Text Image... ", "open=[" + PathImgs + DirVar + "_" + ListDirect[i] + "_" + TrialID + "_" + "Time-" + corrCount + "]");
		}
		run("Images to Stack", "  title=" + ListDirect[i] + " use");
	}
}