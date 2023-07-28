%Binning of matrix 
function B = binning(coordStart,coordEnd,image)

    %Binning [y x]
    blocksize = [1 2];
    disp(coordStart)
    disp(coordEnd)

    Remainder = mod(length(coordStart:coordEnd),blocksize(2));
    A = image(:,coordStart:coordEnd - Remainder); %subsetting the columns to analyse.
    B = mat2cell(A,repmat(blocksize(1),[1 size(A,1)/blocksize(1)]),...
        repmat( blocksize(2),[1 size(A,2)/blocksize(2)]));  %binning the kymograph by 2, only in the X dimension.
    B = cellfun(@(x) min(x,[],'all'),B);
end
