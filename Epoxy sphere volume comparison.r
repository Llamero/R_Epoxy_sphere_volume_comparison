#Set working directory to file directory with data
setwd("E:/Altan Lab/Median Filtered Stacks (5x5x5)/Results/Spreadsheet Output")

#This function counts the total number of bundles in each file and returns the count
countObjects<-function(fileName){
	#Load the file into a temporary matrix
	fileMatrix<-read.table(fileName, sep=",", header=TRUE)
	
	#Return the number of samples in the file
	count<-nrow(fileMatrix)
	return(count)
}

#This function fills a matrix with the corresponding measurements in an excel file
excelToMatrix<-function(fileName, maxCount){
	#Load the file into a temporary matrix
	fileMatrix<-read.table(fileName, sep=",", header=TRUE)
	
	#Retrieve the measured radii and the scaling factor
	xyRadiusVector<-fileMatrix$xyRadius
	zRadiusVector<-fileMatrix$zRadius
	inflationVector<-fileMatrix$Inflation_Factor
	
	#Fill the remaining vector with NA so that sapply creates a matrix (all returned vectors are of equal length)
	for (a in length(xyRadiusVector):maxCount){
		xyRadiusVector[a+1]<-NA
		zRadiusVector[a+1]<-NA
		inflationVector[a+1]<-NA
	}
	
	#Calcaulate the volume of the corresponding ellipsoid
	volumeVector<-xyRadiusVector * xyRadiusVector * zRadiusVector * (4/3) * pi
	
	#Return the volume measurements column
	return(volumeVector)
}

#Get all file names that end with a *.xls file extension
fileNameList<-list.files(pattern = "\\.csv$",  ignore.case=TRUE)

#Count the number of results in each file
fileObjectCounts<-sapply(fileNameList, countObjects, simplify = TRUE)

#Find the largest number of object counts (this value will be used to setup a matrix to store all volume measurements)
maxCount<-max(fileObjectCounts)

#Add the volume measurements to  each corresponding column in the a new matrix
allVolumeMatrix<-sapply(fileNameList, excelToMatrix, maxCount, simplify = TRUE, USE.NAMES = FALSE)

#Extract the sample ID for each file by removing the prefix and extension and set as the column names in the new matrix
sampleIDvector<-sapply(fileNameList, gsub, pattern=" - Auto.*", replacement="")
colnames(allVolumeMatrix)<-sampleIDvector

#Create a notched box plot for each genotype
par(mar = c(15,6,3,3))
boxplot(allVolumeMatrix, log="y", las = 2, names = sampleIDvector, notch = FALSE, ylim = c(min(1000, na.rm = TRUE), max(allVolumeMatrix, na.rm = TRUE)), ps = 1, cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1, bty="n")
mtext(expression(paste( plain("Epoxy Microsphere Volume Distribution (μm") ^ plain("3"), plain(")") )), side=2, line = 4, cex = 1)
