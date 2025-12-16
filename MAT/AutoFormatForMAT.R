# This script prepares the input fossil file for you as required by the scripts of Joel for MAT reconstructions using dinocyst data
# You only need to put the taxa you have (with the correct abbreviations), there's no need to manually fill a table with all 71 taxa.
# The order of your taxa doesn't matter.
# Do install.packages("analogue") if you haven't done it.
# The file containing your taxa should be a csv file by default.
# Make sure you have "dino1968.txt" under the same directory (your working directory).
# By Xiner Wu: wu.xiner@courrier.uqam.ca

print("Make sure to have put the correct abbreviations as column names! The order doesn't matter. Your data file should be .csv by default.")
library(analogue)

filename <- readline(prompt = "Enter file name (eg. SIP084_original_data.csv): ")
dformat <- as.numeric(readline(prompt = "Select your type of data (1=raw counts; 2=percentages): "))
outname <- readline(prompt = "Enter output file name (eg. SIP084.txt): ")

#load the original data file as oridata
oridata <- read.csv(filename,check.names = FALSE, row.names = 1)

#convert any missing values (NA) into zeros
oridata[is.na(oridata)] <- 0

#load the complete list of taxa from dino1968.txt
taxa <- read.delim('dino1968.txt',row.names = 1)

#process data based on the format specified
if (dformat==1){
  #calculate per mil from raw counts
  tmpdata <- oridata/rowSums(oridata)*1000
}else if (dformat==2){
  #convert percent to per mil
  tmpdata <- oridata*10
}

#auto-completing using the join function from the analogue package
dat <- join(taxa,tmpdata)

#extract target data set from the merged data sets and save as tab delimited text file
tardata <- dat$tmpdata
df <- cbind(stations = rownames(tardata), tardata)
write.table(df,outname,sep = "\t",quote = FALSE, col.names = TRUE, row.names = FALSE,)
print("Done!")