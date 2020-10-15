# general settings
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(stringsAsFactors = FALSE)

# packages
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager") 
if (!requireNamespace("mzR", quietly = TRUE)) 
  BiocManager::install("mzR")  
if (!requireNamespace("readr", quietly = TRUE))
  install.packages("readr")  
if (!requireNamespace("stringr", quietly = TRUE))
  install.packages("stringr")  

library(mzR) # openMSfile
library(readr) # read file
library(stringr) # extract substring


# create output directory 
if(!dir.exists("results")){
  dir.create("results")
}


# # read in scaffold file new
source("readScaffold.R")
rawData = readScaffold(fileName = "mzML/Spectrum Report for Byrum_061019_histones.xls")
rownames(rawData) = NULL


# Replace spaces with underscores in column names
names(rawData) <- gsub(" ", "_", names(rawData))


# Make table of Raw file, Scan number, observed mz
df1 = data.frame(id = 1:nrow(rawData), rawData, check.names = F)
df1$Raw_file = gsub("\\.raw.*$", ".mzML", rawData$`MS/MS_sample_name`)
df1$Scan_number = as.integer(str_extract(rawData$`Spectrum_name`, "(?<=\\.)[[:digit:]]*(?=\\.)")) # requires the following pattern: ".NUMBER."
# df1$id = rownames(rawData)
df2 = df1[,c("id", "Raw_file", "Scan_number", "Observed_m/z")]


# get file names
files = unique(df2$Raw_file)

# loop through mzML files  
df3 = NULL 
for(i in seq_along(files)){
  print(i)
  
  # limit to current file
  local_scans <- df2[df2$Raw_file == files[i],]
  
  # read mzML file
  mzml <- openMSfile(paste0("mzML/", files[i]))
  ms_header <- header(mzml)
  close(mzml)
  a1 = ms_header[c("seqNum", "precursorIntensity")]
  colnames(a1) = c("Scan_number", "Intensity")
  
  # combine data and intensities
  a2 = merge(x=local_scans,y=a1,by="Scan_number")
  a2 = a2[order(as.numeric(a2$id)),]
  a2 = a2[,c("id", "Raw_file", "Scan_number", "Observed_m/z", "Intensity")]

  df3 = rbind(df3, a2)
}

# add intensities to scaffold file
df4 = data.frame(df1, Intensity = df3$Intensity, check.names = F) 

# Remove NON histone proteins
df4 = df4[grepl("Histone H", df4$Protein_name) | grepl("Core histone", df4$Protein_name),]

# write to file
write.table(df4, file = "results/S2_extracted_intensities.txt", quote = F, sep = "\t", row.names = F) # colum order has changed from original (Observed_mz moved)

