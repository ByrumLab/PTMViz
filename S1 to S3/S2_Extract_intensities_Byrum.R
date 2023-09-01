#source("https://bioconductor.org/biocLite.R")
#biocLite("mzR")

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("mzR")


library(tidyverse)
library(mzR)

if(!dir.exists("data")){
  dir.create("data")
}


# remove the dat number from the MS/MS sample name column; check number of header rows
df <- read_csv("mzML/Spectrum Report for Byrum_061019_histones_v1.csv", skip = 1, guess_max = 150000)

#Replace spaces with underscores in column names
names(df) <- gsub(" ", "_", names(df))

#Removes last row
df <- df %>% filter(!grepl("END", Experiment_name))

#Make table of Raw file, Scan number, observed mz
df1 <- df %>%
  mutate(Raw_file = gsub("\\.raw.*$", ".mzML", `MS/MS_sample_name`),
         Scan_number = as.integer(str_extract(Spectrum_name, "(?<=\\.)[[:digit:]]*(?=\\.)"))) %>%
  rownames_to_column("id") # Make sure these line up

df2 <- df1 %>%
  select(id, Raw_file, Scan_number, Observed_mz = `Observed_m/z`)

files <- df2 %>%
  distinct(Raw_file)

output <- list()

for(i in seq_along(files$Raw_file)){
  mzml <- openMSfile(paste0("mzML/", files$Raw_file[[i]]))
  ms_header <- header(mzml)
  local_scans <- filter(df2, Raw_file == files$Raw_file[[i]])
  
  a1 <- ms_header %>%
    select(Scan_number = seqNum, precursorIntensity) %>%
    left_join(local_scans, .) %>%
    rename(Precursor_intensity = precursorIntensity)
  
  output[[i]] <- a1
  close(mzml)
  print(i)
}

df3 <- do.call(rbind, output)


df4 <- df1 %>%
  left_join(df3) %>%
  select(-`Observed_m/z`)

df4 %>%
  write_tsv("data/S2_extracted_intensities.txt")




