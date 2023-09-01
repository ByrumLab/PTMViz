# general settings
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(stringsAsFactors = FALSE)

# packages
if (!requireNamespace("dplyr", quietly = TRUE))
  install.packages("dplyr")   
if (!requireNamespace("stringr", quietly = TRUE))
  install.packages("stringr")  

library(dplyr) # read file
library(stringr) # extract substring


# code to fix PTMs
source("src/fixPTM.R")

# read in S2 file
full_df = read.table("data/S2_extracted_intensities.txt", header = T, sep = "\t", check.names = F) 

# remove "DECOY" lines (I didn't have any)
full_df = full_df[!grepl("DECOY", full_df$Protein_name),]

# Fixing PTMs
full_df = fix_PTMs(full_df)

# filter
df = full_df[full_df$Mascot_Delta_Ion_Score > 15,]



# reduce dataframe
df$Correct_start = df$Peptide_start_index
df$Correct_stop = df$Peptide_stop_index


colnames(df)

df = df[, c("MS/MS_sample_name",
            "Protein_name",
            "Peptide_sequence",
            "PTM",
            "Precursor_intensity",
            "Correct_start",
            "Correct_stop",
            "Scan_number")]


# Aggregates precursor intensity for identical peptides into single row
df1  = aggregate(x = df$Precursor_intensity, FUN = sum, by = list("MS/MS_sample_name" = df$`MS/MS_sample_name`, 
                                                        Protein_name = df$Protein_name, 
                                                        Peptide_sequence = df$Peptide_sequence, 
                                                        PTM = df$PTM, 
                                                        Correct_start = df$Correct_start, 
                                                        Correct_stop = df$Correct_stop))
colnames(df1)[colnames(df1) == "x"] = "Intensity"




#Helper function.  This takes a conventional PTM string and extracts all PTM sites in the form of an ordered integer.

get_PTM_residues <- function(x){
  temp = str_extract_all(x, "[[:alpha:]]+[[:digit:]]+(?=\\: [[:alpha:]])")
  temp = unique(unlist(temp))
  return(temp)
}


# Gets PTM sites for each protein
PTM3 = data.frame()
for(i in unique(df1$Protein_name)){
  temp = df1[df1$Protein_name == i,]
  if(!identical(get_PTM_residues(temp$PTM), character(0)))
    PTM3 = rbind(PTM3, cbind(i,get_PTM_residues(temp$PTM)))
}

colnames(PTM3) = c("Protein", "PTM_residues")
PTM3$PTM_site = as.integer(gsub("^[[:alpha:]]+", "", PTM3$PTM_residues))


# Helper function.  This filters the full_df for all peptides of a specific protein that span the modification sites.
# It then checks if that site is modified or not
filter_function <- function(Protein, PTM_site, PTM_residues) {
  temp = df1[df1$Protein_name == Protein & 
               df1$Correct_start <= PTM_site & 
               df1$Correct_stop >= PTM_site,]
  Is_modified = str_detect(temp$PTM, PTM_residues)
  temp$PTM_corrected = ifelse(Is_modified,
                      str_extract(temp$PTM, paste(PTM_residues, "\\: ", "[[:alpha:]]+", sep = "")),
                      paste(PTM_residues, ": Unmodified", sep = ""))
  return(temp)
}

# Combines Protein and PTM information (including corrected PTM)
library(dmm)
class(PTM3[,3])

PTM5 = data.frame()
for(i in 1:dim(PTM3)[1]){ # warning can be ignored
  temp = cbind(PTM3[i,], filter_function(Protein = PTM3$Protein[i], 
                                         PTM_site = PTM3$PTM_site[i],
                                         PTM_residues = PTM3$PTM_residues[i]))
  PTM5 = rbind(PTM5, temp)
}

PTM6 = PTM5[,c("MS/MS_sample_name", "Protein_name", "PTM_residues", "PTM_corrected", "Intensity")]
PTM6 = PTM6[order(PTM6$`MS/MS_sample_name`, PTM6$Protein_name, PTM6$PTM_residues),]
PTM6$Total_intensity = NA

# Sums up intensities by MS/MS sample name, Protein name and residues
temp = aggregate(x = PTM6$Intensity, FUN = sum, 
                 by = list("MS/MS_sample_name" = PTM6$`MS/MS_sample_name`, 
                           Protein_name = PTM6$Protein_name, 
                           PTM_residues = PTM6$PTM_residues))

# Fills in the total intensities without collapsing rows
for(i in 1:nrow(temp)){
  PTM6[PTM6$`MS/MS_sample_name` == temp$`MS/MS_sample_name`[i] & 
         PTM6$Protein_name == temp$Protein_name[i] & 
         PTM6$PTM_residues == temp$PTM_residues[i],]$Total_intensity = temp$x[i]
}

# Collapses MS/MS sample name, Protein name, residues and corrected PTMs averaging over their total intensities 
# (these are the same total intensities, but we "lose" the total intensities in the next aggregate step, aggregating with means as function handles the dimensions)
Total_intensity = aggregate(x = PTM6$Total_intensity, FUN = mean, 
                            by = list("MS/MS_sample_name" = PTM6$`MS/MS_sample_name`, 
                                      Protein_name = PTM6$Protein_name, 
                                      PTM_residues = PTM6$PTM_residues, 
                                      PTM_corrected = PTM6$PTM_corrected))$x

# Collapses MS/MS sample name, Protein name, residues and corrected PTMs summing up their intensities
PTM7 = aggregate(x = PTM6$Intensity, FUN = sum, 
                 by = list("MS/MS_sample_name" = PTM6$`MS/MS_sample_name`, 
                           Protein_name = PTM6$Protein_name, 
                           PTM_residues = PTM6$PTM_residues, 
                           PTM_corrected = PTM6$PTM_corrected))

colnames(PTM7)[5] = "Intensity"
PTM7$Total_intensity = Total_intensity
PTM7$Abundance = PTM7$Intensity / PTM7$Total_intensity
PTM7 = PTM7[order(PTM7$`MS/MS_sample_name`, PTM7$Protein_name, PTM7$PTM_residues),]
PTM7$betaValue = PTM7$Intensity / (PTM7$Total_intensity + 100)
PTM7$MValue = log2( PTM7$betaValue / (1-PTM7$betaValue))

write.csv(PTM7, file = "Histone_beta.csv", row.names = F, quote = F)


# write.table(PTM7, file = "S3_single_output_sg.txt", quote = F, sep = "\t", row.names = F)
# 
# S3 <- read.delim("S3_single_output_sg.txt")
# write.csv(S3, "S3.csv")

library(tidyverse)

#PTM <- read.csv("PTMViz_inputTable4.csv")

# x <- read_csv("PTMViz_inputTable4.csv")
# 
# intensity <- x %>% 
#   separate(col = `Custom ID`, into = c("col1", "col2", "treatment", "sample")) %>% 
#   select(sample, Histone, `Intensity`) %>% 
#   pivot_wider(names_from = sample, values_from = `Intensity`)


# m_value <- x %>% 
#   separate(col = `Custom ID`, into = c("col1", "col2", "treatment", "sample")) %>% 
#   select(sample, Histone, `M Value`) %>% 
#   pivot_wider(names_from = sample, values_from = `M Value`)
# 
# 
# beta_value <- x %>% 
#   separate(col = `Custom ID`, into = c("col1", "col2", "treatment", "sample")) %>% 
#   select(sample, Histone, `Beta Value`) %>% 
#   pivot_wider(names_from = sample, values_from = `Beta Value`)

##################################

PTM8 <- PTM7
colnames(PTM8)[colnames(PTM8)=="MS/MS_sample_name"] = "sample_name"

intensity <- pivot_wider(data = PTM8, id_cols = c("Protein_name","PTM_corrected"), names_from ="sample_name", values_from="Intensity")

beta <- pivot_wider(data = PTM8, id_cols = c("Protein_name","PTM_corrected"), names_from ="sample_name", values_from="betaValue")

mvalue <- pivot_wider(data = PTM8, id_cols = c("Protein_name","PTM_corrected"), names_from ="sample_name", values_from="MValue")


write.csv(mvalue, "m-values.csv")
write.csv(beta, "beta-values.csv")
write.csv(intensity, "intensity-values.csv")

#PTM8 <- PTM7 %>%
#  spread(`MS/MS_sample_name`, betaValue) %>%
#  write.csv("data/S3_single_spread_output_beta.csv")
# 

#PTM7 <- PTM6 %>%
#   spread(`MS/MS_sample_name`, Abundance) %>%
#   write_tsv("data/S3_single_spread_output.tsv")
# 
# 
# PTM_NA <- read_tsv("data/S3_single_output_NA.txt")
# 
# library(readr)
# hpmed <- read_csv("hpmed.csv")
# rownames(hpmed) <- hpmed$X1
# hprep <- hpmed[,2:3]
# rownames(hprep) <- hpmed$X1
# 
# hp <- PTM6[206:580,] %>%
#   mutate(Protein_name = gsub("Histone ", "", Protein_name) %>%
#            gsub(" OS.*$", "", .)) %>%
#   unite(a, Protein_name, PTM_corrected) %>%
#   select(-PTM_residues) %>%
#   group_by(a) %>%
#   # filter(n() > 5) %>%
#   ungroup() %>%
#   filter(!grepl("Unmodified", a)) %>%
#   spread(`MS/MS_sample_name`, Abundance, fill = 0) %>%
#   column_to_rownames("a") %>%
#   as.matrix() %>%
#   {a <- t(apply(., 1, scale))
#   colnames(a) <- colnames(.)
#   a} 
# #%>%
# # Heatmap(.)
# library(ComplexHeatmap)
# library(circlize)
# library(colorspace)
# 
# class(hp)
# write.csv(hp, "hp.csv")
# 
# 
# h.tmt.p <- Heatmap(hprep, name = "Striatum", 
#                    #column_title = paste("CK vs CKD(", dim(mat.tmt.p)[1], "proteins)\n TMT quantification\n min unique pept =",N_u+1, ",p.adj<",round(p_adj.p,2)),
#                    #column_title_side = "top", 
#                    row_title = "Proteins",na_col = "grey",
#                    col = colorRamp2(c(-2, 0, 2),
#                                     c("blue","white","red")),
#                    cluster_rows = TRUE,
#                    show_row_names=TRUE,
#                    row_names_max_width = unit(8, "cm"),
#                    row_names_gp = gpar(fontsize = 8),
#                    clustering_distance_rows = "euclidean",
#                    clustering_method_rows = "complete",
#                    cluster_columns = FALSE,
#                    show_column_names = TRUE,
#                    clustering_distance_columns = "euclidean",
#                    clustering_method_columns = "complete")
# # km=2,km_title = "C%i",
# # top_annotation_height = unit (0.25, "cm"),
# # top_annotation = ha.tmt.p.top,
# # bottom_annotation = ha.tmt.p.bottom,
# # bottom_annotation_height = unit(4, "cm"))
# h.tmt.p
# 
# 
# #################### 
# ### Nucleus Accumbens
# 
# 
# hp <- PTM6[1:205,] %>%
#   mutate(Protein_name = gsub("Histone ", "", Protein_name) %>%
#            gsub(" OS.*$", "", .)) %>%
#   unite(a, Protein_name, PTM_corrected) %>%
#   select(-PTM_residues) %>%
#   group_by(a) %>%
#   # filter(n() > 5) %>%
#   ungroup() %>%
#   filter(!grepl("Unmodified", a)) %>%
#   spread(`MS/MS_sample_name`, Abundance, fill = 0) %>%
#   column_to_rownames("a") %>%
#   as.matrix() %>%
#   {a <- t(apply(., 1, scale))
#   colnames(a) <- colnames(.)
#   a} 
# 
# class(hp)
# write.csv(hp, "hp_NA.csv")
# 
# library(readr)
# hpmed <- read_csv("hpmed_NA.csv")
# rownames(hpmed) <- hpmed$X1
# hprep <- hpmed[,2:3]
# rownames(hprep) <- hpmed$X1
# 
# h.tmt.p <- Heatmap(hprep, name = "Relative Abundance", 
#                    #column_title = paste("CK vs CKD(", dim(mat.tmt.p)[1], "proteins)\n TMT quantification\n min unique pept =",N_u+1, ",p.adj<",round(p_adj.p,2)),
#                    #column_title_side = "top", 
#                    row_title = "Proteins",na_col = "grey",
#                    col = colorRamp2(c(-2, 0, 2),
#                                     c("blue","white","red")),
#                    cluster_rows = TRUE,
#                    show_row_names=TRUE,
#                    row_names_max_width = unit(8, "cm"),
#                    row_names_gp = gpar(fontsize = 8),
#                    clustering_distance_rows = "euclidean",
#                    clustering_method_rows = "complete",
#                    cluster_columns = FALSE,
#                    show_column_names = TRUE,
#                    clustering_distance_columns = "euclidean",
#                    clustering_method_columns = "complete")
# # km=2,km_title = "C%i",
# # top_annotation_height = unit (0.25, "cm"),
# # top_annotation = ha.tmt.p.top,
# # bottom_annotation = ha.tmt.p.bottom,
# # bottom_annotation_height = unit(4, "cm"))
# h.tmt.p
