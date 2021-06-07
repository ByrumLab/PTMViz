
fix_PTMs <- function(df){
  # If histone, subtracts 1 from Peptide start index.
  # df$Peptide_start_index <- ifelse(grepl("Histone H", df$Protein_name),
  #                                  df$Peptide_start_index-1L,
  #                                  df$Peptide_start_index)
  df$Peptide_start_index <- df$Peptide_start_index-1
  
  # Filters for rows with modifications (excludes TMT)
  keepPTM = str_detect(df$Variable_modifications_identified_by_spectrum, "(?<=[[:alpha:]])[[:digit:]]+(?=\\: (Acetyl |Methyl|Dimethyl|Trimethyl|GlyGly|Phospho))")
  keepPTM[is.na(keepPTM)] = F
  df_PTM = df[keepPTM,]
  df_noPTM = df[!keepPTM,]
  
  # extract PTMs
  full_PTM = str_extract_all(df_PTM$Variable_modifications_identified_by_spectrum, "[[:alpha:]][[:digit:]]+\\: (Acetyl |Methyl|Dimethyl|Trimethyl|GlyGly|Phospho)")
  
  # extract amino acid and position
  PTM_amioAcid = lapply(full_PTM, FUN = function(x) unlist(str_extract_all(x, "[[:alpha:]](?=[[:digit:]]+\\: (Acetyl |Methyl|Dimethyl|Trimethyl|GlyGly|Phospho))")))
  PTM_position = lapply(full_PTM, FUN = function(x) as.numeric(unlist(str_extract_all(x, "(?<=[[:alpha:]])[[:digit:]]+(?=\\: (Acetyl |Methyl|Dimethyl|Trimethyl|GlyGly|Phospho))"))))
  
  # change peptide position to protein position
  for(i in 1:dim(df_PTM)[1]){
    PTM_position[[i]] = PTM_position[[i]] + df_PTM$Peptide_start_index[i]
  }
  PTM_position = lapply(PTM_position, as.character)
  
  # extract modification
  PTM_modification = lapply(full_PTM, FUN = function(x) str_extract_all(x,"(?<=[[:digit:]]{1,3})\\: [[:alpha:]]*"))
  
  # combine amino acid postion and modification 
  correct_PTM = list()
  for(i in 1:length(PTM_amioAcid)){
    correct_PTM[[i]] = paste(PTM_amioAcid[[i]], PTM_position[[i]], PTM_modification[[i]], sep = "")
    correct_PTM[[i]] = paste(correct_PTM[[i]], collapse = ", ")
  }
  correct_PTM = unlist(correct_PTM)
  
  # return
  df_PTM$PTM = correct_PTM
  df_noPTM$PTM = "Unmodified"
  return(rbind(df_PTM, df_noPTM))
  
}



