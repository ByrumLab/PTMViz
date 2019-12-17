##### Packages ####
 if (!require(shiny)) install.packages('shiny')
 library(shiny)

 if (!require(shinydashboard)) install.packages('shinydashboard')
 library(shinydashboard)

 if (!require(shinyWidgets)) install.packages('shinyWidgets')
 library(shinyWidgets)

 if (!require(rhandsontable)) install.packages('rhandsontable')
 library(rhandsontable)

if (!require(e1071)) install.packages('e1071')
library(e1071)

 if (!require(ggplot2)) install.packages('ggplot2')
 library(ggplot2)

 if (!require(dplyr)) install.packages('dplyr')
 library(dplyr)

 if (!require(plyr)) install.packages('plyr')
 library(plyr)

 if (!require(crosstalk)) install.packages('crosstalk')
 library(crosstalk)

 if (!require(DT)) install.packages('DT')
 library(DT)

 if (!require(matrixStats)) install.packages('matrixStats')
 library(matrixStats)

 if (!requireNamespace("BiocManager", quietly = TRUE))
   install.packages("BiocManager")
  library(edgeR)

 if (!require(installr)) install.packages('installr')
 library(installr)

 if (!require(devtools)) install.packages("backports")
 library(backports)

 if (!require(devtools)) install.packages("devtools")
 library(devtools)

if (!require(heatmaply)) install.packages('heatmaply')
library(heatmaply)

 if (!require(ggfortify)) install.packages('ggfortify')
 library(ggfortify)

if (!require(data.table)) install.packages("data.table")
library(data.table)

  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
 library(minfi)

if (!require(limma)) install.packages('limma')
library(limma)

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(gplots)) install.packages('gplots')
library(gplots)

if (!require(grid)) install.packages('grid')
library(grid)

if (!require(gridExtra)) install.packages('gridExtra')
library(gridExtra)

if (!require(plotly)) install.packages('plotly')
library(plotly)

if (!require(reshape)) install.packages('reshape')
library(reshape)

if (!require(colourpicker)) install.packages('colourpicker')
library(colourpicker)

if (!require(RColorBrewer)) install.packages('RColorBrewer')
library(RColorBrewer)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

if (!requireNamespace("limma", quietly = TRUE))
  BiocManager::install("limma") else library(limma)

if (!requireNamespace("gplots", quietly = TRUE))
  install.packages("gplots") else library(gplots)

#### Shiny App####
options(stringsAsFactors = F)
####Shiny UI####
ui <-shinyUI(
  ####Creates the general format####
  dashboardPage(
    dashboardHeader(title = "PTMViz"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Upload", tabName = "Upload",
                 menuSubItem("Protein Upload",
                             tabName = "Upload2"),
                 menuSubItem("PTM Upload",
                             tabName = "Upload1")),
        menuItem("Protein", tabName = "Protein",
                 menuSubItem("Preliminary", tabName = "Preliminary"),
                 menuSubItem("Protein_Data", tabName = "Data")),
        menuItem("Post Translational Modifications", tabName = "PTM"),
        radioButtons("dnld_options", "Select Download File Type", choices = list("png", "pdf")),
        actionBttn("tst", "Test")
      )),
    dashboardBody(
      tabItems(
        ####Preliminary Protein Data ####
        tabItem(tabName = "Preliminary", 
                fluidRow(
                  tabBox(title = "Histogram", 
                         tabPanel("Raw", 
                                  plotOutput("Histogram_Raw"), 
                                  downloadButton("H_raw", "Download")),
                         
                         tabPanel("Normalized", 
                                  plotOutput("Histogram_Norm"),
                                  downloadButton("H_norm", "Download")),
                         
                         tabPanel("Settings", 
                                  textInput("hist_raw_title", label =h3("Raw Data Histogram Title")),
                                  textInput("hist_norm_title", label =h3("Normal Data Histogram Title")),
                                  textInput("hist_xlab", label =h3("X-axis Label")),
                                  textInput("hist_ylab", label =h3("Y-axis Label")),
                                  colourInput("hist_fill_colour", label = "Choose Fill color", value = "red"),
                                  colourInput("hist_border_colour", label = "Choose Border color", value = "black"),
                                  sliderInput("hist_slider", label = "Binwidth", min = 1, max = 10, value = 2))),
                  tabBox(title = "Boxplot", 
                         tabPanel("Raw", 
                                  plotOutput("Boxplot_Raw"),
                                  downloadButton("B_raw", "Download")),
                         tabPanel("Normalized", 
                                  plotOutput("Boxplot_Norm"),
                                  downloadButton("B_norm", "Download")),
                         tabPanel("Settings", 
                                  textInput("box_raw_title", label =h3("Raw Data Histogram Title")),
                                  textInput("box_norm_title", label =h3("Normal Data Histogram Title")),
                                  textInput("box_xlab", label =h3("X-axis Label")),
                                  textInput("box_ylab", label =h3("Y-axis Label")),
                                  colourInput("box_fill_colour", label = "Choose Fill color", value = "red"),
                                  colourInput("box_border_colour", label = "Choose Border color", value = "black")))),
                fluidRow(
                  tabBox(tabPanel("PCA", 
                                  plotOutput("PCA"),
                                  downloadButton("PCA_dnld", "Download")),
                         tabPanel("MDS", 
                                  plotOutput("MDS"),
                                  downloadButton("MDS_dnld", "Download"))))),
            
        
        ####PTM Page####
        tabItem(tabName = "PTM", h1("Post Translational Modification"),
                fluidRow(
                  box( width = 3, uiOutput("picker1"), uiOutput("picker2"), uiOutput("picker3"), uiOutput("picker4"), 
                       uiOutput("Dif_Anal_choice1")),
                  box(title= "Histone_data_table", width = 9,  dataTableOutput("Histone_data_table"))
                  ),
                fluidRow(
                  box(dataTableOutput("DFTable"))),
                fluidRow(
                  box(
                    width = 12,
                    uiOutput("Sample_PTM_Barplot"),
                    uiOutput("PTM_Barplot"),
                    plotOutput("PTM_Graph"),
                    downloadButton("PTM_BP", "Download"))),
                fluidRow(
                  box(
                    title = "Differential Analysis Heatmap",
                    width = 12,
                    plotlyOutput("PTM_heatmaply")
                  ))),
            
        ####Protein Page####
        tabItem(tabName = "Data", h2("Protein"),
                fluidRow(
                  box(title =" Volcano Plot", plotlyOutput("Volcano"),                        
                      sliderInput("logFC_Sig", "LogFC Significance Threshhold", -4, 4, c(-2,2)),
                      sliderInput("P.Val_thresh", "P-Value Threshhold", 0, 1, 0.05),
                      radioButtons("P.Value","P-Value",c("P-Value","Adj P-Value"), inline = T),
                      downloadButton("VC_plot", "Download")),
                  tabBox(title = "Data",
                         tabPanel("All Data", 
                                  dataTableOutput("Datatable")),
                         tabPanel("Significant Data",
                                  dataTableOutput("Datatable_Sig")),
                         tabPanel("All Intensity Data",
                                  dataTableOutput("Datatable_I")),
                         tabPanel("Significant Intesnity Data",
                                  dataTableOutput("Datatable_I_Sig")))),
                fluidRow(
                  tabBox(width = 12,
                         tabPanel("Heatmap Significant Data", radioButtons("scl2", "Choose Scale",c('none','column','row'), inline = T), plotlyOutput("Heatmap_Sig"), downloadButton("HM_Sig", "Download")),
                         tabPanel("Heatmap all", radioButtons("scl1", "Choose Scale",c('none','column','row'), inline = T), plotlyOutput("Heatmap_all"), downloadButton("HM", "Download")),
                         tabPanel("Color Options", 
                                  colourInput("High_colour", label = "Choose High Intensity Color", value = "red"),
                                  colourInput("Mid_colour", label = "Choose Mid Intensity Color", value = "white"),
                                  colourInput("Low_colour", label = "Choose Low Intensity Color", value = "green"))
                         # tabPanel("Color Otions", radioButtons("CLRS", "Select a Color Pallet", choices = c(rownames(brewer.pal.info))))
                         ))),
        ####PTM Upload Page####
        tabItem(tabName = "Upload1", h2("PTM Upload"),
                fluidRow(
                  box( width = 3,
                       fileInput("file1", "Choose CSV File", multiple = F, accept = c("text/csv","text/comma-seperated-values,text/plain",".csv")),
                       tags$hr(),
                       checkboxInput("header","Header",T),
                       radioButtons("sep", "Seperator", choices = c(Comma = ",", 
                                                                    Semicolon =";",
                                                                    Tab = "\t"),
                                    selected = ","),
                       radioButtons("quote", "Quote", 
                                    choices = c(None = "",
                                                "Double Quote" = '"',
                                                "Single Quote" = "'"),
                                    selected = '"'),
                       tags$hr(),
                       radioButtons("disp", "Display",
                                    choices = c(Head = "head",
                                                All = "all"),
                                    selected = "head"),
                       actionButton("PTM_Upload_Bttn", "Upload")
                  ),
                  box(width = 9, tableOutput("contents"))),
                fluidRow(
                  box(title = "Edit", textInput('name_control', label =h3("Name of Control"), value = "Control"),textInput('name_treatment', label = h3("Name of Treatment"), value = "Treatment") ,rHandsontableOutput("AB"),
                      actionButton("Update", label = "Update Data")),
                  box(dataTableOutput("AA")))),
        
        ####Protein Upload Page####
        tabItem(tabName = "Upload2", h2("Protein Upload"),
                fluidRow(
                  box( width = 3,
                       fileInput("file2", "Choose CSV File", multiple = F, accept = c("text/csv","text/comma-seperated-values,text/plain",".csv")),
                       tags$hr(),
                       checkboxInput("header2","Header",T),
                       radioButtons("sep2", "Seperator", choices = c(Comma = ",", 
                                                                    Semicolon =";",
                                                                    Tab = "\t"),
                                    selected = ","),
                       radioButtons("quote2", "Quote", 
                                    choices = c(None = "",
                                                "Double Quote" = '"',
                                                "Single Quote" = "'"),
                                    selected = '"'),
                       tags$hr(),
                       radioButtons("disp2", "Display",
                                    choices = c(Head = "head",
                                                All = "all"),
                                    selected = "head")),
                  
                  box(width = 9, tableOutput("contents2"))),
                fluidRow(
                  box(title = "Edit2", rHandsontableOutput("Protein_HOT"),
                      actionButton("Update_PHOT", label = "Update Data"),
                      textOutput("Protein_Text")),
                  box(dataTableOutput("PHOT_DT"))))
        ######

        )
      )
    )
  )

###############

####Shiny Server####
server <- function(input, output){
  ####PTM####
  ####Creates an Uplaoding feature for the PTM Upload####
  output$contents <- renderTable({
    req(input$file1)
    tryCatch({
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
    },
    error = function(e){
      stop(safeError(e))
    })
    if(input$disp == "head"){
      return(head(df))
    }
    else{
      return(df)
    }
  })  
  ####Places the uplaod into an object called Raw_Data()####
  Raw_Data <- reactive({
    req(input$file1, input$header, file.exists(input$file1$datapath))
    read.csv(input$file1$datapath, header = input$header)
  })  
  ####Creates a Dataframe that will be used to edit the Raw_data() dataframe####
  DF1 <- reactive({data.frame(File.Name = unique(Raw_Data()$MS.MS_sample_name),
                              Sample = letters[1:length(unique(Raw_Data()$MS.MS_sample_name))],
                              Replicate = 1:length(unique(Raw_Data()$MS.MS_sample_name)),
                              Experimental_Group = factor(c("Control", "Treatment"), labels = c(input$name_control, input$name_treatment)),
                              stringsAsFactors = F)})
  
  output$AC <- renderTable({
    req(Raw_Data())
    Raw_Data()
  })
    
  ###Creates the handsontable where DF1() will be edited####
  DF2 <- reactive({
    req(DF1())
    rhandsontable(DF1()) %>% hot_col("File.Name", readOnly = T) %>% hot_col("Sample", type = "autocomplete")})
    
  ####Changes the handsontable back into a dataframe####
  DF3 <- eventReactive(input$Update, {hot_to_r(input$AB)})
    
  ####Outputs the Rhandsontable ####
  output$AB<- renderRHandsontable({DF2()})
  
  output$AA <- renderDataTable({
    DF3()})
  
  Blank <- function(x){
    
    print(colnames(x))
    print(colnames(DF3()))
    
    for(i in 1:length(unique(x$MS.MS_sample_name))){
      HD <- x[x$MS.MS_sample_name %in% DF3()$File.Name[i],]
      HD$Replicate <- DF3()$Replicate[i]
      HD$MS.MS_sample_name <- mapvalues(HD$MS.MS_sample_name, from = HD$MS.MS_sample_name[1], to = as.character(DF3()$Sample[i]))
      HD$Treatment <- DF3()$Experimental_Group[i]
      if(!exists("Histone_data")){
        Histone_data <- HD
      } else {
        Histone_data <- rbind(Histone_data, HD)
      }
    }
    
    print(colnames(Histone_data))
    print(Histone_data$PTM_corrected[1])
    
    PTMs <- data.frame(matrix(ncol=2, nrow = length(Histone_data$PTM_residues)))
    for( i in 1: length(Histone_data$PTM_corrected)){
      PTMs[i,] <- t(matrix(unlist(strsplit(as.character(Histone_data$PTM_corrected[i]), split= ":" ))))
    }
    colnames(PTMs) <- c("Residue", "PTM")
    PTMs$Histone <- factor(PTMs$Residue)
    
    Histone <- data.frame(matrix(ncol=2, nrow = length(Histone_data$PTM_corrected)))
    for( i in 1: length(Histone_data$PTM_corrected)){
      Histone[i,] <- t(matrix(unlist(strsplit(as.character(Histone_data$Protein_name[i]), split= "OS=" ))))
    }
    colnames(Histone) <- c("Histone2", "Ignore")

    
    Histone_data <- cbind(Histone_data,PTMs,Histone)
    Histone_data2 <- data.frame(Histone_data$MS.MS_sample_name, Histone_data$Treatment, Histone_data$Replicate,Histone_data$Histone2, Histone_data$Residue,
                                Histone_data$PTM, Histone_data$Intensity, Histone_data$Total_intensity, Histone_data$Abundance, Histone_data$betaValue, Histone_data$MValue)
    colnames(Histone_data2) <- c("Sample", "Treatment", "Replicate", "Histone", "PTM Residue", "PTM", "Intensity","Total Inensity", "Abundance",
                                 "Beta Value", "M Value")

    
    return(Histone_data2)
  }

  
  Histone_PTM2 <- reactive({Blank(Raw_Data())})
  
  output$AD <-renderUI({bscols(datatable(Histone_PTM2(), rownames = FALSE, width = "99%", height = 500, extensions = c('Scroller', 'Buttons'),
                                         options = list( scrollY = 400,  scroller = TRUE,  dom = "Bfrtip",
                                                         buttons = c('copy', 'csv', 'pdf'))) %>% formatRound(3, 1) %>% formatSignif( 4:5))})  
  ####Picker input####
  output$picker1 <- renderUI({pickerInput("Sample","Sample", choices=levels(factor(Histone_PTM2()$Sample)), options = list(`actions-box` = TRUE),multiple = T)})
  output$picker2 <- renderUI({pickerInput("Replicate","Replicate", choices=levels(factor(Histone_PTM2()$Replicate)), options = list(`actions-box` = TRUE),multiple = T)})
  output$picker3 <- renderUI({pickerInput("Histone","Histone", choices=levels(factor(Histone_PTM2()$Histone)), options = list(`actions-box` = TRUE),multiple = T)})
  output$picker4 <- renderUI({pickerInput("PTM_Residue","PTM Residue", choices=levels(factor(Histone_PTM2()$`PTM Residue`)), options = list(`actions-box` = TRUE),multiple = T)})
  
  output$Dif_Anal_choice1 <- renderUI({selectInput("Dif_Anal_choice1","Dif_Anal_choice1", choices=levels(factor(Histone_PTM2()$Sample)))})
  
  # output$Sample_PTM_Barplot <- renderUI({selectInput("Sample_PTM_Barplot", "Sample Selction", choices = levels(factor(Histone_PTM2()$Sample)))})
  output$Sample_PTM_Barplot <- renderUI({pickerInput("Sample_BP","Sample Selection", choices=levels(factor(Histone_PTM2()$Sample)), options = list(`actions-box` = TRUE),multiple = T)})
  
  output$PTM_Barplot <- renderUI({selectInput("PTM_Barplot", "Histone Selction", choices = levels(factor(Histone_PTM2()$Histone)))})
   
  ####Histone_PTM Table####
  PTM_filter <- function(x){
    
      data1 <- x
      
      if(isTruthy(input$Sample)){
        data1 <- data1[data1$Sample %in% input$Sample,]
      }
      if(isTruthy(input$Replicate)){
        data1 <- data1[data1$Replicate %in% input$Replicate,]
      }
      if(isTruthy(input$Histone)){
        data1 <- data1[data1$Histone %in% input$Histone,]
      }
      if(isTruthy(input$PTM_Residue)){
        data1 <- data1[data1$`PTM Residue` %in% input$PTM_Residue,]
      }
      
     return(data1)
  }
  

  output$Histone_data_table <- renderDataTable(PTM_filter(Histone_PTM2()), server = F,rownames = F,extensions = c('Buttons','FixedColumns', 'Scroller'),
                                      options = list(scroller = T, scrollY = 400, scrollX=T, fixedColumns = T , dom = "Bfrtip", buttons = c('copy', 'csv', 'pdf')))
 
  ####Differential Analysis####
  differentialAnalysis <- function(df, Smple){
    # creating additional required varibles
    
    df$groups = paste(df$Sample, df$Replicate, df$Treatment, sep = "_")
    df$fullPTM = paste(df$Histone, df$'PTM Residue', df$PTM, sep = " ")
    
    
    # exclude unmodified
    df = df[!grepl("Unmodified", df$PTM),]
    
    # limit to samples of interest
    df = df[df$Sample %in% Smple,]

    # create beta matrix like structur
    beta = matrix(NA, nrow = length(unique(df$fullPTM)), ncol = length(unique(df$groups)), dimnames = list(unique(df$fullPTM), unique(df$groups)))
    for(i in unique(df$groups)){
      beta[df[df$groups == i,]$fullPTM, i] = df[df$groups == i,]$'Beta Value'
    }

    # defining treatments per sample
    treatmentDesign = NULL
    for(i in 1:ncol(beta)){
      treatmentDesign[i] = df$Treatment[colnames(beta)[i] == df$groups][1]
    }
    
    
    beta = cbind(beta, fullPTM = paste( "k", unlist(lapply(strsplit(rownames(beta), "k"), tail, n = 1)), sep = ""), histone = trimws(unlist(lapply(strsplit(rownames(beta), "k"), head, 1))))
    
    duplicates = any(duplicated(beta[,colnames(beta) != "histone"]))
    while(duplicates){
      identifyer = NULL
      for(i in 1:nrow(beta)){
        if(class(beta[duplicated(beta[,colnames(beta) != "histone"]),colnames(beta) != "histone"]) == "matrix")
          identifyer[i] = identical(beta[i,colnames(beta) != "histone"], beta[duplicated(beta[,colnames(beta) != "histone"]),colnames(beta) != "histone"][1,])
        if(class(beta[duplicated(beta[,colnames(beta) != "histone"]),colnames(beta) != "histone"]) == "character")
          identifyer[i] = identical(beta[i,colnames(beta) != "histone"], beta[duplicated(beta[,colnames(beta) != "histone"]),colnames(beta) != "histone"])
      }
      removal = beta[identifyer,]
      beta = beta[!identifyer,]
      combinedHistones = str_replace(removal[,"histone"], "Histone ", "")
      combinedHistones = paste("Histone ", paste(combinedHistones[order(combinedHistones)], collapse = "|"), sep = "")
      removal[,"histone"] = combinedHistones
      rownames(removal) = rep(paste(combinedHistones, removal[1,"fullPTM"], sep = " "), nrow(removal))
      addition = removal[1,]
      beta = rbind(beta, addition)
      rownames(beta)[nrow(beta)] = rownames(removal)[1]
      
      duplicates = any(duplicated(beta[,colnames(beta) != "histone"]))
    }
    
    
    beta = beta[,!colnames(beta) %in% c("fullPTM", "histone")]
    
    class(beta) = "numeric"
    
    
    # differential analysis
    mvals = log2(beta / (1-beta))
    design = model.matrix(~ treatmentDesign)
    limmaFit = lmFit(mvals, design)
    pval = eBayes(limmaFit)$p.value[,2]
    fdr = p.adjust(pval, method = "fdr")
    results = cbind(beta, pVal = pval, FDR = fdr)
    results = round(results[order(results[,"pVal"]),],3)
    
    return(results)
  }
  
  Diff_Anal_Table <- reactive({differentialAnalysis(Histone_PTM2(), input$Dif_Anal_choice1)})
   
  ####Differential Analysis Chart####
  # output$DFTable <-renderUI({bscols(datatable(as.data.frame(Diff_Anal_Table()), rownames = T, width = "99%", height = 500, extensions = c('Scroller', 'Buttons','FixedColumns'),
  #                                                          options = list( scrollY = 400,  scroller = TRUE,  dom = "Bfrtip", fixedColumns = TRUE,
  #                                                                          buttons = c('copy', 'csv', 'pdf', 'colvis'))))})
  output$DFTable <- renderDataTable(as.data.frame(Diff_Anal_Table()), server = F,rownames = T,extensions = c('Buttons','FixedColumns', 'Scroller'),
                                               options = list(scroller = T, scrollY = 400, scrollX=T, fixedColumns = T , dom = "Bfrtip", buttons = c('copy', 'csv', 'pdf')))
  
  
  # output$DFTable <-renderDataTable({as.data.frame(Diff_Anal_Table())})
  
 
  ####Creating Bar Graphs for Histone PTMS####
  
  
  observeEvent(input$tst,{
    print(input$Sample_BP)
  })
  
  
  
  Protein_Graph_Table <- reactive({Histone_PTM2() %>% filter(Histone %in% input$PTM_Barplot)})
  
  P_Graph_Prep <- function(z,Tissue){

    z$PTM = trimws(z$PTM)
    
    aggregatedDF = aggregate(x = z$'Beta Value', FUN = mean, 
                             by = list(tissue = z$Sample,
                                       treatment = z$Treatment, 
                                       position = z$'PTM Residue', 
                                       modification = z$PTM))
    colnames(aggregatedDF)[colnames(aggregatedDF) =="x"] = "Mean Beta-Value"
    temp = aggregatedDF[aggregatedDF$tissue %in% c(Tissue),]
    temp$sample <- paste(temp$tissue, temp$treatment, sep = "_")
    temp$modification = factor(temp$modification, levels = c("Acetyl", "Methyl", "Dimethyl", "Trimethyl", "Unmodified"))
    temp$position = factor(temp$position, levels = unique(temp$position)[order(as.numeric(substring(unique(temp$position),2)))])
    
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    
    modificationList = unique(temp$modification)
    clrs = gg_color_hue(5)
    names(clrs) = modificationList
    
    PTM_barplot <- ggplot(temp, aes(x = sample, y = `Mean Beta-Value`, fill = modification)) +
      geom_bar(stat = 'identity', position = 'fill') + facet_grid(~ position) +
      scale_fill_manual("Legend", values = clrs) +
      scale_y_continuous(labels = scales::percent_format()) +
      ggtitle(input$PTM_Barplot) + theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(angle = 45, hjust = 1))

    return(PTM_barplot)
  }
  
  BGraph_PTM <- reactive({P_Graph_Prep(Protein_Graph_Table(),input$Sample_BP)})
  
  
  output$PTM_Graph <- renderPlot({
    req(BGraph_PTM())
    BGraph_PTM()
    })
  
  ####Heatmap for DFA####
  
  output$PTM_heatmaply <- renderPlotly({
    a <- Diff_Anal_Table()[,c(1:(length(colnames(Diff_Anal_Table()))-2))]
    heatmaply(scale(a), colors = colorRampPalette(brewer.pal(11, "BrBG")),Rowv = FALSE, Colv = F)
  })
  
  output$PCA <- renderPlot({
    autoplot(prcomp(Normal_PData()), shape = FALSE,  label.size = 4) + theme_classic()
  })
  
  
  ####Protein####
  ####Creates an Upload feature for the Protein Upload####
  output$contents2 <- renderTable({
    req(input$file2)
    tryCatch({
      df <- read.csv(input$file2$datapath,
                     header = input$header2,
                     sep = input$sep2,
                     quote = input$quote2)
    },
    error = function(e){
      stop(safeError(e))
    })
    if(input$disp2 == "head"){
      return(head(df))
    }
    else{
      return(df)
    }
  })
    
  ####Places the upload into an object called Raw_Data()####
  Raw_Protein <- reactive({
    req(input$file2, input$header2, file.exists(input$file2$datapath))
    read.csv(input$file2$datapath, header = input$header2)
  })
  
  Process_Protein_data <- function(x){
    
    df <- x %>%
      
      mutate(Description = str_extract(Fasta.headers, "(?<= )[^\\|]+(?= OS\\=)"),
             
             Gene_name = str_extract(Fasta.headers, "(?<=GN\\=)[^\\|]+(?= PE\\=)"),
             
             Uniprot_ID = str_extract(Fasta.headers, "(?<=\\|)[^\\|]+(?=\\|)")) %>%
      na.omit()
    
    df1 <- data.frame(Fasta.headers = df$Fasta.headers, Uniprot_ID = df$Uniprot_ID, Gene_ID = df$Gene_name, Description = df$Description)
    
    df2 <- merge(x = df1, x)
    
    Protein_data_final <- df2[,-1]

    
    return(Protein_data_final)
  }
  
  
  
  #### Creates a Hands on table for protein data####
  
  DFP1 <- reactive({data.frame(File.Name = colnames(Raw_Protein()[,-1]),
                              Sample = letters[1:length(colnames(Raw_Protein()[,-1]))],
                              Replicate = 1:length(colnames(Raw_Protein()[,-1])),
                              Experimental.Group = factor(c("Control", "Treatment")),
                              stringsAsFactors = F)})
  
  DFP2 <- reactive({
    req(DFP1())
    rhandsontable(DFP1()) %>% hot_col("File.Name", readOnly = T) %>% hot_col("Sample", type = "autocomplete")})
  
  output$Protein_HOT<- renderRHandsontable({DFP2()})
  
  DFP3 <- eventReactive(input$Update_PHOT, {hot_to_r(input$Protein_HOT)})
  
  output$PHOT_DT <- renderDataTable({
    DFP3()})
  
  ####Create a values only DF####
  
  Values_only <- function(x){
    VO <- x[,-c(1:3)]
    colnames(VO) <- paste(DFP3()$Sample, DFP3()$Replicate, sep = "_")
    rownames(VO) <- make.names(names = x[,1], unique = T)
    return(VO)
  } 

  ####Function for Median Normalizing the dataset####
  Median_Normalization <- function(x){
    VO1 <-log1p(x)
    
    colMedians <- colMedians(as.matrix(VO1))
    Max_Median <- max(colMedians)
    adjusted_Medians <- Max_Median/colMedians
    DF_norm_VO <- VO1
    
    for(i in 1:length(adjusted_Medians)){
      DF_norm_VO[,i] <- adjusted_Medians[i]*VO1[,i]
    }
    
    return(DF_norm_VO)
  }
  
   
  ####LIMMA####
  LIMMA <- function(x){
    limma_data_frame <- x
    DFP3 <- DFP3()
    print("Error 1")
    
    print(colnames(DFP3))
    print(DFP3)
    print(DFP3[,4])
    print(DFP3$Replicate)
    print(DFP3$Experimental.Group)
    print("end")
    
    vec <- DFP3$Experimental.Group
    

    group <- factor(vec, levels=c("Control", "Treatment"))
    print(group)
    
    design = model.matrix(~0 + group)
    colnames(design) <- levels(group)
    print(levels(group))
    print("Error 2")
    
    contr.matrix = makeContrasts(
      ControlvsTreatment =  Treatment - Control,
      levels = colnames(design))
    
    fit <- lmFit(limma_data_frame, design)
    print("Error 3")
    
    fit_contr <- contrasts.fit(fit, contr.matrix)
    fit_eBayes <- eBayes(fit_contr)
    
    results.coef1 <- topTable(fit_eBayes, coef=1, number=Inf, sort.by="none")
    results.coef1 <- cbind(Proteins = rownames(results.coef1), results.coef1)
    print("It works")
    return(results.coef1)
  }
  
  ####Actual work####
  
  Protein_Data <- reactive({Process_Protein_data(Raw_Protein())})
  Protein_Data_Values_Only <- reactive({Values_only(Protein_Data())})
  Normal_PData <- reactive({Median_Normalization(Protein_Data_Values_Only())})
  results.coef1 <- reactive({LIMMA(Normal_PData())})
  results.coef1_Sig <- reactive({results.coef1()[results.coef1()$logFC >= input$logFC_Sig[2] | results.coef1()$logFC <= input$logFC_Sig[1] & 
                                                   if(input$P.Value == "P-Value"){results.coef1()$P.Value < input$P.Val_thresh} else {results.coef1()$adj.P.Val < input$P.Val_thresh},]
    })
  
  Sorting_Significant <- function(x,y){
    Sig_results <- x[x$logFC >= input$logFC_Sig[2] | x$logFC <= input$logFC_Sig[1] &
                       if(input$P.Value == "P-Value"){results.coef1()$P.Value < input$P.Val_thresh} else {results.coef1()$adj.P.Val < input$P.Val_thresh},]
    Normal_int_data <- as.data.frame(y)
    Significant_int_data <- Normal_int_data[rownames(Normal_int_data) %in% rownames(Sig_results),]
    return(Significant_int_data)
  }
  
  Significant <- reactive({Sorting_Significant(results.coef1(), Normal_PData())})
 
   
  ####Outputs the Dashboard Raw and Normalized histogram####
  raw_hist <- reactive({
     ggplot(melt(Protein_Data_Values_Only()), aes(x = value)) + 
        geom_histogram(col = input$hist_border_colour,fill = input$hist_fill_colour) + 
        xlab(input$hist_xlab) + ylab(input$hist_ylab) + ggtitle(input$hist_raw_title) + theme(plot.title = element_text(hjust = 0.5))
  })
  
  norm_hist <- reactive({
    ggplot(melt(Normal_PData()), aes(x = value)) + 
      geom_histogram(col = input$hist_border_colour,fill = input$hist_fill_colour, binwidth = input$hist_slider) + 
      xlab(input$hist_xlab) + ylab(input$hist_ylab) + ggtitle(input$hist_norm_title) + theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$Histogram_Raw <- renderPlot({
    req(raw_hist())
    raw_hist()
    })
  
  output$Histogram_Norm <- renderPlot({
    req(norm_hist())
    norm_hist()
  })
   
  ####Outputs the Dashboard Raw and Normalized Boxplot####
  raw_box <- reactive({
    ggplot(melt(Protein_Data_Values_Only()), aes(x=variable, y=value)) + geom_boxplot(col = input$box_border_colour,fill = input$box_fill_colour) + 
      xlab(input$box_xlab) + ylab(input$box_ylab) + ggtitle(input$box_raw_title) + theme(plot.title = element_text(hjust = 0.5))
  })
  
  norm_box <- reactive({
    ggplot(melt(Normal_PData()), aes(x=variable, y=value)) + geom_boxplot(col = input$box_border_colour,fill = input$box_fill_colour) + 
      xlab(input$box_xlab) + ylab(input$box_ylab) + ggtitle(input$box_norm_title) + theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$Boxplot_Raw <- renderPlot({
    req(raw_box())
    raw_box()
  })
  output$Boxplot_Norm <- renderPlot({
    req(norm_box())
    norm_box()
  })
  
  ####Outputs the Dashboard MDS and PCA Plots####
  output$PCA <- renderPlot({
    autoplot(prcomp(t(Normal_PData())), shape = FALSE,  label.size = 4) + theme_classic()
  })
  
  output$MDS <- renderPlot({
    plotMDS(Normal_PData(), labels = colnames(Normal_PData()), col=c(rep("blue",5), rep("red",5)))
  })
   
  ####Protein: Volcano Plot and Data####
  
  output$Volcano <- renderPlotly({
    results.coef1 <- results.coef1() %>% mutate_if(is.numeric, round, 3)
    key = results.coef1$Proteins
    
    
     ggplotly(
       ggplot(results.coef1, aes(x=logFC, y=-log10(P.Value), 
                                 color = ifelse(results.coef1$logFC >= input$logFC_Sig[2] | results.coef1$logFC <= input$logFC_Sig[1] & results.coef1$P.Value < input$P.Val_thresh, "Significant", "Not Significant"),
                                 text = paste("Protein :", results.coef1$Proteins, "\n", "Log Fold Change :", results.coef1$logFC ,"\n", "P Value :", ifelse(input$P.Value == "P-Value", results.coef1$P.Value, results.coef1$adj.P.Val)), key = key))+
      geom_point()+
      geom_vline(xintercept = c(input$logFC_Sig[1],input$logFC_Sig[2]), color = "black", linetype = "dashed")+
      geom_hline(yintercept = -log10(input$P.Val_thresh),color = "black", linetype ="dashed")+
      scale_color_manual(name = "Threshold",
                         values = c("Significant" = "red", "Not Significant" = "black"))+
      ylab(ifelse(input$P.Value == "P-Value",  "-log10(P-Value)", "-log10(Adj P-Value)")), tooltip = "text")
    
     # ggplotly(ggplot(as.data.frame(VC(results.coef1())), aes(x=results.coef1()$logFC, y=-log10(results.coef1()$P.Value)))+
     #  geom_point())

  })
  
  

  
  output$Datatable <- renderDataTable({
    
    
    
    d <- event_data("plotly_selected")[,5]
    if (is.null(d)) d <-results.coef1()$Proteins else d
    
    results.coef1()[results.coef1()$Proteins %in% d,] %>% mutate_if(is.numeric, round, 3)}, server = F,rownames = F,extensions = c('Buttons','FixedColumns', 'Scroller')
                                      ,options = list(scroller = T, scrollY = 900, scrollX=T, fixedColumns = T , dom = "Bfrtip", buttons = c('copy', 'csv', 'pdf'))
    )
  
  output$Datatable_Sig <- renderDataTable(results.coef1_Sig() %>% mutate_if(is.numeric, round, 3), server = F,rownames = F,extensions = c('Buttons','FixedColumns', 'Scroller'),
                                      options = list(scroller = T, scrollY = 900, scrollX=T, fixedColumns = T , dom = "Bfrtip", buttons = c('copy', 'csv', 'pdf')))
  
  output$Datatable_I <- renderDataTable(
    
    as.data.frame(Normal_PData()) %>% mutate_if(is.numeric, round, 3), server = F,rownames = T,extensions = c('Buttons','FixedColumns', 'Scroller'),
                                      options = list(scroller = T, scrollY = 900, scrollX=T, fixedColumns = T , dom = "Bfrtip", buttons = c('copy', 'csv', 'pdf'))
    )
  
  output$Datatable_I_Sig <- renderDataTable(as.data.frame(Significant()) %>% mutate_if(is.numeric, round, 3), server = F,rownames = T,extensions = c('Buttons','FixedColumns', 'Scroller'),
                                      options = list(scroller = T, scrollY = 900, scrollX=T, fixedColumns = T , dom = "Bfrtip", buttons = c('copy', 'csv', 'pdf')))

  ####Protein: Heatmaps####
  #Heat maply
  output$Heatmap_all <- renderPlotly({
    heatmaply(t(Normal_PData()), scale = input$scl1 ,scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(high = input$High_colour, mid = input$Mid_colour, low = input$Low_colour))
  })
  
  output$Heatmap_Sig <- renderPlotly({
    heatmaply(t(Significant()), scale = input$scl2, scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(high = input$High_colour, mid = input$Mid_colour, low = input$Low_colour))
  })
   
  ####Downloads figures####
  output$H_raw = downloadHandler(
    filename = function(){paste("Raw Histogram", input$dnld_options, sep=".")},
    content = function(file){
      ggsave(file,plot=raw_hist())
      }
    )
  output$H_norm = downloadHandler(
    filename = function(){paste("Normalized Histogram", input$dnld_options, sep=".")},
    content = function(file){
      ggsave(file,plot=norm_hist())
    }
  )
  
  output$B_raw = downloadHandler(
    filename = function(){paste("Raw Boxplot", input$dnld_options, sep=".")},
    content = function(file){
      ggsave(file,plot=raw_box())
    }
  )
  
  output$B_norm = downloadHandler(
    filename = function(){paste("Normalized Boxplot", input$dnld_options, sep=".")},
    content = function(file){
      ggsave(file,plot=norm_box())
    }
  )
  
  output$PCA_dnld = downloadHandler(
    filename = function(){paste("PCA", input$dnld_options, sep=".")},
    content = function(file){
      ggsave(file,plot=autoplot(prcomp(Normal_PData()), shape = FALSE,  label.size = 4) + theme_classic())
    }
  )
  
  output$MDS_dnld = downloadHandler(
    filename = function(){paste("MDS", input$dnld_options, sep=".")},
    content = function(file){
      ggsave(file,plot=plotMDS(t(Normal_PData()), labels = rownames(Normal_PData()), col=c(rep(colorlist$blue,3), rep(colorlist$red,3))))
    }
  )
  
  output$PTM_BP = downloadHandler(
    filename = function(){paste("PTM Bar Graphs", input$dnld_options, sep=".")},
    content = function(file){
      ggsave(file, plot=BGraph_PTM())
    }
  )
  
  ####broken############
  # G_Volcano <- reactive({results.coef1()})
  # G_Volcano()$Sig<- reactive({ifelse(G_Volcano()$Proteins %in% results.coef1_Sig()$Proteins, "Significant", "Not Significant")})
  
  output$VC_plot = downloadHandler(
    filename = function(){paste("Volcano Plot", input$dnld_options, sep=".")},
    content = function(file){
      G_Volcano <- results.coef1()
      G_Volcano$Sig<- ifelse(G_Volcano$Proteins %in% results.coef1_Sig()$Proteins, "Significant", "Not Significant")
      ggsave(file,plot=ggplot(G_Volcano, aes(x=logFC, y = -log10(adj.P.Val), colour = Sig))+geom_point()+scale_colour_manual(values = c("black", "red")))
    }
  )
  
  observeEvent(input$HM_Sig2,{
    print("This Works")
    tmp <- heatmaply(Significant(), scale = input$scl2, colors = colorRampPalette(brewer.pal(11, "BrBG")), file = "folder/heatmaply_plot.pdf")
    rm(tmp)
  })
  
  output$HM = downloadHandler(
    filename = function(){paste("Heatmap All Values", input$dnld_options, sep=".")},
    content = function(file){
      ggsave(file,plot=d3heatmap(Normal_PData(), scale = "column", color = input$CLRS))
    }
  )
  
  # output$HM_Sig = downloadHandler(
  #   filename = function(){paste("Heatmap Significant values", input$dnld_options, sep=".")},
  #   content = function(file){
  #     heatmaply(Significant(), scale = input$scl2, colors = colorRampPalette(brewer.pal(11, "BrBG")), file = "folder/heatmaply_plot.pdf")
  #   }
  # )
  ###################
  
} 

###############
shinyApp(ui = ui, server = server)

