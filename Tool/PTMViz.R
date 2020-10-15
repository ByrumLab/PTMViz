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

if (!require(ggplot2)) install.packages(
  "ggplot2",
  repos = c("http://rstudio.org/_packages",
            "http://cran.rstudio.com")
)
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

# if (!require(heatmaply)) install.packages('heatmaply')
# library(heatmaply)

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


library(scales)



#### Shiny App####
options(stringsAsFactors = F)
####Shiny UI####
ui <-shinyUI(
  ####Creates the general format####
  dashboardPage(
    dashboardHeader(title = "PTMViz"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Protein", tabName = "Protein",
                 menuSubItem("Protein Upload", tabName = "Upload2"),
                 menuSubItem("Preliminary Analysis", tabName = "Preliminary"),
                 menuSubItem("Protein Analysis", tabName = "Data")),
        menuItem("PTM", tabName = "PTM",
                 menuSubItem("PTM Upload", tabName = "Upload1"),
                 menuSubItem("PTM Analysis", tabName = "PTM2")
        ),
        menuItem("Figure Settings", tabName = "Figure_Settings")
        # actionBttn("tst", "Test")
      )),
    dashboardBody(
      tabItems(
        ####Preliminary Protein Data ####
        tabItem(tabName = "Figure_Settings",
                fluidPage(
                  fluidRow(
                    radioButtons("dnld_options", "Select Download File Type", choices = list("png", "pdf", "jpeg", "tiff", "bmp", "svg"), inline = T),
                    radioButtons("unit", "Select Unit", choices = list("in", "cm", "mm"), inline = T),
                    numericInput("width", label = "Figure width", value = 10),
                    numericInput("height", label = "Figure Heigt", value = 5),
                    numericInput("dpi", label = "dpi", value = 300)
                    
                    )
                )
                ),
        tabItem(tabName = "Preliminary", 
                fluidRow(
                  tabBox(title = "Histogram", 
                         tabPanel("Histogram", 
                                  plotOutput("Histogram_Raw"),
                                  downloadButton("HS_DNLD", "Download")), 

                         tabPanel("Settings", 
                                  fluidPage(
                                                   fluidRow(
                                                    column(6, 
                                                           textInput("hist_raw_title", label="Histogram Title", value = "Histogram")), 
                                                    column(6, 
                                                           numericInput("hist_title_size", label = "Title Size", value = 18))),
                                                   fluidRow(
                                                    column(6,
                                                           textInput("hist_xlab", label ="X-axis Label", value = "Counts")),
                                                    column(6,
                                                           numericInput("hist_xlab_size", label = "X-Axis Text Size", value = 12))),
                                                   fluidRow(
                                                    column(6,
                                                           textInput("hist_ylab", label ="Y-axis Label", value = "Intensity")),
                                                    column(6,
                                                           numericInput("hist_ylab_size", label = "Y-Axis Text Size", value = 12))),
                                            
                                                    colourInput("hist_fill_colour", label = "Choose Fill color", value = "red"),
                                                    colourInput("hist_border_colour", label = "Choose Border color", value = "black"),
                                                    sliderInput("hist_slider", label = "Binwidth", min = 1, max = 10, value = 2)))),
                  tabBox(title = "Boxplot", 
                         tabPanel("Boxplot", 
                                  plotOutput("Boxplot_Raw"),
                                  downloadButton("BX_DNLD", "Download")),

                         tabPanel("Settings", 
                                  fluidPage(
                                    fluidRow(
                                      column(6,
                                             textInput("box_raw_title", label ="Boxplot Title", value = "Boxplot")),
                                      column(6,
                                             numericInput("box_title_size", label = "Title Size", value = 18))),
                                    fluidRow(
                                      column(6,
                                             textInput("box_xlab", label ="X-axis Label", value = "Custom ID")),
                                      column(6,
                                             numericInput("box_xlab_size", label = "X-Axis Title Size", value = 12))),
                                    fluidRow(
                                      column(6
                                             # textInput("box_xtext", label ="Y-axis Label")),
                                      ),
                                      column(6,
                                             numericInput("box_xtext_size", label = "X-Axis Text Size", value = 12))),
                                  colourInput("box_fill_colour", label = "Choose Fill color", value = "red"),
                                  colourInput("box_border_colour", label = "Choose Border color", value = "black"))))),
                fluidRow(
                  tabBox(tabPanel("PCA", 
                                  plotOutput("PCA"),
                                  downloadButton("PCA_DNLD", "Download")),
                         tabPanel("MDS", 
                                  plotOutput("MDS"),
                                  downloadButton("MDS_DNLD", "Download"))))),
        
        
        ####PTM Page####
        tabItem(tabName = "PTM2", h1("Post Translational Modification"),
                fluidRow(
                  # box( width = 3, uiOutput("picker1"), uiOutput("picker2"), uiOutput("picker3"), uiOutput("picker4")
                  #      ),
                  box(
                    width = 12, 
                    title= "Histone_data_table", 
                    fluidRow(
                      column(width =3, uiOutput("picker1")),
                      column(width =3, uiOutput("picker2")),
                      column(width =3, uiOutput("picker3")),
                      column(width =3, uiOutput("picker4"))),
                    dataTableOutput("Histone_data_table"))),
                fluidRow(
                  tabBox( title = "Global PTM Values", width = 12, 
                          tabPanel("PTM Values",
                                   fluidRow(column(width = 3, uiOutput("Sample_PTM_Barplot")),
                                            column(width = 3, uiOutput("PTM_Residue_BP")),
                                            column(width = 3, uiOutput("PTM_Trt")),
                                            column(width = 3, uiOutput("PTM_Barplot"))),
                                   checkboxInput("PTM_mean_chkbx", label = "View Individual Samples", value = F),
                                   plotOutput("PTM_Graph", height = 600),
                                   downloadButton("PTM_DNLD", "Download")),
                          tabPanel("Settings",
                                   textInput("PTM_BP_title_text", label ="Barchart Title", value = "Histone"),
                                   numericInput("PTM_BP_axis_txt", label = "Input axis text size", value = 12),
                                   numericInput("PTM_BP_yaxis_title", label = "Input y axis title size", value = 12),
                                   numericInput("PTM_BP_title", label = "Input title size", value = 18),
                                   numericInput("PTM_BP_residue_size", label = "Input residue text size", value = 12)
                                   
                          )
                  )
                ),
                
                
                fluidRow(
                  tabBox(title = "Differential Analysis", width = 12,
                         tabPanel("Choose Data for Differential Analysis",
                                  uiOutput("Dif_Anal_choice1"),
                                  uiOutput("Dif_Anal_choice2")
                                  ), 
                         tabPanel("Differential Analysis Table",
                                  
                                  dataTableOutput("DFTable")),
                         tabPanel("Differential Analysis Heatmap",
                                  title = "Differential Analysis Heatmap",
                                  plotlyOutput("PTM_heatmaply"),
                                  downloadButton("DA_DNLD", "Download")),
                         tabPanel("Settings",
                                  fluidPage(
                                    fluidRow(
                                      column(4,colourInput("High_colour_DA", label = "Choose High Value Color", value = "red")),
                                      column(4,colourInput("Mid_colour_DA", label = "Choose Mid Value Color", value = "white")),
                                      column(4,colourInput("Low_colour_DA", label = "Choose Low Value Color", value = "green"))),
                                    fluidRow(
                                      column(4,colourInput("Missing_colour_DA", label = "Choose Missing Value Color", value = "white"))),
                                    fluidRow(
                                      column(6,radioButtons("scl3", "Scale",c('On','Off'), inline = T)),
                                      column(6,radioButtons("nmbr2", "Tile Numeric Values",c('On','Off'), inline = T))
                                    ),
                                    fluidRow(
                                      column(6,
                                             textInput("DA_title", label ="Heat Map Title", value = "Heat Map")),
                                      column(6,
                                             numericInput("DA_title_size", label = "Title Size", value = 12))),
                                    fluidRow(
                                      column(6),
                                      column(6,
                                             numericInput("DA_xlab_size", label = "X-Axis Label Size", value = 8))),
                                    fluidRow(
                                      column(6),
                                      column(6,
                                             numericInput("DA_ylab_Size", label = "Y-Axis Label Size", value = 12)))
                                  )
                                  )
                         )
                  )
        ),
        
        ####Protein Page####
        tabItem(tabName = "Data", h2("Protein"),
                fluidRow(
                  tabBox(title = "Data", width = 12,
                         tabPanel("All Data", 
                                  dataTableOutput("Datatable")),
                         tabPanel("Significant Data",
                                  dataTableOutput("Datatable_Sig")),
                         tabPanel("All Intensity Data",
                                  dataTableOutput("Datatable_I")),
                         tabPanel("Significant Intesnity Data",
                                  dataTableOutput("Datatable_I_Sig")))),
                fluidRow(
                  tabBox( width = 12, title =" Volcano Plot",
                          tabPanel("Volcano Plot",
                                   plotlyOutput("Volcano"),
                                   downloadButton("VC_DNLD", "Download")), 
                          tabPanel("Settings",
                                   fluidPage(
                                   fluidRow(
                                     column(width = 4, sliderInput("logFC_Sig", "LogFC Significance Threshhold", step = .25,-4, 4, c(-2,2))),
                                     column(width = 4, sliderInput("P.Val_thresh", "P-Value Threshhold", 0, 1, 0.05)),
                                     column(width = 4, radioButtons("P.Value","P-Value",c("P-Value","Adj P-Value"), inline = T))),
                                   fluidRow(
                                     column(6, colourInput("VC_Sig_color", label = "Choose Significant Color", value = "red")),
                                     column(6, colourInput("VC_notSig_color", label = "Choose Not Significant Color", value = "black"))),
                                   fluidRow(
                                     column(6,
                                            textInput("VC_title", label ="Volcano Plot Title", value = "Volcano Plot")),
                                     column(6,
                                            numericInput("VC_title_size", label = "Title Size", value = 12))),
                                    fluidRow(
                                     column(6,
                                            textInput("VC_xlab", label ="X-Axis Label", value = "Log Fold Change")),
                                     column(6,
                                            numericInput("VC_xlab_size", label = "X-Axis Label Size", value = 12))),
                                  fluidRow(
                                     column(6
                                            # textInput("VC_ylab", label ="Y-Axis Label Title", value = "-log(P-Value")),
                                     ),
                                     column(6,
                                            numericInput("VC_ylab_Size", label = "Y-Axis Label Size", value = 12)))
                                   )
                                   )
                                   )
                          ),
                fluidRow(
                  tabBox(width = 12,
                         # tabPanel("Heatmap Significant Data", radioButtons("scl2", "Choose Scale",c('none','column','row'), inline = T), plotlyOutput("Heatmap_Sig"), downloadButton("HM_Sig", "Download")),
                         tabPanel("Heatmap Significant Data", plotlyOutput("Heatmap_Sig"), downloadButton("HM_DNLD", "Download")),
                         # tabPanel("Heatmap all", radioButtons("scl1", "Choose Scale",c('None','Scaled'), inline = T), plotlyOutput("Heatmap_all"), downloadButton("HM", "Download")),
                         tabPanel("Color Options", 
                                  fluidPage(
                                    fluidRow(
                                      column(4,colourInput("High_colour", label = "Choose High Value Color", value = "red")),
                                      column(4,colourInput("Mid_colour", label = "Choose Mid Value Color", value = "white")),
                                      column(4,colourInput("Low_colour", label = "Choose Low Value Color", value = "green"))),
                                    fluidRow(
                                      column(6,radioButtons("scl2", "Scale",c('On','Off'), inline = T)),
                                      column(6,radioButtons("nmbr", "Tile Numeric Values",c('On','Off'), inline = T))
                                    ),
                                    fluidRow(
                                      column(6,
                                             textInput("HM_title", label ="Heat Map Title", value = "Heat Map")),
                                      column(6,
                                             numericInput("HM_title_size", label = "Title Size", value = 12))),
                                    fluidRow(
                                      column(6,
                                             textInput("HM_xlab", label ="X-Axis Label", value = "Gene Ids")),
                                      column(6,
                                             numericInput("HM_xlab_size", label = "X-Axis Label Size", value = 12))),
                                    fluidRow(
                                      column(6,
                                             textInput("HM_ylab", label ="Y-Axis Label Title", value = "Sample")),
                                      column(6,
                                             numericInput("HM_ylab_Size", label = "Y-Axis Label Size", value = 12)))
                                  )
                                  )
                         # tabPanel("Color Otions", radioButtons("CLRS", "Select a Color Pallet", choices = c(rownames(brewer.pal.info))))
                  ))),
        ####PTM Upload Page####
        tabItem(tabName = "Upload1", h2("PTM Upload"),
                fluidRow(
                  box( width =12,
                       column( width = 3,
                               fileInput("file1", "Choose CSV File", multiple = F, accept = c("text/csv","text/comma-seperated-values,text/plain",".csv")),
                               checkboxInput("header","Header",T),
                               radioButtons("disp", "Display",
                                            choices = c(Head = "head",
                                                        All = "all"),
                                            selected = "head"),
                               actionButton("PTM_Upload_Bttn", "Upload")),
                       column( width = 9,
                               tableOutput("contents")))),
                fluidRow(
                  box( width = 12,
                       column( width = 6,
                               title = "Edit", 
                               # column(width = 6, textInput('name_control', label =h3("Name of Control"), value = "Control")),
                               # column(width = 6, textInput('name_treatment', label = h3("Name of Treatment"), value = "Treatment")),
                               rHandsontableOutput("AB"),
                               actionButton("Update", label = "Update Data")),
                       column( width = 6,
                               dataTableOutput("AA"))))),
        
        ####Protein Upload Page####
        tabItem(tabName = "Upload2", h2("Protein Upload"),
                fluidRow(
                  box( width =12,
                       column( width = 3,
                               fileInput("file2", "Choose CSV File", multiple = F, accept = c("text/csv","text/comma-seperated-values,text/plain",".csv")),
                               tags$hr(),
                               checkboxInput("header2","Header",T),
                               radioButtons("disp2", "Display",
                                            choices = c(Head = "head",
                                                        All = "all"),
                                            selected = "head")),
                       column(width = 9, tableOutput("contents2"))),
                  
                  fluidRow(
                    box(title = "Data Labels", width = 12,
                        column(width = 6,
                               column(width = 6, textInput('name_control_protein', label =h3("Name of Control"), value = "Control")),
                               column(width = 6, textInput('name_treatment_protein', label = h3("Name of Treatment"), value = "Treatment")),
                               rHandsontableOutput("Protein_HOT"),
                               actionButton("Update_PHOT", label = "Update Data"),
                               textOutput("Protein_Text")),
                        column(width = 6, dataTableOutput("PHOT_DT")))
                  )
                )
        )
        ######
        
      )
    )
  )
)

###############

####Shiny Server####
server <- function(input, output){
  
  ####Shows the user the file they have used as the input####
  output$contents2 <- renderTable({
    req(input$file2)
    tryCatch({
      df <- read.csv(input$file2$datapath,
                     header = input$header2)
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
  
  ####Allows a file to be loaded into the tool as the protein input####
  Raw_Protein <- reactive({
    req(input$file2, input$header2, file.exists(input$file2$datapath))
    read.csv(input$file2$datapath, header = input$header2)
  })
  
  ####Creates a Dataframe where Metadata can be added####
  DFP1 <- reactive({data.frame(File.Name = colnames(Raw_Protein()[,-1]),
                               Sample.Group = NA_character_[1:length(colnames(Raw_Protein()[,-1]))],
                               Replicate = NA_integer_[1:length(colnames(Raw_Protein()[,-1]))],
                               Experimental.Group = factor(c(input$name_control_protein, input$name_treatment_protein)),
                               stringsAsFactors = F, 
                               Custom.ID=  NA_character_[1:length(colnames(Raw_Protein()[,-1]))])})
  
  DFP2 <- reactive({
    req(DFP1())
    rhandsontable(DFP1()) %>% hot_col("File.Name", readOnly = T) %>% hot_col("Sample.Group", type = "autocomplete")})
  
  output$Protein_HOT<- renderRHandsontable({DFP2()})
  
  DFP3 <- eventReactive(input$Update_PHOT, {hot_to_r(input$Protein_HOT)})
  
  DFP4 <- reactive({Custom_ID(DFP3())})
  
  Custom_ID <- function(x){
    
    DF4 <- x
    
    for(i in 1:length(DF4$File.Name)){
      if(is.na(DF4$Custom.ID[i])){
        DF4$Custom.ID[i] <- paste(DF4$Sample.Group[i], DF4$Replicate[i], DF4$Experimental.Group[i], sep = "_")
      } else {
        next
      }
    }
    return(DF4)
  }
  
  output$PHOT_DT <- renderDataTable({
    DFP4()})
  
  ####Uses the Metadata Raw input file to make dataframes for figures and tables####
  
  #Seperates the Fasta header into seperate columns
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
  
  Values_only <- function(x, y){
    VO <- x[,-c(1:3)]
    colnames(VO) <- y$Custom.ID
    rownames(VO) <- make.names(names = x[,2], unique = T)
    return(VO)
  } 
  
  Protein_Data <- reactive({Process_Protein_data(Raw_Protein())})
  
  Protein_Data_Values_Only <- reactive({Values_only(Protein_Data(), DFP4())})
  
  Protein_Data_Sig_Values <- reactive({Protein_Data_Values_Only()[Significant_Proteins()$Gene_ID ,] })
  
    #Runs the Data through LIMMA to get the P-Value and Fold Change#
    LIMMA <- function(x,y,z,c,t){
      limma_data_frame <- x
      DFP <- y
      
      vec <- DFP$Experimental.Group
      
      group <- factor(vec, levels = c(c,t))
     
      levels(group) <- list("Control" = c, "Treatment" = t)
     
      # group <- factor(vec, levels=c("Control", "Treatment"))
      
      
      design = model.matrix(~0 + group)
      colnames(design) <- levels(group)
      
      
      contr.matrix = makeContrasts(
        ControlvsTreatment =  Treatment - Control,
        levels = colnames(design))
      
      fit <- lmFit(limma_data_frame, design)
      
      fit_contr <- contrasts.fit(fit, contr.matrix)
      fit_eBayes <- eBayes(fit_contr)
      
      results.coef1 <- topTable(fit_eBayes, coef=1, number=Inf, sort.by="none")
      results.coef1 <- cbind(Gene_ID = rownames(results.coef1), results.coef1)
      results.coef2 <- merge(results.coef1, z[,c(1:3)], by.x = "Gene_ID" , by.y ="Gene_ID")
      
      results.coef3 <- cbind(results.coef2["Uniprot_ID"], results.coef2["Gene_ID"], results.coef2["Description"], results.coef2["logFC"], results.coef2["AveExpr"], results.coef2["t"], results.coef2["P.Value"], results.coef2["adj.P.Val"], results.coef2["B"])
      
      return(results.coef3)
    }
  
  LIMMA_results <- reactive({LIMMA(Protein_Data_Values_Only(),DFP4(), Protein_Data(), input$name_control_protein, input$name_treatment_protein)})
  
  
  # A function to determine Statistically significant genes based on Limma results
  find_Sig <- function(x){
    Limma <- x
    
    Sig <- Limma[Limma$logFC >= input$logFC_Sig[2] | Limma$logFC <= input$logFC_Sig[1], ]
    if(input$P.Value == "P-Value"){
      Sig2 <- Sig[Sig$P.Value < input$P.Val_thresh ,]
    } else {
      Sig2 <- Sig[Sig$adj.P.Val < input$P.Val_thresh ,]
    }
     return(Sig2)
  }
  
  Significant_Proteins <- reactive({ find_Sig(LIMMA_results())})
  

  
  ####Create a Histogram for the Data####
  raw_hist <- reactive({
    ggplot(melt(Protein_Data_Values_Only()), aes(x = value)) + 
      geom_histogram(col = input$hist_border_colour,fill = input$hist_fill_colour, binwidth = input$hist_slider) + 
      xlab(input$hist_xlab) + ylab(input$hist_ylab) + ggtitle(input$hist_raw_title) + theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_text(hjust = 1, vjust=1,size = 12),
            plot.title = element_text(size= input$hist_title_size, face="bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
            axis.title.x = element_text(size = input$hist_xlab_size, face = "bold"),
            axis.title.y = element_text(size = input$hist_ylab_size, face = "bold"))
  })
  
  output$Histogram_Raw <- renderPlot({
    req(raw_hist())
    raw_hist()
  })
  
  ####Create a Boxplot for the Data####
  raw_box <- reactive({
    ggplot(melt(Protein_Data_Values_Only()), aes(x=variable, y=value)) + geom_boxplot(col = input$box_border_colour,fill = input$box_fill_colour) + 
      xlab(input$box_xlab) + ylab(input$box_ylab) + ggtitle(input$box_raw_title) + theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_text(size = input$box_xtext_size, face = "bold"),
            plot.title = element_text(size= input$box_title_size, face="bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
            axis.title.x = element_text(size = input$box_xlab_size, face = "bold"),
            axis.title.y = element_text(size = input$box_ylab_size, face = "bold"))
  })
  
  output$Boxplot_Raw <- renderPlot({
    req(raw_box())
    raw_box()
  })
  
  ####Create a PCA and MDS Plot for the Data####
  output$PCA <- renderPlot({
    autoplot(prcomp(t(Protein_Data_Values_Only())), shape = FALSE,  label.size = 4) + theme_classic()
  })
  
  output$MDS <- renderPlot({
    plotMDS(Protein_Data_Values_Only(), labels = colnames(Protein_Data_Values_Only()), col=c(rep("blue",length(DFP3()$Experimental.Group[DFP3()$Experimental.Group == 'Treatment'])), rep("red",length(DFP3()$Experimental.Group[DFP3()$Experimental.Group == 'Control']))))
  })
  
  ####Create the Datatables for the Data####
  output$Datatable <- renderDataTable({
    
    d <- event_data("plotly_selected")[,5]
    if (is.null(d)) d <-LIMMA_results()$Gene_ID else d
    
    LIMMA_results()[LIMMA_results()$Gene_ID %in% d ,] %>% mutate_if(is.numeric, round, 3)}, server = F,rownames = F,extensions = c('Buttons','FixedColumns', 'Scroller')
    ,options = list(scroller = T, scrollY = 900, scrollX=T, fixedColumns = T , dom = "Bfrtip", buttons = c('copy', 'csv', 'pdf'))
  )
  
  output$Datatable_Sig <- renderDataTable(Significant_Proteins() %>% mutate_if(is.numeric, round, 3), server = F,rownames = F,extensions = c('Buttons','FixedColumns', 'Scroller'),
                                          options = list(scroller = T, scrollY = 900, scrollX=T, fixedColumns = T , dom = "Bfrtip", buttons = c('copy', 'csv', 'pdf')))
  
  output$Datatable_I <- renderDataTable(
    
    Protein_Data_Values_Only(), server = F,rownames = T,extensions = c('Buttons','FixedColumns', 'Scroller'),
    options = list(scroller = T, scrollY = 900, scrollX=T, fixedColumns = T , dom = "Bfrtip", buttons = c('copy', 'csv', 'pdf'))
    
    )
  
  
  output$Datatable_I_Sig <- renderDataTable(Protein_Data_Sig_Values(), server = F,rownames = T,extensions = c('Buttons','FixedColumns', 'Scroller'),
                                            options = list(scroller = T, scrollY = 900, scrollX=T, fixedColumns = T , dom = "Bfrtip", buttons = c('copy', 'csv', 'pdf')))
  
  
  ####Create the Volcano Plot using Plotly####
  output$Volcano <- renderPlotly({
    results.coef1 <- LIMMA_results() %>% mutate_if(is.numeric, round, 3)
    key = results.coef1$Gene_ID
    
    
    ggplotly(
      ggplot(results.coef1, aes(x=logFC, y=-log10(P.Value), 
                                color = ifelse(Gene_ID %in% Significant_Proteins()$Gene_ID, "Significant", "Not Significant"),
                                text = paste("Protein :", Uniprot_ID, "\n", "Gene Name", Gene_ID,"\n", "Description", Description, "\n",
                                             "Log Fold Change :", logFC ,"\n", "P Value :",  P.Value, "\n", "Adjusted P Value", adj.P.Val), key = key))+
        geom_point()+
        geom_vline(xintercept = c(input$logFC_Sig[1],input$logFC_Sig[2]), color = "black", linetype = "dashed")+
        geom_hline(yintercept = -log10(input$P.Val_thresh),color = "black", linetype ="dashed")+
        scale_color_manual(name = "Threshold", values = c("Significant" = input$VC_Sig_color, "Not Significant" = input$VC_notSig_color))+
        ylab(ifelse(input$P.Value == "P-Value",  "-log10(P-Value)", "-log10(Adj P-Value)")) +
        ggtitle(input$VC_title) +
        xlab(input$VC_xlab) +
        theme(axis.text.x = element_text(hjust = 1, vjust=1,size = 12),
              plot.title = element_text(hjust = 0.5, size= input$VC_title_size, face="bold"),
              axis.text.y = element_text(size = 12, face = "bold"),
              axis.title.x = element_text(size = input$VC_xlab_size, face = "bold"),
              axis.title.y = element_text(size = input$VC_ylab_size, face = "bold"))
      , tooltip = "text") 
        
  })
  
  ####Create the Heatmaps for the Protein Data####
  
  HeatMP <- reactive({
    HM <- Protein_Data_Sig_Values()
    HM["Gene_ID"] <- rownames(HM)
    
    if(input$scl2 == 'On'){
      HMM <- melt(HM)
      HMM['value'] <- scale(HMM['value'])
    }else{
      HMM <- melt(HM)
    }
    
    HeatM <- ggplot(HMM, aes( x = Gene_ID, y = variable)) + geom_tile(aes(fill= value)) +
      theme(panel.grid.major.x=element_blank(), #no gridlines
            panel.grid.minor.x=element_blank(), 
            panel.grid.major.y=element_blank(), 
            panel.grid.minor.y=element_blank(),
            panel.background=element_rect(fill="white"), # background=white
            axis.text.x = element_text(angle=45, hjust = 1,vjust=1,size = input$HM_xlab_size,face = "bold"),
            plot.title = element_text(hjust = 0.5, size=input$HM_title_size,face="bold"),
            axis.text.y = element_text(size = input$HM_ylab_size,face = "bold"),
            legend.title.align = 0.5,
            legend.box.just = "center")+
      xlab(input$HM_xlab)+
      ylab(input$HM_ylab)+
      ggtitle(input$HM_title)+ 
      labs(fill = "Protein \nExpression") +
      scale_fill_gradient2(low = input$Low_colour, 
                           mid = input$Mid_colour, 
                           high = input$High_colour, 
                           midpoint = 0)
    
    if(input$nmbr == "On"){
      HeatM2 <- HeatM + geom_text(aes(fill = value, label = round(value, 2)))
    } else {
      HeatM2 <- HeatM
    }
    
    return(HeatM2)
    
  })
  
  output$Heatmap_Sig<- renderPlotly({
    
    ggplotly( HeatMP() )
    
  })
  
  ####Allows a file to be loaded into the tool as the PTM input####
  output$contents <- renderTable({
    req(input$file1)
    tryCatch({
      df <- read.csv(input$file1$datapath,
                     header = input$header)
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
  DF1 <- reactive({data.frame(File.Name= unique(Raw_Data()$MS.MS_sample_name),
                              Sample.Group= NA_character_[1:length(unique(Raw_Data()$MS.MS_sample_name))],
                              Replicate= NA_integer_[1:length(unique(Raw_Data()$MS.MS_sample_name))],
                              # Experimental.Group= factor(c(input$name_control, input$name_treatment), labels = c(input$name_control, input$name_treatment)),
                              Experimental.Group = NA_character_[1:length(unique(Raw_Data()$MS.MS_sample_name))],
                              stringsAsFactors= F,
                              Custom.ID= NA_character_[1:length(unique(Raw_Data()$MS.MS_sample_name))])})
  
  output$AC <- renderTable({
    req(Raw_Data())
    Raw_Data()
  })
  
  observeEvent(input$tst,{
    citations()
  })
  
  
  ###Creates the handsontable where DF1() will be edited####
  DF2 <- reactive({
    req(DF1())
    rhandsontable(DF1()) %>% hot_col("File.Name", readOnly = T) %>% hot_col("Sample.Group", type = "autocomplete")
  })
  
  output$AB<- renderRHandsontable({DF2()})
  
  ####Changes the handsontable back into a dataframe####
  DF3 <- eventReactive(input$Update, {hot_to_r(input$AB)})
  
  Custom_ID <- function(x){
    
    DF4 <- x
    
    for(i in 1:length(DF4$File.Name)){
      if(is.na(DF4$Custom.ID[i])){
        DF4$Custom.ID[i] <- paste(DF4$Sample.Group[i], DF4$Replicate[i], DF4$Experimental.Group[i], sep = "_")
      } else {
        next
      }
    }
    return(DF4)
  }
  
  
  DF4 <- reactive({
    Custom_ID(DF3())
  })
  
  output$AA <- renderDataTable({
    DF4()
  })
  ####Outputs the Rhandsontable ####
  
  Blank <- function(x){
    
    for(i in 1:length(unique(x$MS.MS_sample_name))){
      HD <- x[x$MS.MS_sample_name %in% DF4()$File.Name[i],]
      HD$Replicate <- DF4()$Replicate[i]
      HD$MS.MS_sample_name <- mapvalues(HD$MS.MS_sample_name, from = HD$MS.MS_sample_name[1], to = as.character(DF4()$Sample.Group[i]))
      HD$Treatment <- DF4()$Experimental.Group[i]
      HD$Custom.ID <- DF4()$Custom.ID[i]
      if(!exists("Histone_data")){
        Histone_data <- HD
      } else {
        Histone_data <- rbind(Histone_data, HD)
      }
    }
    
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
    Histone_data2 <- data.frame(Histone_data$Custom.ID, Histone_data$MS.MS_sample_name, Histone_data$Treatment, Histone_data$Replicate,Histone_data$Histone2, Histone_data$Residue,
                                Histone_data$PTM, Histone_data$Intensity, Histone_data$Total_intensity, Histone_data$Abundance, Histone_data$betaValue, Histone_data$MValue)
    colnames(Histone_data2) <- c("Custom ID", "Sample Group", "Treatment", "Replicate", "Histone", "PTM Residue", "PTM", "Intensity","Total Inensity", "Abundance",
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
  
  output$Dif_Anal_choice1 <- renderUI({selectInput("Dif_Anal_choice1","Control Group for Differential Analysis", choices=unique(paste(Histone_PTM2()$Sample, Histone_PTM2()$Treatment, sep = "_")))})
  output$Dif_Anal_choice2 <- renderUI({selectInput("Dif_Anal_choice2","Treatment Group for Differential Analysis", choices=unique(paste(Histone_PTM2()$Sample, Histone_PTM2()$Treatment, sep = "_")))})
  
  # output$Sample_PTM_Barplot <- renderUI({selectInput("Sample_PTM_Barplot", "Sample Selction", choices = levels(factor(Histone_PTM2()$Sample)))})
  output$Sample_PTM_Barplot <- renderUI({pickerInput("Sample_BP","Sample Selection", choices=levels(factor(Histone_PTM2()$Sample)), options = list(`actions-box` = TRUE),multiple = T,
                                                     selected = levels(factor(Histone_PTM2()$Sample)))})
  
  output$PTM_Barplot <- renderUI({selectInput("PTM_Barplot", "Histone Selction", choices = levels(factor(Histone_PTM2()$Histone)))})
  
  output$PTM_Trt <- renderUI({pickerInput("Treatment_BP","Experimental Group Selection", choices=levels(factor(Histone_PTM2()$Treatment)), options = list(`actions-box` = TRUE),multiple = T,
                                          selected = levels(factor(Histone_PTM2()$Treatment)))})
  
  output$PTM_Residue_BP <- renderUI({pickerInput("Residue_BP","Residue Selection", choices=levels(factor(Protein_Graph_Table()$"PTM Residue")), options = list(`actions-box` = TRUE),multiple = T,
                                                 selected = levels(factor(Protein_Graph_Table()$"PTM Residue")))})
  
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
  
  
  output$Histone_data_table <- renderDataTable(
    PTM_filter(Histone_PTM2()), server = F,rownames = F,extensions = c('Buttons','FixedColumns', 'Scroller'),
                                               options = list(scroller = T, scrollY = 400, scrollX=T, fixedColumns = T , dom = "Bfrtip", buttons = c('copy', 'csv', 'pdf')))
  
  ####Differential Analysis####
  
  DE <- function(df1,x,y){
    
    split <- unlist(strsplit(df1$Histone, "\\Histone "))
    split <- split[!c(split == "")]
    
    
    fullPTM <- paste(split, df1$'PTM Residue', df1$PTM, sep = " ")
    treat <- paste(df1$'Sample Group', df1$Treatment, sep = "_")
    df <- cbind(df1, fullPTM, treat)
    
    
    ####limit to samples of interest####
    df = df[df$treat %in% c(x, y),]

    
    ####exclude unmodified####
    df = df[!grepl("Unmodified", df$PTM),]

    
    ####create beat and Mvalue matrix####
    beta = matrix(NA, nrow = length(unique(df$fullPTM)), ncol = length(unique(df$'Custom ID')), dimnames = list(unique(df$fullPTM), unique(df$'Custom ID')))
    for(i in unique(df$'Custom ID')){
      beta[df[df$'Custom ID' == i,]$fullPTM, i] = df[df$'Custom ID' == i,]$'Beta Value'
    }
    
    M = matrix(NA, nrow = length(unique(df$fullPTM)), ncol = length(unique(df$'Custom ID')), dimnames = list(unique(df$fullPTM), unique(df$'Custom ID')))
    for(i in unique(df$'Custom ID')){
      M[df[df$'Custom ID' == i,]$fullPTM, i] = df[df$'Custom ID' == i,]$'M Value'
    }
    
    treatmentDesign = NULL
    for(i in 1:ncol(beta)){
      treatmentDesign[i] = df$Treatment[colnames(beta)[i] == df$'Custom ID'][1]
    }
    
    beta = cbind(beta, fullPTM = paste( "k", unlist(lapply(strsplit(rownames(beta), "k"), tail, n = 1)), sep = ""), histone = trimws(unlist(lapply(strsplit(rownames(beta), "k"), head, 1))))
    
    ####Check for duplicates in Beta Matrix####
    #Checks for duplicate rows within the matrix
    
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
      
      combinedHistones = paste(combinedHistones[order(combinedHistones)], collapse = "|")
      
      removal[,"histone"] = combinedHistones
      
      rownames(removal) = rep(paste(combinedHistones, removal[1,"fullPTM"], sep = " "), nrow(removal))
      
      addition = removal[1,]
      
      beta = rbind(beta, addition)
      
      rownames(beta)[nrow(beta)] = rownames(removal)[1]
      
      duplicates = any(duplicated(beta[,colnames(beta) != "histone"]))
    }
    
    beta = beta[,!colnames(beta) %in% c("fullPTM", "histone")]
    
    class(beta) = "numeric"
    
    
    ####Check for duplicates in M Matrix####
    #Checks for duplicate rows within the matrix
    
    M = cbind(M, fullPTM = paste( "k", unlist(lapply(strsplit(rownames(M), "k"), tail, n = 1)), sep = ""), histone = trimws(unlist(lapply(strsplit(rownames(M), "k"), head, 1))))
    
    duplicates = any(duplicated(M[,colnames(M) != "histone"]))
    
    while(duplicates){
      
      identifyer = NULL
      
      for(i in 1:nrow(M)){
        if(class(M[duplicated(M[,colnames(M) != "histone"]),colnames(M) != "histone"]) == "matrix")
          identifyer[i] = identical(M[i,colnames(M) != "histone"], M[duplicated(M[,colnames(M) != "histone"]),colnames(M) != "histone"][1,])
        if(class(M[duplicated(M[,colnames(M) != "histone"]),colnames(M) != "histone"]) == "character")
          identifyer[i] = identical(M[i,colnames(M) != "histone"], M[duplicated(M[,colnames(M) != "histone"]),colnames(M) != "histone"])
      }
      removal = M[identifyer,]
      
      M = M[!identifyer,]
      
      combinedHistones = str_replace(removal[,"histone"], "Histone ", "")
      
      combinedHistones = paste(combinedHistones[order(combinedHistones)], collapse = "|")
      
      removal[,"histone"] = combinedHistones
      
      rownames(removal) = rep(paste(combinedHistones, removal[1,"fullPTM"], sep = " "), nrow(removal))
      
      addition = removal[1,]
      
      M = rbind(M, addition)
      
      rownames(M)[nrow(M)] = rownames(removal)[1]
      
      duplicates = any(duplicated(M[,colnames(M) != "histone"]))
    }
    
    M = M[,!colnames(M) %in% c("fullPTM", "histone")]
    
    class(M) = "numeric"
    
    ####Limma####
  
    vec = NULL
    for(i in 1:ncol(M)){
      vec[i] <- unique(df[colnames(M)[i] == df$'Custom ID', 14])
      vec[i] <- ifelse(x == vec[i], "control", "treatment")
    }
    print(vec)
    
    group <- factor(vec, levels= c("control", "treatment"))
    print(group)
    
    design = model.matrix(~0 + group)
    colnames(design) <- levels(group)
    
    contr.matrix = makeContrasts(
      treatmentvscontrol = treatment - control,
      levels = colnames(design))
    contr.matrix
    
    design
    
    fit <- lmFit(M, design)
    
    cont_fit <- contrasts.fit(fit, contr.matrix)
    
    fit2 <- eBayes(cont_fit)
    results.coef1 <- topTable(fit2, coef=1, number=Inf, sort.by="none")
    
    fdr = p.adjust(results.coef1$P.Value, method = "fdr")
    results = cbind('Fold Change' = results.coef1$logFC,pVal = results.coef1$P.Value, FDR = fdr, beta)
    results = round(results[order(results[,"pVal"]),],3)
    
    return(results)
  }
  
  Diff_Anal_Table <- reactive({DE(Histone_PTM2(), input$Dif_Anal_choice1, input$Dif_Anal_choice2)})
  
  ####Differential Analysis Chart####
  output$DFTable <- renderDataTable(Diff_Anal_Table(), server = F,rownames = T,extensions = c('Buttons','FixedColumns', 'Scroller'),
                                    options = list(scroller = T, scrollY = 400, scrollX=T, fixedColumns = T , dom = "Bfrtip", buttons = c('copy', 'csv', 'pdf')))
  
  
  ####Creating Bar Graphs for Histone PTMS####
  
  Protein_Graph_Table <- reactive({Histone_PTM2() %>% filter(Histone %in% input$PTM_Barplot & c(Treatment %in% input$Treatment_BP))})
  
  Barplot <- function(x,y,z,w){
    x$PTM = trimws(x$PTM)
    
    aggregatedDF = aggregate(x = x$'Beta Value', FUN = mean, 
                             by = list(tissue = x$Sample,
                                       treatment = x$Treatment, 
                                       names = x$'Custom ID',
                                       position = x$'PTM Residue', 
                                       modification = x$PTM))
    colnames(aggregatedDF)[colnames(aggregatedDF) =="x"] = "Mean Beta-Value"
    
    temp = aggregatedDF[aggregatedDF$tissue %in% c(y) & aggregatedDF$position %in% c(w),]
    temp$sample <- paste(temp$tissue, temp$treatment, sep = "_")
    temp$modification = factor(temp$modification, levels = c("Acetyl", "Methyl", "Dimethyl", "Trimethyl", "Unmodified"))
    temp$position = factor(temp$position, levels = unique(temp$position)[order(as.numeric(substring(unique(temp$position),2)))])
    
    modificationsList = unique(z$PTM)
    myColorScale <- brewer.pal(length(modificationsList),"Set1")
    names(myColorScale) <- levels(temp$modification)
    colScale <- scale_fill_manual(name = "PTM", values = myColorScale)
    
    
    
    PTM_Barplot<- ggplot(temp, aes(x = if(input$PTM_mean_chkbx == F){sample}else{names}, y = `Mean Beta-Value`, fill = modification)) +
      geom_bar(stat = 'identity', position = 'fill') + facet_grid(~ position) +
      scale_y_continuous(labels = scales::percent_format()) +
      ggtitle("Global Histone PTM for", subtitle = input$PTM_Barplot) +
      theme(plot.title = element_text(hjust = 0.5, size = input$PTM_BP_title ),
            plot.subtitle = element_text(hjust = 0.5, size = c(input$PTM_BP_title-5)),
            axis.title = element_text(size = input$PTM_BP_yaxis_title),
            axis.text = element_text(angle = 45, hjust = 1, size = input$PTM_BP_axis_txt),
            strip.text.x = element_text(size = input$PTM_BP_residue_size, face = 'bold'))+
      xlab(NULL) + ylab("Mean Beta Value") + colScale
    
    return(PTM_Barplot)
    
  }
  
  BGraph_PTM <- reactive({
    req(Protein_Graph_Table())
    Barplot(Protein_Graph_Table(), input$Sample_BP ,Histone_PTM2(), input$Residue_BP)
  })
  
  
  output$PTM_Graph <- renderPlot({
    req(BGraph_PTM())
    BGraph_PTM()
  })
  
  ####Heatmap for DFA####
  
  Heatmap_DA <- reactive({
    a <- Diff_Anal_Table()[,-c(1:3)]
    
    if(input$scl3 == 'On'){
      HMM <- melt(a)
      HMM['value'] <- scale(HMM['value'])
    }else{
      HMM <- melt(a)
    }
    
    HeatM <- ggplot(HMM, aes( x = X1, y = X2)) + geom_tile(aes(fill= value)) +
      theme(panel.grid.major.x=element_blank(), #no gridlines
            panel.grid.minor.x=element_blank(), 
            panel.grid.major.y=element_blank(), 
            panel.grid.minor.y=element_blank(),
            panel.background=element_rect(fill= input$Missing_colour_DA), # background=white
            axis.text.x = element_text(angle=45, hjust = 1,vjust=1,size = input$DA_xlab_size,face = "bold"),
            plot.title = element_text(hjust = 0.5, size=input$DA_title_size,face="bold"),
            axis.text.y = element_text(size = input$DA_ylab_size,face = "bold"))+
      xlab(input$DA_xlab)+
      ylab(input$DA_ylab)+
      ggtitle(input$DA_title)+ 
      labs(fill = "Beta Value") +
      scale_fill_gradient2(low = input$Low_colour_DA, 
                           mid = input$Mid_colour_DA, 
                           high = input$High_colour_DA, 
                           midpoint = 0)
    
    if(input$nmbr2 == "On"){
      HeatM2 <- HeatM + geom_text(aes(fill = value, label = round(value, 2)))
    } else {
      HeatM2 <- HeatM
    }
    
    return(HeatM2)
  })
  
  output$PTM_heatmaply <- renderPlotly({
    req(Heatmap_DA)
    ggplotly(Heatmap_DA())
  })
  
  ####Download options for each figure and graph####
  #Download Histogram#
  output$HS_DNLD = downloadHandler(
    filename = function(){paste("Histogram", input$dnld_options, sep=".")},
    content = function(file){
      ggsave(file,plot=raw_hist(),
             width = input$width,
             height = input$height,
             units = input$unit,
             dpi = input$dpi,
             device = input$dnld_options)
    }
  )
  #Download Boxplot#
  output$BX_DNLD = downloadHandler(
    filename = function(){paste("Boxplot", input$dnld_options, sep=".")},
    content = function(file){
      ggsave(file,plot=raw_box(),
             width = input$width,
             height = input$height,
             units = input$unit,
             dpi = input$dpi,
             device = input$dnld_options)
    }
  )
  #Download PCA#
  output$PCA_DNLD = downloadHandler(
    filename = function(){paste("PCA", input$dnld_options, sep=".")},
    content = function(file){
      ggsave(file,plot=autoplot(prcomp(t(Protein_Data_Values_Only())), shape = FALSE,  label.size = 4) + theme_classic(),
             width = input$width,
             height = input$height,
             units = input$unit,
             dpi = input$dpi,
             device = input$dnld_options)
    }
  )
  #Download MDS#
  output$MDS_DNLD = downloadHandler(
    filename = function(){paste("MDS", input$dnld_options, sep=".")},
    content = function(file){
      ggsave(file,plot=plotMDS(Protein_Data_Values_Only(), labels = colnames(Protein_Data_Values_Only()), col=c(rep("blue",length(DFP3()$Experimental.Group[DFP3()$Experimental.Group == 'Treatment'])), rep("red",length(DFP3()$Experimental.Group[DFP3()$Experimental.Group == 'Control'])))))
    }
  )
  #Download Volcano Plot#
  output$VC_DNLD = downloadHandler(
    filename = function(){paste("Volcano Plot", input$dnld_options, sep=".")},
    content = function(file){
      results.coef1 <- LIMMA_results()
      ggsave(file,plot=ggplot(results.coef1, aes(x=logFC, y=-log10(P.Value), 
                                                 color = ifelse(Gene_ID %in% Significant_Proteins()$Gene_ID, "Significant", "Not Significant")))+
               geom_point()+
               geom_vline(xintercept = c(input$logFC_Sig[1],input$logFC_Sig[2]), color = "black", linetype = "dashed")+
               geom_hline(yintercept = -log10(input$P.Val_thresh),color = "black", linetype ="dashed")+
               scale_color_manual(name = "Threshold", values = c("Significant" = input$VC_Sig_color, "Not Significant" = input$VC_notSig_color))+
               ylab(ifelse(input$P.Value == "P-Value",  "-log10(P-Value)", "-log10(Adj P-Value)")) +
               ggtitle(input$VC_title) +
               xlab(input$VC_xlab) +
               theme(axis.text.x = element_text(hjust = 1, vjust=1,size = 12),
                     plot.title = element_text(hjust = 0.5, size= input$VC_title_size, face="bold"),
                     axis.text.y = element_text(size = 12, face = "bold"),
                     axis.title.x = element_text(size = input$VC_xlab_size, face = "bold"),
                     axis.title.y = element_text(size = input$VC_ylab_size, face = "bold")),
             width = input$width,
             height = input$height,
             units = input$unit,
             dpi = input$dpi,
             device = input$dnld_options
             )
    }
  )
  
  #Dornload Protein Heatmap#
  
  output$HM_DNLD = downloadHandler(
    filename = function(){paste("Heat Map", input$dnld_options, sep=".")},
    content = function(file){
      ggsave(file,plot=HeatMP(),
             width = input$width,
             height = input$height,
             units = input$unit,
             dpi = input$dpi,
             device = input$dnld_options)
    }
  )
  #Download PTM BarChart#
  
  output$PTM_DNLD = downloadHandler(
    filename = function(){paste("PTM Barchart", input$dnld_options, sep=".")},
    content = function(file){
      ggsave(file,plot=BGraph_PTM(),
             width = input$width,
             height = input$height,
             units = input$unit,
             dpi = input$dpi,
             device = input$dnld_options)
    }
  )
  #Download PTM Heatmap#
    
    output$DA_DNLD = downloadHandler(
      filename = function(){paste("Differential Analysis", input$dnld_options, sep=".")},
      content = function(file){
        ggsave(file,plot=Heatmap_DA(),
               width = input$width,
               height = input$height,
               units = input$unit,
               dpi = input$dpi,
               device = input$dnld_options)
      }
    )
  
  
} 



###############
shinyApp(ui = ui, server = server)
