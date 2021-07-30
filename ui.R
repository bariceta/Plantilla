library(shiny)
library(readxl)
library(stringr)
library(xlsx)
library(openxlsx)
library(XLConnect)
library(shinymanager)


setwd("~/")

`%ni%` <- Negate(`%in%`)


genes_CNV <- c("TAS2R1","MEF2C","EGR1","TES","WRN","FGFR1","LYN","EYA1","MYC","ATG2B","GSKIP","TCL1A","CDC25B","JAG1","PTPRT","MYBL2")
genes_CNV_MM <- "TAS2R1"
interesting_cols <- c("gene","type","codingConsequence","chromosome","genome_position","depth","var_percent","exon_rank","c.DNA","protein","ref","alt","refNum","altNum","tx_name","dbSNP","g1000","esp5400","id_cosmic_coding","id_clinvar","P_COMMENT","ExAC","PROCESSED_CLNSIG","CLNSIGLAB","GnomAD","Relative Frequency in Run","Relative Frequency in Client","Relative Frequency in Community")
coding_Consequence <- c("intronic","synonymous")

varsome_root <- "https://varsome.com/variant/hg19"

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 12000000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 12000000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"



transformFile <- function(df1,panel,archivo,usuario){
  sampleAnalyzing <- read_excel(df1)
  sampleAnalyzing$chromosome <- sub("\\.\\d+$","",sampleAnalyzing$chromosome)
  sampleFiltered <- sampleAnalyzing[sampleAnalyzing$codingConsequence %ni% coding_Consequence,]
  sampleFiltered <- sampleFiltered[sampleFiltered$ExAC < 0.01 | is.na(sampleFiltered$ExAC),]
  
  sampleFiltered_Save <- cbind(sampleFiltered[1:4],Categorizacion=rep(NA,nrow(sampleFiltered)),Germinal=rep(NA,nrow(sampleFiltered)),INFO_Categorizacion=rep(NA,nrow(sampleFiltered)),IGV=rep(NA,nrow(sampleFiltered)),sampleFiltered[5:ncol(sampleFiltered)],Varsome=paste(varsome_root,paste(sampleFiltered$gene,sampleFiltered$c.DNA,sep=":"),sep = "/"))
  
  if (isTRUE(panel == "PMPv2")){
    for (j in 1:nrow(sampleFiltered_Save)) {
      if (isTRUE(sampleFiltered_Save$gene[j] %in% genes_CNV)){
        sampleFiltered_Save$Categorizacion[j] <- "CNV"
      }
    }
  }
  
  if (isTRUE(panel == "MM")){
    for (j in 1:nrow(sampleFiltered_Save)) {
      if (isTRUE(sampleFiltered_Save$gene[j] %in% genes_CNV_MM)){
        sampleFiltered_Save$Categorizacion[j] <- "CNV"
      }
    }
  }
  
  logData <- as.data.frame(matrix(NA,6,2))
  
  logData[,2] <- as.character(logData[,2])
  logData[1,1] <- "Cultivo"
  logData[2,1] <- "Panel"
  logData[3,1] <- "Usuario aplicación"
  logData[4,1] <- "Fecha aplicación"
  logData[5,1] <- "Usuario categorización"
  logData[6,1] <- "Fecha categorización"
  
  logData[1,2] <- unlist(strsplit(archivo,split = "-"))[1]
  logData[2,2] <- panel
  logData[3,2] <- usuario
  logData[4,2] <- as.character(Sys.Date())
  
  colnames(logData) <- c("Cultivo",logData[1,2])
  logData <- logData[-1,]
  
  
  list_of_sheets <- list("Information"=logData,"Retained Variants"=sampleAnalyzing,"Filtered Variants"=sampleFiltered_Save)
  #write.xlsx(list_of_sheets,file = excel_files[i])
  df2 <- list_of_sheets
  return(df2)
}

ui <- secure_app(head_auth = tags$script(inactivity),enable_admin = T,
                 fluidPage(
                   titlePanel("GENERADOR DE PLANTILLA PARA VARIANTES DE SOPHIA DDM"),
                   sidebarLayout(
                     sidebarPanel(
                       h2("Aplicación para generar la plantilla (1.0.0)"),
                       p("Para poder generar la plantilla, hay que seleccionar un archivo Excel, descargarse la modificación y elegir otro archivo despues."),
                       br(),
                       textOutput("userID"),
                       br(),
                       radioButtons("panel",label = "Selecciona el panel:",choices = c("PMPv2","MM"),selected = "PMPv2"),
                       br(),
                       br(),
                       fileInput('file1', 'Selecciona el archivo.',
                                 accept = c(".xlsx")),
                       downloadButton('file2', "Descarga el nuevo archivo"),
                       br(),
                       br(),
                       br(),
                       h6("Solo para uso interno del laboratorio")
                     ),
                     mainPanel(
                       tableOutput('contents'))
                   )
                 ))

