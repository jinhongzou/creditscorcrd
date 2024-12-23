#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(data.table)
library(shiny)
#library(Seurat)
#library(shinydashboard)

pb<-c()#readRDS("data/pbmc3k_final.rds")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(p("Score Card Analysis" , style = "color:#3474A7")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput(inputId='file1', label='Choose csv File'
      ),
      
      textInput(inputId = "charcolname",
                label =  "字符型变量",
                value = "flag cst_unn_id",
                placeholder=TRUE
      ),
      
      conditionalPanel(
        condition = "input.smoother == ture",
        selectInput("sample_rate","抽样比例:",list(0.7,0.2,0.3,0.4,0.5,0.6,0.8,0.9))
      ),
      
      textInput(inputId = "filter_var",
                label =  "过滤变量",
                value = "cst_unn_id"
      ),
      
      conditionalPanel(
        condition = "inpcst_unn_id flagut.cluster == ture",
        selectInput("Choosecluster",'ChooseCluster',as.list( levels(pb) )) #  levels(pb)  as.list(pb@meta.data$seurat_clusters)
        
      ),
      
      actionButton("goButton", "数据加载")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #h3("DimPlot"),
      fluidPage(title='数据浏览', 
        tabsetPanel(
          #tableOutput('contents'),
          #imageOutput("p1", width = "100%", height = "400px", click = NULL,
          #            dblclick = NULL, hover = NULL, hoverDelay = NULL,
          #            hoverDelayType = NULL, brush = NULL, clickId = NULL, hoverId = NULL,
          #            inline = FALSE),
          
          tabPanel(title= "数据说明", textOutput("dat_smy")),
          tabPanel(title= "源数据"  , dataTableOutput("contents")),
          tabPanel(title= "训练样本", dataTableOutput("dat_train")),
          tabPanel(title= "测试样本", dataTableOutput("dat_test")),
          tabPanel("FeaturePlot",plotOutput("FeaturePlot"))
        )
        
      )
    )
  )
))

# runApp('/Users/john/ScorCrd')
# runApp('G:/MyPrj/ScorCrd')

