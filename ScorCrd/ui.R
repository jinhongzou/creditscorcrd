#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 

# Define UI for application that draws a histogram
shinyUI( 
  dashboardPage (

    # Header
    dashboardHeader(
      title = 'HNB-零售银行部'
      ),
    
    # Siderbar
    dashboardSidebar(
      sidebarMenu(
        menuItem("样本加载", icon = icon("table")         , tabName = "menuItem_A" ),
        menuItem("样本分析", icon = icon("bar-chart-o")   , tabName = "menuItem_A2"),
        menuItem("WOE转换" , icon = icon("credit-card")   , tabName = "menuItem_B"),
        menuItem("特征表现", icon = icon("line-chart")    , tabName = "menuItem_C"),
        menuItem("模型评估", icon = icon("dashboard")     , tabName = "menuItem_D"),
        menuItem("评分卡"  , icon = icon("list-alt")      , tabName = "menuItem_E")
      )
    ),
    # Body
    dashboardBody(
      fluidRow(
        tabItems(
          tabItem(tabName = "menuItem_A",
            # 操作面板
            sidebarLayout(
                sidebarPanel(
                  width =3,
                  fileInput(inputId='file1', accept = ".csv", label = "查找", placeholder = "传入文件…"),
                  conditionalPanel(
                    condition = "input.smoother == ture",
                    selectInput("sample_rate","抽样比例:",list(0.7,0.2,0.3,0.4,0.5,0.6,0.8,0.9))
                  ),
                  textInput(inputId = "charcolname",
                            label =  "字符型变量",
                            value = "",
                            placeholder=TRUE
                  ),
                  textInput(inputId = "filter_var",
                            label =  "过滤变量",
                            value = ""
                  ),
                  conditionalPanel(
                    condition = "inpcst_unn_id flagut.cluster == ture",
                    selectInput(inputId = "target",label =  'target', c('flag')) #  levels(pb)  as.list(pb@meta.data$seurat_clusters)
                  ),
                  actionButton("goButton_A", "Run", width="100%", class = "btn-success",icon = icon("refresh") )
                ),
              
                # 展示区
                mainPanel(
                  width = 9,
                  fluidPage( 
                    position = c("left") ,
                    tabsetPanel(
                      tabPanel(title= "样本概况", fluidPage( verbatimTextOutput("dat_str"), verbatimTextOutput("dat_smy"))),
                      tabPanel(title= "源样本数据"  , DT::dataTableOutput("contents")),
                      tabPanel(title= "训练样本清单", DT::dataTableOutput("dat_train")),
                      tabPanel(title= "测试样本清单", DT::dataTableOutput("dat_test"))
                    )
                )
               )
            ) #sidebarLayout
          ) , #tabItem
          tabItem( tabName = "menuItem_A2",
            # 展示区
            mainPanel(
              width = 12,
                fluidPage(
                  useShinyjs()
                  , position = c("left") 
                  , actionButton("goButton_A2", "Run", width ="20%", class = "btn-success",icon = icon("refresh"))
                  , tabsetPanel(
                      tabPanel(title= "样本关系", 
                               sidebarLayout(
                                 sidebarPanel(
                                   width =2,
                                   conditionalPanel(
                                     condition = "inpcst_unn_id flagut.cluster == ture",
                                     selectInput("FeaturePlot_1_y", label="特征名称y:",choices=NULL)
                                   ),
                                   conditionalPanel(
                                     condition = "inpcst_unn_id flagut.cluster == ture",
                                     selectInput("FeaturePlot_1_x", label="特征名称x",choices=NULL)
                                   )
                                 ),
                                 # 展示区
                                 mainPanel(
                                   width = 10,
                                   fluidPage( 
                                     plotOutput("FeaturePlot_1", width = "100%", height = "1200px")
                                   )
                                 )
                               ) #sidebarLayout
                      ),
                      tabPanel(title= "样本箱线图", 
                               #sidebarLayout(
                               # sidebarPanel(
                               #    width =2,
                                #   conditionalPanel(
                                #     condition = "inpcst_unn_id flagut.cluster == ture",
                                #     selectInput("FeaturePlot_4_method", label="method=",choices=c("pearson", "kendall","spearman"), selected='pearson')
                                #   ),
                                #   conditionalPanel(
                                #     condition = "inpcst_unn_id flagut.cluster == ture",
                                #     checkboxGroupInput(inputId = "FeaturePlot_4_var",label ="选择变量", choices = NULL)
                                #   )
                                # ),
                                # 展示区
                                # mainPanel(
                                # width = 10,
                                   fluidPage( 
                                     plotOutput("FeaturePlot_4", width = "100%", height = "800px")
                                   )
                               #  )
                              # ) #sidebarLayout
                      ), #tabPanel
                      tabPanel(title= "样本分布", 
                               sidebarLayout(
                                 sidebarPanel(
                                   width =2,
                                   conditionalPanel(
                                     condition = "inpcst_unn_id flagut.cluster == ture",
                                     selectInput("FeaturePlot_0_method", label="method=",choices=c("pearson", "kendall","spearman"), selected='pearson')
                                   ),
                                   conditionalPanel(
                                     condition = "inpcst_unn_id flagut.cluster == ture",
                                     checkboxGroupInput(inputId = "FeaturePlot_0_var",label ="选择变量", choices = NULL)
                                   )
                                 ),
                                 # 展示区
                                 mainPanel(
                                   width = 10,
                                   fluidPage( 
                                     plotOutput("FeaturePlot_0", width = "100%", height = "800px")
                                   )
                                 )
                               ) #sidebarLayout
                      ), #tabPanel
                      tabPanel(title= "样本相关性", 
                               sidebarLayout(
                                 sidebarPanel(
                                   width =2,
                                   conditionalPanel(
                                     condition = "inpcst_unn_id flagut.cluster == ture",
                                     selectInput("FeaturePlot_2_type", label="type=",choices=c('full', 'lower', 'upper'), selected='full')
                                   ),
                                   conditionalPanel(
                                     condition = "inpcst_unn_id flagut.cluster == ture",
                                     selectInput("FeaturePlot_2_method", label="method=",choices=c('circle', 'square', 'ellipse', 'number', 'shade', 'color', 'pie'), selected='circle')
                                   ),
                                   conditionalPanel(
                                     condition = "inpcst_unn_id flagut.cluster == ture",
                                     selectInput("FeaturePlot_2_order", label="order=",choices=c('original','AOE', 'FPC', 'hclust', 'alphabet'),  selected='original')
                                   ),
                                   conditionalPanel(
                                     condition = "inpcst_unn_id flagut.cluster == ture",
                                     checkboxGroupInput(inputId = "FeaturePlot_2_var",label ="选择变量", choices = NULL)
                                   ),
                                   conditionalPanel(
                                     condition = "inpcst_unn_id flagut.cluster == ture",
                                     numericInput(inputId = "FeaturePlot_2_srt",label =  "tl.srt=",value = "90")
                                   )
                                 ),
                                 # 展示区
                                 mainPanel(
                                   width = 10,
                                   fluidPage( 
                                     plotOutput("FeaturePlot_2", width = "100%", height = "800px")
                                   )
                                 )
                               ) #sidebarLayout
                      ), #tabPanel
                      tabPanel(title= "样本聚类", 
                               sidebarLayout(
                                 sidebarPanel(
                                   width =2,
                                   conditionalPanel(
                                     condition = "inpcst_unn_id flagut.cluster == ture",
                                     selectInput("FeaturePlot_3_clust_cnt", label="集群个数=",choices=c(2,3,4,5,6,7,8,9), selected='3')
                                   ),
                                   conditionalPanel(
                                     condition = "inpcst_unn_id flagut.cluster == ture",
                                     checkboxGroupInput(inputId = "FeaturePlot_3_var", label ="选择变量", choices = NULL)
                                   )
                                 ),
                                 # 展示区
                                 mainPanel(
                                   width = 10,
                                   fluidPage(
                                     fluidRow(
                                        column(6,plotOutput("FeaturePlot_wss"   , width = "100%"))
                                       ,column(6,plotOutput("FeaturePlot_hclust", width = "100%"))
                                       ),
                                     plotOutput("FeaturePlot_kmeans", width = "100%")
                                   )
                                 )
                               ) #sidebarLayout
                      ) #tabPanel
                     )
                )
            )
          ) , #tabItem
         tabItem(tabName = "menuItem_B",
              mainPanel(
                width =12,
                 fluidPage(
                   useShinyjs(),
                   position = c("left") ,
                   fluidRow(
                     column(2,actionButton("goButton_B", "Run", width="100%", class = "btn-success",icon = icon("refresh"))),
                     column(5, conditionalPanel(
                       condition = "inpcst_unn_id flagut.cluster == ture",
                       textInput("woebin_breaks_list",  label="断点列表"   , width="100%",value=NULL, placeholder='年龄=c(26,35,37,"Inf%,%missing"), 单位性质=c("个体经营","事业单位","党政机关","其他,%missing")')
                     )),
                     column(5,conditionalPanel(
                       condition = "inpcst_unn_id flagut.cluster == ture",
                       textInput("woebin_special_values", label="特定值", width="100%",value=NULL, placeholder='年龄=c(18,35,40,"50%,%missing"), 单位性质=c("个体经营","事业单位","党政机关","其他%,%missing")')
                     ))
                   ),
                    tabsetPanel(
                      tabPanel(title= "WOE转换结果" , verbatimTextOutput("bin_smy")),
                      tabPanel(title= "训练样本WOE", dataTableOutput("train_woe")),
                      tabPanel(title= "测试样本WOE", dataTableOutput("test_woe")),
                      tabPanel(title= "特征表现" , plotOutput("bin_plot_all", width = "100%", height = "1200px")),
                      tabPanel(title= "特征表现-单个", fluidPage(selectInput("show_bin_num", label="特征名称:",choices=NULL),
                                                           plotOutput("bin_plot", width = "50%"),
                                                           dataTableOutput("bin_tb") 
                      )                      
                      )
                   )
                 )
              )
    
          ), #tabItem
          tabItem(tabName = "menuItem_C",
            mainPanel(
              width = 12,
                fluidPage(
                  position = c("left") 
                  , actionButton("goButton_C", "Run", width="20%", class = "btn-success",icon = icon("refresh")),
                  tabsetPanel(
                    #tabPanel(title= "特征重要性-IV值", dataTableOutput("bin_var_tb")),
                    #tabPanel(title= "特征重要性-随机森林" , dataTableOutput("cf_var")) ,
                    tabPanel(title= "特征重要性", fluidRow(
                                                   box(title='IV值'    ,column(6,dataTableOutput("bin_var_tb"))),
                                                   box(title='随机森林',column(6,dataTableOutput("cf_var")))
                                                  )
                    )
                  )
                )
            )
         ),
         tabItem(tabName = "menuItem_D",
                 mainPanel(
                   width = 12,
                   fluidPage(
                     position = c("left") 
                     , fluidRow(column(5, actionButton("goButton_D", label="Run", width="40%", class = "btn-success",icon = icon("refresh"))), 
                                column(3, numericInput(inputId = "scorecard_point",label =  "基准分",value = "600")),
                                column(3, numericInput(inputId = "scorecard_pdo"  ,label =  "PDO"   ,value = "50" ))
                                
                     ),
                     tabsetPanel(
                       tabPanel(title= "模型summary",   fluidPage(verbatimTextOutput("model_summary" )
                                                                 ,plotOutput("model_desc")
                                                                 , height = "800px" )
                                ),
                      
                       #tabPanel(title= "训练样本ROC",   fluidRow(
                       #  column(6,plotOutput("perf_eva_train_1")) ,column(6,plotOutput("perf_eva_train_2")),
                       #  column(6,plotOutput("perf_eva_train_3")) ,column(6,plotOutput("perf_eva_train_4"))
                       #  )
                       tabPanel(title= "训练样本ROC",   plotOutput("perf_eva_train", width = "100%", height = "800px")),
                       #),
                       #tabPanel(title= "测试样本ROC",   fluidRow(
                       #  column(6,plotOutput("perf_eva_test_1")) ,column(6,plotOutput("perf_eva_test_2")),
                       #  column(6,plotOutput("perf_eva_test_3")) ,column(6,plotOutput("perf_eva_test_4"))
                       #)
                       tabPanel(title= "测试样本ROC",   plotOutput("perf_eva_test", width = "100%", height = "800px")),
                       #),
                      
                       tabPanel(title= "模型稳定性PSI", plotOutput("perf_eva_psi", width = "100%", height = "800px"))
                     )
                   )
                 )
         ),
         tabItem(tabName = "menuItem_E",
            sidebarLayout(
              sidebarPanel(
                width =3,
                fluidPage(
                     useShinyjs(),
                     position = c("left"),
                     actionButton("goButton_E", label="Run", width="100%", class = "btn-success",icon = icon("refresh")),
                     h1(""),
                     downloadButton("Dw_Button_ScoreCard","写出评分卡" , width="100%") ,
                     downloadButton("Dw_Button_Train_Score","Train分值", width="100%") ,
                     downloadButton("Dw_Button_Test_Score","Test分值"  , width="100%")
                 )
              ),
              mainPanel(
                width = 9,
                 fluidPage(
                    position = c("left") 
                    ,tabsetPanel(
                     tabPanel(title= "评分卡",  dataTableOutput("score_card"))
                  )
                )
              )
           )
         )

        #-------主框架-------------
       ) #tabItems
     ) #fluidRow
    ) #dashboardBody
  ) #dashboardPage
) #The End 

# runApp('/Users/john/ScorCrd')
# runApp('G:/MyPrj/ScorCrd')

