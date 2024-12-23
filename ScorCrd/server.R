
#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# 1 免登陆
DEBUG_FLAG = 1
#加载文件大小限制
options(shiny.maxRequestSize=30*1024^2)
#print(getOption("shiny.maxRequestSize"))
#加载库文件
#source("{你的目录}/require_libs.R", local=T, encoding="UTF-8")

#日志
flog.appender(appender.file("ScorCrd-INFO.log"),'INFO')
flog.appender(appender.file("ScorCrd-WARN.log"),'WARN')
flog.appender(appender.file("ScorCrd-ERROR.log"),'ERROR')
flog.info("Start ScorCrd ...", name='INFO')

#options(shiny.maxRequestSize=70*1024^2)
#flog.threshold(DEBUG)
#library(shinydashboard)
Logged = FALSE
my_username <- "test"
my_password <- "test"
cols <- c('yellow','yellow green','green','blue green' ,'blue', 'blue violet','violet','red violet', 'red', 'red orange', 'orange', 'yellow orange')
# 将“字符型字段”转换成数值字符串
mStr2c <- function(val_str){
  ret <- c(unlist(strsplit(val_str," ")))
  names(ret) <-c(unlist(strsplit(ret," ")))
  #print(length(ret))
  for(i in 0:length(unlist(strsplit(ret," ")))) {
    ret[i]="character"
  }
  return(ret)
}

mStr2c2 <- function(val_str){
  ret <- c(unlist(strsplit(val_str," ")))
  names(ret) <- c(unlist(strsplit(ret," ")))
  
  return(c(names(ret)))
}


browse_dt<-function(filepart, charcols){
  inFile <- filepart
  if (is.null(inFile)){
    print("文件目录为空")
    return(NULL)
  }else{
    
    if(length(charcols)>0){
      pb <- data.table::fread(inFile$datapath, colClasses = charcols, integer64="numeric", header=TRUE ) # , encoding="UTF-8")
    }else{
      pb <- data.table::fread(inFile$datapath, colClasses = NULL    , integer64="numeric", header=TRUE ) # , encoding="UTF-8")
    }

    return(data.frame(pb))
  }
}


# 数据抽样
mSample<-function(inp_data, sample_rate){

  i = 186
  . <- good_rows <- bad_rows <- NULL
  
  set.seed(i)
  
  if(is.null(inp_data))
    return(NULL)

  sample_rate<-as.double(sample_rate)
  
  if ( round(length(which(inp_data$flag == 0)) * sample_rate, 0) > 0 ) {
    good_rows <-  round(length(which(inp_data$flag == 0)) * sample_rate, 0) 
  }else{
    good_rows <- 1
  }
  if ( round(length(which(inp_data$flag == 1)) * sample_rate, 0) > 0 ) {
    bad_rows <-  round(length(which(inp_data$flag == 1)) * sample_rate , 0) 
  }else{
    bad_rows <- 1
  }
  
  #flog.info(good_rows)
  
  train_good_idx <- sample((which(inp_data$flag == 0)), good_rows, replace = F)
  train_bad_idx  <- sample((which(inp_data$flag == 1)), bad_rows , replace = F)
  idx_train_total <- c(train_good_idx, train_bad_idx)
  train <- inp_data[idx_train_total,]
  test  <- inp_data[-idx_train_total,]
  
  return (list(train=train, test=test))
}


mWoebin<-function(dt, y){
  bins <- scorecard::woebin(dt=dt, y = y, print_step = 0)
  
  return(bins)
}

#获取WOE转换后的结果转成data.frame
woe_2_dataframe <- function(x) {
  DM<-data.frame()
  for(i in 1:length(names(x)))
  {
    DM<-rbind(DM,data.frame(x[[i]]))
  }
  DM
}

#获取card转换后的结果转成data.frame
card_2_dataframe <- function(x) {
  DM<-data.frame()
  for(i in 1:length(names(x)))
  {
    DM<-rbind(DM,cbind(x[[i]]$variable, x[[i]]$bin, x[[i]]$points))
  }
  #names(DM) <- c('特征','区间','分值')
  names(DM) <- c('VAR','BIN','POINTS')
  
  return(DM)
}

errorFunc <-function(err, buttonId){
  
  errEl    <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- err$message
  shinyjs::html(html=errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim=TRUE, animType = "fade")
}

withBusyIndicatorServer<-function(buttonId, bnt_list, expr){
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)  
  donegEl   <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)  
  errEl     <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  
#  shinyjs::disable(buttonId)
#  shinyjs::show(selector=loadingEl)
#  shinyjs::hide(selector=donegEl)
#  shinyjs::hide(selector=errEl)
  
  #msg_id <- showNotification("正在处理，请稍后...", duration = 0, type = "message")
  #disable("goButton_A")
  
  
#  on.exit({
#    shinyjs::enable(buttonId)
#    shinyjs::hide(selector=loadingEl)
#  })
  
  disable(selector = 'body')

  tryCatch({
    value <- expr
    
    #shinyjs::show(selector = donegEl)
    #shinyjs::delay(2000, shinyjs::hide(selector = donegEl, anim=TRUE , animType = 'fade', time=0.5))

    value
    
  },error = function(err){errorFunc(err, buttonId) })
  
  enable(selector = 'body')

  for(i in 1:length(bnt_list$id))
  {
    if(i <= bnt_list$id[bnt_list$bnt==buttonId]+1)
    {
      shinyjs::enable(bnt_list[i,]$bnt)
      #print(paste("enable：", bnt_list[i,]$bnt) ) 
    }else
    {
      shinyjs::disable(bnt_list[i,]$bnt)
      #print(paste("disable：", bnt_list[i,]$bnt) ) 
    }
  }
}

withBusyIndicatorServer_2 <-function(buttonId, msg, expr){
  
  msg_id <- showNotification(paste(msg, "：正在处理，请稍后..." ), duration = 0, type = "message")
  
  tryCatch({
    value <- expr
    value
    
  },error = function(err){errorFunc(err, buttonId) })
 
  on.exit({

      removeNotification(msg_id)

  })
  
}

#画箱线图
mplotbox <- function(y, x, title, lab_x, lab_y )
{
  ggplot2::ggplot() +
    geom_violin(aes(y=y, x=x, group=x, col=x), trim=FALSE) +
    geom_boxplot(aes(y=y, x=x, group=x, col=x), width=0.2) +
    ggtitle(title) + 
    xlab(label=lab_x) + 
    ylab(label=lab_y) + 
    theme_classic()
}

bnt_list<-data.frame(id=c(1,2,3,4,5,6)
                           ,bnt=c('goButton_A','goButton_A2','goButton_B','goButton_C','goButton_D','goButton_E')
)

shinyServer(function(input, output, session) {
  
  # 全局变量
  login <- reactiveValues(authenticated = FALSE)
  g_status <- reactiveValues(running = FALSE)
  g_var <- reactiveValues()
  g_dt  <- reactiveValues()
  g_srt <- reactiveValues()
  g_bnt <- reactiveValues()
  
  observe({
    g_var$browse_file <- input$file1
    g_var$charcolname <- input$charcolname
    g_var$sample_rate <- input$sample_rate
    g_var$filter_var  <- input$filter_var
    g_var$scorecard_pdo    <- input$scorecard_pdo
    g_var$scorecard_point  <- input$scorecard_point
    g_var$woebin_breaks_list   <- input$woebin_breaks_list
    g_var$woebin_special_values<- input$woebin_special_values
  })
  
  
  # Return the UI for a modal dialog with data selection input. If 'failed' 
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      title = "身份认证",
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      footer = tagList(
        #modalButton("Cancel"),
        actionButton("ok", "登陆")
      )
    )
  }
  pMessage <- function(msg='告警！！！') {
    modalDialog(
      title = "提示信息",
      msg,
      footer = tagList(
        modalButton("Cancel")
      )
    )
  }
  
  # Show modal when button is clicked.  
  # This `observe` is suspended only whith right user credential
  obs1 <- observe({
    if (DEBUG_FLAG == 0){
      showModal(dataModal())  
    }
    else
    {
      print("DEBUG")
    }
  })
  
  obs2 <- observe({
    req(input$ok)
    isolate({
      Username <- input$username
      Password <- input$password
    })
    Id.username <- which(my_username == Username)
    Id.password <- which(my_password == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username == Id.password) {
        Logged <- TRUE
        login$authenticated <- TRUE
        obs1$suspend()
        removeModal()
        flog.info("[登录] user(%s) login", Username, name='INFO')
      } else {
        login$authenticated <- FALSE
      }     
    }
  })
  
  output$dataInfo <- renderPrint({
    if (login$authenticated) "OK!!!!!"
    else "You are NOT authenticated"
  })
  
  obs_menuItem_A <- observeEvent(input$goButton_A, {
    withBusyIndicatorServer("goButton_A", bnt_list, {
      
      #disable(selector = 'body')
      
      flog.info("[数据加载] execution", name='INFO')
      flog.info("input file is %s" , g_var$browse_file, name='INFO')
      flog.info("char_var is %s"   , g_var$charcolname, name='INFO')
      flog.info("filter_var is %s" , g_var$filter_var , name='INFO')
      flog.info("sample_rate is %s", g_var$sample_rate, name='INFO')
  
      if( !is.null(isolate(g_var$browse_file)) ) {
        tryCatch(g_dt_file_tmp <- browse_dt(isolate(g_var$browse_file) , mStr2c(isolate(g_var$charcolname)))
                 ,warning=function(w){
                   TryCatch_msg = paste("obs_menuItem_A.browse_dt: ", w)
                   flog.info(TryCatch_msg, w, name='WARN')
                   sprintf('告警:%s', TryCatch_msg)
                   showModal(pMessage(paste("告警：", TryCatch_msg)))
                 }
                 ,error=function(e){
                   TryCatch_msg = paste("obs_menuItem_A.browse_dt: ", e)
                   flog.info(TryCatch_msg, e, name='ERROR')
                   sprintf('错误:%s', TryCatch_msg)
                   showModal(pMessage(paste("错误：", TryCatch_msg, "")))
                   
                 }
        )
        
        vars <- names(g_dt_file_tmp)
        var_filter <- vars[!vars %in% mStr2c2(isolate(g_var$filter_var))]
  
        g_dt$g_dt_file  <- data.frame(g_dt_file_tmp[, var_filter])
  
        tryCatch(mod_dat <- mSample(g_dt$g_dt_file, isolate(g_var$sample_rate))
                 ,warning=function(w){
                   TryCatch_msg = paste("obs_menuItem_A.mSample: ", w)
                   flog.info(TryCatch_msg, w, name='WARN')
                   sprintf('告警:%s', TryCatch_msg)
                   showModal(pMessage(paste("告警：", TryCatch_msg)))
                 }
                 ,error=function(e){
                   TryCatch_msg = paste("obs_menuItem_A.mSample: ", e)
                   flog.info(TryCatch_msg, e, name='ERROR')
                   sprintf('错误:%s', TryCatch_msg)
                   showModal(pMessage(paste("错误：", TryCatch_msg," [常见错误：缺少flag字段，或者其非0、1]")))
                   
                 }
        )
        ##保留源数据
        g_srt$train <-mod_dat$train
        g_srt$test  <-mod_dat$test
        
        g_dt$train <- data.frame(mod_dat$train)
        g_dt$test  <- data.frame(mod_dat$test)
        flog.info("Sample Data：train = %s, train bad= %s, test= %s, test bad= %s"
                  , nrow(mod_dat$train ), length(mod_dat$train$flag=='1')
                  , nrow(mod_dat$test)  , length(mod_dat$test$flag =='1')
        , name='INFO')
  
        output$dat_smy <- renderPrint({print(summary(g_dt$g_dt_file))})
        
        output$dat_str<- renderPrint({print(str(g_dt$g_dt_file))})
        
        output$contents <- DT::renderDataTable({g_dt$g_dt_file}
                                               ,  options = list(scrollX= TRUE, searchHlight=TRUE)
        )
        
        output$dat_train <- DT::renderDataTable({g_dt$train}
                                                ,  options = list(scrollX= TRUE, searchHlight=TRUE)
        )
        
        output$dat_test <- DT::renderDataTable({g_dt$test}
                                               ,  options = list(scrollX= TRUE, searchHlight=TRUE)
        )
      }
      
      if(input$select=="error"){
        stop("choose anoter option")
      }
      
     #enable(selector = 'body')
     
    })
    
    })
  
  #数据分析
  obs_menuItem_A2 <- observeEvent(input$goButton_A2, {
    
    withBusyIndicatorServer("goButton_A2", bnt_list, {
      
      flog.info("[样本分析] executing ...", name='INFO')
      
      #变量列表处理(所有)
      lab_var= as.vector(names(g_dt$g_dt_file))
      lab_var= lab_var[!lab_var %in% c('cst_unn_id','flag')]
      

      #变量列表处理(只有数值变量)
      is_whole <- function(x) all(class(x) %in% c('integer','numeric'))
      g_dt.numeric <- data.frame(select_if(g_dt$g_dt_file, is_whole))
      lab_var.numeric= as.vector(names(g_dt.numeric))
      lab_var.numeric= lab_var.numeric[!lab_var.numeric %in% c('cst_unn_id','flag')]
      
     updateSelectizeInput(session, "FeaturePlot_1_y"
                           ,label = "选取Y值"
                           ,choices = lab_var.numeric
                           , selected = head(lab_var.numeric, 1)
      )
      updateSelectizeInput(session, "FeaturePlot_1_x"
                           ,label = "选取X值"
                           ,choices = lab_var.numeric
                           , selected = head(lab_var.numeric, 2)
      )
      
      #聚类归一化
      clust_dt <- g_dt.numeric[ ,lab_var.numeric]
      clust_dt <- na.omit(clust_dt)
      clust_dt <- data.frame(scale(clust_dt))
      
      updateCheckboxGroupInput(session, "FeaturePlot_0_var", choices = lab_var.numeric ,selected = tail(lab_var.numeric, 10) )
      updateCheckboxGroupInput(session, "FeaturePlot_2_var", choices = lab_var.numeric ,selected = tail(lab_var.numeric, 10) )
      updateCheckboxGroupInput(session, "FeaturePlot_3_var", choices = lab_var.numeric ,selected = lab_var.numeric )
      
       #数据分布情况
        output$FeaturePlot_0 <- renderPlot({
          flog.info("[样本分析] 数据分布情况 执行完成。", name='INFO')
          
          tryCatch( chart.Correlation(g_dt.numeric[ ,input$FeaturePlot_0_var]
                                      , method=input$FeaturePlot_0_method
                                      )
                   # ,warning=function(w){
                   #   flog.info("obs_menuItem_A2: ", w, name='WARN')
                   #   sprintf('告警:%s', w)
                   #   showModal(pMessage("告警"))
                   # }
                   # ,error=function(e){
                   #   flog.info("obs_menuItem_A2: ", e, name='ERROR')
                   #   sprintf('错误:%s', e)
                   #   showModal(pMessage("数据错误，请检查输入参数"))
                   # }
                    
                    ,warning=function(w){
                      TryCatch_msg = paste("obs_menuItem_A2.chart.Correlation: ", w)
                      flog.info(TryCatch_msg, w, name='WARN')
                      sprintf('告警:%s', TryCatch_msg)
                      showModal(pMessage(paste("告警：", TryCatch_msg)))
                    }
                    ,error=function(e){
                      TryCatch_msg = paste("obs_menuItem_A2.chart.Correlation: ", e)
                      flog.info(TryCatch_msg, e, name='ERROR')
                      sprintf('错误:%s', TryCatch_msg)
                      showModal(pMessage(paste("错误：", TryCatch_msg)))
                      
                    }
          )
          
        })
        
        #变量分析
        output$FeaturePlot_1 <- renderPlot({
          
          if(input$FeaturePlot_1_x=='' || input$FeaturePlot_1_y=='')
          {
            return(NULL)
          }
          input_x=input$FeaturePlot_1_x
          input_y=input$FeaturePlot_1_y
          x=g_dt.numeric[ ,input_x]
          y=g_dt.numeric[ ,input_y]
          col=g_dt$g_dt_file[ ,'flag']
          
          df1=data.frame(x=x, y=y, col=col)
          df_x=data.frame(x=1:length(x), y=x, col=col)
          df_y=data.frame(x=1:length(y), y=y, col=col)
          
          p1 <- ggplot(df1 , aes(x=x, y=y)) + geom_pointdensity( size=3) + scale_colour_viridis_c() + facet_wrap( ~ col) + geom_density2d(size =0.5,col='red') + ggtitle(paste0(input_x,"-",input_y)) + xlab(label=input_x) + ylab(label=input_y) + theme_classic()
          p2 <- ggplot(df_y, aes(x=x, y=y)) + geom_pointdensity( size=3) + scale_colour_viridis_c() + facet_wrap( ~ col) + geom_density2d(size =0.5,col='red') + ggtitle(label = input_y) + ylab(label=input_y)+ xlab(label="") 
          p3 <- ggplot(df_x, aes(x=x, y=y)) + geom_pointdensity( size=3) + scale_colour_viridis_c() + facet_wrap( ~ col) + geom_density2d(size =0.5,col='red') + ggtitle(label = input_x) + ylab(label=input_x)+ xlab(label="") 
          
          flog.info("[样本分析] 变量分析 执行完成。", name='INFO')
          tryCatch( #cowplot::plot_grid(p1, plot_grid(p2, p3, ncol=2), nrow=2)
                    cowplot::plot_grid(p1, p2, p3, nrow=3)
                    #,warning=function(w){
                    #  flog.info("obs_menuItem_A2: ", w, name='WARN')
                    #  sprintf('告警:%s', w)
                    #  showModal(pMessage("告警"))
                    #}
                    #,error=function(e){
                    #  flog.info("obs_menuItem_A2: ", e, name='ERROR')
                    #  sprintf('错误:%s', e)
                    #  showModal(pMessage("数据错误，请检查输入参数"))
                    #}
                    ,warning=function(w){
                      TryCatch_msg = paste("obs_menuItem_A2.plot_grid: ", w)
                      flog.info(TryCatch_msg, w, name='WARN')
                      sprintf('告警:%s', TryCatch_msg)
                      showModal(pMessage(paste("告警：", TryCatch_msg)))
                    }
                    ,error=function(e){
                      TryCatch_msg = paste("obs_menuItem_A2.plot_grid: ", e)
                      flog.info(TryCatch_msg, e, name='ERROR')
                      sprintf('错误:%s', TryCatch_msg)
                      showModal(pMessage(paste("错误：", TryCatch_msg)))
                    }
          )
          
        })
        #相关性
        output$FeaturePlot_2 <- renderPlot({
          flog.info("[样本分析] 相关性 执行完成。", name='INFO')
          tryCatch( corrplot(cor(g_dt.numeric[ ,input$FeaturePlot_2_var])
                             , type  =input$FeaturePlot_2_type
                             , order =input$FeaturePlot_2_order
                             , tl.srt=input$FeaturePlot_2_srt
                             , tl.col = "black"
                             , method=input$FeaturePlot_2_method
                             )
                    #,warning=function(w){
                    #  flog.info("obs_menuItem_A2: ", w, name='WARN')
                    #  sprintf('告警:%s', w)
                    #  showModal(pMessage("告警"))
                    #}
                    #,error=function(e){
                    #  flog.info("obs_menuItem_A2: ", e, name='ERROR')
                    #  sprintf('错误:%s', e)
                    #  showModal(pMessage("数据错误，请检查输入参数"))
                    #}
                    ,warning=function(w){
                      TryCatch_msg = paste("obs_menuItem_A2.corrplot: ", w)
                      flog.info(TryCatch_msg, w, name='WARN')
                      sprintf('告警:%s', TryCatch_msg)
                      showModal(pMessage(paste("告警：", TryCatch_msg)))
                    }
                    ,error=function(e){
                      TryCatch_msg = paste("obs_menuItem_A2.corrplot: ", e)
                      flog.info(TryCatch_msg, e, name='ERROR')
                      sprintf('错误:%s', TryCatch_msg)
                      showModal(pMessage(paste("错误：", TryCatch_msg)))
                    }
          )
          
        })
        
        #碎石图
        output$FeaturePlot_wss <- renderPlot({
          if(length(input$FeaturePlot_3_var)<2){
            showModal(pMessage("数据错误"))
            return(NULL)
          }
          
          clust_dt <-clust_dt[ ,input$FeaturePlot_3_var]
          wss <- (nrow(clust_dt)-1) * sum(apply(clust_dt, 2, var)) #计算离均差平方和
          for(i in 2:15) {
            wss[i] <- sum( kmeans(clust_dt, centers=i)$withinss )  #计算不通聚类格式的组内平方和
          }
          flog.info("[样本分析] 碎石图 执行完成。", name='INFO')
          tryCatch( plot(1:length(wss), wss, type='b', xlab='集群个数', ylab='离均差平方和')
                    #,warning=function(w){
                    #  flog.info("obs_menuItem_A2: ", w, name='WARN')
                    #  sprintf('告警:%s', w)
                    #  showModal(pMessage("告警"))
                    #}
                    #,error=function(e){
                    #  flog.info("obs_menuItem_A2: ", e, name='ERROR')
                    #  sprintf('错误:%s', e)
                    #  showModal(pMessage("数据错误，请检查输入参数"))
                    #}
                    ,warning=function(w){
                      TryCatch_msg = paste("obs_menuItem_A2.plot: ", w)
                      flog.info(TryCatch_msg, w, name='WARN')
                      sprintf('告警:%s', TryCatch_msg)
                      showModal(pMessage(paste("告警：", TryCatch_msg)))
                    }
                    ,error=function(e){
                      TryCatch_msg = paste("obs_menuItem_A2.plot: ", e)
                      flog.info(TryCatch_msg, e, name='ERROR')
                      sprintf('错误:%s', TryCatch_msg)
                      showModal(pMessage(paste("错误：", TryCatch_msg)))
                    }
          )
         
        })
        #箱线图
        output$FeaturePlot_4 <- renderPlot({
          
          dt <- g_dt.numeric[lab_var.numeric]
          flag= g_dt.numeric[ ,'flag']
          #dt=iris
          #flag=iris[,'Species']
          len_nm <- length(dt)
          
          plist =list
          plist=NULL
          p=  list()
          
          for(i in 1:len_nm)
          {
            p[[i]]  <- do.call(mplotbox, args=list( y= dt[,i], x=flag, title=lab_var.numeric[i],lab_x='flag', lab_y=names(dt[i])))
            plist  <- c(plist, paste0("p[[", i,"]]", collapse = ','))
          }
          
          plist<-paste0(plist,collapse = ',')
          #print(plist)
          plist = eval(parse(text = paste0("list(", plist,")")))
          #print(plist)
          byrows <- sqrt(len_nm)
          
          if(byrows %% 1 > 0 )
          {
            nrow = floor(byrows)+1
          }else
          {
            nrow = floor(byrows)
          }
          
          if((len_nm/nrow) %% 1 > 0)
          {
            ncol = floor(len_nm/nrow) +1
          }else
          {
            ncol = floor(len_nm/nrow)
          }
          
          flog.info("[样本分析] 箱线图 执行完成。", name='INFO')
          
          plist = c(plist, list(nrow = nrow, ncol= ncol))
          do.call(cowplot::plot_grid, plist)
          
          
        })
        
        #层次聚类
        output$FeaturePlot_hclust <- renderPlot({
          if(length(input$FeaturePlot_3_var)<2)
          {
            showModal(pMessage("数据错误"))
            return(NULL)
          }
          
          clust_dt <-clust_dt[ ,input$FeaturePlot_3_var]
          d <- dist(clust_dt, method = 'euclidean')
          fit_hclust <- hclust(d, method='ward.D')
          
         # rect.hclust(fit_hclust, k=3, border='red')
          
          flog.info("[样本分析] 层次聚类 执行完成。", name='INFO')
          tryCatch(  plot(fit_hclust,  labels = FALSE, hang = -1, main = "Tree")
                     #,warning=function(w){
                     # flog.info("obs_menuItem_A2: ", w, name='WARN')
                     #  sprintf('告警:%s', w)
                     #  showModal(pMessage("告警"))
                     #}
                     #,error=function(e){
                     # flog.info("obs_menuItem_A2: ", e, name='ERROR')
                     # sprintf('错误:%s', e)
                     # showModal(pMessage("数据错误，请检查输入参数"))
                     #}
                     
                     ,warning=function(w){
                       TryCatch_msg = paste("obs_menuItem_A2.plot.fit_hclust: ", w)
                       flog.info(TryCatch_msg, w, name='WARN')
                       sprintf('告警:%s', TryCatch_msg)
                       showModal(pMessage(paste("告警：", TryCatch_msg)))
                     }
                     ,error=function(e){
                       TryCatch_msg = paste("obs_menuItem_A2.plot.fit_hclust: ", e)
                       flog.info(TryCatch_msg, e, name='ERROR')
                       sprintf('错误:%s', TryCatch_msg)
                       showModal(pMessage(paste("错误：", TryCatch_msg)))
                     }
          )
          
        })
        
        #kmean聚类
        output$FeaturePlot_kmeans <- renderPlot({
          # 数据处理
          if(length(input$FeaturePlot_3_var)<2)
          {
            showModal(pMessage("数据错误"))
            return(NULL)
          }
          clust_dt <-clust_dt[ ,input$FeaturePlot_3_var]

          fit_kmeans <- kmeans(clust_dt, centers=input$FeaturePlot_3_clust_cnt)
          res <- data.frame(clust_dt, fit_kmeans$cluster)
          flog.info("[样本分析] kmean聚类 执行完成。", name='INFO')
          tryCatch(  clusplot(clust_dt, fit_kmeans$cluster, color = TRUE, shade = TRUE)
                    #,warning=function(w){
                    #  flog.info("obs_menuItem_A2: ", w, name='WARN')
                    #  sprintf('告警:%s', w)
                    #  showModal(pMessage("告警"))
                    # }
                    #,error=function(e){
                    #  flog.info("obs_menuItem_A2: ", e, name='ERROR')
                    #  sprintf('错误:%s', e)
                    #  showModal(pMessage("数据错误，请检查输入参数"))
                    #}
                     ,warning=function(w){
                       TryCatch_msg = paste("obs_menuItem_A2.plot.fit_kmeans: ", w)
                       flog.info(TryCatch_msg, w, name='WARN')
                       sprintf('告警:%s', TryCatch_msg)
                       showModal(pMessage(paste("告警：", TryCatch_msg)))
                     }
                     ,error=function(e){
                       TryCatch_msg = paste("obs_menuItem_A2.plot.fit_kmeans: ", e)
                       flog.info(TryCatch_msg, e, name='ERROR')
                       sprintf('错误:%s', TryCatch_msg)
                       showModal(pMessage(paste("错误：", TryCatch_msg)))
                     }
          )
          
        })
        
      if(input$select=="error"){
        stop("choose anoter option")
      }
      
    })
  })
  
  #Woe转换
  obs_menuItem_B <- observeEvent(input$goButton_B, {

    withBusyIndicatorServer("goButton_B", bnt_list, {
      flog.info("[Woe转换] executing ...", name='INFO')
      woebin_breaks_list <- isolate(g_var$woebin_breaks_list)
      woebin_special_values <- isolate(g_var$woebin_special_values)
      
      woebin_breaks_list    <- paste0("list(",isolate(woebin_breaks_list),")")
      woebin_special_values <- paste0("list(",isolate(woebin_special_values),")")
      flog.info("[Woe转换] 分箱 执行中...", name='INFO')
      tryCatch(  #g_dt$bins <- mWoebin(dt = g_dt$train, y='flag' , breaks_list=list(g_var$woebin_breaks_list) )
                   g_dt$bins <- scorecard::woebin(dt=g_dt$train, y = 'flag' 
                                                  , breaks_list=eval(parse(text=woebin_breaks_list))
                                                  , special_values=eval(parse(text=woebin_special_values))
                                                  , method = "chimerge"
                                            )
                  #,error=function(e){
                  #   flog.info("obs_menuItem_B: ", e, name='ERROR')
                  #   sprintf('错误:%s', e)
                  #   showModal(pMessage("数据错误，请检查输入参数"))
                  # }
                   ,error=function(e){
                     TryCatch_msg = paste("obs_menuItem_B.woebin: ", e)
                     flog.info(TryCatch_msg, e, name='ERROR')
                     sprintf('错误:%s', TryCatch_msg)
                     showModal(pMessage(paste("错误：", TryCatch_msg)))
                   }
        )
      flog.info("[Woe转换] 分箱 执行完", name='INFO')
      g_dt$train_woe <- data.frame(woebin_ply(g_dt$train, g_dt$bins, print_step = 0))#, to='woe'))
      flog.info("[Woe转换] 训练集woe转换 执行完成", name='INFO')
      g_dt$test_woe  <- data.frame(woebin_ply(g_dt$test, g_dt$bins , print_step = 0))#, to='woe'))
      flog.info("[Woe转换] 测试集woe转换 执行完成", name='INFO')
      output$bin_smy <- renderPrint({ summary(g_dt$bins) })
      
      bin_col_nm <- c(names(g_dt$bins))
      
      output$train_woe <- renderDataTable({
        #woe_tmp <- data.frame(woebin_ply(g_dt$train, g_dt$bins, print_step = 0, to='woe'))
        #bin_tmp <- data.frame(woebin_ply(g_dt$train, g_dt$bins, print_step = 0, to='bin'))
        #g_dt$train_woe <-  cbind(bin_tmp,woe_tmp[,-1])
        flog.info("[Woe转换] 训练集woe展示 执行完成", name='INFO')
        g_dt$train_woe #<-  woe_tmp
        #flog.info("[Woe转换] Woe转换 执行完成", name='INFO')
      }
      ,  options = list(scrollX= TRUE, searchHlight=TRUE)
      )
      
      output$test_woe <- renderDataTable({
        #woe_tmp <- data.frame(woebin_ply(g_dt$test, g_dt$bins, print_step = 0, to='woe'))
        #bin_tmp <- data.frame(woebin_ply(g_dt$test, g_dt$bins, print_step = 0, to='bin'))
        #$test_woe <-  cbind(bin_tmp,woe_tmp[,-1])
        flog.info("[Woe转换] 测试集woe展示 执行完成", name='INFO')
        g_dt$test_woe #<-  woe_tmp
      }
      ,  options = list(scrollX= TRUE, searchHlight=TRUE)
      )
      
      #变量分箱结果可视化变
      lab_var= as.vector(bin_col_nm)
      updateSelectizeInput(session, "show_bin_num"
                           ,label = paste("Select input label", length(lab_var))
                           ,choices = lab_var
                           , selected = head(lab_var, 1)
      )
      
      output$bin_plot<-renderPlot({
        
        flog.info("[Woe转换] 测试集woe展示 执行完成", name='INFO')
        if (length(bin_col_nm[bin_col_nm %in% as.character(input$show_bin_num)]) > 0 ){
          woebin_plot(g_dt$bins[which(bin_col_nm == as.character(input$show_bin_num))] )
        }
        else {
          woebin_plot(g_dt$bins[bin_col_nm[1]])
        }

      })
      
      output$bin_plot_all<-renderPlot({
        
        plist =list
        plist=NULL
        p=  list()
        for(i in 1:length(bin_col_nm))
        {
          p[[i]] = woebin_plot(g_dt$bins[bin_col_nm[i]])[[bin_col_nm[i]]]
          plist <- c(plist, paste0("p[[", i,"]]", collapse = ','))
        }
        
        plist<-paste0(plist,collapse = ',')
        #print(plist)
        plist = eval(parse(text = paste0("list(", plist,")")))
        
        len_nm <- length(bin_col_nm)
        byrows <- sqrt(len_nm)
        
        if(byrows %% 1 > 0 )
        {
          nrow = floor(byrows)+1
        }else
        {
          nrow = floor(byrows)
        }
        
        if((len_nm/nrow) %% 1 > 0)
        {
          ncol = floor(len_nm/nrow) +1
        }else
        {
          ncol = floor(len_nm/nrow)
        }
        
        plist = c(plist, list(nrow = nrow, ncol= ncol))
        do.call(cowplot::plot_grid, plist)
      })
      
      #
      output$bin_tb<-renderDataTable({
        
        if (length(bin_col_nm[bin_col_nm %in% as.character(input$show_bin_num)]) > 0 ){
          var_iv <- g_dt$bins[[which(bin_col_nm == as.character(input$show_bin_num))]]
        }
        else {
          var_iv <- g_dt$bins[[bin_col_nm[1]]]
        }
        data.frame(var_iv)
      }
      ,  options = list(scrollX= TRUE, searchHlight=TRUE)
      )
      
     #str( g_dt$train_woe)
     #str(g_dt$bins)
      if(input$select=="error"){
        stop("choose anoter option")
      }
      
    })
  })
  
  obs_menuItem_C <- observeEvent(input$goButton_C, {
    withBusyIndicatorServer("goButton_C", bnt_list, {

      flog.info("[特征表现] executing ...", name='INFO')

      bins_data <- woe_2_dataframe(g_dt$bins)
      total_iv <- unique(data.frame(bins_data[1],bins_data[10]))
      total_iv_sort <- total_iv[order(total_iv$total_iv ,decreasing = T),]
      #print(total_iv_sort)
      #print(sort(total_iv$total_iv, decreasing = T))

      names(total_iv_sort) <- c('vars', 'woe_iv');
      g_dt$bins_woe_iv <- total_iv_sort
      
      #变量分箱结果IV排序
      output$bin_var_tb<-renderDataTable({
        #total_iv_sort
        #barplot(woe_iv ~ vars, data = g_dt$bins_woe_iv, main="IV"
                #, col=c(love_color(type = "lightnihon_8x1"), love_color(type = "darknihon_8x1"))
        #       ) 
        flog.info("[特征表现] 变量分箱结果IV排序 执行完成。", name='INFO')
        data.frame(g_dt$bins_woe_iv)
        #plot_bar(dat= g_dt$bins_woe_iv, x='vars',y='woe_iv')
        
       }
       ,  options = list(scrollX= TRUE, searchHlight=TRUE)
      )
      
      #随机森林
      output$cf_var<-renderDataTable({
        cft <-party::cforest(flag~., data = g_dt$train_woe,
                             controls = cforest_unbiased(mtry = 3, ntree=25))
        var1 <- party::varimp(cft)
        var1 <- var1[order(var1,decreasing = F)]
        cf_var_tb <- data.frame(vars=names(var1), imp_rt=as.numeric(var1))
        
        #barplot(imp_rt ~ vars, data = cf_var_tb, ylab="重要系数", main="随机森林特征筛选"
        #        #, col=c(love_color(type = "lightnihon_8x1"), love_color(type = "darknihon_8x1"))
        #)
        flog.info("[特征表现] 随机森林 执行完成。", name='INFO')
        data.frame(cf_var_tb)
        
      }
      ,  options = list(scrollX= TRUE, searchHlight=TRUE)
      )

      if(input$select=="error"){
        stop("choose anoter option")
      }
      
    })
  })

  obs_menuItem_D <- observeEvent(input$goButton_D, {
   withBusyIndicatorServer("goButton_D", bnt_list, {

     flog.info("[模型评估] executing ...", name='INFO')
    
withProgress({
  setProgress(message = '模型评估中...', value = 0)
  g_progress_cnt=6
      # 回归
      data_model <- glm(flag~., data = g_dt$train_woe, family = "binomial")#,col_idx
      flog.info("[模型评估] glm逻辑回归 执行完成。", name='INFO')
  incProgress(1/g_progress_cnt)
  showNotification("glm逻辑回归执行完成", duration = 3, type='message')
      #逐步回归，选择变量
      data_stepwise <- step(data_model, direction = "both")
      g_dt$eval_m   <- eval(data_stepwise$call)
      flog.info("[模型评估] data_stepwise 执行完成。", name='INFO')
  incProgress(1/g_progress_cnt)
  showNotification("data_stepwise 执行完成", duration = 3, type='message')
      #预测的概率
      g_dt$train_woe_pred <- predict(g_dt$eval_m, g_dt$train_woe, type='response')
      g_dt$test_woe_pred  <- predict(g_dt$eval_m, g_dt$test_woe , type='response')
      flog.info("[模型评估] 预测概率 执行完成。", name='INFO')
      showNotification("预测概率 执行完成", duration = 3, type='message')
  incProgress(1/g_progress_cnt)
      
      #生成评分卡
      pdo = g_var$scorecard_pdo
      points = g_var$scorecard_point
      g_dt$score_card <- scorecard(g_dt$bins, g_dt$eval_m , points=points, pdo=pdo )
      flog.info("[模型评估] 生成评分卡 执行完成。", name='INFO')
      showNotification("生成评分卡 执行完成", duration = 3, type='message')
  incProgress(1/g_progress_cnt)
      
      #验证评分集
      g_dt$train_score <- scorecard_ply(dt=g_dt$train, card=g_dt$score_card, print_step=0)
      g_dt$test_score  <- scorecard_ply(dt=g_dt$test , card=g_dt$score_card, print_step=0)
      showNotification("模型性能评估 执行完成", duration = 3, type='message')
  incProgress(1/g_progress_cnt)
      
      perf_train<- perf_eva(label=g_dt$train_woe$flag, g_dt$train_woe_pred, title='train'
                      , type = c('roc', 'ks', 'lift', 'pr')
                      , show_plot = TRUE)
      perf_test<- perf_eva(label=g_dt$test_woe$flag, g_dt$test_woe_pred, title='test'
                      , type = c('roc', 'ks', 'lift', 'pr')
                      , show_plot = TRUE)
      flog.info("[模型评估] 验证评分集 执行完成。", name='INFO')
  showNotification("验证评分集 执行完成", duration = 3, type='message')
  incProgress(1/g_progress_cnt)
      
      output$model_summary<-renderPrint({
        summary(g_dt$eval_m)
      })
      output$model_desc <- renderImage({

        filename <- normalizePath(file.path('./model_desc.jpeg'))
        list(src=filename)
      },
      deleteFile = FALSE
      )
      #---------------训练样本性能 ROC-AUC,KS
      #后期使用ggplot来优化
      #as_ggplot(train_perf$pic$grobs$pks)
      
      output$perf_eva_train <- renderPlot({
        p1 <- as_ggplot(perf_train$pic$grobs$pks)   #ks
        p2 <- as_ggplot(perf_train$pic$grobs$plift) #plift
        p3 <- as_ggplot(perf_train$pic$grobs$proc)  #auc
        p4 <- as_ggplot(perf_train$pic$grobs$ppr)   #p-R
        
        cowplot::plot_grid(p1, p2, p3, p4, nrow = 2, ncol= 2)
      })
      
      output$perf_eva_train_1<-renderPlot({
        plot(perf_train$pic$grobs$pks)   #ks
      })
      output$perf_eva_train_2<-renderPlot({
        plot(perf_train$pic$grobs$plift) #lift
      })
      output$perf_eva_train_3<-renderPlot({
        plot(perf_train$pic$grobs$proc)  #auc
      })
      output$perf_eva_train_4<-renderPlot({
        plot(perf_train$pic$grobs$ppr)   #p-R
      })
    
      #---------------测试样本性能 ROC-AUC,KS
      output$perf_eva_test <- renderPlot({
        p1 <- as_ggplot(perf_test$pic$grobs$pks)   #ks
        p2 <- as_ggplot(perf_test$pic$grobs$plift) #plift
        p3 <- as_ggplot(perf_test$pic$grobs$proc)  #auc
        p4 <- as_ggplot(perf_test$pic$grobs$ppr)   #p-R
        
        cowplot::plot_grid(p1, p2, p3, p4, nrow = 2, ncol= 2)
      })
      
      
      output$perf_eva_test_1<-renderPlot({
        plot(perf_test$pic$grobs$pks)   #ks
      })
      output$perf_eva_test_2<-renderPlot({
        plot(perf_test$pic$grobs$plift) #lift
      })
      output$perf_eva_test_3<-renderPlot({
        plot(perf_test$pic$grobs$proc)  #auc
      })
      output$perf_eva_test_4<-renderPlot({
        plot(perf_test$pic$grobs$ppr)   #p-R
      })
      
      output$perf_eva_psi<-renderPlot({
  
        #模型的稳定性度量：PSI
        train_score <-na.omit(g_dt$train_score)
        test_score  <-na.omit(g_dt$test_score)
        psi_ret <- perf_psi(
          score=list(train=train_score    , test=test_score),
          label=list(train=g_dt$train_woe$flag, test=g_dt$test_woe$flag)
        )
        psi_ret$pic
      })
      
      if(input$select=="error"){
        stop("choose anoter option")
      }
      #setProgress(message = '模型评估完成', value = 100)
      #removeNotification()
})
   })
  })
  
  obs_menuItem_E <- observeEvent(input$goButton_E, {
    withBusyIndicatorServer("goButton_E", bnt_list, {

      flog.info("[评分卡] executing ...", name='INFO')
      output$score_card <- renderDataTable({
        #将评分卡转变成数组
        data.frame(card_2_dataframe(g_dt$score_card ))
      })
      
      if(input$select=="error"){
        stop("choose anoter option")
      }
      
    })
  })
  
#  obs_menuItem_E <- observeEvent(input$goButton_E, {
#    withBusyIndicatorServer("goButton_E", {
#      
#      flog.info("[评分卡] executing ...", name='INFO')
#      output$score_card <- renderDataTable({
#        #将评分卡转变成数组
#        data.frame(card_2_dataframe(g_dt$score_card ))
#      })
      
#      if(input$select=="error"){
#        stop("choose anoter option")
#      }
#     
#    })
#  })

  output$Dw_Button_ScoreCard <- downloadHandler(
    filename = function() {
      paste0("ScoreCard_", Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
       dt <- data.frame( card_2_dataframe(g_dt$score_card ))
      write.csv(dt, file)
    },
    contentType = "text/csv"
  )
  output$Dw_Button_Train_Score <- downloadHandler(
    filename = function() {
      paste0("train_score", Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      #write.csv(data.frame(cbind(g_dt$train, pred=g_dt$train_woe_pred, score=g_dt$train_score)),file)
       write.csv(data.frame(cbind(g_srt$train, pred=g_dt$train_woe_pred, score=g_dt$train_score)),file)
    },
    contentType = "text/csv"
  )
  output$Dw_Button_Test_Score <- downloadHandler(
    filename = function() {
      paste0("test_score", Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      #write.csv(data.frame(cbind(g_dt$test, pred=g_dt$test_woe_pred, score=g_dt$test_score)) ,file)
       write.csv(data.frame(cbind(g_srt$test, pred=g_dt$test_woe_pred, score=g_dt$test_score)),file)
    },
    contentType = "text/csv"
  )
})

# runApp('/Users/john/ScorCrd/ScorCrd_v2.0.1', host="0.0.0.0", port=5555)
# runApp('G:/MyPrj/ScorCrd_v2.0.x/ScorCrd', host="0.0.0.0", port=5554)

