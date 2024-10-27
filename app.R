# 加载必要的库
library(shiny)
library(shinycssloaders)
library(shinyjs)  # Use shinyjs for controlling the loading animation
library(openxlsx)
library(DT)
library(limma)
library(DESeq2)
library(dplyr)
library(NGLVieweR)
library(RCurl)
library(httr)
library(r3dmol)
library(shinyWidgets)
library(visNetwork)
library(shinydashboard)
library(markdown)
# abstract<-readLines("abstract.txt",n=1)
# table_mutation<-read.xlsx("../突变数据/FVIII_Mut_Filter.xlsx")
# Clinical_Presentations<-read.xlsx("Clinical Presentations.xlsx")
# Human_Phenotypes<-read.xlsx("Human_Phenotypes.xlsx")
# Drug_Discovery<-read.xlsx("Drug_Discovery.xlsx")
# data_input1<-read.xlsx("HA_count_data_all.xlsx")
# data_input2<-read.xlsx("F8_KO_mRNA_count.xlsx")
# data_input3<-read.xlsx("F8_KO_miRNA_count.xlsx")
# protein_list<-read.xlsx("./all_data/Protein_table.xlsx")[,1]
# protein_table1<-read.xlsx("./all_data/Protein_table.xlsx")
# ligand_list <- read.xlsx("ligand_list.xlsx")
# ligand<-read.xlsx("HTMDv2_ligand.xlsx")
# id1 <- read.xlsx("nodes1.xlsx")[,1]
# label1<-read.xlsx("nodes1.xlsx")[,2]
# from1 <- read.xlsx("edges1.xlsx")[,1]
# to1<-read.xlsx("edges1.xlsx")[,2]
# id2 <- read.xlsx("nodes2.xlsx")[,1]
# label2<-read.xlsx("nodes2.xlsx")[,2]
# from2 <- read.xlsx("edges2.xlsx")[,1]
# to2<-read.xlsx("edges2.xlsx")[,2]
load("HemoAtlas.Rdata")
# save(abstract,table_mutation,Clinical_Presentations,Human_Phenotypes,
#      Drug_Discovery,protein_table1,data_input1,data_input2,data_input3,
#      protein_list,ligand_list,ligand,id1,id2,label1,label2,from1,from2,
#      to1,to2,
#      file = "HemoAtlas.Rdata")
# UI部分
ui <- fluidPage(
  # 启用shinyjs
  shinyjs::useShinyjs(),
  # 自定义CSS
  tags$head(
    tags$style(HTML("
    html, body {
        height: 100%;
        margin: 0;
        padding: 0;
    }
    .container {
        width: 100%;
        max-width: 1200px;
        margin: 0 auto;
    }
    .responsive-image {
      max-width: 100%;
      height: auto;
    }
      
    .home-page {
      background-image: url('https://geng-1317909610.cos.ap-beijing.myqcloud.com/HA/backgroud.jpg');
      background-size: cover;
      background-position: center;
      background-repeat: no-repeat;
      height: 90vh;
      display: flex;
      justify-content: center;
      align-items: center;
      text-align: center;
    }
    h1 {
      font-family: 'Arial', sans-serif;
      font-size: 40px;
      color: white;
    }
    h4 {
      font-family: 'Arial', sans-serif;
      font-size: 15px;
      color: white;
    }
    .info-box {
      background-color: rgba(255, 255, 255, 0.8);
      border: 2px solid #ccc;
      padding: 20px;
      border-radius: 10px;
      font-family: 'Arial', sans-serif;
      font-size: 20px;
      color: #333;
      max-width: 1000px;
      margin: 0 auto;
    }
    .image-link {
        display: inline-block; /* 或者 use 'float: left' */
        margin: 5px; /* 控制图片之间的间距 */
        text-align: center;
      }
    #search {
      display: block;
      width: 350%;
      border: 2px solid #ccc;
      padding: 20px;
      margin-left: 110px;
      border-radius: 10px;
      font-family: 'Arial', sans-serif;
      font-size: 20px;
      color: #333;
      max-width: 1000px;
    }
    .btn-center {
      display: block;
      margin: 10px auto;
      font-size: 18px;
    }

    /* Loading animation */
    #loading {
      position: absolute;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      z-index: 100;
      display: none;
    }
    
    "))
  ),
  tags$style(HTML(
    "
  .text-center {
    text-align: center;
  }
  "
  )),
  div(id = "loading",
      tags$img(src = "https://geng-1317909610.cos.ap-beijing.myqcloud.com/HA/load.gif",
               height = "150px", width = "150px")
  ),
  
  # 创建导航栏
  navbarPage("HemoAtlas", id = "navbar",
             
             # 首页标签
             tabPanel("Home",
                      div(class = "home-page", 
                          div(
                            h1("Hemophilia A Omics Repository"),
                            div(class = "info-box",
                                p(abstract)
                            ),
                            br(),
                            textInput("search", label = NULL, placeholder = "Input gene name"),
                            actionButton("search_button", "Search", class = "btn-center"),
                            br(),
                            a(href = "https://dbs.eahad.org/FVIII", class = "image-link1",
                              tags$img(src = "https://geng-1317909610.cos.ap-beijing.myqcloud.com/HA/1.1.png", alt = "Image1", width = "200px", height = "220px"),
                              a(href = "https://www.ncbi.nlm.nih.gov/gds/?term=hemophilia+a", class = "image-link2",
                                tags$img(src = "https://geng-1317909610.cos.ap-beijing.myqcloud.com/HA/2.1.png", alt = "Image2", width = "200px", height = "220px"),
                              a(href = "https://www.malacards.org/card/hemophilia_a#Genes_Related_wrapper", class = "image-link3",
                                tags$img(src = "https://geng-1317909610.cos.ap-beijing.myqcloud.com/HA/3.1.png", alt = "Image3", width = "200px", height = "220px"),
                              a(href = "https://www.uniprot.org/uniprotkb?query=hemophilia+a", class = "image-link4",
                                tags$img(src = "https://geng-1317909610.cos.ap-beijing.myqcloud.com/HA/4.1.png", alt = "Image4", width = "200px", height = "220px"),
                              a(href = "https://go.drugbank.com/unearth/q?searcher=indications&query=hemophilia+a&button=", class = "image-link5",
                                tags$img(src = "https://geng-1317909610.cos.ap-beijing.myqcloud.com/HA/5.1.png", alt = "Image5", width = "200px", height = "220px"),
                              a(href = "https://rpubs.com/gengjie/1237335", class = "image-link6",
                                tags$img(src = "https://geng-1317909610.cos.ap-beijing.myqcloud.com/HA/6.1.png", alt = "Image6", width = "200px", height = "220px")
                              )))))),
                            br(),
                            br(),
                            div(
                              h4("E-mail: geng20210226@163.com"))
                            ))),
             tabPanel("Genetic Information",
                      h2("FVIII gene mutation statistics of hemophilia patients"),
                      div(downloadButton("mutation_type", "mutation type"),
                          downloadButton("Patient_severity_JPG", "Patient severity")),
                      sidebarLayout(
                          column(3,
                                 fluidRow(  # 图片列，占页面宽度的3/12
                                   h3("mutation type"),
                                   imageOutput("Degree_of_mutation_type", height = "300px", width ="300px"),
                                   h3("Patient severity"),
                                   imageOutput("Patient_severity", height = "300px", width ="300px")  # 设置高度和宽度
                                 )),
                        mainPanel(
                          width = 9,
                          DTOutput("table_mutation")
                          )
                        )
                      ),
             tabPanel("Clinical Presentations",
              fluidRow(
                column(8,
                h2("Diseases related to Hemophilia A"),
                DTOutput("Clinical_Presentations")),
                column(4,
                       # 输出网络图的UI组件
                       visNetworkOutput("network1")
                      )),
              fluidRow(
                 column(8,
                 h2("Human Phenotypes for Hemophilia A"),
                 DTOutput("Human_Phenotypes")),
                 column(4,
                   # 输出网络图的UI组件
                   visNetworkOutput("network2")
                 ))
             ),
             tabPanel("Transcriptomic Data",
                      sidebarLayout(
                        sidebarPanel(
                            width = 3,
                            h3("data preprocessing"),
                          
                          # fileInput("file", "上传表达谱数据 (.xlsx)", accept = c(".xlsx")),
                          selectInput("dataset_choice", "selecting data", 
                                      choices = c("F8-KO mRNA" = "F8_KO_mRNA_count",
                                                  "F8-KO miRNA" = "F8_KO_miRNA_count",
                                                  "HA mRNA count" = "HA_count_data_all")),
                          uiOutput("control_samples_ui"),
                          uiOutput("experiment_samples_ui"),
                          actionButton("analyze_button", "RUN"),
                          br(),
                          br(),
                          div(div(downloadButton("download_volcano", "Volcano"),
                            downloadButton("download_KEGG_plot", "KEGG"),
                                  downloadButton("download_GO_plot", "GO"))
                        )),
                        mainPanel(
                          width = 9,
                          DTOutput("results_table"),
                          
                          # 使用 fluidRow 和 column 实现平行布局
                          fluidRow(
                            column(4,  # 第二列，占页面宽度的6/12
                                   h3("Volcano"),
                                   imageOutput("volcano_plot", height = "350px",width ="350px")  # 设置高度
                            ),
                            column(4,  # 第二列，占页面宽度的6/12
                                   h3("KEGG"),
                                   imageOutput("pathway_KEGG_plot", height = "350px",width ="350px")  # 设置高度
                            ),
                            column(4,  # 第二列，占页面宽度的6/12
                                   h3("GO"),
                                   imageOutput("pathway_GO_plot", height = "350px",width ="400px")  # 设置高度
                            )
                        )
                      ))),
             # 结果展示标签
             tabPanel("Protein Information",
               titlePanel("FVIII Protein Structure Visualization"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("pdbId", "Choose a PDB ID:", 
                               choices = protein_list
                               ),
                   actionButton("load", "Load Structure"),
                   downloadButton("downloadPDB", "Download PDB File"),
                   br(),
                   br(),
                   NGLVieweROutput("structure1",width = "450px",height = "293px")
                 ),
                 mainPanel(
                   DTOutput("protein_table")
                 )
               ),
               titlePanel("Small Molecule Structure Visualization"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("PubChemId", "Choose a PubChem ID:", 
                               choices = ligand_list),
                   actionButton("loadDrug", "Load Structure"),
                   downloadButton("downloadDrug", "Download PDB File"),
                   br(),
                   br(),
                   uiOutput("structure_3D_view_UI",height = "293px")
                 ),
                 mainPanel(
                   DTOutput("HTMDv2_ligand")
                 )
               )
               ),
             tabPanel("Drug Discovery",
                      h2("Drugs for Hemophilia A"),
                      DTOutput("Drug_Discovery")
             ),
             
             
             # 帮助标签
             tabPanel("Support",
                      fluidRow(
                        column(7,
                        box(
                          width = 7,
                          height = "800px",
                          collapsible = TRUE,
                          collapsed = TRUE,
                          tags$div(
                            class = "box-body",
                            tags$iframe(src = "HemoAtlas.html", 
                                        style = "width: 180%; height: 800px; border: none;")
                          ))),
                        column(5,
                               h2("Drugs for Hemophilia A"),
                               fluidRow(
                                 div(
                                   style = "display: flex; justify-content: center; align-items: center; gap: 20px;",
                                   box(title = "GSE169009", status = "success", solidHeader = TRUE, width = 10,
                                       h4("Experiment type: Expression profiling by high throughput sequencing"),
                                       p(a("GSE169009", href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE169009", target = "_blank"))  # 超链接
                                   ))),
                               # 第二行
                               fluidRow(
                                 div(
                                   style = "display: flex; justify-content: center; align-items: center; gap: 20px;",
                                   box(title = "GSE142080", status = "success", solidHeader = TRUE, width = 10,
                                       h4("Experiment type: Expression profiling by array"),
                                       p(a("GSE142080", href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE142080", target = "_blank"))  # 超链接
                                   ))),
                               # 第三行
                               fluidRow(
                                 div(
                                   style = "display: flex; justify-content: center; align-items: center; gap: 20px;",
                                   box(title = "GSE38765", status = "success", solidHeader = TRUE, width = 10,
                                       h4("Experiment type: Expression profiling by array"),
                                       p(a("GSE38765", href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE38765", target = "_blank"))  # 超链接
                                   )))
                        )))
                      
             ))

# 服务器部分
server <- function(input, output, session) {

  # 初始化标志，判断用户是否进行了搜索
  search_triggered <- reactiveVal(FALSE)
  
  # 监听搜索按钮事件
  observeEvent(input$search_button, {
    gene_input <- input$search  # 获取用户输入的基因名称
    
    # 如果用户输入了基因名称并点击了搜索按钮，进行搜索
    if (gene_input != "") {
      # 设置标志为TRUE，表示用户进行了搜索
      search_triggered(TRUE)
      
      # 基于用户输入筛选数据
      filtered_data <- data1[grep(gene_input, data1$gene_name, ignore.case = TRUE), ]
      
      # 渲染过滤后的数据表
      output$results_table1 <- renderDT({
        datatable(filtered_data,
                  options = list(
                    pageLength = 5,  # 每页显示5行
                    lengthMenu = c(5, 10, 15),  # 用户可选择每页显示5行、10行或15行
                    searching = FALSE,  # 禁用搜索框（因为我们已经通过输入框搜索）
                    ordering = TRUE,  # 启用列排序
                    autoWidth = TRUE  # 自动调整列宽
                  ),
                  rownames = FALSE  # 隐藏行名
        )
      })
      
      # 自动跳转到“结果展示”标签页
      updateTabsetPanel(session, "navbar", selected = "Transcriptomic Data")
    }
  })
###基因突变模块------------
  output$table_mutation <- renderDT({
    if (!search_triggered()) {
      # 如果没有搜索，显示所有数据
      datatable(table_mutation,
                options = list(
                  pageLength = 15,  # 每页显示5行
                  lengthMenu = c(5, 10, 15,20,50,100,500),  # 用户可选择每页显示5行、10行或15行
                  searching = TRUE,  # 启用搜索框
                  ordering = TRUE,  # 启用列排序
                  autoWidth = TRUE  # 自动调整列宽
                ),
                rownames = FALSE  # 隐藏行名
      )
    }
  })
  output$Degree_of_mutation_type <- renderImage({
    list(
      src = "www/Degree of mutation type.jpg",  # Path to the KEGG image file
      contentType = "image/jpg",
      width = 300,  # Adjust width
      height = 300  # Adjust height
    )
  }, deleteFile = FALSE)
  output$Patient_severity <- renderImage({
    list(
      src = "www/Patient severity.jpg",  # Path to the KEGG image file
      contentType = "image/jpg",
      width = 300,  # Adjust width
      height = 300  # Adjust height
    )
  }, deleteFile = FALSE)
  output$mutation_type <- downloadHandler(
    filename = function() { "mutation_type.png" },
    content = function(file) {
      ggsave(file, Degree_of_mutation_type, width = 15, height = 15, units = "in",dpi = 300)
    }
  )
  output$Patient_severity_JPG <- downloadHandler(
    filename = function() { "Patient_severity.png" },
    content = function(file) {
      ggsave(file, Patient_severity, width = 15, height = 15, units = "in",dpi = 300)
    }
  )
  # output$table_kk <- renderDT({
  #   datatable(kk_data, 
  #             options = list(
  #               pageLength = 10,  # 每页显示的行数
  #               lengthMenu = c(5, 10, 15),  # 可选的每页显示行数
  #               columnDefs = list(
  #                 list(targets = which(colnames(kk_data) == "geneID"), # 确定geneID列的索引
  #                      render = JS(
  #                        "function(data, type, row, meta) {",
  #                        "  var genes = data.split('/');",  # 将geneID列通过\分割
  #                        "  if (genes.length > 5) {",  # 如果基因数量超过5个
  #                        "    var displayed_genes = genes.slice(0, 5).join('/');",  # 显示前5个基因
  #                        "    return displayed_genes + '...<span style=\"color:blue;cursor:pointer;\" onclick=\"$(this).next().toggle();\">More</span><span style=\"display:none;\">/' + genes.slice(5).join('/') + '</span>';",  # 显示“更多”按钮，点击后显示剩余的基因
  #                        "  } else {",
  #                        "    return data;",  # 如果少于5个基因，直接显示
  #                        "  }",
  #                        "}"
  #                      ))
  #               )
  #             ),
  #             rownames = T,
  #             escape = FALSE  # 允许 HTML 渲染
  #   )
  # })
####临床数据模块----------- 
  # 创建网络图
  # nodes <- data.frame(id = c("A", "B", "C"), label = c("Node A", "Node B", "Node C"))
  # edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
  nodes1 <- data.frame(id = c(id1), label = c(label1))
  edges1 <- data.frame(from = c(from1), to = c(to1))
  output$network1 <- renderVisNetwork({
    visNetwork(nodes1, edges1) %>%
      visOptions(highlightNearest = TRUE)
  })
  
  nodes2 <- data.frame(id = c(id2), label = c(label2))
  edges2 <- data.frame(from = c(from2), to = c(to2))
  output$network2 <- renderVisNetwork({
    visNetwork(nodes2, edges2) %>%
      visOptions(highlightNearest = TRUE)
  })
  output$Clinical_Presentations <- renderDT({
    if (!search_triggered()) {
      # 如果没有搜索，显示所有数据
      datatable(Clinical_Presentations,
                options = list(
                  pageLength = 8,  # 每页显示5行
                  lengthMenu = c(5, 10, 15,20,50,100,500),  # 用户可选择每页显示5行、10行或15行
                  searching = TRUE,  # 启用搜索框
                  ordering = TRUE,  # 启用列排序
                  autoWidth = F  # 自动调整列宽
                ),
                rownames = FALSE  # 隐藏行名
      )
    }
  })

  output$Human_Phenotypes <- renderDT({
    if (!search_triggered()) {
      # 如果没有搜索，显示所有数据
      datatable(Human_Phenotypes,
                options = list(
                  pageLength = 5,  # 每页显示5行
                  lengthMenu = c(5, 10, 15,20,50,100,500),  # 用户可选择每页显示5行、10行或15行
                  searching = TRUE,  # 启用搜索框
                  ordering = TRUE,  # 启用列排序
                  autoWidth = F  # 自动调整列宽
                ),
                rownames = FALSE  # 隐藏行名
      )
    }
  })
### 动态更新样本选择框----
  selected_data <- reactive({
    switch(input$dataset_choice,
           "HA_count_data_all" = data_input1,
           "F8_KO_mRNA_count" = data_input2,
           "F8_KO_miRNA_count" = data_input3)
  })
  observe({
    # req(input$file)
    data <- selected_data()
    output$results_table <- renderDT({
      datatable(data[1:7], options = list(pageLength = 8), rownames = TRUE)
    })
    sample_names <- colnames(data)[2:ncol(data)]
    
    # 更新对照组和实验组选择框
    output$control_samples_ui <- renderUI({
      selectInput("control_samples", "选择对照组样本", choices = sample_names, multiple = TRUE)
    })
    output$experiment_samples_ui <- renderUI({
      selectInput("experiment_samples", "选择实验组样本", choices = sample_names, multiple = TRUE)
    })
  })
  
#### 监听分析按钮事件---开始差异分析
  observeEvent(input$analyze_button, {
    req(input$control_samples, input$experiment_samples)
    # Show loading animation
    shinyjs::show("loading")
      # input$analyze_button
    rawdata <- selected_data()
    colnames(rawdata)<-c("gene_symbol",colnames(rawdata)[2:ncol(rawdata)])
    
    temp_data=rawdata[2:ncol(rawdata)]
    #使用apply函数，获取重复基因的均值
    temp_list=apply(temp_data,
                    2,
                    function(x)aggregate(x~rawdata$gene_symbol,data=rawdata,mean)
    )
    #对结果进行merge
    temp_merge=c()
    for(i in 1:length(temp_list)){
      temp_merge=cbind(temp_merge,temp_list[[i]]$x)
    }
    temp_df=data.frame(temp_merge)
    colnames(temp_df)=names(temp_list)
    temp_df$gene_symbol=temp_list[[1]][[1]]
    #获取无重复geneSymbol的表达矩阵
    rt=temp_df
    rt<-rt[,c(ncol(rt),1:ncol(rt)-1)]
    
    rawdata<-rt
    rownames(rawdata)<-rawdata[,1]
    rawdata<-rawdata[,-1]
    
    control_samples <- input$control_samples
    experiment_samples <- input$experiment_samples
    
    # 差异表达分析
    condition <- factor(c(rep("Control", length(control_samples)), rep("Experiment", length(experiment_samples))))
    design <- model.matrix(~ condition)
    
    if (any(rawdata != round(rawdata))) {
      rawdata <- round(rawdata)  # Round to nearest integer
    }
    
    dds <- DESeqDataSetFromMatrix(countData = rawdata[, c(control_samples, experiment_samples)], colData = data.frame(condition), design = ~ condition)
    dds <- DESeq(dds)
    res <- results(dds)
    
    # 保存差异表达结果
    diff_res <- as.data.frame(res)
    # 使用 adj.p 过滤
    diffSig <- diff_res[(diff_res$padj < 0.05 & abs(diff_res$log2FoldChange) > 1),]
    diffSig$gene_id<-rownames(diffSig)
    diffSig <- diffSig[!is.na(diffSig$gene_id) & !grepl("NA", diffSig$gene_id), ]
    diffSig<-diffSig[,c(2,4,5,6,7)]
    num_cols <- sapply(diffSig, is.numeric)  # 找到数值列
    diffSig[, num_cols] <- round(diffSig[, num_cols], 3)
    shinyjs::hide("loading")
    output$results_table <- renderDT({
      datatable(diffSig, options = list(pageLength = 8), rownames = TRUE)
    })
  })
####火山图、通路展示-----
  output$volcano_plot <- renderImage({
    list(
      src = "www/volcano_plot.jpg",  # Path to the KEGG image file
      contentType = "image/jpg",
      width = 350,  # Adjust width
      height = 350  # Adjust height
    )
  }, deleteFile = FALSE)
  output$pathway_KEGG_plot <- renderImage({
    list(
      src = "www/pathway_KEGG_plot.jpg",  # Path to the KEGG image file
      contentType = "image/jpg",
      width = 350,  # Adjust width
      height = 350  # Adjust height
    )
  }, deleteFile = FALSE)
  output$pathway_GO_plot <- renderImage({
    list(
      src = "www/pathway_GO_plot.jpg",  # Path to the KEGG image file
      contentType = "image/jpg",
      width = 350,  # Adjust width
      height = 350  # Adjust height
    )
  }, deleteFile = FALSE)
  
  
  output$download_volcano <- downloadHandler(
    filename = function() { "volcano_plot.png" },
    content = function(file) {
      ggsave(file, volcano_plot, width = 15, height = 15, units = "in",dpi = 300)
    }
  )
  # 支持下载通路图
  output$download_KEGG_plot <- downloadHandler(
    filename = function() { "pathway_KEGG_plot.png" },
    content = function(file) {
      ggsave(file, pathway_KEGG_plot, width = 15, height = 15, units = "in",dpi = 300)
    }
  )
  # 支持下载通路图
  output$download_GO_plot <- downloadHandler(
    filename = function() { "pathway_GO_plot.png" },
    content = function(file) {
      ggsave(file, pathway_GO_plot, width = 15, height = 15, units = "in",dpi = 300)
    }
  )
####蛋白质模块-----------
#####可视化蛋白质-----
  output$structure1 <- renderNGLVieweR({
    NGLVieweR(input$pdbId) %>%
      addRepresentation("cartoon", param = list(name = "cartoon", colorScheme = "residueindex")) %>%
      addRepresentation("ball+stick", param = list(name = "ligand", sele = "ligand")) %>%
      stageParameters(backgroundColor = "white")
  })
  
  observeEvent(input$load, {
    output$structure1 <- renderNGLVieweR({
      NGLVieweR(input$pdbId) %>%
        addRepresentation("cartoon", param = list(name = "cartoon", colorScheme = "residueindex")) %>%
        addRepresentation("ball+stick", param = list(name = "ligand", sele = "ligand"))
    })
  })
  output$downloadPDB <- downloadHandler(
    filename = function() {
      paste0(input$pdbId, ".pdb")
    },
    content = function(file) {
      # Download the PDB file from RCSB PDB
      url <- paste0("https://files.rcsb.org/download/", input$pdbId, ".pdb")
      download.file(url, file)
    })
  output$protein_table <- renderDT({
    # 如果没有搜索，显示所有数据
    datatable(protein_table1,
              options = list(
                pageLength = 10,  # 每页显示5行
                lengthMenu = c(5, 10, 15,20,50,100,500),  # 用户可选择每页显示5行、10行或15行
                searching = TRUE,  # 启用搜索框
                ordering = TRUE,  # 启用列排序
                autoWidth = F  # 自动调整列宽
              ),
              rownames = FALSE  # 隐藏行名
    )
  })
####可视化小分子结构-------
  output$downloadDrug <- downloadHandler(
    filename = function() {
      paste0(input$PubChemId, ".pdb")
    },
    content = function(file) {
      a(em(""),href=paste(input$PubChemId,".pdb",sep = ""))
    })
  
  output$structure_3D_view_UI <- renderUI({
    pdb_path <-paste("./www/",input$PubChemId,".pdb",sep = "")
    result<-r3dmol(
      viewer_spec = m_viewer_spec(
        cartoonQuality = 10,
        lowerZoomLimit = 50,
        upperZoomLimit = 500
      ),
      width = "450px",height = "293px"
    ) %>%
      m_add_model(                  # Add model to scene
        data = pdb_path,
        format = "pdb"
      ) %>%
      m_zoom_to() %>%
      m_set_style(                  # Set style of structures
        style = m_style_stick(
          color = "#00cc96"
        )
      ) %>%
      m_rotate(                     # Rotate the scene by given angle on given axis
        angle = 90,
        axis = "y"
      ) %>%
      m_spin()
    return(result)
    includeHTML("./www/No.html")
  })
  output$HTMDv2_ligand <- renderDT({
      # 如果没有搜索，显示所有数据
      datatable(ligand,
                options = list(
                  pageLength = 7,  # 每页显示5行
                  lengthMenu = c(5, 10, 15,20,50,100,500),  # 用户可选择每页显示5行、10行或15行
                  searching = TRUE,  # 启用搜索框
                  ordering = TRUE,  # 启用列排序
                  autoWidth = F  # 自动调整列宽
                ),
                rownames = FALSE  # 隐藏行名
      )
  })
  ####药物发现模块----------
  output$Drug_Discovery <- renderDT({
    if (!search_triggered()) {
      # 如果没有搜索，显示所有数据
      datatable(Drug_Discovery,
                options = list(
                  pageLength = 20,  # 每页显示5行
                  lengthMenu = c(5, 10, 15,20,50,100,500),  # 用户可选择每页显示5行、10行或15行
                  searching = TRUE,  # 启用搜索框
                  ordering = TRUE,  # 启用列排序
                  autoWidth = TRUE  # 自动调整列宽
                ),
                rownames = FALSE  # 隐藏行名
      )
    }
  })
}

# 运行应用
shinyApp(ui = ui, server = server)
