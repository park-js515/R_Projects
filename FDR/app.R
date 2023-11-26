# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("OCplus")

library(BiocManager)
options(repos = BiocManager::repositories())

library(shiny)
library(shinyBS) # pop
library(shinythemes) # themes
library(bslib)
library(reactable)
library(DT)
library(locfdr) # 1D
library(OCplus) # 2D
library(htmltools)
source("functions.R") # functions from out sides
source("bh2.R") # B-H method

# Ramos 
source("real_flex.R")
source("real_func.R")
source("ramos_fdr.R")

# Youngrae Kim
source("kim function.R") # if you want use shiny::em, you must use like shiny::em("message")

# rejection table style

mat <- matrix(NA, 3, 6)
rejection_table <- data.frame(mat)
dimnames(rejection_table) <- list(c("BH", "Efron", "Ramos"), 
                                  c("truncated_data", "left_cut", "right_cut", "left_reject", 
                                    "right_reject","total_reject"))

mat2 <- matrix(NA, 3, 2)
rejection_table2 <- data.frame(mat2)
dimnames(rejection_table2) <- list(c("Ploner", "Kim_I", "Kim_U"),
                                   c("truncated_data", "total_reject"))


BS1 <- paste("Range of X-axis : percentile or value that 1st to 2nd argment",
             "",
             paste("If you choose percentile and", strong("if you select out of range"), "then there must be an error"), 
             shiny::em("solution: fix the range & reload the data"), sep = "<br>")

BS2 <- paste("Insert non-duplicate numbers",
             "",
             shiny::em("EX) if you insert (1, 4:7, 10) then 1, 4, 5, 6, 7, 10 columns are set as group1."),
             shiny::em("and 4:7 means the number from 4 to 7."),
             sep = "<br>")

BS3 <- paste("Insert non-duplicate number(s)",
             "",
             "If (-1) is input, all indices except for group1 indices are treated as group2 indices.",
             "default is (-1)",
             sep = "<br>")

BS4 <- paste("Data size of group1, group2.",
             "Arguments are must be positive integers.", 
             sep = "<br>")

BS5 <- paste("Reference value of the experiment for fdr.",
             "",
             "The arguments are rational numbers between 0 and 1,",
             paste("and the arguments cannot exceed", strong("three.")),
             "If the number of factors exceeds 3, or if there is no argument",
             "only one value of 0.05 is used automatically.",
             sep = "<br>")
########

ui <- navbarPage(
  # bs_theme(bootswatch = "flatly", version = 3)
  theme = shinytheme("flatly"),
  id = "mainnav",
  title = "FDR",
  
  # 1
  tabPanel(
    title = "Intro",
    
    tags$style(
      "li a {
    font-size : 20px;
    font-weight : bold;
    }"),
    
    tags$style(".popover{
               max-width : 100%;
               }"),
    
    h4("The people who made this"),
    p("Juseong Park, Shinjune Kim, Youngjae Oh, Jaesik Jeong","(CNU : Cheonnam National University, Korea)"),
    br(),
    
    h4("File type"),
    div(style = "color : red",p(strong("!"), HTML("&#160;"), "Only csv file is possible")),
    div(style = "line-height : 50%;", br()),
    
    p("You can use t/z, p-value (statistics) or raw data(group1 vs group2)"),
    p("To use raw data, you need to put observations in the column and genes(characteristics) in the row."),
    p("Non-raw data must consist of one column."),
    #파일 크기에 대해서도 정보가 필요할 것으로 판단됨.
    br(),
    
    h4("Goals"),
    p("We support using", strong("FDR"),"methods or comparing them to each other."),
    p("1D or 2D FDR is available."),
    br(),
    
    h4("Hyperlinks for FDR papers"),
    div(style = "line-height : 50%;", br()),
    
    h5("- 1D"),
    p("Benjamini & Hochberg : ", a("link", href = "https://www.jstor.org/stable/2346101?seq=1#metadata_info_tab_contents",
                                   target = "_blank")),
    p("Efron : ", a("link", href = "https://www.jstor.org/stable/3085878?seq=1#metadata_info_tab_contents",
                    target = "_blank")),
    p("Ramos :", a("link", href = "https://onlinelibrary.wiley.com/doi/full/10.1002/bimj.202000256",
                   target = "_blank")),
    div(style = "line-height : 50%;", br()),
    
    h5("- 2D"),
    p("Ploner :", a("link", href = "https://academic.oup.com/bioinformatics/article/22/5/556/205477",
                    target = "_blank")),
    p("Youngrae Kim", a("link", href = "https://www.sciencedirect.com/science/article/pii/S0169743917308092",
                        target = "_blank")),
    br(),
    
    h4("Information of R used(SessionInfo)"),
    div(img(src = "sesson_info.png", style = "max-width : 50%; height : auto;"))
  ),
  
  # 2
  tabPanel(
    title = "Data load",
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          
          h4("File selection"),
          selectInput("dim", "Select data type", list("p-value", "t", "z", "raw_data")),
          conditionalPanel(condition = "input.dim != 'raw_data'",
                           textInput("len", "# of n1, n2", placeholder = "Separate with , (comma)")),
          bsPopover(id = "len", title = "args(n1, n2)", content = BS4, options = list(container = "body")),
          conditionalPanel(condition = "input.dim == 'raw_data'",
                           checkboxInput("pool", strong("Use pooled variance (only for 1D)"), value = F)),
          conditionalPanel(condition = "input.dim == 'raw_data'",
                           textInput("g1Index", "group1 index", placeholder = "Separate with , (comma)")),
          bsPopover(id = "g1Index", title = "numeric()", content = BS2, options = list(container = "body")),
          
          conditionalPanel(condition = "input.dim == 'raw_data'",
                           textInput("g2Index", "group2 index", placeholder = "Separate with , (comma)" ,
                                     value = "-1")),
          bsPopover(id = "g2Index", title = "numeric()", content = BS3, options = list(container = "body")),
          # hr(style = "border-color : #66ccff"),
          
          fileInput("file", "File Selection", placeholder = "Select .csv file with header", accept = c(".csv"),
                    buttonLabel = "Browse or Drop ..."),
          div(style = "text-align : right", actionButton(inputId = "a1", label = "histogram(default)")),
          
          br(),
          br(),
          h4("Configuration of histogram"),
          
          textInput("xlim", "Range of x", placeholder = "Separate with ,(comma)", 
                    value = "0.00, 1.00"), # 0 : percentile or 1 : value
          checkboxInput("c2", strong("Use percentile for X's range"), value = T),
          bsPopover(id = "xlim", title = "args(min, max)", content = BS1, options = list(container = "body")),
          textInput("ylim", "Range of y", placeholder = "Separate with ,(comma)", value = "0, 1"),
          numericInput("nclass", "Bins of histogram (Integer)", min = 1, max = Inf, value = 50),
          div(style = "display : inline-block; width : 60%; text-align : center",
              actionButton(inputId = "a2", 
                           label = "Adjust histogram", class = "btn-lg btn-success")),
          div(style = "display : inline-block; width : 35%; text-align : center",
              actionButton(inputId = "a3", 
                           label = "KDE", class = "btn-lg btn-info"))
        ),
        
        mainPanel(
          plotOutput("hist", height = "550px"),
          checkboxInput("c1", strong("Show Information about the data used"), value = F),
          fluidRow(
            column(6, verbatimTextOutput("s1")),
            column(6, verbatimTextOutput("s2"))
          ),
          
          # conditionalPanel : use javascript 
          conditionalPanel(condition = "input.c1 == true",
                           actionButton(inputId = "a4", label = "Show Data"),
          ),
          
          br(),
          
          conditionalPanel(condition = "input.c1 == true && input.a4 % 2 == 1",
                           tabsetPanel(id = "tabset", 
                                       tabPanel("Data", DTOutput("t1")),
                                       tabPanel("Statistics", DTOutput("t2"),
                                                conditionalPanel(condition = "input.dim != 'raw_data'",
                                                                 div(style = "color : red", h4("Not raw_data"))))))
        )
      )
    )
  ),
  
  # 3
  tabPanel(
    title = "Method",
    splitLayout(
      wellPanel(
        h3("1D configuration"),
        br(),
        splitLayout(cellArgs = list(style = "padding : 2px", style = "padding : 2px"),
                    
                    # 3 - 1
                    wellPanel(style = "border : 1px solid black; background : #33cccc;", h4(strong("Common factors (1D)")),
                              br(),
                              
                              conditionalPanel(condition = "input.dim != 'p-value'",
                                               selectInput("dc11", label = "Inference type of f", choices = list("Natural_Spline", "Polynomial_Spline")),
                                               numericInput("dc12", label = "Poisson DF", min = 0, max = Inf, value = 7, step = 1),
                                               numericInput("dc13", label = "Bins of histogram", min = 1, max = Inf, value = 50, step = 1),
                                               checkboxInput("dc14", label = strong("Use truncated data"), value = F)
                                               ),
                              
                              conditionalPanel(condition = "input.dc14",
                                               textInput("dc15", label = "Range of statistics", value = "-Inf, Inf"))
                    ),
                    
                    wellPanel(style = "border : 1px solid black; background : pink", h4(strong("Methods")),
                              br(),
                              
                              fluidRow(selectInput("m1", "Select 1D fdr method(s)", choices = list("B-H", "Efron", "Ramos"),
                                                   multiple = T)
                              ),
                              br(),
                              
                              conditionalPanel(condition = "input.m1[0] == 'B-H' || input.m1[1] == 'B-H' || input.m1[2] == 'B-H' 
                                     || input.m1[3] == 'B-H'", 
                                               fluidRow(style = "background : white; border : 1px solid black; padding : 3px",
                                                        downloadButton("bh", shiny::em(strong("B-H"))),
                                                        br(),
                                                        # div(style = "text-align : right", 
                                                        #     actionButton("de111", label = "step down", icon = icon("arrow-down"),
                                                        #                  class = "btn-sm"))
                                               ),
                                               
                                               br(),
                              ),
                              
                              conditionalPanel(condition = "input.m1[0] == 'Efron' || input.m1[1] == 'Efron' || input.m1[2] == 'Efron' 
                                     || input.m1[3] == 'Efron'",
                                               fluidRow(style = "background : white; border : 1px solid black; padding : 3px",
                                                        downloadButton("Efron", shiny::em(strong("Efron"))),
                                                        br(),
                                                        br(),
                                                        selectInput("de121", "Null(f0) type", 
                                                                    choices = list("Theorical", "MLE", "CME"), selected = "MLE"),
                                                        conditionalPanel(condition = "input.de121 == 'CME'",
                                                                         sliderInput("de122", "Interval for central matching",
                                                                                     min = 0, max = 1, value = c(0.25, 0.75), step = 0.01))
                                               ),
                                               
                                               br(),
                              ),
                              
                              
                              conditionalPanel(condition = "input.m1[0] == 'Ramos' || input.m1[1] == 'Ramos' || input.m1[2] == 'Ramos' 
                                     || input.m1[3] == 'Ramos'",
                                               fluidRow(style = "background : white; border : 1px solid black; padding : 3px",
                                                        downloadButton("Ramos", shiny::em(strong("Ramos"))),
                                                        br(),
                                                        br(),
                                                        numericInput("de131", "Afac", min = 0, max = Inf, value = 0),
                                                        numericInput("de132", "Initial C", min = 0, max = Inf, value = 0, step = 0),
                                                        numericInput("de133", "Last C", min = 0, max = Inf, value = 0)
                                               ),
                                               
                                               br(),
                              ),
                              

                    )
        )
      ),
      
      # 3 - 2
      conditionalPanel(condition = "input.dim == 'raw_data'",
                       
                       wellPanel(
                         h3("2D configuration"),
                         br(),
                         splitLayout(
                           wellPanel(style = "border : 1px solid black; background : #33cccc", h4(strong("Common factors (2D)")),
                                     br(),
                                     
                                     checkboxInput("dc21", strong("Use truncated data"), value = F),
                                     conditionalPanel(condition = "input.dc21",
                                                      textInput("dc22", "Range of t", value = "-Inf, Inf")),
                                     conditionalPanel(condition = "input.dc21",
                                                      textInput("dc23", "Range of sd", value = "0, Inf"))
                                     ),
                           
                           wellPanel(style = "border : 1px solid black; background : pink ", h4(strong("Methods")),
                                     br(),
                                     
                                     fluidRow(selectInput("m2", "Select 2D fdr method(s)", 
                                                          choices = list("Ploner", "Kim"), multiple = T)
                                     ),
                                     br(),
                                     
                                     conditionalPanel(condition = "input.m2[0] == 'Ploner' || input.m2[1] == 'Ploner' 
                                     || input.m2[2] == 'Ploner' || input.m2[3] == 'Ploner'",
                                                      fluidRow(style = "background : white; border : 1px solid black; 
                                                               padding : 3px",
                                                               downloadButton("Ploner", shiny::em(strong("Ploner"))),
                                                               br(),
                                                               br(),
                                                               numericInput("de211", "nperm", min = 0, max = Inf, value = 100,
                                                                            step = 1),
                                                               numericInput("de212", "nr", min = 0, max = Inf, value = 15,
                                                                            step = 1),
                                                               sliderInput("de213", "smooth", min = 0.01, max = 0.99, 
                                                                           value = 0.2, step = 0.01)
                                                      ),
                                                      
                                                      br(),
                                     ),
                                     
                                     conditionalPanel(condition = "input.m2[0] == 'Kim' || input.m2[1] == 'Kim' 
                                     || input.m2[2] == 'Kim' || input.m2[3] == 'Kim'",
                                                      fluidRow(style = "background : white; border : 1px solid black; 
                                                               padding : 3px",
                                                               downloadButton("Kim", shiny::em(strong("Kim"))),
                                                               br(),
                                                               br(),
                                                               selectInput("de221", strong("type"), 
                                                                           choices = list("Intersection", "Union")),
                                                               sliderInput("de222", strong("adjust"), min = 0, max = 1, 
                                                                           value = 0.4, step = 0.01)
                                                      ),
                                                      
                                                      br(),
                                     ),
                                     
                                     
                           )
                         )
                       )
      )
    ),
    
    fluidRow(column(4, offset = 8, 
                    wellPanel(
                      textInput("q_values", "Insert q_values (max : 3)", 
                                value = "0.05, 0.10, 0.15", placeholder = "Separate with , (comma)")
                    ))),
    bsPopover(id = "q_values", title = "args(n1, n2, n3)", content = BS5, options = list(container = "body"),
              placement = "left"),
    div(style = "text-align : right", actionButton("a5", label = "Align", class = "btn-warning btn-lg")),
    br()
  ),
  
  # 4
  tabPanel(
    title = "Result",
    h3(strong("Plot")),
    br(),
    tabsetPanel(id = "tabset2",
                
                tabPanel("1D",
                         br(),
                         fluidRow(
                           column(6, 
                                  plotOutput("r11", height = "550px", brush = brushOpts(id = "plot1_brush", resetOnNew = TRUE)
                                  ),
                                  conditionalPanel(condition = "(input.m1[0] == 'B-H' || input.m1[1] == 'B-H' 
                                                 || input.m1[2] =='B-H' || input.m1[0] =='Efron' || input.m1[1] =='Efron'
                                                 || input.m1[2] =='Efron' || input.m1[0] =='Ramos' || input.m1[1] =='Ramos'
                                                 || input.m1[2] =='Ramos') && input.a5 % 2 == 1",
                                                   wellPanel(selectInput("r1s", "Select type of plot",
                                                                         choices = list("B-H plot", "p0f0 lines","fdr lines")
                                                   ))
                                  ),
                                  conditionalPanel(condition = "(input.r1s == 'p0f0 lines' || input.r1s == 'fdr lines')
                                                 && input.a5 % 2 == 1",
                                                   checkboxInput("c3", strong("Draw user-defined dist or fdr"), value =  F)
                                  ),
                                  conditionalPanel(condition = "input.c3 && input.r1s != 'B-H plot'",
                                                   column(6,
                                                          wellPanel(
                                                            h3("NULL"),
                                                            br(),
                                                            textInput("rv1", "proportion", placeholder = "Separate with , (comma)"),
                                                            textInput("rv2", "Mean", placeholder = "Separate with , (comma)"),
                                                            textInput("rv3", "SD", placeholder = "Separate with , (comma)")
                                                          )
                                                   ),
                                                   
                                                   column(6,
                                                          wellPanel(
                                                            h3("Alternative"),
                                                            br(),
                                                            textInput("rv4", "proportion", placeholder = "Separate with , (comma)"),
                                                            textInput("rv5", "Mean", placeholder = "Separate with , (comma)"),
                                                            textInput("rv6", "SD", placeholder = "Separate with , (comma)")
                                                          )
                                                   ),
                                                   
                                                   wellPanel(selectInput("rv7", "Draw type", 
                                                                         choices = list("f", "p0f0", "p1f1", "fdr"), multiple = T),
                                                             div(style = "text-align : right", 
                                                                 actionButton("a6", "Draw", class = "btn-lg btn-success" )))
                                                   
                                  )
                           ),
                           column(6, 
                                  plotOutput("r12", height = "550px"),
                                  conditionalPanel(condition = "(input.m1[0] == 'B-H' || input.m1[1] == 'B-H' 
                                                 || input.m1[2] =='B-H' || input.m1[0] =='Efron' || input.m1[1] =='Efron'
                                                 || input.m1[2] =='Efron' || input.m1[0] =='Ramos' || input.m1[1] =='Ramos'
                                                 || input.m1[2] =='Ramos') && input.a5 % 2 == 1",
                                                   div(style = "text-align : right; margin : 10px 5px 0px 0px", 
                                                       downloadButton("download1", "Download .png"))
                                  )
                           )
                         ),
                         conditionalPanel(condition = "false",
                                          div(style = "color : red", h4("dummy")
                                              )
                                          )
                ),
                tabPanel("2D",
                         br(),
                         fluidRow(
                           column(6,
                                  plotOutput("r21", height = "550px",
                                             brush = brushOpts(id = "plot2_brush", resetOnNew = TRUE)
                                  ),
                                  conditionalPanel(condition = "(input.m2[0] == 'Ploner' || input.m2[1] == 'Ploner'
                                                   || input.m2[0] == 'Kim' || input.m2[1] == 'Kim') && input.a5 % 2 == 1",
                                                   wellPanel(selectInput("r2s", "Select type of plot",
                                                                         choices = list("t&logse", "t&locfdr","fdr2d_est",
                                                                                        "tornado", "volcano")
                                                   ))
                                  ),
                                  
                           ),
                           ## 주의할 곳
                           column(6,
                                  plotOutput("r22", height = "550px"),
                                  conditionalPanel(condition = "(input.m2[0] == 'Ploner' || input.m2[1] == 'Ploner'
                                                   || input.m2[0] == 'Kim' || input.m2[1] == 'Kim') && input.a5 % 2 == 1",
                                                   div(style = "text-align : right; margin : 10px 5px 0px 0px",
                                                       downloadButton("download2", "Download .png"))
                                  )
                           )
                         ),
                         conditionalPanel(condition = "input.dim != 'raw_data'",
                                          div(style = "color : red", h4("Not 2D data"))
                                          )
                         )
                
    ),
    
    conditionalPanel(condition = "input.a5 % 2 == 1",
                     
                     hr(style = "border-top: 1px dashed #000000;"),
                     h3(strong("Rejection Table")),
                     br(),
                     
                     fluidRow(
                       
                              column(4,
                                     wellPanel(selectInput("q_values2", "Select the q_value to use to represent the table",
                                                           choices = c(0.05)
                                                           )
                                               ),
                                     
                                     div(style = "text-align : right; margin : 10px 5px 0px 0px", 
                                         downloadButton("download3", "Download index of result"))
                                     
                                     ),
                              
                              column(8,
                                     h4("1D"),
                                     DTOutput("t3"),
                                     br(),
                                     h4("2D"),
                                     DTOutput("t4")
                                     )
                              )
                     )
    
  )
)


############################################################################################################################

server <- function(input, output, session){
  
  ### 1
  
  ###
  
  ### 2
  userFile <- reactive({
    validate(need(input$file, message = "Please choose a csv. file from your computer"))
    input$file
  })
  
  data_temp <- reactive(read.csv(userFile()$datapath))
  
  g1Index <- reactive({
    extract2(input$g1Index)})
  g2Index <- reactive({
    extract2(input$g2Index)})
  
  len1 <- reactive(
    if (input$dim == "raw_data"){
      length(g1Index())
    }
    else {
      extract1(input$len)[1]
    }
  )
  
  len2 <- reactive(
    if (input$dim == "raw_data"){
      if (!(-1 %in% extract2(input$g2Index))){
        length(extract2(input$g2Index))}
      else {
        dim(data_temp())[2] - len1()
      }
    }
    else {
      extract1(input$len)[2]
    }
  )
  
  xlims <- reactive({
    validate(need(is.na(sum(extract1(input$xlim))) == F,  message = "please input xlims"))
    extract1(input$xlim)
  })
  
  data <- reactive(
    if (input$dim != "raw_data"){
      data_temp()
    }
    else {
      ftn1(data = data_temp(), g1Index = g1Index(), g2Index = g2Index(), type = input$pool)
    }
  )
  
  # hist
  d_hist <- reactive(data()[, 1])
  
  # truncated data
  d_tr <- reactive({
    if (!input$c2){
      subset(data(), data()[, 1] >= xlims()[1] & data()[, 1] <= xlims()[2])
    }
    else 
      subset(data(), data()[, 1] >= quantile(data()[, 1], xlims()[1]) & data()[, 1] <= quantile(data()[, 1], xlims()[2]))
  })
  
  output$hist <- renderPlot({
    xlim1 <- extract1(isolate(input$xlim))
    xlim2 <- sort(xlim1)
    
    if (input$c2){
      xlim3 <- c(quantile(d_hist(), xlim2[1]), quantile(d_hist(), xlim2[2]))
    }
    
    else {
      xlim3 <- xlim2
    }
    
    ylim <- sort(extract1(isolate(input$ylim)))
    
    
    if (input$a1 %% 2 == 1){
      hist(d_hist(), nclass = 50, freq = F, xlab = "", main = "default histogram")
    }
    
    if (input$a2 %% 2 == 1){
      hist(d_hist(), xlim = xlim3, ylim = ylim, nclass = isolate(input$nclass), freq = F, col = 0,
           xlab = "", main = "adjusted histogram")
    }
    
    if ((input$a1 %% 2 == 1 | input$a2 %% 2 == 1) & input$a3 %% 2 == 1){
      den <- density(d_hist())
      lines(den, lty = 2, lwd = 2)
    }
  })
  
  output$s1 <- renderPrint({
    if (input$c1){
      list(summary = summary(data()), data_size = dim(data())[1])
    }
  })
  
  output$s2 <- renderPrint({
    if (input$c1){
      if (!(dim(data())[1] == dim(d_tr())[1]) & input$c2){
        truncated <- sum(data()[, 1] >= quantile(data()[, 1], xlims()[1]) & data()[, 1] <= quantile(data()[, 1], xlims()[2]))
        cut_offs <- c(quantile(data()[, 1], xlims()[1]), quantile(data()[, 1], xlims()[2]))
        list(summary_truncated = summary(d_tr()), cut_offs = cut_offs, number_of_truncated = truncated)
      }
      else if (!(dim(data())[1] == dim(d_tr())[1]) & !input$c2){
        truncated <- sum (data()[, 1] >= xlims()[1] & data()[, 1] <= xlims()[2])
        a <- paste0(round(sum(data() <= xlims()[1])/dim(data())[1]*100, 2), "%")
        b <- paste0(round(sum(data() <= xlims()[2])/dim(data())[1]*100, 2), "%")
        cut_offs <- c(xlims()[1], xlims()[2])
        names(cut_offs) <- c(a, b)
        list(summary_truncated = summary(d_tr()), cut_offs = cut_offs, truncated_data_size = truncated)
      }
    }
  })
  
  
  
  
  output$t1 <- renderDT({
    if (input$c1 & input$a4 %% 2 == 1){
      datatable(data_temp(), options = list(pageLength = 5, scrollX = T, searching = F))
    }
  })
  
  output$t2 <- renderDT({
    if (input$c1 & input$a4 %% 2 == 1 & input$dim == "raw_data"){
      datatable(data(), options = list(pageLength = 5, scrollX = T, searching = F))
    }
  })
  
  
  ###
  
  ### 3
  # sw1 <- reactive(switch(input$de111 %% 2 + 1, "step down", "step up"))
  # 
  # observeEvent(input$de111, 
  #              if (input$de111 %% 2 == 1){
  #                updateActionButton(inputId = "de111", label = sw1(), icon = icon("arrow-up"))
  #                }
  #              else {
  #                updateActionButton(inputId = "de111", label = sw1(), icon = icon("arrow-down"))
  #                }
  #              )
  
  
  q_values <- reactive({
    if (length(extract1(input$q_values)) > 3 | length(extract1(input$q_values)) == 0){
      0.05
    }
    else if (is.na(sum(extract1((input$q_values))))){
      0.05
    }
    else if (min(extract1(input$q_values)) < 0){
      0.05
    }
    else {
      extract1(input$q_values)
    }
  })
  
  # D1
  
  # B-H
  d1b <- reactive({
    if (input$dim == "p_value" & !input$dc14){
      data()[, 1]
    }
    else if ((input$dim == "t" | input$dim == "raw_data") & !input$dc14){
      validate(need(is.na(sum(len1(), len2())) == F, message = "Check # of n1, n2"))
      ftn2(data(), len1(), len2())
    }
    else if (input$dim == "z" & !input$dc14){
      ftn3(data())
    }
    else if ((input$dim == "t" | input$dim == "raw_data") & input$dc14){
      validate(need(is.na(sum(len1(), len2())) == F, message = "Check # of n1, n2"))
      subset(ftn2(data(), len1(), len2()),  ftn4(data(), len1(), len2()) >= dc15()[1] & ftn4(data(), len1(), len2()) <= dc15()[2])
    }
    else if (input$dim == "z" & input$dc14){
      subset(ftn3(data()), data()[, 1] >= dc15()[1] & data()[, 1] <= dc15()[2])
      
    }
  })
  
  dc15 <- reactive({
    validate(need(is.na(sum(extract1((input$dc15)))) == F | sum(is.infinite(extract1((input$dc15)))) >= 1,  
                  message = "please check the truncated range."))
    sort(extract1(input$dc15))
  })
  
  # Efron
  d1e <- reactive({
    validate(need(input$dim != "p-value", message = "check data type"))
    if (input$dc14 == F){
      if (input$dim == "z"){
        data()[, 1]
      }
      else {
        validate(need(is.na(sum(len1(), len2())) == F, message = "Check # of n1, n2"))
        ftn4(data(), len1(), len2())
      }
    }
    else {
      if (input$dim == "z"){
        subset(data()[, 1], data()[, 1] >= dc15()[1] & data()[, 1] <= dc15()[2])
      }
      else {
        validate(need(is.na(sum(len1(), len2())) == F, message = "Check # of n1, n2"))
        subset(ftn4(data(), len1(), len2()), ftn4(data(), len1(), len2()) >= dc15()[1] & ftn4(data(), len1(), len2()) <= dc15()[2])
      }
    }
  })
  
  # Ramos
  # 일단은 t 아니면 z 그냥 둘다 받는 걸로?
  d1r <- reactive({
    validate(need(input$dim != "p-value", message = "check data type"))
    if (input$dim == "z"){
      data()[, 1]
    }
    else {
      validate(need(is.na(sum(len1(), len2())) == F, message = "Check # of n1, n2"))
      ftn4(data(), len1(), len2())
    }
  })
  
  # D2
  dc22 <- reactive({
    validate(need(is.na(sum(extract1((input$dc22)))) == F | sum(is.infinite(extract1((input$dc22)))) >= 1, 
                  message = "please check the truncated range."))
    sort(extract1(input$dc22))
  })
  
  dc23 <- reactive({
    validate(need(is.na(sum(extract1((input$dc23)))) == F | sum(is.infinite(extract1((input$dc23)))) >= 1,  
                  message = "please check the truncated range."))
    sort(extract1(input$dc23))
  })
  
  
  # Ploner
  d2 <- reactive({
    validate(need(is.na(sum(len1(), len2())) == F, message = "Check # of n1, n2"))
    
    if (input$dc21 == F){
        data_temp()
    }
    
    else {
        subset(data_temp(), 
               (data()[, 1] >= dc22()[1] & data()[, 1] <= dc22()[2]) & (data()[, 2] >= dc23()[1] & data()[, 2] <= dc23()[2]))
    }
    
  })
  
  grp <- reactive({
    rep(c("n1", "n2"), c(len1(), len2()))
  })
  
  # Kim
  d2k <- reactive({
    validate(need(is.na(sum(g1Index(), g2Index())) == F, message = "Check group indices"))
    
    if (input$dc21 == F){
      data_temp()
    }
    
    else {
      subset(data_temp(), 
             (data()[, 1] >= dc22()[1] & data()[, 1] <= dc22()[2]) & (data()[, 2] >= dc23()[1] & data()[, 2] <= dc23()[2]))
    }
      
  })
  
  # update select
  observe({
    input$dim
    
    z <- list()
    
    if (input$dim == "p-value"){
      z <- list("B-H")
    }
    else {
      z <- list("B-H", "Efron", "Ramos")
    }
    
    updateSelectInput(session, "m1",
                      label = "Select 1D fdr method(s)", choices = z)
  })
  
  # observeEvent : 설명서
  
  # updateActionbutton
  sw1 <- reactive(switch(input$de221 , Intersection = 0, Union = 1))
  
  
  # Benjamini&Hochiberg
  output$bh <- downloadHandler(
    filename = "bh.pdf",
    content = function(file){
      file.copy("www/raw_bh.pdf", file)
    }
  )
  
  # Efron
  output$Efron <- downloadHandler(
    filename = "Efron.pdf",
    content = function(file){
      file.copy("www/raw_Efron.pdf", file)
    }
  )
  
  # Ramos
  output$Ramos <- downloadHandler(
    filename = "Ramos.pdf",
    content = function(file){
      file.copy("www/raw_ramos.pdf", file)
    }
  )
  
  # Ploner
  output$Ploner <- downloadHandler(
    filename = "Ploner.pdf",
    content = function(file){
      file.copy("www/raw_ploner.pdf", file)
    }
  )
  
  # Kim
  output$Kim <- downloadHandler(
    filename = "Kim.pdf",
    content = function(file){
      file.copy("www/raw_kim.pdf", file)
    }
  )
  
  ###
  
  ### 4
  
  #@ 1D
  
  # bh
  bh <- reactive({
    validate(need("B-H" %in% input$m1, message = "B-H is not selected"))
    bhm(p_value = d1b(), q_value = q_values(), plot = F)
  })
  
  # efron
  nt <- reactive({
    validate(need("Efron" %in% input$m1, message = "Efron is not selected")) 
    switch(input$de121, Theorical = 0, MLE = 1, CME = 2)
  })
  
  pct0 <- reactive({
    validate(need("Efron" %in% input$m1 & "CME" %in% input$de121, message = "Efron & CME is not selected"))
    input$de122
  })
  
  etype <- reactive({
    validate(need("Efron" %in% input$m1 | "Ramos" %in% input$m1, message = "Efron or Ramos is not selected"))
    switch(input$dc11, Natural_Spline = 0, Polynomial_Spline = 1)
  })
  
  efron <- reactive({
    validate(need("Efron" %in% input$m1, message = "Efron is not selected"))
    if ("CME" %in% input$de121){
      locfdr(d1e(), df = input$dc12, bre = input$dc13, nulltype = nt(), type = etype(), 
             plot = F, pct0 = pct0())
    }
    else {
      locfdr(d1e(), df = input$dc12, bre = input$dc13, nulltype = nt(), type = etype(), plot = F)
    }
  })
  
  # ramos
  dc152 <- reactive({
    validate(need("Ramos" %in% input$m1, message = "Ramos is not selected"))
    if(input$dc14){
      dc15()
    }
    else {
      c(-Inf, Inf)
    }
  })
  
  ramos_idx <- reactive({
    validate(need(length(d1r()) > 0, message = "Check data"))
    order(d1r())
  })
  
  ramos <- reactive({
    validate(need("Ramos" %in% input$m1, message = "Ramos is not selected"))
    ramos_fdr(Zval = d1r()[ramos_idx()], DC = dc152(), df = input$dc12, type = etype(), 
              bre = input$dc13, Afac = input$de131, inicialC = input$de132, lastC = input$de133)
  })
  
  # plotting
  ranges1 <- reactiveValues(x = NULL, y = NULL)
  rv1 <- reactive({
    validate(need(is.na(sum(extract1(input$rv1))) == F, message = "Check Null proportion"))
    extract2(input$rv1)})
  rv2 <- reactive({
    validate(need(is.na(sum(extract1(input$rv2))) == F, message = "Check Null Mean"))
    extract1(input$rv2)})
  rv3 <- reactive({
    validate(need(is.na(sum(extract1(input$rv3))) == F, message = "Check Null SD"))
    extract1(input$rv3)})
  rv4 <- reactive({
    validate(need(is.na(sum(extract1(input$rv4))) == F, message = "Check Alter. proportion"))
    extract1(input$rv4)})
  rv5 <- reactive({
    validate(need(is.na(sum(extract1(input$rv5))) == F, message = "Check Alter. Mean"))
    extract1(input$rv5)})
  rv6 <- reactive({
    validate(need(is.na(sum(extract1(input$rv6))) == F, message = "Check Alter. SD"))
    extract1(input$rv6)})
  dtr <- reactive({
    if (input$dc14){
      dc15()
    }
    else {
      range(data()[, 1])
    }
  })
  
  
  
  output$r11 <- renderPlot({
    if (input$a5 %% 2 == 1 & !is.null(input$m1)){
      if ("B-H plot" %in% input$r1s){
        bhm(d1b(), q_value = q_values(), plot = 1:length(q_values()), main = "original")
      }
      
      if ("p0f0 lines" %in% input$r1s){
        hist(d1e(), freq = F, xlab = "stat", main = "original", col = 0, 
             border  = adjustcolor(1, 0.5), nclass = input$dc13)
        if ("Efron" %in% input$m1){
          lines(sort(d1e()), 
                efron()$fp0[nt() * 2 + 1, 3] * dnorm(sort(d1e()), efron()$fp0[nt() * 2 + 1, 1], 
                                                     efron()$fp0[nt() * 2 + 1, 2]), col = "seagreen4", lty = 2)
        }
        if ("Ramos" %in% input$m1){
          ramos_plot(ramos(), plot = 1)
        }
        
        if ("Efron" %in% input$m1 | "Ramos" %in% input$m1){
          if(input$c3 & input$a6 %% 2 == 1){
            if ("f" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 1)
            }
            if ("p0f0" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 2)
            }
            if ("p1f1" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 3)
            }
            if ("fdr" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 4)
            }
            
          }
        }
        
        legend("topright", c("Efron", "Ramos"), col = c("seagreen4", 4), lty = c(2, 4), lwd = 1.5, cex = 0.85)
      }
      
      if ("fdr lines" %in% input$r1s){
        if ("Efron" %in% input$m1){
          idx1 <- order(d1e())
          stat1 <- d1e()[idx1]
          fdr1 <- efron()$fdr[idx1]
          
          plot(stat1, fdr1, type = "l", main = "original", xlab = "stat", ylab = "fdr", col = "seagreen4", lty = 2)
        }
        
        if ("Ramos" %in% input$m1){
          if ("Efron" %in% input$m1){
            ramos_plot(ramos(), plot = 3)
          }
          else {
            ramos_plot(ramos(), plot = 4, main = "original", xlab = "stat", ylab = "fdr")
          }
        }
        
        if ("Efron" %in% input$m1 | "Ramos" %in% input$m1){
          if(input$c3 & input$a6 %% 2 == 1){
            if ("f" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 1)
            }
            if ("p0f0" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 2)
            }
            if ("p1f1" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 3)
            }
            if ("fdr" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 4)
            }
            
          }
        }
        
        # h line
        if (length(q_values()) > 0){
          for (i in q_values()){
            abline(h = i, col = 2, lty = 3, lwd = 1.5)
          }
        }
        
        
        
        legend("topright", c("Efron", "Ramos"), col = c("seagreen4", 4), lty = c(2, 4), lwd = 1.5, cex = 0.85)
      }
    }
  })
  
  output$r12 <- renderPlot({
    if (input$a5 %% 2 == 1 & !is.null(input$m1)){
      if ("B-H plot" %in% input$r1s){
        bhm(d1b(), q_value = q_values(), plot = 1:length(q_values()), main = "zoom in", 
            xlim = ranges1$x, ylim = ranges1$y)
      }
      
      if ("p0f0 lines" %in% input$r1s){
        if (is.null(ranges1$x) & is.null(ranges1$y)){
          hist(d1e(), nclass = input$dc13, freq = F, xlab = "", main = "zoom in", col = 0, border  = adjustcolor(1, 0.5))
        }
        else {
          hist(d1e(), nclass = input$dc13, freq = F, xlab = "", main = "zoom in", col = 0, border  = adjustcolor(1, 0.5),
               xlim = ranges1$x, ylim = ranges1$y)
        }
        if ("Efron" %in% input$m1){
          lines(sort(d1e()), 
                efron()$fp0[nt() * 2 + 1, 3] * dnorm(sort(d1e()), efron()$fp0[nt() * 2 + 1, 1], 
                                                     efron()$fp0[nt() * 2 + 1, 2]), col = "seagreen4", lty = 2)
        }
        if ("Ramos" %in% input$m1){
          ramos_plot(ramos(), plot = 1)
        }
        
        if ("Efron" %in% input$m1 | "Ramos" %in% input$m1){
          if(input$c3 & input$a6 %% 2 == 1){
            if ("f" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 1)
            }
            if ("p0f0" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 2)
            }
            if ("p1f1" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 3)
            }
            if ("fdr" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 4)
            }
            
          }
        }
        
        legend("topright", c("Efron", "Ramos"), col = c("seagreen4", 4), lty = c(2, 4), lwd = 1.5, cex = 0.85)
      }
      
      if ("fdr lines" %in% input$r1s){
        if ("Efron" %in% input$m1){
          idx1 <- order(d1e())
          stat1 <- d1e()[idx1]
          fdr1 <- efron()$fdr[idx1]
          
          plot(stat1, fdr1, type = "l", main = "zoom in", xlab = "stat", ylab = "fdr", col = "seagreen4", lty = 2,
               xlim = ranges1$x, ylim = ranges1$y)
        }
        
        if ("Ramos" %in% input$m1){
          if ("Efron" %in% input$m1){
            ramos_plot(ramos(), plot = 3)
          }
          else {
            ramos_plot(ramos(), plot = 4, main = "zoom in", xlab = "stat", ylab = "fdr",
                       xlim = ranges1$x, ylim = ranges1$y)
          }
        }
        
        if ("Efron" %in% input$m1 | "Ramos" %in% input$m1){
          if(input$c3 & input$a6 %% 2 == 1){
            if ("f" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 1)
            }
            if ("p0f0" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 2)
            }
            if ("p1f1" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 3)
            }
            if ("fdr" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 4)
            }
            
          }
        }
        
        # h line
        if (length(q_values()) > 0){
          for (i in q_values()){
            abline(h = i, col = 2, lty = 3, lwd = 1.5)
          }
        }
        
        legend("topright", c("Efron", "Ramos"), col = c("seagreen4", 4), lty = c(2, 4), lwd = 1.5, cex = 0.85)
      }
    }
  })
  
  p1 <- reactive({
    if (input$a5 %% 2 == 1 & !is.null(input$m1)){
      if ("B-H plot" %in% input$r1s){
        bhm(d1b(), q_value = q_values(), plot = 1:length(q_values()), main = "zoom in", 
            xlim = ranges1$x, ylim = ranges1$y)
      }
      
      if ("p0f0 lines" %in% input$r1s){
        if (is.null(ranges1$x) & is.null(ranges1$y)){
          hist(d1e(), nclass = input$dc13, freq = F, xlab = "", main = "zoom in", col = 0, border  = adjustcolor(1, 0.5))
        }
        else {
          hist(d1e(), nclass = input$dc13, freq = F, xlab = "", main = "zoom in", col = 0, border  = adjustcolor(1, 0.5),
               xlim = ranges1$x, ylim = ranges1$y)
        }
        if ("Efron" %in% input$m1){
          lines(sort(d1e()), 
                efron()$fp0[nt() * 2 + 1, 3] * dnorm(sort(d1e()), efron()$fp0[nt() * 2 + 1, 1], 
                                                     efron()$fp0[nt() * 2 + 1, 2]), col = "seagreen4", lty = 2)
        }
        if ("Ramos" %in% input$m1){
          ramos_plot(ramos(), plot = 1)
        }
        
        if ("Efron" %in% input$m1 | "Ramos" %in% input$m1){
          if(input$c3 & input$a6 %% 2 == 1){
            if ("f" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 1)
            }
            if ("p0f0" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 2)
            }
            if ("p1f1" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 3)
            }
            if ("fdr" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 4)
            }
            
          }
        }
        
        legend("topright", c("Efron", "Ramos"), col = c("seagreen4", 4), lty = c(2, 4), lwd = 1.5, cex = 0.85)
      }
      
      if ("fdr lines" %in% input$r1s){
        if ("Efron" %in% input$m1){
          idx1 <- order(d1e())
          stat1 <- d1e()[idx1]
          fdr1 <- efron()$fdr[idx1]
          
          plot(stat1, fdr1, type = "l", main = "zoom in", xlab = "stat", ylab = "fdr", col = "seagreen4", lty = 2,
               xlim = ranges1$x, ylim = ranges1$y)
        }
        
        if ("Ramos" %in% input$m1){
          if ("Efron" %in% input$m1){
            ramos_plot(ramos(), plot = 3)
          }
          else {
            ramos_plot(ramos(), plot = 4, main = "zoom in", xlab = "stat", ylab = "fdr",
                       xlim = ranges1$x, ylim = ranges1$y)
          }
        }
        
        if ("Efron" %in% input$m1 | "Ramos" %in% input$m1){
          if(input$c3 & input$a6 %% 2 == 1){
            if ("f" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 1)
            }
            if ("p0f0" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 2)
            }
            if ("p1f1" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 3)
            }
            if ("fdr" %in% input$rv7){
              draw_true(range = dtr(), pr1 = rv1(), m1 = rv2(), s1 = rv3(), pr2 = rv4(), m2 = rv5(), s2 = rv6(), plot = 4)
            }
            
          }
        }
        
        # h line
        if (length(q_values()) > 0){
          for (i in q_values()){
            abline(h = i, col = 2, lty = 3, lwd = 1.5)
          }
        }
        
        legend("topright", c("Efron", "Ramos"), col = c("seagreen4", 4), lty = c(2, 4), lwd = 1.5, cex = 0.85)
      }
    }
  })
  
  output$download1 <- downloadHandler(
    filename = function() {
      paste0(input$r1s, ".png")
    },
    
    content = function(file){
      png(file, width = 550, height = 550)
      p1()
      dev.off()
    }
  )
  
  observe({
    brush1 <- input$plot1_brush
    if (!is.null(brush1)) {
      ranges1$x <- c(brush1$xmin, brush1$xmax)
      ranges1$y <- c(brush1$ymin, brush1$ymax)
      
    } else {
      ranges1$x <- NULL
      ranges1$y <- NULL
    }
  })
  
  observe({
    input$m1
    
    z <- list()
    
    if ("B-H" %in% input$m1){
      z$BH_plot <- "B-H plot"
    }
    
    if ("Efron" %in% input$m1 | "Ramos" %in% input$m1){
      z$FDR_1D <- c("p0f0 lines","fdr lines")
    }
    
    updateSelectInput(session, "r1s",
                      label = "Select type of plot",
                      choices = z
    )
  })
  
  #@ 2D
  
  # 1d 
  # nr = input$de212
  ploner1 <- reactive({
    validate(need("Ploner" %in% input$m2, message = "Ploner is not selected"))
    fdr1d(xdat = d2(), grp = grp(), nperm = input$de211, seed = 0) # check 
  })
  
  
  # 2d
  
  # ploner
  ploner <- reactive({
    validate(need("Ploner" %in% input$m2, message = "Ploner is not selected"))
    fdr2d(xdat = d2(), grp = grp(), nperm = input$de211, nr = input$de212, smooth = input$de213, seed = 0) # check 
  })
  
  # kim
  kim <- reactive({
    validate(need("Kim" %in% input$m2, message = "Kim is not selected"))
    yrkim(data = d2k(), idx1 = g1Index(), idx2 = g2Index(), cutoff = q_values(), trc = input$de222)
  })
  
  # plotting
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  col <- reactive({
    c(3, 4, 2)
  })
    
  output$r21 <- renderPlot({
    if (input$dim == "raw_data" & input$a5 %% 2 == 1 & ("Ploner" %in% input$m2 | "Kim" %in% input$m2)){
      if ("t&logse" %in% input$r2s){
        plot(ploner()[, 1], ploner()[, 2], main = "original", col = adjustcolor("black", alpha.f = 0.5),
             xlab = names(ploner())[1], ylab = names(ploner())[2], pch = ".")
      }
      else if ("t&locfdr" %in% input$r2s){
        plot.fdr1d.result(ploner1(), main = "original", rug = F, xlab = "tstat", ylab = "loc.fdr1d")
      }
      else if ("fdr2d_est" %in% input$r2s){
        plot.fdr2d.result(ploner(), vfont = c("sans serif", "bold italic"), main = "original", 
                          lcol = col()[1], levels = q_values()[1])
        if (length(q_values()) >= 2){
          for (i in 2:length(q_values())){
            par(new = T)
            plot.fdr2d.result(ploner(), vfont = c("sans serif", "bold italic"), main = "original", 
                              lcol = col()[i], levels = q_values()[i])
          }
        }
      }
      else if ("tornado" %in% input$r2s){
        
        if ("Ploner" %in% input$m2){
          Tornadoplot(ploner(), label = T, main = "original", vfont = c("sans serif", "bold italic"),
                      lcol = col()[1], levels = q_values()[1])
          
          if (length(q_values()) >= 2){
            for (i in 2:length(q_values())){
              par(new = T)
              Tornadoplot(ploner(), label = T, main = "original", vfont = c("sans serif", "bold italic"),
                          lcol = col()[i], levels = q_values()[i])
            }
          }
        }
        
        if ("Kim" %in% input$m2){
          if ("Ploner" %in% input$m2){
            kim_plot(kim(), plot = 2 * (sw1()) + 2, main = "original")
          }
          else {
            kim_plot(kim(), plot = 2 * (sw1()) + 1, main = "original")
          }
        }
        

      }
      else if ("volcano" %in% input$r2s){
        
        if ("Ploner" %in% input$m2){
          Volcanoplot(ploner(), df = len1() + len2() - 2, label = T, main = "original",
                      vfont = c("sans serif", "bold italic"), lcol = col()[1], levels = q_values()[1])
          
          if (length(q_values()) >= 2){
            for (i in 2:length(q_values())){
              par(new = T)
              Volcanoplot(ploner(), df = len1() + len2() - 2, label = T, main = "original",
                          vfont = c("sans serif", "bold italic"), lcol = col()[i], levels = q_values()[i])
            }
          }
        }
        
        if ("Kim" %in% input$m2){
          if ("Ploner" %in% input$m2){
            kim_plot(kim(), plot = 2 * (sw1()) + 6, main = "original")
          }
          else {
            kim_plot(kim(), plot = 2 * (sw1()) + 5, main = "original")
          }
        }
        
      }
    }
    
    legend("topright", c("Ploner", "Kim"), lty = c(1, 2), lwd = 1.5, cex = 0.85)
    legend("topleft", c(as.character(q_values())), col = c(3, 4, 2)[1:length(q_values())], lwd = 1.5, cex = 0.85)
  })
  
  output$r22 <- renderPlot({
    if (input$dim == "raw_data" & input$a5 %% 2 == 1 & ("Ploner" %in% input$m2 | "Kim" %in% input$m2)){
      if ("t&logse" %in% input$r2s){
        plot(ploner()[, 1], ploner()[, 2], main = "zoom in", col = adjustcolor("black", alpha.f = 0.5),
             xlim = ranges2$x, ylim = ranges2$y, xlab = names(ploner())[1], ylab = names(ploner())[2], pch = ".")
      }
      else if ("t&locfdr" %in% input$r2s){
        plot.fdr1d.result(ploner1(), main = "zoom in",
                          xlim = ranges2$x, ylim = ranges2$y, rug = F, xlab = "tstat", ylab = "loc.fdr1d")
      }
      else if ("fdr2d_est" %in% input$r2s){
        plot.fdr2d.result(ploner(), xlim = ranges2$x, ylim = ranges2$y, vfont = c("sans serif", "bold italic"), 
                          main = "zoom in", lcol = col()[1], levels = q_values()[1])
        if (length(q_values()) >= 2){
          for (i in 2:length(q_values())){
            par(new = T)
            plot.fdr2d.result(ploner(), xlim = ranges2$x, ylim = ranges2$y, vfont = c("sans serif", "bold italic"), 
                              main = "zoom in", lcol = col()[i], levels = q_values()[i])
          }
        }
      }
      else if ("tornado" %in% input$r2s){
        if ("Ploner" %in% input$m2){
          Tornadoplot(ploner(), label = T, main = "zoom in", xlim = ranges2$x, ylim = ranges2$y,
                      vfont = c("sans serif", "bold italic"), lcol = col()[1], levels = q_values()[1])
          if (length(q_values()) >= 2){
            for (i in 2:length(q_values())){
              par(new = T)
              Tornadoplot(ploner(), label = T, main = "zoom in", xlim = ranges2$x, ylim = ranges2$y,
                          vfont = c("sans serif", "bold italic"), lcol = col()[i], levels = q_values()[i])
            }
          }
        }
        
        if ("Kim" %in% input$m2){
          if ("Ploner" %in% input$m2){
            kim_plot(kim(), plot = 2 * (sw1()) + 2, main = "zoom in")
          }
          else {
            kim_plot(kim(), plot = 2 * (sw1()) + 1, main = "zoom in", xlim = ranges2$x, ylim = ranges2$y)
          }
        }
      }
      else if ("volcano" %in% input$r2s){
        
        if ("Ploner" %in% input$m2){
          Volcanoplot(ploner(), df = len1() + len2() - 2, label = T, main = "zoom in", xlim = ranges2$x, ylim = ranges2$y,
                      vfont = c("sans serif", "bold italic"), lcol = col()[1], levels = q_values()[1])
          if (length(q_values()) >= 2){
            for (i in 2:length(q_values())){
              par(new = T)
              Volcanoplot(ploner(), df = len1() + len2() - 2, label = T, main = "zoom in", xlim = ranges2$x, ylim = ranges2$y,
                          vfont = c("sans serif", "bold italic"), lcol = col()[i], levels = q_values()[i])
            }
          }
        }
        
        if ("Kim" %in% input$m2){
          if ("Ploner" %in% input$m2){
            kim_plot(kim(), plot = 2 * (sw1()) + 6, main = "zoom in")
          }
          else {
            kim_plot(kim(), plot = 2 * (sw1()) + 5, main = "zoom in", xlim = ranges2$x, ylim = ranges2$y)
          }
        }
      }
    }
    
    legend("topright", c("Ploner", "Kim"), lty = c(1, 2), lwd = 1.5, cex = 0.85)
    legend("topleft", c(as.character(q_values())), col = c(3, 4, 2)[1:length(q_values())], lwd = 1.5, cex = 0.85)
  })
  
  p2 <- reactive({
    if (input$dim == "raw_data" & input$a5 %% 2 == 1 & ("Ploner" %in% input$m2 | "Kim" %in% input$m2)){
      if ("t&logse" %in% input$r2s){
        plot(ploner()[, 1], ploner()[, 2], main = "zoom in", col = adjustcolor("black", alpha.f = 0.5),
             xlim = ranges2$x, ylim = ranges2$y, xlab = names(ploner())[1], ylab = names(ploner())[2], pch = ".")
      }
      else if ("t&locfdr" %in% input$r2s){
        plot.fdr1d.result(ploner1(), main = "zoom in",
                          xlim = ranges2$x, ylim = ranges2$y, rug = F, xlab = "tstat", ylab = "loc.fdr1d")
      }
      else if ("fdr2d_est" %in% input$r2s){
        plot.fdr2d.result(ploner(), xlim = ranges2$x, ylim = ranges2$y, vfont = c("sans serif", "bold italic"), 
                          main = "zoom in", lcol = col()[1], levels = q_values()[1])
        if (length(q_values()) >= 2){
          for (i in 2:length(q_values())){
            par(new = T)
            plot.fdr2d.result(ploner(), xlim = ranges2$x, ylim = ranges2$y, vfont = c("sans serif", "bold italic"), 
                              main = "zoom in", lcol = col()[i], levels = q_values()[i])
          }
        }
      }
      else if ("tornado" %in% input$r2s){
        if ("Ploner" %in% input$m2){
          Tornadoplot(ploner(), label = T, main = "zoom in", xlim = ranges2$x, ylim = ranges2$y,
                      vfont = c("sans serif", "bold italic"), lcol = col()[1], levels = q_values()[1])
          if (length(q_values()) >= 2){
            for (i in 2:length(q_values())){
              par(new = T)
              Tornadoplot(ploner(), label = T, main = "zoom in", xlim = ranges2$x, ylim = ranges2$y,
                          vfont = c("sans serif", "bold italic"), lcol = col()[i], levels = q_values()[i])
            }
          }
        }
        
        if ("Kim" %in% input$m2){
          if ("Ploner" %in% input$m2){
            kim_plot(kim(), plot = 2 * (sw1()) + 2, main = "zoom in")
          }
          else {
            kim_plot(kim(), plot = 2 * (sw1()) + 1, main = "zoom in", xlim = ranges2$x, ylim = ranges2$y)
          }
        }
      }
      else if ("volcano" %in% input$r2s){
        
        if ("Ploner" %in% input$m2){
          Volcanoplot(ploner(), df = len1() + len2() - 2, label = T, main = "zoom in", xlim = ranges2$x, ylim = ranges2$y,
                      vfont = c("sans serif", "bold italic"), lcol = col()[1], levels = q_values()[1])
          if (length(q_values()) >= 2){
            for (i in 2:length(q_values())){
              par(new = T)
              Volcanoplot(ploner(), df = len1() + len2() - 2, label = T, main = "zoom in", xlim = ranges2$x, ylim = ranges2$y,
                          vfont = c("sans serif", "bold italic"), lcol = col()[i], levels = q_values()[i])
              }
            }
          }
        
        if ("Kim" %in% input$m2){
          if ("Ploner" %in% input$m2){
            kim_plot(kim(), plot = 2 * (sw1()) + 6, main = "zoom in")
          }
          else {
            kim_plot(kim(), plot = 2 * (sw1()) + 5, main = "zoom in", xlim = ranges2$x, ylim = ranges2$y)
          }
        }
      }
    }
    
    legend("topright", c("Ploner", "Kim"), lty = c(1, 2), lwd = 1.5, cex = 0.85)
    legend("topleft", c(as.character(q_values())), col = c(3, 4, 2)[1:length(q_values())], lwd = 1.5, cex = 0.85)
  })
  
  output$download2 <- downloadHandler(
    filename = function() {
      paste0(input$r2s, ".png")
    },
    
    content = function(file){
      png(file, width = 1000, height = 1000)
      p2()
      dev.off()
    }
  )
  
  observe({
    brush2 <- input$plot2_brush
    if (!is.null(brush2)) {
      ranges2$x <- c(brush2$xmin, brush2$xmax)
      ranges2$y <- c(brush2$ymin, brush2$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
  ###
  
  ### 4 - 2 Table
  
  observe({
    
    z <- q_values()
    
    updateSelectInput(session, "q_values2",
                      label = "Select the q_value to use to represent the rejection table", choices = z)
  })
  
  
  q_values2 <- reactive({
    as.numeric(input$q_values2)
  })
  
  r_bh1 <- reactive({
    validate(need("B-H" %in% input$m1, message = "B-H is not selected"))
    bhm(p_value = d1b(), q_value = q_values2(), plot = F)
  })
  
  
  ## problem 5/28
  r_bh <- reactive({
    if ("B-H" %in% input$m1){
      c(dim(data())[1] - length(d1b()),
        if (sum(r_bh1()$rej_n) == 0){
          NA
        }
        else {
          sort(d1b())[r_bh1()$rej_n]
        },
        NA,
        r_bh1()$rej_n,
        NA,
        r_bh1()$rej_n)
    }
    else {
      rep(NA, 6)
    }
  })
  
  r_efron_idx <- reactive({
    validate(need("Efron" %in% input$m1, message = "Efron is not selected"))
    which(efron()$fdr > q_values2())
  })
  
  r_efron <- reactive({
    if ("Efron" %in% input$m1){
      c(dim(data())[1] - length(d1e()),
        min(d1e()[r_efron_idx()]),
        max(d1e()[r_efron_idx()]),
        sum(d1e() < min(d1e()[r_efron_idx()])),
        sum(d1e() > max(d1e()[r_efron_idx()])),
        sum(efron()$fdr <= q_values2())
        )
    }
    else {
      rep(NA, 6)
    }
  })
  
  Ramos_fdr <- reactive({
    validate(need("Ramos" %in% input$m1, message = "Ramos is not selected"))
    ramos()$Ramos_fdr})
  
  zval <- reactive({
    validate(need("Ramos" %in% input$m1, message = "Ramos is not selected"))
    d1r()[ramos_idx()]
  })
  
  r_ramos <- reactive({
    if ("Ramos" %in% input$m1){
      c(NA,
        zval()[tail(which(zval() < (-input$de131) & Ramos_fdr()[, 2] < q_values2()), 1)],
        zval()[head(which(zval() > (input$de131) & Ramos_fdr()[, 2] < q_values2()), 1)],
        sum(zval() <= zval()[tail(which(zval() < (-input$de131) & Ramos_fdr()[, 2] < q_values2()), 1)]),
        sum(zval() >= zval()[head(which(zval() > (input$de131) & Ramos_fdr()[, 2] < q_values2()), 1)]),
        sum(Ramos_fdr()[, 2] <= q_values2())
        )
    }
    else {
      rep(NA, 6)
    }
  })
  
  
  # 
  r_ploner <- reactive({
    if ("Ploner" %in% input$m2){
      c(dim(data())[1] - dim(d2())[1],
        sum(ploner()$fdr.local < q_values2())
        )
    }
    
    else {
      rep(NA, 2)
    }
  })
  
  
  idx_kim <- reactive({
    which(q_values2() == q_values())
  })
  
  r_kim_i <- reactive({
    if ("Kim" %in% input$m2){
      c(dim(data())[1] - dim(d2k())[1],
        kim()[[1]]$rejection_table[idx_kim(), 1]
        )
    }
    else {
      rep(NA, 2)
    }
  })
  
  r_kim_u <- reactive({
    if ("Kim" %in% input$m2){
      c(dim(data())[1] - dim(d2k())[1],
        kim()[[1]]$rejection_table[idx_kim(), 2]
        )
    }
    else {
      rep(NA, 2)
    }
  })
  
  
  rejection_table_1 <- reactive({
    data.frame(truncated_data = c(r_bh()[1], r_efron()[1], r_ramos()[1]),
               left_cut = round(c(r_bh()[2], r_efron()[2], r_ramos()[2]), 4),
               right_cut = round(c(r_bh()[3], r_efron()[3], r_ramos()[3]), 4),
               left_reject = c(r_bh()[4], r_efron()[4], r_ramos()[4]),
               right_reject = c(r_bh()[5], r_efron()[5], r_ramos()[5]),
               total_reject = c(r_bh()[6], r_efron()[6], r_ramos()[6]), row.names = rownames(rejection_table)
               )
  })
  

  rejection_table2_1 <- reactive({
    data.frame(truncated_data = c(r_ploner()[1], r_kim_i()[1], r_kim_u()[1]),
               total_reject = c(r_ploner()[2], r_kim_i()[2], r_kim_u()[2]), row.names = rownames(rejection_table2)
               )
  })
  
  output$t3 <- renderDT({
    datatable(rejection_table_1(), options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                  searching = F, dom = 'tip')
              )
  })
  
  output$t4 <- renderDT({
    datatable(rejection_table2_1(), options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                   searching = F, dom = 'tip')
    )
  })
  ### 
  
  
  ### making csv info. (trimmed data & rejections)
  
  ## idx
  d1_idx <- reactive({
    if ((input$dim == "t" | input$dim == "raw_data") & input$dc14){
      validate(need(is.na(sum(len1(), len2())) == F, message = "Check # of n1, n2"))
      (ftn4(data(), len1(), len2()) >= dc15()[1] & ftn4(data(), len1(), len2()) <= dc15()[2])
    }
    else if (input$dim == "z" & input$dc14){
      (data()[, 1] >= dc15()[1] & data()[, 1] <= dc15()[2])
    }
    else {
      rep(T, dim(data())[1])
    }
  })
  
  
  d2_idx <- reactive({
    validate(need(is.na(sum(len1(), len2())) == F, message = "Check # of n1, n2"))
    
    if (input$dc21 == F){
      rep(T, dim(data())[1])
    }
    
    else {
      ((data()[, 1] >= dc22()[1] & data()[, 1] <= dc22()[2]) & (data()[, 2] >= dc23()[1] & data()[, 2] <= dc23()[2]))
    }
    
  })
  
  ## trn idx
  d1_trn <- reactive({
    which(!d1_idx())
  })
  
  d2_trn <- reactive({
    which(!d2_idx())
  })
  
  ## remain idx
  d1_remain <- reactive({
    which(d1_idx())
  })
  
  d2_remain <- reactive({
    which(d2_idx())
  })
  
  ## reject idx
  
  # bh idx (need sort)
  bh_rej <- reactive({
    if ("B-H" %in% input$m1){
      d1_remain()[ (r_bh1()[[4]])[r_bh1()[[3]]] ]
    }
    else {
      NA
    }
  })
  
  # efron idx
  efron_rej <- reactive({
    if ("Efron" %in% input$m1){
      d1_remain()[efron()$fdr <= q_values2()] 
    }
    else {
      NA
    }
  })
  
  # ramos idx (need sort)
  ramos_rej <- reactive({
    if ("Ramos" %in% input$m1){
      (1:length(d1r()))[ramos_idx()[Ramos_fdr()[, 2] <= q_values2()]] 
    }
    else {
      NA
    }
  })
  
  # ploner idx
  ploner_rej <- reactive({
    if ("Ploner" %in% input$m2){
      d2_remain()[ploner()$fdr.local < q_values2()] 
    }
    else {
      NA
    }
  })
  
  # kim idx
  kim_i_rej <- reactive({
    if ("Kim" %in% input$m2){
      d2_remain()[ kim()[[1]][[idx_kim()]] ] 
    }
    else {
      NA
    }
  })
  
  kim_u_rej <- reactive({
    if ("Kim" %in% input$m2){
      d2_remain()[ kim()[[1]][[idx_kim() + length(q_values())]] ]
    }
    else {
      NA
    }
  })
  ###
  
  ### csv out
  n_trn_max <- reactive({
    max(length(d1_trn()), length(d2_trn()), 0, na.rm = T)
  })
  
  n_rej_max <- reactive({
    max(length(bh_rej()), length(efron_rej()), length(ramos_rej()), length(ploner_rej()),
        length(kim_u_rej()), length(kim_i_rej()), 0, na.rm = T)
  })
  
  rej_mat1 <- reactive({
    matrix(
      c(
        vwn(n_rej_max(), bh_rej()), vwn2(n_trn_max(), d1_trn()),
        vwn(n_rej_max(), efron_rej()), vwn2(n_trn_max(), d1_trn()),
        vwn(n_rej_max(), ramos_rej()), rep(NA, n_trn_max()),
        vwn(n_rej_max(), ploner_rej()), vwn2(n_trn_max(), d2_trn()),
        vwn(n_rej_max(), kim_i_rej()), vwn2(n_trn_max(), d2_trn()),
        vwn(n_rej_max(), kim_u_rej()), vwn2(n_trn_max(), d2_trn())
        ),
      
      nrow = n_trn_max() + n_rej_max(), ncol = 6, 
      dimnames = list(NULL, c(rownames(rejection_table), rownames(rejection_table2)))
      )
  })
  
  rej_df <- reactive({
    data.frame(type = rep(c("rejected", "truncated"), c(n_rej_max(), n_trn_max())), rej_mat1())
  })
  ###

  output$download3 <- downloadHandler(
    filename = paste0("result_idx.csv")
    ,
    content = function(file) {
      write.csv(rej_df(), file, row.names = F)
    }
  )
  
}

shinyApp(ui, server)