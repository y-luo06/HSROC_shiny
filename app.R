library(shiny)
library(shinythemes)
library(rstan)
library(tidyverse)
library(ggplot2)
library(HDInterval)

lmcss <- "
#plot-container1 {
  position: absolute; left: 50%; top: 40%; z-index: -1;
}
#loading-spinner1 {
  position: absolute; left: 50%; top: 50%; z-index: -1;
  margin-top: -33px;  /* half of the spinner's height */
  margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
  z-index: -2;
}
#loadmessage1 {
  position: absolute; top: 50%; left: 10%; width: 80%; padding: 5px 0px 5px 0px; 
  text-align: center; font-size:130%;  font-style:italic; color: #708090;
  background-color:white; z-index: -1;
}
"

df = read.csv("sampledata_wide.csv")

ui <- fluidPage(
  fluidRow(
    headerPanel(h2("CAST-HSROC: CAlculator for the Summary poinT from HSROC model"), 
                windowTitle = "CAST-HSROC: Calculator for the Summary point from HSROC Model")
  ),
  fluidRow(
    navbarPage(theme = shinytheme("flatly"), "Menu",
               tabPanel("Introduction", style = "position:absolute; margin-left: 80px; margin-right: 100px",
                 h3("Estimating the Certainty of Evidence for HSROC Model in the Meta-analysis of Diagnostic Test Accuracy", 
                    align = "center"),
                 br(),
                 p("The Grading of Recommendations Assessment, Development and Evaluation (GRADE) Working Group 
                   have published a new guideline regarding estimating the certainty of evidence for the meta-analysis 
                   of diagnostic test accuracy (DTA) [1]. However, no concensus has been achieved on how to estimate the certainty of evidence for ", 
                   span("hierarchical summary receiver operating characteristic (HSROC) model.", style = "color:lightseagreen"), style = "font-size:19px"),
                   p("HSROC model is a statistical model based on latent scale logistic regression. It considers variability 
                   both within and between studies (for example, different thresholds used in primary studies) [2]. 
                   HSROC cannot estimate a summary point of sensitivity and specificity. Instead, we can present", 
                   strong("the estimate of specificity and its 95% credible interval (CrI), if a fixed value of sensitity is given, or vice versa,", style = "color:steelblue"),
                   " to demonstrate the changes in sensitivity and specificity along the curve.", style = "font-size:19px"), 
                 p("We think this result should be included in the Summary of Finding (SoF) table for a meta-analysis of DTA using HSROC model. 
                   However, in order to obtain these values, difficult calculation based on log diagnostic odds ratio (log DOR) is needed [2]. 
                   Therefore, we develop this calculator for users to easily estimate it. We also present ",
                   strong("an example of the SoF table for meta-analyses of DTA", style = "color:steelblue"),
                   " in below: ", style = "font-size:19px"),
                 div(img(src = "sof_table.png", height = "200%", width = "80%"), align = "center"),
                 hr(),
                 p("[1] Schunemann HJ, Mustafa RA, Brozek J, Steingart KR, Leeflang M, Murad MH, et al. 
                   GRADE guidelines: 21 part 2. Inconsistency, Imprecision, publication bias and other domains for rating 
                   the certainty of evidence for test accuracy and presenting it in evidence profiles and summary of findings 
                   tables. J Clin Epidemiol 2020.", style = "font-size: 15px; color: grey"),
                 p("[2] Deeks JJ, Bossuyt PM, Gatsonis C (editors). Cochrane Handbook for Systematic Reviews of Diagnostic 
                   Test Accuracy Version 1.0. Available from: http://srdta.cochrane.org/.; 2010 [accessed March 1 2020].", 
                   style = "font-size: 15px; color: grey")
               ),
               tabPanel("Upload Data",
                 sidebarLayout(
                   sidebarPanel(width = 3,
                     fileInput("file", label = "Please select the file",
                               multiple = FALSE, 
                               accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
                       
                     ),
                     tags$hr(style = "border-color: lightgray;"),
                     strong("File Options", style = "font-size: 16px"),
                     br(),
                     checkboxInput("header", label = "First row as column headings", value = TRUE),
                     br(),
                     radioButtons("sep", label = "File Delimiter/Separator",
                                  choices = c(Comma = ",", Semicolon = ";", Tab = "\t", Space = " "), selected = ","),
                     br(),
                     radioButtons("display", label = "Display the full dataset or the first 10 rows",
                                  choices = c(All = "all", Head = "head"), selected = "all"),
                     tags$hr(style="border-color: lightgray;"),
                     strong("Download the example dataset", style = "font-size: 16px"),
                     br(),
                     br(),
                     downloadButton("dlexample", label = "Download the example")
                   ),
                   mainPanel(
                     tabsetPanel(
                       tabPanel("Data Format Requirement",
                         br(),
                         h4("Please upload your file first."),
                         br(),
                         p("(1) The file should be in the formats that use delimiter-separated values (DSV), meaning that to store 
                           two-dimensional arrays of data by separating the values in each row with specific delimiter characters. 
                           The supported delimiters are: comma (,), semicolon (;), tab (\t), and space ( ). Please make sure to 
                           select the corresponsing file delimiter in the left panel. ", 
                           strong("We recommend to upload a comma-separated values (CSV) file!", style = "color:steelblue"), 
                           style = "font-size:17px"),
                         br(),
                         p("(2) The dataset should have 5 columns.", style = "font-size:17px"),
                         p(strong("Column 1"), "should be named as", strong(" 'study_name' ", style = "color:steelblue"), 
                           ", referring to the study ID, which can be can be numerics or characters. Each study contains fourfold (2x2) table information.", style = "font-size:17px"),
                         p(strong("Column 2"), "should be named as", strong(" 'TP' ", style = "color:steelblue"), 
                           ", the number of true positive patients (disease + positive result).", style = "font-size:17px"),
                         p(strong("Column 3"), "should be named as", strong(" 'FN' ", style = "color:steelblue"), 
                           ", the number of false negative patients (disease + negative result).", style = "font-size:17px"),
                         p(strong("Column 4"), "should be named as", strong(" 'FP' ", style = "color:steelblue"), 
                           ", the number of false positive patients (no disease + positive result).", style = "font-size:17px"),
                         p(strong("Column 5"), "should be named as", strong(" 'TN' ", style = "color:steelblue"), 
                           ", the number of true negative patients (no disease + negative result).", style = "font-size:17px"),
                         br(),
                         p("(3) If you upload the dataset successfully, you can visualize your data by clicking the tab",
                           strong(" 'Data Confirmation' ", style = "color:steelblue"), style = "font-size:17px"),
                         br(),
                         p("(4) The example dataset looks like this in below. You can also download it. ", style = "font-size:17px"),
                         div(tableOutput("showexample"), style = "margin-left: 150px"),
                         br(),
                         br()
                       ),
                       tabPanel("Data Confirmation",
                         br(),
                         tableOutput("dataconfirm")
                       )
                     )
                   )
                 )       
               ),
               tabPanel("Results",
                 sidebarLayout(
                   sidebarPanel(width = 3,
                     strong("Input the parameter", style = "font-size: 18px"),
                     br(),
                     br(),
                     radioButtons("parametername", label = "Choose the class of input parameter",
                                  choices = c("Sensitivity", "Specificity"), selected = "Sensitivity"),
                     textInput("parameter", label = "Input the parameter value", placeholder = "Enter your parameter value"),
                     tags$hr(style="border-color: lightgray;"),
                     strong("Note: ", style = "color:gray"),
                     p("*The ", span("sensitivity or specificity parameter", style = "color:steelblue;font-style:italic"), 
                       " can be selected based on the clinical experience or previous studies. For example, it can be the 
                       average value of the studies included in the meta-analysis.", style = "color:gray"),
                     withMathJax(),
                     p("**The HSROC model takes the form [1]: ", 
                     span("$$logit(sensitivity_i) = (\\theta_i + 0.5\\alpha_i)exp(-0.5\\beta)$$
                     $$logit(specificity_i) = 1 - (\\theta_i - 0.5\\alpha_i)exp(0.5\\beta)$$", style = "font-size:13px"),
                      "\\(i\\) refers to the \\(i\\)th study. If we set sensitivity or specificity to 0.5, the left side of the above equation becomes 
                       0. Since exp is constantly positive, \\(\\beta\\) can be any value, making it a inderminate equation. 
                       Therefore, if you input a sensitivity or specificity value as 0.5, you may get an error message.", style = "color:gray"),
                     p("***We provide the details about the MCMC methods. You should check if convergence is reached before 
                       interpreting the result. There are two ways: (1) to check if the Rhat of other_snsp[max] is 1.1 or less; 
                       (2) to check Markov Chain Trace Plot for other_snsp[max].", style = "color:gray"),
                     p("****The model used in this calculator was built in RStan. The code can be found at:", 
                       span("https://github.com/y-luo06/HSROC_shiny.", style = "color:lightseagreen;font-style:italic"), 
                       style = "color:gray"),
                     tags$hr(style="border-color: lightgray;"),
                     p("[1] Macaskill P, Gatsonis C, Deeks J, Harbord R, Takwoingi Y. Cochrane handbook for systematic reviews 
                       of diagnostic test accuracy. The Cochrane Collaboration 2010.", style = "font-size: 14px; color: grey")
                   ),
                   mainPanel(
                     tabsetPanel(
                       tabPanel("Results", style = "margin-left:20px;",
                         fluidRow(
                           h4("Estimation"),
                           br(),
                           uiOutput("estimation"),
                           br(),
                           br(),
                           tags$hr(style="border-color: lightgray;"), 
                          ),
                          fluidRow(
                           h4("Probability Density Plot"),
                           br(),
                           #Add a loading message
                           tags$head(tags$style(HTML(lmcss))),
                           conditionalPanel(condition = "$('html').hasClass('shiny-busy')", 
                                            tags$div(id = "plot-container1",tags$img(src = "spinner.gif", id = "loading-spinner1")),
                                            tags$div("Please wait a moment for the analysis to finish.",id="loadmessage1")),
                           div(plotOutput("densityplot", width = "600px", height = "500px"), align = "center"),
                           br(),
                           p("You can download this probability density plot here.", style = "font-size:17px"),
                           downloadButton("dldensityplot", label = "Download the plot"),
                           br(),
                           br()
                          )
                       ),
                       tabPanel("Details about MCMC Method", style = "margin-left:20px;",
                         fluidRow(
                           h4("Markov Chain Trace Plot"),
                           br(),
                           #Add a loading message
                           tags$head(tags$style(HTML(lmcss))),
                           conditionalPanel(condition = "$('html').hasClass('shiny-busy')", 
                                            tags$div(id = "plot-container1",tags$img(src = "spinner.gif", id = "loading-spinner1")),
                                            tags$div("Please wait a moment for the analysis to finish.",id="loadmessage1")),
                           div(plotOutput("traceplot", width = "80%", height = "500px"), align = "center"),
                           br(),
                           p("You can download this trace plot here.", style = "font-size:17px"),
                           downloadButton("dltraceplot", label = "Download the plot"),
                           br(),
                           hr()
                         ),
                         fluidRow(
                           h4("MCMC Method Details"),
                           verbatimTextOutput("mcmcfit"),
                           br(),
                           p("You can download it in the form of csv here.", style = "font-size:17px"),
                           downloadButton("dlfit", label = "Download the file"),
                           br()
                         )
                       )
                     )
                   )
                 )
               )
    )
  ),
  fluidRow(column(2,div(style = "height:1000px;background-color:rgba(0,0,0,0);"))),
  fluidRow(
    fillRow(div(style = "height:110px;background-color:rgba(229,232,235,1);")),
    br(),
    column(1),
    column(1,
           br(),
           img(src = "ccby.png", height = 30, width = 80, align = "center")),
    column(10, 
           strong("Please cite:", style = "color:rgba(21,45,70,1)"),
           p("Banno M, Tsujimoto Y, Luo Y, Miyakoshi C, Kataoka Y. Estimating the certainty of evidence in Grading of 
             Recommendations Assessment, Development and Evaluation for test accuracy.", em(" (In submission)"), style = "font-size: 14px; color:rgba(21,45,70,1)")
    )
  )
)

server <- function(input, output, session) {
  
  input_file <- reactive({
    read.csv(input$file$datapath,
             header = input$header,
             sep = input$sep)
  }）

  file_reshape <- reactive({
    df.tmp <- read.csv(input$file$datapath,
                       header = input$header,
                       sep = input$sep)
    df.tmp$study <- seq.int(1, length(unique(df.tmp$study_name)))
    names(df.tmp)[names(df.tmp) == "TP"] <- "np.1"
    names(df.tmp)[names(df.tmp) == "FN"] <- "nn.1"
    names(df.tmp)[names(df.tmp) == "FP"] <- "np.0"
    names(df.tmp)[names(df.tmp) == "TN"] <- "nn.0"
    reshape(data = df.tmp, idvar = "study_name", direction = "long", varying = c(2:5), timevar = "status", sep = "." )
  })
  
  output$dlexample <- downloadHandler(
    filename = "example_dataset.csv",
    content = function(file){
      file.copy("sampledata_wide.csv", file)
    }
  )
  
  output$showexample <- renderTable(bordered = TRUE, striped = TRUE, {
    return(head(df, 6))
  })
  
  output$dataconfirm <- renderTable(bordered = TRUE, striped = TRUE, {
    req(input$file)
    if (input$display == "head") {
      return(head(input_file(), 10))
    } else {
      return(input_file())
    }
  })
  
  fit <- reactive({
    parameter <- as.numeric(input$parameter)
    req(input$file, input$parameter, parameter > 0 & parameter <1 & parameter != 0.5)
    datalist <- list(
      N = nrow(file_reshape()),
      I = length(unique(file_reshape()$study)),
      study = file_reshape()$study,
      status = file_reshape()$status,
      np = file_reshape()$np,
      nn = file_reshape()$nn,
      one_snsp = parameter
    )
    
    model <- stan_model("HSROC.stan")
    set.seed(1234)
    fit <- sampling(model, 
                    data = datalist,
                    iter = 1000,
                    warmup = 500,
                    chains = 4,
                    thin = 1,
                    cores = 4)
  })
  
  other_snsp <- reactive({ 
    other_snsp <- rstan::extract(fit())$other_snsp
    list = 
    colnames(other_snsp) <- c(as.character(input_file()$study_name), "all")
    other_snsp <- as.data.frame(other_snsp) %>%
      tidyr::gather(key = "study_name",value = "prob")
  })
  
  other_snsp_global <- reactive({
    filter(other_snsp(), study_name == "all")
  })
  
  densityplot <- reactive({
    ggplot() + 
      geom_density(data = other_snsp_global(), aes(x=prob), fill = "red", alpha = 0.2) +
      geom_density(data = other_snsp(), aes(x=prob, color = study_name))
  })
  
  traceplot <- reactive({
    stan_trace(fit(), pars = "other_snsp")
  })
  
  output$estimation <- renderUI({
    parameter <- as.numeric(input$parameter)
      validate(
        need(input$file, "Please make sure to upload a dataset with required format."),
        need(parameter >= 0 & parameter <= 1, "Please make sure to input a correct parameter value between 0 to 1.")
    )
     if (parameter == 0.5) {
        withMathJax(
          helpText("Please input a parameter value other than 0.5.",
                   "This is because HSROC model takes the form [1]: 
                    $$logit(sensitivity_i) = (\\theta_i + 0.5\\alpha_i)exp(-0.5\\beta)$$
                    $$logit(specificity_i) = 1 - (\\theta_i - 0.5\\alpha_i)exp(0.5\\beta)$$
                    \\(i\\) refers to the \\(i\\)th study. If we set sensitivity or specificity to 0.5, the left side of the above equation becomes 
                    0. Since exp is constantly positive, \\(\\beta\\) can be any value, making it a inderminate equation. 
                    Therefore, please input a value other than 0.5."))
     } else {
     if (input$parametername == "Sensitivity") {
       paste("The estimated specificity is ", round(mean(other_snsp_global()$prob), digits = 3),
             ", with a 95% credible interval from ",
             format(round(hdi(other_snsp_global()$prob)[[1]], digits = 3), nsmall = 3), " to ", 
             format(round(hdi(other_snsp_global()$prob)[[2]], digits = 3), nsmall = 3), ".")
      } else {
        paste("The estimated sensitivity is ", round(mean(other_snsp_global()$prob), digits = 3), 
              ", with a 95% credible interval from ",
              format(round(hdi(other_snsp_global()$prob)[[1]], digits = 3), nsmall = 3), " to ", 
              format(round(hdi(other_snsp_global()$prob)[[2]], digits = 3), nsmall = 3), ".")
      }
    }
  })
  
  output$densityplot <- renderPlot({
    req(input$file, input$parameter)
    densityplot()
  })
  
  output$dldensityplot <- downloadHandler(
    filename = "probability_density_plot.png",
    content = function(file){
      ggsave(file, plot = densityplot(), width = 6, height = 5, device = "png")
    }
  )
  
  output$traceplot <- renderPlot({
    req(input$file, input$parameter)
    traceplot()
  })
  
  output$dltraceplot <- downloadHandler(
    filename = "trace_plot.png",
    content = function(file){
      ggsave(file, plot = traceplot(), width = 8, height = 5, device = "png")
    }
  )
  
  output$mcmcfit <- renderPrint({
    print(fit())
  })
  
  output$dlfit <- downloadHandler(
    filename = "fit_summary.csv",
    content = function(file){
      write.table(data.frame(summary(fit())$summary), file, sep=',', row.names = TRUE, col.names = NA)
    }
  )
  
}

shinyApp(ui = ui, server = server)



