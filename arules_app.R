library(shiny)
library(shinythemes)
library(data.table)
library(arules)
library(arulesViz)
library(plotly)
library(plyr)
library(dplyr)

app_instructions <- HTML(paste('<center><a href="https://iconscout.com/illustration/computer-vision-1533033"><img src="https://i.ibb.co/t3S8xtV/app.jpg" alt="app" border="0" height=250></a></center>',
                        '<div style="font-size:22px; margin:20px;">
                         Congratulations on getting here! By now, you should have a working understanding of the steps involved in 
                         performing association rule mining to gain insights into co-occurrence patterns in a large dataset.
                         
                         <br><br>
                         
                         However, even after all the filtering, you are likely to end up with more association rules than one can 
                         quite make sense of at a glance. No fear, this Shiny app for interactive visualizations of the rules
                         has been created by Michael Hahsler, the author of the <code>arules</code> package. Where needed,
                         this can even serve as a light-weight recommendation engine to be used in real time to suggest new 
                         products or course of actions regarding a customer.
                         
                         <br><br>
                         
                         From the Telco customer churn dataset, we have obtained 1,704 association rules that fulfill our 
                         various criteria of robustness, uniqueness, and statistical significance. Using the controls in the 
                         side bar, you can specify the subset of rules of interest and progress through the next few tabs to
                         visualize them. Two types of visualizations, out of the myriad available through the 
                         <code>arulesViz</code> package, are chosen here as they provide the easiest overview of 
                         the relationships between various customer characteristics and churn, our outcome of interest. Finally, 
                         the rules of interest can be exported in a CSV file for further analysis.
                         
                         <br>
                         <hr>
                         
                         <i>Note: The app shown here is adapted from the original, so please see 
                         the <a href="https://github.com/mhahsler/arules"><link>Github repo</link></a> for the <code>arules</code>
                         package for the full source code.</i>
                         
                         </div>'))

## Convert dataframe to transaction format
tData <- read.transactions('./final_df.csv', 
                           format = "basket", sep = ",", 
                           header=TRUE)

## Create rules
quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
} 

assn_rules <- quiet(apriori(tData, 
                            parameter = list(supp = 0.001, 
                                             conf=0.9,      
                                             minlen=3,      
                                             maxlen=5)))   

## Filter rules by lift
filtered_rules <- subset(assn_rules, subset = lift > 1.5)

## Filter out redundant rules
nonr_rules <- filtered_rules[!is.redundant(filtered_rules)]   

## Filter out statistically insignificant rules
sig_rules <- nonr_rules[!is.significant(nonr_rules, 
                                        tData, 
                                        method = "fisher", 
                                        adjust = 'bonferroni')]

x <- sig_rules

parameter = NULL

### rounding helpers
roundUp <- function(x, digits = 3) round(x+.5*10^-digits, digits)
roundDown <- function(x, digits = 3) round(x-.5*10^-digits, digits)

### dataset can be rules or transactions
dataset <- x
aparameter <- as(parameter,'APparameter')
supp <- aparameter@support
conf <- aparameter@confidence

### make sure we have transactions or rules
if(is(dataset, "data.frame")) {
    dataset <- discretizeDF(dataset)
    dataset <- as(dataset, "transactions")
}

### default measures to use
xIndexCached <- "support"
yIndexCached <- "confidence"
zIndexCached <- "lift"

### js cannot handle very large arrays
itemLabels <- itemLabels(dataset)
if(length(itemLabels) > 10000)
    itemLabels <- list('Disabled because of excessive number of items (>10,000)'= c(""))

if(is(dataset, "rules")) {
    if(length(dataset) < 1) stop("Zero rules provided!")
    
    minSupp <- roundDown(min(quality(dataset)$support), 3)
    maxSupp <- roundUp(max(quality(dataset)$support), 3)
    minConf <- roundDown(min(quality(dataset)$confidence), 3)
    maxConf <- roundUp(max(quality(dataset)$confidence), 3)
    minLift <- floor(min(quality(dataset)$lift))
    maxLift <- ceiling(max(quality(dataset)$lift))
    
    supp <- minSupp
    conf <- minConf
    lift <- minLift
} else {
    ### transactions
    minSupp <- 0
    maxSupp <- 1
    minConf <- 0
    maxConf <- 1
    minLift <- 0
    maxLift <- 25
    lift <- 0
    
}

## create Shiny UI and server code
shiny::shinyApp(ui = shiny::shinyUI(
    
    shiny::fluidPage(
        theme = shinythemes::shinytheme("journal"),
    
        shiny::titlePanel(" "),
        
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                
                shiny::htmlOutput('numRulesOutput'),
                shiny::br(),
                
                shiny::sliderInput("supp", "Minimum Support:", min = minSupp, max = maxSupp,
                                   value = supp , step = (maxSupp-minSupp)/100, sep =""),
                shiny::sliderInput("conf", "Minimum Confidence:", min = minConf, max = maxConf,
                                   value = conf , step =  (maxConf-minConf)/1000, sep = ""),
                shiny::sliderInput("lift", "Minimum Lift:", min = minLift, max = maxLift,
                                   value = lift , step =  (maxLift-minLift)/1000, sep = ""),
                shiny::sliderInput("length", "Rule length (from-to):", min = 2, max = 20,
                                   value = c(2,10) , step =  1, sep = ""),
                
                shiny::br(),
                shiny::em(shiny::HTML('Filter rules by items:')),
                shiny::br(),
                shiny::selectInput('colsType',NULL,c('Exclude items:'='rem','Require items:'='req')),
                shiny::uiOutput("choose_columns"),
                shiny::br(),
                shiny::selectInput('colsLHSType',NULL,c('Exclude items from LHS:'='rem','Require items in LHS:'='req')),
                shiny::uiOutput("choose_lhs"),
                shiny::br(),
                shiny::selectInput('colsRHSType',NULL,c('Exclude items from RHS:'='rem','Require items in RHS:'='req')),
                shiny::uiOutput("choose_rhs")
            ),
            
            shiny::mainPanel(
                shiny::tabsetPanel(id='tabs',
                                   
                                   shiny::tabPanel(HTML('<font size=4>Introduction</font>'), value='intro',
                                                   app_instructions
                                   ),
                                   

                                   # shiny::tabPanel('Data Table', value='datatable',
                                   #                 shiny::br(),
                                   #                 DT::dataTableOutput("rulesDataTable")
                                   # ),
                                   
                                   # shiny::tabPanel('Scatter', value='scatter',
                                   #                 shiny::wellPanel(
                                   #                     shiny::fluidRow(
                                   #                         shiny::column(3, shiny::uiOutput("xAxisSelectInput")),
                                   #                         shiny::column(3, shiny::uiOutput("yAxisSelectInput")),
                                   #                         shiny::column(3, shiny::uiOutput("cAxisSelectInput")),
                                   #                         shiny::column(3, shiny::sliderInput("max_scatter", "Top rules shown (keep below 500):",
                                   #                                                             min = 1, max = length(x), value = min(100, length(x)), step = 1, sep = ""))
                                   #                     )),
                                   #                 plotly::plotlyOutput("scatterPlot", width='100%', height='100%')
                                   # ),
                                   
                                   # shiny::tabPanel('Matrix', value='matrix',
                                   #                 shiny::wellPanel(
                                   #                     shiny::fluidRow(
                                   #                         shiny::column(6, shiny::uiOutput("cAxisSelectInput_matrix")),
                                   #                         shiny::column(6, shiny::sliderInput("max_matrix", "Top rules shown (keep below 500):",
                                   #                                                             min = 1, max = length(x), value = min(100, length(x)), step = 1, sep = ""))
                                   #                     )),
                                   #                 
                                   #                 plotly::plotlyOutput("matrixPlot", width='100%', height='100%')
                                   # ),
                                   
                                   shiny::tabPanel(HTML('<font size=4>Grouped plot</font>'), value='grouped',
                                                   shiny::wellPanel(
                                                       shiny::fluidRow(
                                                           shiny::column(6, shiny::uiOutput("kSelectInput")),
                                                           shiny::column(6, shiny::uiOutput("cAxisSelectInput_grouped"))
                                                       )),
                                                   fluidRow(
                                                       column(width=12, align='center', shiny::plotOutput("groupedPlot")))
                                   ),
                                   
                                   shiny::tabPanel(HTML('<font size=4>Network view</font>'), value='graph',
                                                   shiny::wellPanel(
                                                       shiny::fluidRow(
                                                           shiny::column(6, shiny::uiOutput("cAxisSelectInput_graph")),
                                                           shiny::column(6, shiny::sliderInput("max_graph", "Top rules shown (keep below 500):",
                                                                                               min = 1, max = length(x), value = min(100, length(x)), step = 1, sep = ""))
                                                       )),
                                                   
                                                   visNetwork::visNetworkOutput("graphPlot", width='100%', height='800px')
                                   ),
                                   
                                   shiny::tabPanel(HTML('<font size=4>Export selected rules</font>'), value='export',
                                                   shiny::br(),
                                                   shiny::downloadButton('rules.csv', 'Download CSV'))
                )
            )
        ))),
    
    
    server = function(input, output, session) {
        
        output$numRulesOutput <- shiny::renderUI({
            shiny::em(shiny::HTML(paste('Selected rules: ', length(rules()))))
        })
        
        output$kSelectInput <- shiny::renderUI({
            shiny::sliderInput('k', label='Choose # of rule clusters', min=1, max=50, step=1, value=15)
        })
        
        output$xAxisSelectInput <- shiny::renderUI({
            shiny::selectInput("xAxis","X Axis:", colnames(quality(rules())), selected=xIndexCached)
        })
        
        output$yAxisSelectInput <- shiny::renderUI({
            shiny::selectInput("yAxis","Y Axis:", colnames(quality(rules())), selected=yIndexCached)
        })
        
        output$cAxisSelectInput <- shiny::renderUI({
            shiny::selectInput("cAxis","Shading:", colnames(quality(rules())), selected=zIndexCached)
        })
        
        output$cAxisSelectInput_matrix <- shiny::renderUI({
            shiny::selectInput("cAxis_matrix","Shading:", colnames(quality(rules())), selected=zIndexCached)
        })
        
        output$cAxisSelectInput_grouped <- shiny::renderUI({
            shiny::selectInput("cAxis_grouped","Shading:", colnames(quality(rules())), selected=zIndexCached)
        })
        
        output$cAxisSelectInput_graph <- shiny::renderUI({
            shiny::selectInput("cAxis_graph","Shading:", colnames(quality(rules())), selected=zIndexCached)
        })
        
        output$choose_columns <- shiny::renderUI({
            shiny::selectizeInput('cols', NULL, itemLabels, multiple = TRUE)
        })
        
        output$choose_lhs <- shiny::renderUI({
            shiny::selectizeInput('colsLHS', NULL, itemLabels, multiple = TRUE)
        })
        
        output$choose_rhs <- shiny::renderUI({
            shiny::selectizeInput('colsRHS', NULL, itemLabels, multiple = TRUE)
        })
        
        ## caching data
        cachedRules <- NULL
        cachedSupp <- supp
        cachedConf <- conf
        cachedLift <- lift
        cachedMinL <- minLift
        cachedMaxL <- maxLift
        
        if(is(dataset, "rules")) {
            cachedRules <- dataset
            cachedSupp <<- info(dataset)$support
            cachedConf <<- info(dataset)$confidence
            cachedLift <<- min(quality(dataset)$lift)
            cachedMinL <<- min(size(dataset))
            cachedMaxL <<- max(size(dataset))
        }
        
        ### remine rules if necessary dataset is transactions!
        remineRules <- shiny::reactive({
            
            ### use a minimum of 1 absolute support!
            supp <- input$supp
            if(supp == 0) supp <- 1/length(dataset)
            
            rules <- apriori(dataset, parameter = list(
                support = as.numeric(supp),
                confidence = as.numeric(input$conf),
                minlen = input$length[1],
                maxlen = input$length[2]),
                control = list(verbose = TRUE))
            quality(rules) <- interestMeasure(rules, transactions = dataset)
            
            message("Remined ", length(rules), " rules.")
            
            cachedRules <<- rules
            cachedSupp <<- input$supp
            cachedConf <<- input$conf
            cachedLift <<- input$lift
            cachedMinL <<- input$length[1]
            cachedMaxL <<- input$length[2]
        })
        
        ### handle warning for too low support
        override <- shiny::reactiveVal(FALSE)
        
        shiny::observeEvent(input$cancel, {
            shiny::removeModal()
            # reset the slider (Note this does not change input$supp!)
            shiny::updateSliderInput(session, "supp", value = cachedSupp)
        })
        
        shiny::observeEvent(input$continue, {
            shiny::removeModal()
            override(TRUE)
        })
        
        rules <- shiny::reactive({
            ### recalculate rules?
            
            if(is(dataset, 'transactions')) {
                
                # check for low minimum support first
                if(input$supp*length(dataset) > 10 || override()) {
                    
                    if(is.null(cachedRules)) remineRules()
                    if((input$supp < cachedSupp) || input$conf < cachedConf) remineRules()
                    if(input$length[1] < cachedMinL || input$length[1] > cachedMaxL) remineRules()
                    
                } else {
                    shiny::showModal(shiny::modalDialog(
                        title='Warning',
                        'Very low minimum support! Too low values can result in long wait times and memory issues.',
                        footer=shiny::tagList(
                            shiny::actionButton('cancel','cancel'),
                            shiny::actionButton('continue','proceed')
                        )
                    ))
                }
            }
            
            ar <- cachedRules
            
            ### filter rules
            if(input$supp > cachedSupp) {
                ar <- subset(ar, subset = support > input$supp)
            }
            
            if(input$conf > cachedConf) {
                ar <- subset(ar, subset = quality(ar)$confidence > input$conf)
            }
            
            if(input$lift > cachedLift) {
                ar <- subset(ar, subset= lift > input$lift)
            }
            
            if(input$length[1] > cachedMinL) {
                ar <- ar[size(ar) >= input$length[1]]
            }
            
            if(input$length[2] < cachedMaxL) {
                ar <- ar[size(ar) <= input$length[2]]
            }
            
            if(input$colsType == 'rem' && length(input$cols) > 0) {
                ar <- subset(ar, subset=!(items %in% input$cols))
            }
            
            if(input$colsType == 'req' && length(input$cols) > 0) {
                ar <- subset(ar, subset=items %in% input$cols)
            }
            
            if(input$colsLHSType == 'rem' && length(input$colsLHS) > 0) {
                ar <- subset(ar, subset=!(lhs %in% input$colsLHS))
            }
            
            if(input$colsLHSType == 'req' && length(input$colsLHS) > 0) {
                ar <- subset(ar, subset=lhs %in% input$colsLHS)
            }
            
            if(input$colsRHSType == 'rem' && length(input$colsRHS) > 0) {
                ar <- subset(ar, subset=!(rhs %in% input$colsRHS))
            }
            
            if(input$colsRHSType == 'req' && length(input$colsRHS) > 0) {
                ar <- subset(ar, subset=rhs %in% input$colsRHS)
            }
            
            shiny::validate()
            
            ar
        })
        
        # remember settings for other plots
        shiny::observe({ shiny::req(input$xAxis); xIndexCached <<- input$xAxis })
        shiny::observe({ shiny::req(input$yAxis); yIndexCached <<- input$yAxis })
        shiny::observe({ shiny::req(input$cAxis); zIndexCached <<- input$cAxis })
        shiny::observe({ shiny::req(input$cAxis_matrix); zIndexCached <<- input$cAxis_matrix })
        shiny::observe({ shiny::req(input$cAxis_grouped); zIndexCached <<- input$cAxis_grouped })
        shiny::observe({ shiny::req(input$cAxis_graph); zIndexCached <<- input$cAxis_graph })
        
        
        # Present errors nicely to the user
        handleErrors <- shiny::reactive({
            shiny::validate(
                shiny::need(length(rules())>0, 'No rules to visualize! Decrease support, confidence or lift.')
            )
        })
        
        
        ## Grouped Plot #########################
        output$groupedPlot <- shiny::renderPlot({
            shiny::req(input$cAxis_grouped, input$k)
            handleErrors()
            
            plot(rules(), method='graph', cex=1.6, shading = input$cAxis_grouped, control=list(main=NULL, k=input$k, layout=igraph::in_circle()))
        }, height=700, width=900)
        
        
        ## Circular plot ##########################
        output$graphPlot <- visNetwork::renderVisNetwork({
            shiny::req(input$cAxis_graph, input$max_graph)
            handleErrors()
            
            plt <- plot(rules(), method='graph', shading = input$cAxis_graph, engine='htmlwidget',
                        control = list(max=input$max_graph))
            plt$sizingPolicy <- htmlwidgets::sizingPolicy(
                viewer.paneHeight=1000,
                browser.defaultHeight=1000,
                knitr.defaultHeight=1000,
                defaultHeight=1000,defaultWidth=1000,
                browser.fill=TRUE
            )
            plt$height <- 1000
            plt$x$height <- 1000
            plt
        })
        
        
        ## Scatter Plot ##########################
        output$scatterPlot <- plotly::renderPlotly({
            shiny::req(input$xAxis, input$yAxis, input$cAxis, input$max_scatter)
            handleErrors()
            plot(rules(), method = 'scatterplot',
                 measure=c(input$xAxis, input$yAxis), shading = input$cAxis, engine = 'htmlwidget',
                 control = list(max=input$max_scatter))
        })
        
        
        ## Matrix Plot ###################
        output$matrixPlot <- plotly::renderPlotly({
            shiny::req(input$cAxis_matrix, input$max_matrix)
            handleErrors()
            plot(rules(), method="graph", shading = input$cAxis_matrix, engine = 'htmlwidget',
                 control=list(layout=igraph::in_circle()))
        })
        
        ## Data Table ##########################
        output$rulesDataTable <- DT::renderDataTable({
            handleErrors()
            inspectDT(rules())
        }
        )
        
        ## Export ########################
        output$rules.csv <- shiny::downloadHandler(
            filename = 'rules.csv',
            content = function(file) {
                write.csv(as(rules(), "data.frame"), file)
            }
        )
        
        
    }, options = list(height = 1000)
)
