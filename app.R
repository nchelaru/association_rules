## Import libraries
library(shiny)
library(gentelellaShiny)
library(shinyWidgets)
library(icon)
library(data.table)
library(DT)
library(ggplot2)
library(arulesViz)
library(shinyjs)
library(shinyBS)

welcome_msg <- HTML('<center><h3>Welcome to an Intelligence Refinery data science microlearning series!</h3></center>
                   
                    <hr>
                    
                    <p style="margin:10px; font-size: 20px;"><b>Association rule mining</b> is a widely used method for exploring patterns of co-occurrence in large datasets, 
                    with applications in many diverse fields such as business, medicine and engineering. 
                    
                    <br><br>
                    
                    Throughout this series you will assemble, in bite-sized pieces, a starter-kit of concepts and tools that you can use
                    to discover interesting relationships in other datasets.
                    
                    <br><br>
                    
                    You can navigate through sections of this tutorial by <i>double</i> clicking on the tabs in the sidebar. 
                    
                    <br><br>
                    
                    Have fun! <i class="fas fa-smile-beam"></i>
                    
                    <br><br>
                    
                    <center><a href="https://www.intelligencerefinery.io"><img src="full_logo.png" style="height:150px;"></a></center>
                    
                    </p>')

what <- HTML('<div style="font-size:17px; margin:10px;">
              
              <b>Association rule mining</b> is an approach to discovering patterns of co-occurrence 
              in a (large) dataset, by identifying entities that frequently appear together in a group. 
              As an <i>unsupervised</i> learning technique, association rule mining can be used to identify novel
              patterns/relationships amongst entities in a large set of data.
              
              <br><br>
              
              <center><img src="mind_map.png" style="height:200px;"></center>
              
              <br><br>
              
              This type of patterns are summarized by association rules, which predicts the occurrence of 
             one or more entities based on the occurrences of other entities in a certain grouping, 
             such as a transaction or an individual. For example, an association rule found in a grocery retailer database may be:
             
             <br><br>
             
             <center>{toilet paper, apples, cheese} --> {shampoo, flour, lettuce}</center>
             
             <br>
             
             The rule can be interpreted as <b>conditional probability</b>: 
             if a customer bought toilet paper, apples, and cheese, they are more 
             likely to also buy shampoo, flour, and lettuce in the same purchase. 
             
             <br><br>
             
             It is important to note that <b>the association rule does not imply a causal relationship</b>
            between itemsets on the left and right hand side of the rule.
             </div>
             ')


when <- HTML('<div style="font-size:17px; margin:10px;">Some <a href="https://www.slideshare.net/rdatamining/rdatamining-slidesassociationruleminingwithr"><link>applications</link></a> of association rule mining:
              <br><br>
              <ul style="list-style-type:circle;">
                <li><b>Market basket analysis</b>:
                    <ul style="list-style-type:disc;">
                      <li>Which items are frequently purchased together?</li>
                    </ul>
                    </li>
                <br>
                <li><b>Churn analysis</b>: 
                    <ul style="list-style-type:disc;">
                      <li>What are the characteristics and behaviours of customers who are likely/unlikely to switch to other companies?</li>
                    </ul>
                    </li>
                <br>
                <li><b>Selective marketing</b>: 
                    <ul style="list-style-type:disc;">
                    <li>Which customer groups who are likely to purchase a new service or product?</li>
                    </ul>
                    </li>
                <br>
                <li><b>Stock market analysis</b>: 
                    <ul style="list-style-type:disc;">
                    <li>What relationships exist between individual stocks, and between stocks and economic factors?</li>
                    </ul>
                    </li>
                <br>
                <li><b>Medical diagnosis</b>: 
                    <ul style="list-style-type:disc;">
                    <li>What relationships exist between symptoms, test results and illness?</li>
                    </ul>
                    </li>
              </ul> 
              </div>
             
             <br>
             
             <center><img src="applications.png" style="height:200px;"></center>
             
             
             ')

data_prep <- HTML("<div style='margin: 10px 15px 10px 15px;; font-size:20px;'>
                    In this microlearning series, we will perform association rule mining on the 
                  <a href='https://developer.ibm.com/patterns/predict-customer-churn-using-watson-studio-and-jupyter-notebooks/'><link>IBM Telco customer churn dataset</link></a>, 
                  to identify customer characteristics and purchasing behaviours that tend to appear together, 
                  which are typically helpful in informing marketing and customer retention strategies.
                  
                   <br><br>
                 
                  There are two things we need to do before the dataset is ready for analysis:
                  
                  <br><br>
                  
                  <ol>
                    <li>
                    <b>Discretize the two continuous variables whose relationship to customer churn we are interested in: <code>MonthlyCharges</code> and <code>Tenure</code></b>. 
                    <br>
                    To obtain the most informative binning, we will use the supervised 
                   discretization function from the <code>arulesCBA</code> package, which identifies bin 
                    breaks that retain the most predictive power with respect to the target variable that we are interested in, <code>Churn</code>.
                    </li>
                    <br>
                    <li>
                    <b>Convert the dataset into the <i>transaction</i> format, as required by the <code>arules</code> package. </b>
                    <br>
                    In our case, this means that each row corresponds to a single customer, 
                    listing all their personal characteristics and purchasing behaviours as comma-separated items.
                    </li>
                  </ol>
                  
                  <br>

                  Once all of this is done, we can look at the prevalence of each personal characteristic or purchasing behaviour amongst the customers. 
                  For example, most of the customers in this dataset have purchased phone service and use paperless billing.
                  </div>
                  ")

prep_code <- HTML('<div style="margin: 20px;"><script src="https://gist.github.com/nchelaru/7119cad617161ae209cddd64f7c28ac9.js"></script></div>')

rule_mining <- HTML("<div style='font-size: 20px; margin: 15px;'>
                      The Apriori algorithm is most commonly used for association rule mining, which will also be used in this example. 
                      We can set various parameters to limit the number of rules created from the dataset, 
                      usually how often the rule is observed (<b>support</b>), how often it is true (<b>confidence</b>) 
                      and the minimum/maximum length of the rule. As an example, we have generated association rules that 1) appear in at least 0.1% of customers, 
                      2) holds true 90% of the time, and 3) contain 3-5 “items”. 
                      
                      <br><br>
                      
                      Nevertheless, the number of rules obtained at this step will almost certainly be too numerous to provide insights. 
                      We can further filter the rules by several criteria:
                      
                      <ul style='list-style-type:circle; font-size: 20px;'>
                        <li style='font-size: 20px;'><b>Lift</b></li>
                          <ul style='list-style-type:disc;'>
                            <li style='font-size: 20px;'> A measure of how much more or less likely the items in a given rule appear together as compared to by chance. </li>
                            <li style='font-size: 20px;'> Therefore, it is a metric of the <i>importance</i> of a rule. </li>
                          </ul>
                        <li style='font-size: 20px;'><b>Redundance and statistical significance</b></li>
                           <ul style='list-style-type:disc; font-size: 20px;'>
                            <li style='font-size: 20px;'> A rule is considered to be redundant if a more general rules with the same or a higher confidence exists</li>
                            <li style='font-size: 20px;'> Fisher's exact test with multiple comparisons correct is used to identify rules where the association between 
                            items on the left-hand (LHS) and right-hand side (RHS) is statistically significant</li> 
                           </ul>
                        <li style='font-size: 20px;'><b>Items of interest</b></li>
                           <ul style='list-style-type:disc; font-size: 20px;'>
                            <li style='font-size: 20px;'>As in this example we are most interested in characteristics that are associated with customer churn, 
                            we can extract <b>only</b> the rules that contain <code>Churn</code> in the RHS.</li>
                          </ul>
                      </ul>
                        
                      <br>
                      
                      The filtered rules can be viewed in a data table or plotted, as seen on the right. The <code>arulesViz</code> package offers 
                      <i>many</i> different options for visualizing a given rule set (as you can see in the interactive app in the next tab), 
                      but the circular grouped plot shown under the 'Plot' tab is one of the best for displaying a handful (~15) rules of particular interest.
                    </div>")

mining_code <- HTML('<script src="https://gist.github.com/nchelaru/25cdeb4d7f7b457ca3bf735dd595836e.js"></script>')

interpretation <- HTML('After all this is done, we can then convert the association rules to a dataframe 
                      for easy inspection and take a look. “LHS” and “RHS” refer to items on the left-
                      and right-hand side of each rule, respectively. “Count” gives how many instances, 
                      whether it be transactions or customers, in which the rule appears. In our case, the
                      “count” of each rule divided by the total number of customers in the dataset equals its support.')


about_msg <- HTML('<div style="font-size:18px; margin: 0px 15px 15px 15px;"><p><img style="height:150px; float: left; padding: 10px 15px 15px 0px;" src="https://octodex.github.com/images/andycat.jpg" alt="avatar"> 
                  This microlearning series is one of several that I have created for 
                  <a href="https://www.intelligencerefinery.io"><b>Intelligence Refinery</b></a>, 
                  a knowledge repository for all things data science and software development that I co-curate with Mihai Chelaru. 
                  
                  <br><br>
                  
                  While it is so important to keep 
                  learning in order to keep pace with this fast moving field, we know that it is not easy to do with a busy
                  schedule and many competing priorities. These microlearning series are designed to deliver bite-sized 
                  concepts and code that build into full workflows that then can be applied to real problems. 
                  
                  <br><br>
                  
                  On the left, you can find other data science microlearning series and fun hobby projects that I have cooked up. 
                  I would love to hear any comments or suggestions that you may have. 
                  <br><br>
                  
                  Hope that you have enjoyed your time here! <i class="fas fa-smile"></i>
                  </p>
                  
                  <br><hr><br>
                  
                  If you have any comments, questions or suggestions, you can find me at:
                  <br><br>
                  <center><a href="mailto:nancy.chelaru@gmail.com"><i class="fas fa-envelope" style="padding:10px;"></i></a>
                  <a href="https://www.intelligencerefinery.io/contact/"><i class="fas fa-globe" style="padding:10px;"></i></i></a>
                  <a href="https://github.com/nchelaru?tab=repositories"><i class="fab fa-github-alt" style="padding:10px;"></i></a>
                  <a href="https://twitter.com/n_chelaru"><i class="fab fa-twitter" style="padding:10px;"></i></a></center>

                  </div>')

iframe_test <- HTML('<iframe src="https://raindrop.io/collection/8841871" 
                    style="border:0px #ffffff none;" name="myiFrame" scrolling="no" 
                    frameborder="1" marginheight="0px" marginwidth="0px" height="950px" 
                    width="100%" allowfullscreen></iframe>')


shinyApp(
  ui = gentelellaPageCustom(
    title = "Association rule mining",
    sidebar = gentelellaSidebar(
      site_title = HTML(paste('<img src="https://i.ibb.co/LzjJ6LL/logo-blue.png" alt="IntelRefinery" height=42>', "<font size=3>Intelligence Refinery</font>")),
      sidebarMenu(
        br(),
        sidebarItem(
          HTML("<font size=4>Getting started</font>"),
          tabName = "intro", 
          icon = tags$i(class = "far fa-flag"), 
          badgeName = "new",
          badgeStatus = "danger"
        ),
        sidebarItem(
          HTML("<font size=4>Data prep</font>"),
          tabName = "data_proc", 
          icon = tags$i(class = "fas fa-chart-bar"), 
          badgeName = "new",
          badgeStatus = "danger"
        ),
        sidebarItem(
          HTML("<font size=4>Rule mining</font>"),
          tabName = "assn_rules", 
          icon = tags$i(class = "fas fa-list-ul")
        ),
        sidebarItem(
          HTML("<font size=4>Explore</font>"),
          tabName = "app", 
          icon = tags$i(class = "fas fa-project-diagram")
        ),
        sidebarItem(
          HTML("<font size=4>About</font>"),
          tabName = "about", 
          icon = tags$i(class = "fas fa-question")
        )
      )
    ),
    body = gentelellaBody(
      tabItems(
        tabItem(
          tabName = 'intro',
          br(),
          br(),
          bsModal(id = 'startupModal', title = NULL, trigger = '',
                  size = 'large', welcome_msg),
          fluidRow(
            column(width=5,
                   box(what, width = 12, height = 870, title = HTML("<font size=5>What is it?</font>"),
                       subtitle = NULL, collapsible = FALSE, closable = FALSE,
                       dropdownMenu = NULL)
                   ),
            column(width=4,
                   box(when, width = 12, height = 870, title = HTML("<font size=5>When to use it?</font>"),
                       subtitle = NULL, collapsible = FALSE, closable = FALSE,
                       dropdownMenu = NULL)
                   ),
            column(width=3,
                   box(HTML('<center><a href="https://www.intelligencerefinery.io"><img src="resources.png" style="margin: 10px; height:120px;"></a></center>
                            <p style="font-size:17px; margin:10px;">If you want more details, we have found some great introductions to association rule mining:</p><br>'),
                     activityList(
                       activityItem(HTML("<font size=2>Data Mining with R</font>"), 
                                    title = "Association Rule Mining with R", img = "https://i.ibb.co/hx6ctrQ/rmd.png", 
                                    url = "https://www.slideshare.net/rdatamining/rdatamining-slidesassociationruleminingwithr"),
                       activityItem(HTML("<font size=2>AgroParisTech</font>"), 
                                    title = "Frequent itemsets and association rules", img = "https://media.glassdoor.com/sql/1125817/agroparistech-squarelogo-1454497957893.png", 
                                    url = "http://www2.agroparistech.fr/ufr-info/membres/cornuejols/Teaching/Erasmus-IT4BI/Tr-frequent-items-setx4.pdf"),
                       activityItem(HTML("<font size=2>Fordham University</font>"),  
                                    title = "Frequent Patterns and Association Rule Mining", img = "https://upload.wikimedia.org/wikipedia/en/thumb/3/3e/Fordham_University_seal.svg/150px-Fordham_University_seal.svg.png",
                                    url = "https://storm.cis.fordham.edu/~yli/documents/CISC4631Spring17/Chapter5_Asso.pdf"),
                       activityItem(HTML("<font size=2>University of British Columbia</font>"),
                                    title = "Association rules", img = "https://www.mykootenaynow.com/wp-content/uploads/2016/02/UBC-Logo-425x420.jpg",
                                    url = "https://www.cs.ubc.ca/~schmidtm/Courses/340-F15/L11.pdf")
                     ), 
                     width = 12, height = 870, title = HTML("<font size=5>Where to learn about it?</font>"), 
                     collapsible = FALSE, closable = FALSE, dropdownMenu = NULL)
                   )
          )
        ),
        
        tabItem(tabName = 'data_proc',
                br(),
                br(),
                fluidRow(
                  column(
                    width = 7,
                    graph_box(
                      plotOutput("item_freq", height=845),
                      width = 12, 
                      boxtitle = "Frequency of customer characteristics",
                      subtitle = NULL,
                      datepicker = NULL
                    )
                  ),
                column(
                  width = 5,
                  tabSetPanel(
                    id = "tabset1",
                    tabPanel(
                      tabName = "Description",
                      active = TRUE,
                      box(data_prep, width = 12, height=850, collapsible = FALSE, 
                          title = 'Prepare and explore data for association rule mining')
                    ),
                    tabPanel(
                      tabName = "Workflow",
                      active = FALSE,
                      box(prep_code, width = 12, height=850, collapsible = FALSE, 
                          title = 'General workflow', inlineCSS(list("gist" = "font-size: 18px")))
                    )
                  )
                  
                ))), 
        
        tabItem(tabName = "assn_rules",
                br(),
                fluidRow(
                  column(width = 6,
                         tabSetPanel(
                           id = "tabset2",
                           tabPanel(
                             tabName = "Summary",
                             active = TRUE,
                             box(rule_mining, width = 12, height=900, collapsible = FALSE, 
                                 title = 'Creating and refining association rules', inlineCSS(list("li-content" = "font-size: 20px;")))
                             ),
                           tabPanel(
                             tabName = 'Code',
                             active = FALSE,
                              box(width=12, height=900, collapsible=FALSE, mining_code, 
                                  title='General workflow for creating, filtering and visualizing association rules')
                           ))
                           ),
                  column(width = 6,
                         tabSetPanel(
                           right=TRUE,
                           id = "tabset2",
                           tabPanel(
                             tabName = 'Plot',
                             active = FALSE,
                             box(width=12, height=900, collapsible=FALSE, 
                                 title='Grouped plot',
                                 HTML(paste('<p style="font-size: 17px; margin:10px;">
                                            <b>This plot shows the top 14 (in terms of support) non-redundant and statistically significant rules that contain "Churn" in the RHS. 
                                            <br><br>Each circle represents an association rule that connects LHS and RHS items via arrows, the colour of which is proportional to its lift.</b></p>',
                                            '<center><img src="circ_plot.png" height="135%"></center>')))
                             
                           ),
                           tabPanel(
                             tabName = "Table",
                             active = TRUE,
                             box(width=12, height=900,  collapsible=FALSE, 
                                 title='Customer characteristics and purchases associated with churn',
                                 DT::dataTableOutput("churn_rules", height = '76vh')),
                             inlineCSS(list("table" = "font-size: 17px", "h2" = "font-size: 26px", "li"= "font-size: 16px"))
                           )
                           
                           )
                         )
                )),
        
        
        
        tabItem(
          tabName = "app",
          br(), 
          fluidRow(
            column(
              width = 12,
              source('./arules_app.R')
            )
          )
        ),
        
        
        tabItem(
          tabName = "about",
          br(), 
          fluidRow(
                  column(
                    width=8,
                    iframe_test),
                  column(
                    width=4, 
                    box(about_msg, width=12, height=950, collapsible=FALSE,
                              title="Hi, I'm Nancy Chelaru!")
                    
                  )
                   
                   )
                   )
          
      )
      )
    ),
  
  
  server = function(input, output, session) {
    ## Pop up
    toggleModal(session, "startupModal", toggle = "open")
    
    ## Item frequency plot
    tData <- read.transactions('./final_df.csv', 
                               format = "basket", sep = ",", 
                               header=TRUE)

    x <- data.frame(sort(table(unlist(LIST(tData))), decreasing=FALSE))
    
    p <- ggplot(data=x, aes(x=factor(Var1), y=Freq)) +
            geom_col(aes(fill = Freq)) + 
            labs(title = NULL, x = " ", y = "Frequency") + 
            coord_flip() + 
            theme_classic() +
            theme(axis.text.y = element_text(size=16),
                  axis.text.x = element_text(size=10), 
                  legend.position = "none") +
            scale_fill_gradient2(low = "green", 
                                 high = "red", 
                                 midpoint = median(x$Freq)) 
    
    output$item_freq <- renderPlot(p)
    
    ## Churn rules table
    churn_rules_df <- read.csv('./churn_rules.csv') %>% select(-count)  %>% mutate_if(is.numeric, round, 3) 

    output$churn_rules <- DT::renderDataTable(churn_rules_df,
                                             options = list(pageLength = 15, 
                                                            scrollX = TRUE,
                                                            scrollY = TRUE)) 


    
 
  },
  options = list(height = 880)
)