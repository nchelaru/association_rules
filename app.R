## Import libraries
library(shiny)
library(gentelellaShiny)
library(shinyWidgets)
library(icon)
library(data.table)
library(DT)
library(ggplot2)
library(arulesViz)


what <- HTML('<font size=3>Association rule mining is an approach to discovering patterns of co-occurrence 
              in a (large) dataset, by identifying entities that frequently appear together in a group. 
              As an <i>unsupervised</i> learning technique, association rule mining can be used to identify novel
              patterns/relationships amongst entities in a large set of data.
              <br><br>
              This type of patterns are summarized by <b>association rules</b>, which predicts the occurrence of 
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
             </font>')


when <- HTML('<font size=3>Some <a href="https://www.slideshare.net/rdatamining/rdatamining-slidesassociationruleminingwithr"><link>applications</link></a> of association rule mining:
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
              </font>')

data_prep <- HTML("<div style='margin: 10px 15px 10px 15px;; font-size:16px;'>
                    In this microlearning series, we will perform association rule mining on the 
                  <a href='https://developer.ibm.com/patterns/predict-customer-churn-using-watson-studio-and-jupyter-notebooks/'>IBM Telco customer churn dataset</a>, 
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

rule_mining <- HTML("<div style='font-size: 17px; margin: 20px;'><p>
                      The Apriori algorithm is most commonly used for association rule mining, which will also be used in this example. 
                      We can set various parameters to limit the number of rules created from the dataset, 
                      usually how often the rule is observed (<b>support</b>), how often it is true (<b>confidence</b>) 
                      and the minimum/maximum length of the rule. As an example, we have generated association rules that 1) appear in at least 0.1% of customers, 
                      2) holds true 90% of the time, and 3) contain 3-5 “items”. 
                      
                      <br><br>
                      
                      Nevertheless, the number of rules obtained at this step will almost certainly be too numerous to provide insights. 
                      We can further filter the rules by several criteria:
                      
                      <ul style='list-style-type:circle;'>
                        <li><b>Lift</b></li>
                          <ul style='list-style-type:disc;'>
                            <li> A measure of how much more or less likely the items in a given rule appear together as compared to by chance. </li>
                            <li> Therefore, it is a metric of the <i>importance</i> of a rule. </li>
                          </ul>
                        <li><b>Redundance and statistical significance</b></li>
                           <ul style='list-style-type:disc;'>
                            <li> A rule is considered to be redundant if a more general rules with the same or a higher confidence exists</li>
                            <li> Fisher's exact test with multiple comparisons correct is used to identify rules where the association between 
                            items on the left-hand (LHS) and right-hand side (RHS) is statistically significant</li> 
                           </ul>
                        <li><b>Items of interest</b></li>
                           <ul style='list-style-type:disc;'>
                            <li>As in this example we are most interested in characteristics that are associated with customer churn, 
                            we can extract <b>only</b> the rules that contain <code>Churn</code> in the RHS.</li>
                          </ul>
                        </ul>
                        
                      <br>
                      
                      The filtered rules can be viewed in a data table or plotted, as seen on the right. The <code>arulesViz</code> package offers 
                      <i>many</i> different options for visualizing a given rule set (as you can see in the interactive app in the next tab), 
                      but the circular grouped plot shown under the 'Plot' tab is one of the best for displaying a handful (~15) rules of particular interest.
                    </p></div>")

mining_code <- HTML('<script src="https://gist.github.com/nchelaru/25cdeb4d7f7b457ca3bf735dd595836e.js"></script>')

interpretation <- HTML('After all this is done, we can then convert the association rules to a dataframe 
                      for easy inspection and take a look. “LHS” and “RHS” refer to items on the left-
                      and right-hand side of each rule, respectively. “Count” gives how many instances, 
                      whether it be transactions or customers, in which the rule appears. In our case, the
                      “count” of each rule divided by the total number of customers in the dataset equals its support.')


about_msg <- HTML('<div style="font-size:15px; margin:15px;">
                  This microlearning series is one of many that I have created for <b>Intelligence Refinery</b>, 
                  a knowledge repository for all things data science and software development that I co-curate with Mihai Chelaru. 
                  
                  <br><br>
                  
                  While it is so important to keep 
                  learning in order to keep pace with this fast moving field, we know that it is not easy to do with a busy
                  schedule and many competing priorities. These microlearning series are designed to deliver bite-sized 
                  concepts and code that build into full workflows that then can be applied to real problems. 
                  
                  <br><br>
                  
                  If you are interested in what else we have cooked up or want to drop us a line, 
                  the links to Intelligence Refinery and my personal portfolio, The Perennial Beginner, are right below.
                  </div>
                  ')

shinyApp(
  ui = gentelellaPageCustom(
    title = "Association rule mining",
    navbar = gentelellaNavbar(
      navbarItems = notif(
        id = "msg",
        icon = icon("envelope-o"),
        status = "primary",
        expanded = FALSE,
        notifItem(
            title = "John Doe",
            date = "3 min ago",
            "Film festivals used to be do-or-die moments
            for movie makers. They were where...")
    )),
    sidebar = gentelellaSidebar(
      site_title = HTML(paste('<img src="https://i.ibb.co/LzjJ6LL/logo-blue.png" alt="IntelRefinery" height=42>', "<font size=3>Intelligence Refinery</font>")),
      sidebarMenu(
        sidebarItem(
          HTML("<font size=2>Getting started</font>"),
          tabName = "intro", 
          icon = tags$i(class = "far fa-flag"), 
          badgeName = "new",
          badgeStatus = "danger"
        ),
        sidebarItem(
          HTML("<font size=2>Data prep</font>"),
          tabName = "data_proc", 
          icon = tags$i(class = "fas fa-chart-bar"), 
          badgeName = "new",
          badgeStatus = "danger"
        ),
        sidebarItem(
          HTML("<font size=2>Rule mining</font>"),
          tabName = "assn_rules", 
          icon = tags$i(class = "fas fa-list-ul")
        ),
        sidebarItem(
          HTML("<font size=2>Rule explorer</font>"),
          tabName = "app", 
          icon = tags$i(class = "fas fa-project-diagram")
        ),
        sidebarItem(
          HTML("<font size=2>About this app</font>"),
          tabName = "about", 
          icon = tags$i(class = "fas fa-question")
        )
      )
    ),
    body = gentelellaBody(
      tabItems(
        tabItem(
          tabName = 'intro',
          jumbotron(
            title = NULL,
            HTML("Let's start with a quick introduction to what association rule mining is and what it can be used for. 
                 When you are ready, <i>double</i> click on the next tab in the sidebar to get started! <i class='fas fa-smile'></i>")
            ),
          box(what, width = 5, height = 650, title = HTML("<font size=5>What is it?</font>"),
              subtitle = NULL, collapsible = FALSE, closable = FALSE,
              dropdownMenu = NULL),
          box(when, width = 4, height = 650, title = HTML("<font size=5>When to use it?</font>"),
              subtitle = NULL, collapsible = FALSE, closable = FALSE,
              dropdownMenu = NULL),
          box(
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
              width = 3, height = 650, title = HTML("<font size=5>Where to learn about it?</font>"), 
              collapsible = FALSE, closable = FALSE, dropdownMenu = NULL)
        ),
        
        tabItem(tabName = 'data_proc',
                fluidRow(
                  column(
                    width = 7,
                    graph_box(
                      plotOutput("item_freq", height='82vh'),
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
                      box(data_prep, width = 12, height=785, collapsible = FALSE, 
                          title = 'Prepare and explore data for association rule mining')
                    ),
                    tabPanel(
                      tabName = "Workflow",
                      active = FALSE,
                      box(prep_code, width = 12, height=785, collapsible = FALSE, 
                          title = 'General workflow')
                    )
                  )
                  
                ))), 
        
        tabItem(tabName = "assn_rules",
                fluidRow(
                  column(width = 6,
                         tabSetPanel(
                           id = "tabset2",
                           tabPanel(
                             tabName = "Summary",
                             active = TRUE,
                             box(rule_mining, width = 12, height=810, collapsible = FALSE, 
                                 title = 'Creating and refining association rules')
                             ),
                           tabPanel(
                             tabName = 'Code',
                             active = FALSE,
                              box(width=12, height=810, collapsible=FALSE, mining_code, 
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
                             box(width=12, height=810, collapsible=FALSE, 
                                 title='The top 14 non-redundant and statistically significant churn rules with the highest support',
                                 HTML(paste('<p style="font-size: 16px; margin:10px;"><b>Each circle represents an association rule that connects LHS and RHS items via arrows.</b></p>',
                                            '<center><img src="circ_plot.png" height="135%"></center>')))
                             
                           ),
                           tabPanel(
                             tabName = "Table",
                             active = TRUE,
                             box(width=12, height=810,  collapsible=FALSE, 
                                 title='Customer characteristics and purchases associated with churn',
                                 DT::dataTableOutput("churn_rules", height = '76vh'))
                           )
                           
                           )
                         )
                )),
        
        
        
        tabItem(
          tabName = "app",
          fluidRow(
            column(
              width = 12,
              source('./arules_app.R')
            )
          )
        ),
        
        
        tabItem(
          tabName = "about",
          fluidRow(
            contactBox(about_msg, head_title = "Hi, I'm Nancy Chelaru!", main_title = NULL, 
                       img = "https://octodex.github.com/images/andycat.jpg",
                       footer_left = HTML('<a href="https://www.intelligencerefinery.io"><img src="https://i.ibb.co/889HCvZ/full-logo.png" alt="IntelRefinery" height=60 border="0"></a>'), 
                       footer_right = HTML('<a href="https://nancychelaru.rbind.io"><img src="https://i.ibb.co/ysKpwRM/perennial-logo.png"  alt="Portfolio" height=60 border="0"></a>'),
                       width=4),
            column(
              width=3, 
              box(
                width = 12,
                title = "To-dos",
                quickList(collapsible=FALSE,
                  quickListItem(icon = icon("calendar-o"), name = "Settings"),
                  quickListItem(icon = icon("bars"), name = "Subscription")
                )
              )
                   ),
            column(
              width = 5,
              timeline(
                timelineItem("Change", title = "Introduction", url = "https://nancy-chelaru-centea.shinyapps.io/podcast_db/", date ="November 6, 2018",
                             author = "N. Chelaru", tag = NULL),
                timelineItem("hellof", title = NULL, url = NULL, date = "November 29, 2019",
                             author = NULL, tag = NULL)
              )
            )
          )
        )
      )
    )
  ),
  
  
  server = function(input, output, session) {
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