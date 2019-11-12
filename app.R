## Import libraries
library(shiny)
library(gentelellaShiny)
library(shinyWidgets)
library(icon)
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


when <- HTML('<font size=3>Some <a href="https://www.slideshare.net/rdatamining/rdatamining-slidesassociationruleminingwithr">applications</a> of association rule mining:
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

data_prep <- HTML("<div style='margin: 20px; font-size:16px;'>
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

prep_code <- HTML(paste('<p style="font-size: 16px; margin: 20px;">This is the general workflow used for preparing the dataset and 
                        creating a plot of the frequencies of customer characteristics and purchasing behaviours.</p>',
                        '<div style="margin: 20px;"><script src="https://gist.github.com/nchelaru/7119cad617161ae209cddd64f7c28ac9.js"></script></div>'))

rule_mining <- HTML('<p style="font-size: 16px; margin: 20px;">The Apriori algorithm is most commonly used for association rule mining. 
                      We can set various parameters to limit the number of rules created from the dataset, 
                      usually how often the rule is observed (<b>support</b>), how often it is true (<b>confidence</b>) 
                      and the minimum/maximum length of the rule. 
                      <br><br>
                      As an example, we have generated association rules that appear in at least 0.1% of customers, 
                      holds true 90% of the time, and contain 3-5 “items”. 
                      <br><br>
                      The number of rules obtained at this step will almost certainly be too numerous to provide insights. 
                      We can further filter the rules by their <b>lift</b>, which is a measure of how much more or less likely 
                      the items in a given rule appear together as compared to by chance. Therefore, it is a metric of the 
                      importance of a rule. In addition, redundant and statistically insignificant rules should also be 
                      removed to leave only the most informative ones. Finally, as we are most interested in customer churn,
                      we can extract *only* the rules that contain "Churn" in the right-hand side.
                      </p>')

mining_code <- HTML('<script src="https://gist.github.com/nchelaru/25cdeb4d7f7b457ca3bf735dd595836e.js"></script>')

interpretation <- HTML('After all this is done, we can then convert the association rules to a dataframe 
                      for easy inspection and take a look. “LHS” and “RHS” refer to items on the left-
                      and right-hand side of each rule, respectively. “Count” gives how many instances, 
                      whether it be transactions or customers, in which the rule appears. In our case, the
                      “count” of each rule divided by the total number of customers in the dataset equals its support.')


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
                      data_prep
                    ),
                    tabPanel(
                      tabName = "Workflow",
                      active = FALSE,
                      prep_code
                    )
                  )
                  
                ))), 
        
        tabItem(tabName = "assn_rules",
                fluidRow(
                  column(width = 5,
                         tabSetPanel(
                           id = "tabset2",
                           tabPanel(
                             tabName = "Summary",
                             active = TRUE,
                             box(rule_mining, width = 12, collapsible = FALSE)
                             ),
                           tabPanel(
                             tabName = 'Code',
                             active = FALSE,
                              mining_code
                           ))
                           ),
                  column(width = 7,
                         tabSetPanel(
                           id = "tabset2",
                           tabPanel(
                             tabName = "Table",
                             active = TRUE,
                             box(width=12, collapsible=FALSE, DT::dataTableOutput("churn_rules", height = '77vh'))
                           ),
                           tabPanel(
                             tabName = 'Plot',
                             active = FALSE,
                             box(width=12, collapsible=FALSE, 
                                 HTML('<center><img src="circ_plot.png"></center>'))
                           ))
                         )
                )),
        
        
        
        tabItem(
          tabName = "app",
          fluidRow(
            column(
              width = 12,
              "From arules package", 
              source('./arules_app.R')
            ), height='80vh'
          )
        ),
        
        
        tabItem(
          tabName = "about",
          fluidRow(
            column(
              width = 8,
              timeline(
                timelineItem("Change", title = "Introduction", url = "https://nancy-chelaru-centea.shinyapps.io/podcast_db/", date ="November 6, 2018",
                             author = "N. Chelaru", tag = NULL),
                timelineItem("hellof", title = NULL, url = NULL, date = "November 29, 2019",
                             author = NULL, tag = NULL)
              )
            ),
            contactBox("hello", head_title = "test", main_title = "About this app", img = "https://octodex.github.com/images/andycat.jpg",
                       footer_left = HTML(paste('<a href="https://www.intelligencerefinery.io"><img src="https://i.ibb.co/LzjJ6LL/logo-blue.png" alt="IntelRefinery" height=42></a>',
                                                '<a href="https://www.intelligencerefinery.io"><img src="https://i.ibb.co/LzjJ6LL/logo-blue.png" alt="IntelRefinery" height=42></a>')), 
                       footer_right = "left", width=5)
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
    churn_rules <- read.csv('./churn_rules.csv')

    output$churn_rules = DT::renderDataTable(within(churn_rules, rm('count')), options = list(pageLength = 15, scrollX = TRUE))

    
    ## Circular grouped plot
 

    
 
  },
  options = list(height = 880)
)