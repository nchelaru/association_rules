library(shiny)
library(gentelellaShiny)
library(shinyWidgets)

shinyApp(
  ui = gentelellaPageCustom(
    title = "Shiny Gentelella",
    navbar = gentelellaNavbar(
      navbarItems = notif(
        id = "menunotif",
        icon = icon("envelope-o"),
        status = "primary",
        expanded = FALSE,
        lapply(X = 1:5, FUN = function(i) {
          notifItem(
            title = "John Doe",
            date = "3 min ago",
            img = paste0("https://image.flaticon.com/icons/svg/163/16382", i,".svg"),
            "Film festivals used to be do-or-die moments
       for movie makers. They were where..."
          )
        })
      )
    ),
    sidebar = gentelellaSidebar(
      uiOutput("profile"),
      sidebarDate(),
      sidebarMenu(
        sidebarItem(
          "Tab 1",
          tabName = "tab1", 
          icon = tags$i(class = "fas fa-chart-bar"), 
          badgeName = "new",
          badgeStatus = "danger"
        ),
        sidebarItem(
          "Tab 2",
          tabName = "tab2", 
          icon = tags$i(class = "fas fa-info")
        )
      )
    ),
    body = gentelellaBody(
      tabItems(
        tabItem(
          tabName = "tab1",
          fluidRow(
            column(
              width = 4,
              align = "center",
              sliderInput(
                "obs",
                "Number of observations:",
                min = 0,
                max = 1000,
                value = 500
              )
            ),
            column(
              width = 8,
              align = "center",
              plotOutput("distPlot")
            )
          )
        ),
        tabItem(
          tabName = "tab2",
          jumbotron(
            title = "Hello, world!",
            "This is a simple hero unit, a simple jumbotron-style
        component for calling extra attention to featured
        content or information."
          )
        )
      )
    ),
    footer = gentelellaFooter()
  ),
  server = function(input, output, session) {
    output$distPlot <- renderPlot({
      hist(rnorm(input$obs))
    })
    
    counter <- reactiveValues(connect = 0)
    
    observeEvent(counter$connect == 0, {
      inputSweetAlert(
        session = session, 
        inputId = "name",
        title = "What's your name ?"
      )
    })
    
    output$profile <- renderUI({
      sidebarProfile(
        name = input$name,
        img = "https://image.flaticon.com/icons/svg/236/236831.svg"
      )
    })
  }
)