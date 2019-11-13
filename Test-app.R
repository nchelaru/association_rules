library(shiny)
library(gentelellaShiny)
shinyApp(
  ui = gentelellaPageCustom(
    gentelellaBody(
      box(
        width = 4,
        title = "Quick Lists",
        quickList(
          quickListItem(icon = icon("calendar-o"), name = "Settings"),
          quickListItem(icon = icon("bars"), name = "Subscription")
        )
      )
    )
  ),
  server = function(input, output, session) {}
)