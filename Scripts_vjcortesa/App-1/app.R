library(shiny)
library(bslib)
library(ggplot2)
data(penguins, package = "palmerpenguins")

# Define UI for random distribution app ----
# Sidebar layout with input and output definitions ----
ui <- page_sidebar(
  title = "WhereWeMove dashboard",
  
  sidebar = sidebar(
    bg = "white",
    accordion(
      accordion_panel(
        "Primary controls",
        varSelectInput(
          "color_by", "Color by",
          penguins[c("species", "island", "sex")],
          selected = "species"
        )
      ),
      accordion_panel(
        "Other controls",
        "Other controls go here"
      )
    )
  ),
  
  accordion(
    open = c("Bill Length", "About"),
    accordion_panel(
      "Bill Length",
      plotOutput("bill_length")
    ),
    accordion_panel(
      "Bill Depth",
      plotOutput("bill_depth")
    ),
    accordion_panel(
      "Body Mass",
      plotOutput("body_mass")
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  gg_plot <- reactive({
    ggplot(penguins) +
      geom_density(aes(fill = !!input$color_by), alpha = 0.2) +
      theme_bw(base_size = 16) +
      theme(axis.title = element_blank())
  })
  
  output$bill_length <- renderPlot(gg_plot() + aes(bill_length_mm))
  output$bill_depth <- renderPlot(gg_plot() + aes(bill_depth_mm))
  output$body_mass <- renderPlot(gg_plot() + aes(body_mass_g))
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)