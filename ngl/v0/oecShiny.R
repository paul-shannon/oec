library(shiny)
library(NGLVieweR)

ui = fluidPage(

  tags$head(
    tags$style("#structure{height:90vh !important;}"),
    tags$link(rel="icon", href="data:;base64,iVBORw0KGgo=")
    ),


  titlePanel("Viewer with API inputs"),
  sidebarLayout(
    sidebarPanel(
      textInput("selection", "Selection", "1-20"),
      selectInput("type", "Type", c("ball+stick", "cartoon", "backbone")),
      selectInput("color", "Color", c("orange", "grey", "white")),
      actionButton("add", "Add"),
      actionButton("remove", "Remove")
    ),
    mainPanel(NGLVieweROutput("structure"))
  )
)
server = function(input, output) {
  output$structure <- renderNGLVieweR({
    ngl <- NGLVieweR("1crn")
    addRepresentation(ngl, "cartoon", param = list(name = "cartoon", colorScheme="residueindex")) %>%
    stageParameters(ngl, backgroundColor = "beige") %>%
    setQuality(ngl, "high") %>%
    setFocus(ngl,0) %>%
    setSpin(ngl, FALSE)
  })
  observeEvent(input$add, {
    NGLVieweR_proxy("structure") %>%
      addSelection(isolate(input$type),
                   param =
                     list(name="sel1",
                          sele=isolate(input$selection),
                          colorValue=isolate(input$color)))
  })

  observeEvent(input$remove, {
    NGLVieweR_proxy("structure") %>%
      removeSelection("sel1")
  })
}
app <- shinyApp(ui, server)
