library(shiny)
library(NGLVieweR)
#----------------------------------------------------------------------------------------------------
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
      actionButton("show", "Show"),
      actionButton("hide", "Hide"),
      actionButton("remove", "Remove")
    ),
    mainPanel(NGLVieweROutput("structure"))
  )
)

#----------------------------------------------------------------------------------------------------
server = function(input, output) {

  output$structure <- renderNGLVieweR({
     nglViewer <- NGLVieweR("1crn")
     addRepresentation(nglViewer, "cartoon", param = list(name = "cartoon", colorScheme="residueindex"))
     stageParameters(nglViewer, backgroundColor = "beige")
     setQuality(nglViewer, "high")
     setFocus(nglViewer, 0)
     setSpin(nglViewer, FALSE)
     # return(nglViewer)
     })

  #output$structure <- renderNGLVieweR({
  #  NGLVieweR("1crn") %>%
  #    addRepresentation("cartoon", param = list(name = "cartoon", colorScheme="residueindex")) %>%
  #    stageParameters(backgroundColor = "beige") %>%
  #    setQuality("high") %>%
  #    setFocus(0) %>%
  #    setSpin(FALSE)
  #    })

  observeEvent(input$add, {
     NGLVieweR_proxy("structure") %>%
        addSelection(isolate(input$type),
                   param =
                     list(name="sel1",
                          sele=isolate(input$selection),
                          colorValue=isolate(input$color)))
      })

  observeEvent(input$hide, {
     NGLVieweR_proxy("structure") %>%
           updateVisibility("sel1", value = FALSE)})


  observeEvent(input$remove, {
    NGLVieweR_proxy("structure") %>%
      removeSelection("sel1")
      })

} # server
#----------------------------------------------------------------------------------------------------
app <- shinyApp(ui, server)
