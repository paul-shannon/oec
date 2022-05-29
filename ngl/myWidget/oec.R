library(nglShiny)
library(R6)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; font-size: 20px; background-color:white"
textOutputStyle <- paste0("margin:20px; margin-left: 50px;",
		          " padding:5px; width: 200px; height: 60px; color:red; ",
		          "border: 1px solid black; font-size: 20px;")
nglRepresentations = c('angle', 'axes', 'ball+stick', 'backbone', 'base', 'cartoon', 'contact',
                       'dihedral', 'distance', 'helixorient', 'licorice', 'hyperball', 'label',
                       'line', 'surface', 'point', 'ribbon', 'rocket', 'rope', 'spacefill', 'trace', 'unitcell',
                       'validation')
nglColorSchemes <- c('residueIndex', 'chainIndex', 'entityType', 'entityIndex')
defaultRepresentation <- "cartoon"
defaultColorScheme <- "residueIndex"

#----------------------------------------------------------------------------------------------------
components=list(
    A=list(name="A",
           selection=":A",
           representation="cartoon",
           colorScheme="residueIndex",
           visible=TRUE),
    A.ballStick=list(name="A",
           selection=":A",
           representation="ball+stick",
           colorScheme="residueIndex",
           visible=TRUE),
    OEC.spacefill=list(name="OEC.spacefill",
                   selection="OEC AND :A",
                   representation="spacefill", #"line",
                   colorScheme="element",
                   visible=TRUE),
    OEC.line=list(name="OEC.line",
                   selection="OEC AND :A",
                   representation="line",
                   colorScheme="element",
                   visible=TRUE)
    ) # components

#----------------------------------------------------------------------------------------------------
OECApp = R6Class("OECApp",

    #--------------------------------------------------------------------------------
    private = list(latestText=NULL,
                   components=NULL,
                   nglRepresentations=NULL,
                   nglColorSchemes=NULL,
                   defaultRepresentation=NULL,
                   pdbID=NULL,
                   options.oec=NULL,
                   ngl.widget=NULL
                   ),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            print(noquote(sprintf("initializing OEC")))
            options.1s5l <- list(pdbID="1S5L", htmlContainer="nglShiny_1s5l", namedComponents=components)
            private$ngl.widget <- nglShiny(options.1s5l, 300, 300) #, elementId="nglShiny_1s5l")

            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
               tags$head(
                        tags$style("#nglShiny_1s5l{height:90vh !important;}"),
                        tags$link(rel="icon", href="data:;base64,iVBORw0KGgo=")
                        ),
               tabsetPanel(type = "tabs",
                           tabPanel("PhyB",
                                    sidebarLayout(
                                        sidebarPanel(
                                            actionButton("fitButton", "Fit"),
                                            actionButton("hideAllButton", "Hide All"),
                                            actionButton("showAllButton", "Show All"),
                                            br(), br(),
                                            h5("Domains"),
                                            actionButton("toggleAVisibilityButton", "A (cartoon)"),
                                            br(),
                                            actionButton("toggleA.ballStick.VisibilityButton", "A (ball+stick)"),
                                            br(),
                                            actionButton("toggleOEC.spacefill.VisibilityButton", "OEC (spacefill)"),
                                            actionButton("toggleOEC.line.VisibilityButton", "OEC (line)"),
                                            width=2),
                                        mainPanel(nglShinyOutput('nglShiny_1s5l'),width=10)
                                    ),   # sidebarLayout
                                    ) # PhyB tabPanel
                           ) # tabsetPanel
               )}, # ui

        #------------------------------------------------------------
        server = function(input, output, session){

            observeEvent(input$fitButton, ignoreInit=TRUE, {
               fit(session, htmlContainer="nglShiny_1s5l")
               })

            observeEvent(input$defaultViewButton, ignoreInit=TRUE, {
               session$sendCustomMessage(type="removeAllRepresentations", message=list())
               session$sendCustomMessage(type="setRepresentation", message=list(defaultRepresentation))
               session$sendCustomMessage(type="setColorScheme", message=list(defaultColorScheme))
               session$sendCustomMessage(type="fit", message=list())
               })

            observeEvent(input$toggleAVisibilityButton, ignoreInit=TRUE, {
               newState <- !components$A$visible
               components$A$visible <<- newState
               setVisibility(session, htmlContainer="nglShiny_1s5l", "A", newState)
               })

            observeEvent(input$toggleA.ballStick.VisibilityButton, ignoreInit=TRUE, {
               newState <- !components$A.ballStick$visible
               components$A.ballStick$visible <<- newState
               setVisibility(session, htmlContainer="nglShiny_1s5l", "A.ballStick", newState)
               })

            observeEvent(input$toggleOEC.spacefill.VisibilityButton, ignoreInit=TRUE, {
               newState <- !components$OEC.spacefill$visible
               components$OEC.spacefill$visible <<- newState
               setVisibility(session, htmlContainer="nglShiny_1s5l", "OEC.spacefill", newState)
               })

            observeEvent(input$toggleOEC.line.VisibilityButton, ignoreInit=TRUE, {
               newState <- !components$OEC.line$visible
               components$OEC.line$visible <<- newState
               setVisibility(session, htmlContainer="nglShiny_1s5l", "OEC.line", newState)
               })

            observeEvent(input$hideAllButton, ignoreInit=TRUE, {
               components$A$visible <<- FALSE
               components$A.ballStick$visible <<- FALSE
               components$OEC.line$visible <<- FALSE
               components$OEC.spacefill$visible <<- FALSE
               setVisibility(session, htmlContainer="nglShiny_1s5l", "A", FALSE)
               setVisibility(session, htmlContainer="nglShiny_1s5l", "A.ballStick", FALSE)
               setVisibility(session, htmlContainer="nglShiny_1s5l", "OEC.line", FALSE)
               setVisibility(session, htmlContainer="nglShiny_1s5l", "OEC.spacefill", FALSE)
               })

            observeEvent(input$showAllButton, ignoreInit=TRUE, {
               components$A$visible <<- TRUE
               components$A.ballStick$visible <<- TRUE
               components$OEC.line$visible <<- TRUE
               components$OEC.spacefill$visible <<- TRUE
               setVisibility(session, htmlContainer="nglShiny_1s5l", "A", TRUE)
               setVisibility(session, htmlContainer="nglShiny_1s5l", "A.ballStick", TRUE)
               setVisibility(session, htmlContainer="nglShiny_1s5l", "OEC.line", TRUE)
               setVisibility(session, htmlContainer="nglShiny_1s5l", "OEC.spacefill", TRUE)
               })

            observeEvent(input$pdbSelector, ignoreInit=TRUE, {
               choice = input$pdbSelector
               printf("pdb: %s", choice)
               session$sendCustomMessage(type="setPDB", message=list(choice))
               updateSelectInput(session, "pdbSelector", label=NULL, choices=NULL,  selected=choice)
               })

            observeEvent(input$representationSelector, ignoreInit=TRUE, {
               choice = input$representationSelector;
               printf("rep: %s", choice)
               session$sendCustomMessage(type="setRepresentation", message=list(choice))
               updateSelectInput(session, "representationSelector", label=NULL, choices=NULL,  selected=choice)
               })

            observeEvent(input$colorSchemeSelector, ignoreInit=TRUE, {
               choice = input$colorSchemeSelector;
               printf("colorScheme: %s", choice)
               session$sendCustomMessage(type="setColorScheme", message=list(choice))
               updateSelectInput(session, "colorSchemeSelector", label=NULL, choices=NULL,  selected=choice)
               })

            output$nglShiny_1s5l <- renderNglShiny({
               printf("--- rendering nglShiny")
               private$ngl.widget
               })
        } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
deploy <-function()
{
   repos <- options("repos")[[1]]
   stopifnot(sort(names(repos)) == c("BioCann", "BioCsoft", "CRAN"))
   stopifnot(repos$BioCann=="https://bioconductor.org/packages/3.13/data/annotation")
   stopifnot(repos$BioCsoft=="https://bioconductor.org/packages/3.13/bioc")
   stopifnot(repos$CRAN=="https://cran.microsoft.com")
   require(devtools)

      # jim hester suggests, with reference
      # Setting R_REMOTES_NO_ERRORS_FROM_WARNINGS="false" will cause warning
      # messages during calls to install.packages() to become errors. Often warning
      # messages are caused by dependencies failing to install.
   Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")

   require(rsconnect)
   #rsconnect::showLogs(appName="tinyOEC", streaming=TRUE)

   deployApp(account="paulshannon",
              appName="tinyOEC",
              appTitle="tiny OEC",
              appFiles=c("tinyOEC.R"),
              appPrimaryDoc="tinyOEC.R"
              )

} # deploy
#------------------------------------------------------------------------------------------------------------------------
app.obj <- OECApp$new()
if(grepl("hagfish", Sys.info()[["nodename"]]) & !interactive()){
   runApp(shinyApp(app.obj$ui, app.obj$server), port=6867)
   } else {
   shinyApp(app.obj$ui, app.obj$server)
   }

