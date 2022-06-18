library(nglShiny)
library(R6)
#----------------------------------------------------------------------------------------------------
# gnl selection language https://nglviewer.org/ngl/api/manual/usage/selection-language.html
#
# javascript navigations:
#
# javascript api docs here: http://nglviewer.org/ngl/api/class/src/component/component.js~Component.html
#  alex rose and me github issue here:
#     https://github.com/nglviewer/ngl/issues/383
#     https://codepen.io/arose/pen/XebPwK
#     var stage = new NGL.Stage( "viewport" );
#     window.addEventListener( "resize", function( event ){
#        stage.handleResize();
#        }, false );
#
#     stage.loadFile( "rcsb://1IZL" ).then(function(comp){
#        comp.addRepresentation('ball+stick', { sele: 'PHO and :A' })
#        comp.autoView('PHO and :A')
#        });
# x = stage.getComponentsByName("1S5L").list[0]
# x.removeAllRepresentations()
# x.addRepresentation("ball+stick", {sele: "34-41"})
# x.addRepresentation("ball+stick", {sele: "1-40028"})
# x.addRepresentation("cartoon", {sele: ":A"})
# x.addRepresentation("surface", {sele: ".MN1 .MN2 .MN3 .MN4"})
# x.addRepresentation("line", {sele: "[OEC] AND :A"})   just the OEC in chain A
# x.getRepresentationsByName(repName).setVisibility(newState)
#  pick out one chlorophyll molecule:
#      x.addRepresentation("ball+stick", {sele: "[CLA] AND 352 AND :A"})
#
# center (autoview) on a representation: autoView is a method on the component(the loaded structure)
# don't know if there is more than one component (no subcomponents) in normal use.
#  stage.getComponentsByName('1S5L').autoView("OEC AND :A")
#
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
setup.components <- function()
{
   list(
       A.cartoon=list(name="A.cartoon",
              selection=":A",
              representation="cartoon",
              colorScheme="residueIndex",
              visible=TRUE),

       A.ballStick=list(name="A.ballStick",
                        selection=":A",
                        representation="ball+stick",
                        colorScheme="residueIndex",
                        visible=FALSE),

       OEC.spacefill=list(name="OEC.spacefill",
                          selection="OEC AND :A",
                          representation="spacefill", #"line",
                          colorScheme="element",
                          visible=TRUE),

       OEC.line=list(name="OEC.line",
                     selection="OEC AND :A",
                     representation="ball+stick",
                     colorScheme="element",
                     visible=TRUE),

       CLA.348=list(name="CLA.348",
                    selection="[CLA] AND 348 AND :A",
                    representation="ball+stick",
                    colorScheme="element",
                    visible=TRUE),

       CLA.349=list(name="CLA.349",
                    selection="[CLA] AND 349 AND :A",
                    representation="ball+stick",
                    colorScheme="element",
                    visible=TRUE),

       CLA.350=list(name="CLA.350",
                    selection="[CLA] AND 350 AND :A",
                    representation="ball+stick",
                    colorScheme="element",
                    visible=TRUE),

       CLA.352=list(name="CLA.352",
                    selection="[CLA] AND 352 AND :A",
                    representation="ball+stick",
                    colorScheme="element",
                    visible=TRUE)
        ) # list

} # setup.components
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
                   ngl.widget=NULL,
                   spinState=NULL
                   ),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            print(noquote(sprintf("initializing OEC")))
            private$components <- setup.components()
            options.1s5l <- list(pdbID="1S5L", htmlContainer="nglShiny_1s5l",
                                 namedComponents=private$components)
            private$ngl.widget <- nglShiny(options.1s5l, 300, 300)
            private$spinState <- FALSE
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
                                            actionButton("spinButton", "Spin"),
                                            actionButton("hideAllButton", "Hide All"),
                                            actionButton("showAllButton", "Show All"),
                                            br(), br(),
                                            h5("Visibility"),
                                            actionButton("toggleA.cartoon.button", "A (cartoon)"),
                                            br(),
                                            actionButton("toggleA.ballStick.button", "A (ball+stick)"),
                                            br(),
                                            actionButton("toggleOEC.spacefill.VisibilityButton", "OEC (spacefill)"),
                                            br(),
                                            actionButton("toggleOEC.line.VisibilityButton", "OEC (line)"),
                                            br(),
                                            actionButton("toggle.CLA.348", "CHL 348"),
                                            actionButton("toggle.chlorophyll.349", "CHL 349"),
                                            actionButton("toggle.chlorophyll.350", "CHL 350"),
                                            actionButton("toggle.chlorophyll.352", "CHL 352"),
                                            h5("Center"),
                                            actionButton("center.A", "A"),
                                            actionButton("center.OEC", "OEC"),
                                            actionButton("center.CHL348", "CHL 348"),
                                            actionButton("center.CHL349", "CHL 349"),
                                            actionButton("center.CHL350", "CHL 350"),
                                            actionButton("center.CHL352", "CHL 352"),
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

            observeEvent(input$spinButton, ignoreInit=TRUE, {
               private$spinState <- !private$spinState
               spin(session, private$spinState)
               })

            observeEvent(input$defaultViewButton, ignoreInit=TRUE, {
               session$sendCustomMessage(type="removeAllRepresentations", message=list())
               session$sendCustomMessage(type="setRepresentation", message=list(defaultRepresentation))
               session$sendCustomMessage(type="setColorScheme", message=list(defaultColorScheme))
               session$sendCustomMessage(type="fit", message=list())
               })

            observeEvent(input$toggleA.cartoon.button, ignoreInit=TRUE, {

               newState <- !private$components$A.cartoon$visible
               private$components$A.cartoon$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_1s5l", "A.cartoon", newState)
               })

            observeEvent(input$toggleA.ballStick.button, ignoreInit=TRUE, {
               printf("A.ballStick.button click")
               newState <- !private$components$A.ballStick$visible
               private$components$A.ballStick$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_1s5l", "A.ballStick", newState)
               })

            observeEvent(input$toggleOEC.spacefill.VisibilityButton, ignoreInit=TRUE, {
               newState <- !private$components$OEC.spacefill$visible
               private$components$OEC.spacefill$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_1s5l", "OEC.spacefill", newState)
               })

            observeEvent(input$toggleOEC.line.VisibilityButton, ignoreInit=TRUE, {
               newState <- !private$components$OEC.line$visible
               private$components$OEC.line$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_1s5l", "OEC.line", newState)
               })

            observeEvent(input$toggle.CLA.348, ignoreInit=TRUE, {
               newState <- !private$components$CLA.348$visible
               private$components$CLA.348$visible <<- newState
               setVisibility(session, htmlContainer="nglShiny_1s5l", "CLA.348", newState)
               })

            observeEvent(input$toggle.chlorophyll.349, ignoreInit=TRUE, {
               newState <- !private$components$CLA.349$visible
               private$components$CLA.349$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_1s5l", "CLA.349", newState)
               })
            observeEvent(input$toggle.chlorophyll.350, ignoreInit=TRUE, {
               newState <- !private$components$CLA.350$visible
               private$components$CLA.350$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_1s5l", "CLA.350", newState)
               })
            observeEvent(input$toggle.chlorophyll.352, ignoreInit=TRUE, {
               newState <- !private$components$CLA.352$visible
               private$components$CLA.352$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_1s5l", "CLA.352", newState)
               })

            observeEvent(input$center.OEC, ignoreInit=TRUE, {
               center(session, htmlContainer="nglShiny_1s5l", "OEC AND :A")
               })
            observeEvent(input$center.A, ignoreInit=TRUE, {
               center(session, htmlContainer="nglShiny_1s5l", ":A")
               })
            observeEvent(input$center.CHL348, ignoreInit=TRUE, {
               center(session, htmlContainer="nglShiny_1s5l", "[CLA] AND 348 AND :A")
               })
            observeEvent(input$center.CHL349, ignoreInit=TRUE, {
               center(session, htmlContainer="nglShiny_1s5l", "[CLA] AND 349 AND :A")
               })
            observeEvent(input$center.CHL350, ignoreInit=TRUE, {
               center(session, htmlContainer="nglShiny_1s5l", "[CLA] AND 350 AND :A")
               })
            observeEvent(input$center.CHL352, ignoreInit=TRUE, {
               center(session, htmlContainer="nglShiny_1s5l", "[CLA] AND 352 AND :A")
               })

            observeEvent(input$hideAllButton, ignoreInit=TRUE, {
               components <- names(private$components)
               for(component in components){
                  printf("--- hiding %s", component)
                  private$components[[component]]$visible <- FALSE
                  setVisibility(session, htmlContainer="nglShiny_1s5l", component, FALSE)
                  } # for component
               })

            observeEvent(input$showAllButton, ignoreInit=TRUE, {
               components <- names(private$components)
               for(component in components){
                  if(component == "A.ballStick") next;
                  printf("--- showing %s", component)
                  private$components[[component]]$visible <- TRUE
                  setVisibility(session, htmlContainer="nglShiny_1s5l", component, TRUE)
                  } # for component
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

