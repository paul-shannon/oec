library(nglShiny)
library(R6)
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
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
#  (31 jan 2023)
# x = stage.getComponentsByName("6w1-all.pdb").list[0]
# x.removeAllRepresentations()
# stage.getComponentsByName('6w1-all.pdb').autoView("OEX AND :A")
# x.autoView("OEX AND :A")
# x.autoView("OEX AND :P")
# x.addRepresentation("ball+stick", {sele: "OEX"})
# stage.getRepresentationsByName()   # lists all
# stage.getRepresentationsByName("ball+stick").list.length

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
       oex.o=list(name="oex.o",
                   selection="OEX and :A",
                   representation="ball+stick",
                     colorScheme="element",
                   visible=TRUE),
       oex.p=list(name="oex.p",
                   selection="OEX and :P",
                   representation="ball+stick",
                     colorScheme="element",
                   visible=TRUE),
       oex.q=list(name="oex.q",
                   selection="OEX and :Q",
                   representation="ball+stick",
                     colorScheme="element",
                   visible=TRUE),
       oex.r=list(name="oex.r",
                   selection="OEX and :R",
                   representation="ball+stick",
                     colorScheme="element",
                   visible=TRUE),
       oex.t=list(name="oex.t",
                   selection="OEX and :T",
                   representation="ball+stick",
                     colorScheme="element",
                   visible=TRUE),
       oex.u=list(name="oex.u",
                   selection="OEX and :U",
                   representation="ball+stick",
                     colorScheme="element",
                   visible=TRUE),
       oex.v=list(name="oex.v",
                   selection="OEX and :V",
                   representation="ball+stick",
                     colorScheme="element",
                  visible=TRUE),
       tyr.o=list(name="tyr.o",
                notes="Tyr-Z is the interface between the oxidizing power of P680+, the primary donor in PSII and the oxygen evolving CaMn4 cluster.",
                selection="TYR and 161 and :A",
                representation="ball+stick",
                colorScheme="element",
                visible=TRUE),
       tyr.p=list(name="tyr.p",
                notes="Tyr-Z is the interface between the oxidizing power of P680+, the primary donor in PSII and the oxygen evolving CaMn4 cluster.",
                selection="TYR and 161 and :P",
                representation="ball+stick",
                colorScheme="element",
                visible=TRUE),
       pl9.a=list(name="pl9.a",
                  notes="plastoquinone Qa",
                  selection= "PL9 and :A",
                  representation="ball+stick",
                  colorScheme="element",
                  visible=TRUE),
       pl9.d=list(name="pl9.d",
                  selection= "PL9 and :D",
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
            options.6w1 <- list(pdbID="http://localhost:60050/pdb/6w1-all.pdb", htmlContainer="nglShiny_oec_s2",
                                namedComponents=private$components)
            private$ngl.widget <- nglShiny(options.6w1, 300, 300)
            private$spinState <- FALSE
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
               tags$head(
                        tags$style("#nglShiny_oec_s2{height:90vh !important;}"),
                        tags$style(".nav-tabs{font-size: 24px;}"),
                        tags$link(rel="icon", href="data:;base64,iVBORw0KGgo=")
                        ),
               tabsetPanel(type = "tabs",
                           tabPanel("Photosystem II with OEC",
                                    sidebarLayout(
                                        sidebarPanel(
                                            actionButton("demoButton", "Demo"),
                                            actionButton("fitButton", "Fit"),
                                            actionButton("spinButton", "Spin start/stop"),
                                            actionButton("hideAllButton", "Hide All"),
                                            actionButton("showAllButton", "Show All"),
                                            br(), br(),
                                            h5("Show/Hide"),
                                            actionButton("toggle.oex.o.button", "OEX O"), br(),
                                            actionButton("toggle.oex.p.button", "OEX P  t0"), br(),
                                            actionButton("toggle.oex.q.button", "OEX Q  50 μs"), br(),
                                            actionButton("toggle.oex.r.button", "OEX R 150 μs"), br(),
                                            actionButton("toggle.oex.t.button", "OEX T 250 μs"), br(),
                                            actionButton("toggle.oex.u.button", "OEX U 400 μs"), br(),
                                            actionButton("toggle.oex.v.button", "OEX V 200 msec"), br(),
                                            actionButton("step.button", "STEP"),
                                            br(),
                                            width=2),
                                        mainPanel(nglShinyOutput('nglShiny_oec_s2'),width=10)
                                    ),   # sidebarLayout
                                    ) # photosystem II tabPanel
                             #tabPanel("OEC: 'most precious of all jewels'", includeHTML("oec.html"))
                           ) # tabsetPanel
               )}, # ui

        #------------------------------------------------------------
        server = function(input, output, session){

            observeEvent(input$demoButton, ignoreInit=TRUE, {
               fit(session, htmlContainer="nglShiny_oec_s2")
               center(session, htmlContainer="nglShiny_oec_s2", "OEC AND :A")
               setCameraDistance(session, htmlContainer="nglShiny_oec_s2", 50)
               private$spinState <- TRUE
               spin(session, TRUE)
               })

            observeEvent(input$fitButton, ignoreInit=TRUE, {
               fit(session, htmlContainer="nglShiny_oec_s2")
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

            observeEvent(input$toggle.oex.o.button, ignoreInit=TRUE, {
               newState <- !private$components$oex.o$visible
               private$components$oex.o$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_oec_s2", "oex.o", newState)
               })

            observeEvent(input$toggle.oex.p.button, ignoreInit=TRUE, {
               newState <- !private$components$oex.p$visible
               private$components$oex.p$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_oec_s2", "oex.p", newState)
               })

            observeEvent(input$toggle.oex.q.button, ignoreInit=TRUE, {
               newState <- !private$components$oex.q$visible
               private$components$oex.q$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_oec_s2", "oex.q", newState)
               })

            observeEvent(input$toggle.oex.r.button, ignoreInit=TRUE, {
               newState <- !private$components$oex.r$visible
               private$components$oex.r$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_oec_s2", "oex.r", newState)
               })

            observeEvent(input$toggle.oex.t.button, ignoreInit=TRUE, {
               newState <- !private$components$oex.t$visible
               private$components$oex.t$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_oec_s2", "oex.t", newState)
               })

            observeEvent(input$toggle.oex.u.button, ignoreInit=TRUE, {
               newState <- !private$components$oex.u$visible
               private$components$oex.u$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_oec_s2", "oex.u", newState)
               })

            observeEvent(input$toggle.oex.v.button, ignoreInit=TRUE, {
               newState <- !private$components$oex.v$visible
               private$components$oex.v$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_oec_s2", "oex.v", newState)
               })

            hideAll <- function(){
               components <- names(private$components)
               for(component in components){
                  printf("--- hiding %s", component)
                  private$components[[component]]$visible <- FALSE
                  setVisibility(session, htmlContainer="nglShiny_oec_s2", component, FALSE)
                  } # for component
               } # hideAll

            observeEvent(input$step.button, ignoreInit=TRUE, {
               printf("step button")
               for(component in private$components){
                  hideAll()
                  setVisibility(session, htmlContainer="nglShiny_oec_s2", component, TRUE)
                  Sys.sleep(1)
                  }
               })

            observeEvent(input$toggleOEC.spacefill.VisibilityButton, ignoreInit=TRUE, {
               newState <- !private$components$OEC.spacefill$visible
               private$components$OEC.spacefill$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_oec_s2", "OEC.spacefill", newState)
               })

            observeEvent(input$toggleOEC.line.VisibilityButton, ignoreInit=TRUE, {
               newState <- !private$components$OEC.line$visible
               private$components$OEC.line$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_oec_s2", "OEC.line", newState)
               })

            observeEvent(input$toggle.CLA.348, ignoreInit=TRUE, {
               newState <- !private$components$CLA.348$visible
               private$components$CLA.348$visible <<- newState
               setVisibility(session, htmlContainer="nglShiny_oec_s2", "CLA.348", newState)
               })

            observeEvent(input$toggle.chlorophyll.349, ignoreInit=TRUE, {
               newState <- !private$components$CLA.349$visible
               private$components$CLA.349$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_oec_s2", "CLA.349", newState)
               })
            observeEvent(input$toggle.chlorophyll.350, ignoreInit=TRUE, {
               newState <- !private$components$CLA.350$visible
               private$components$CLA.350$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_oec_s2", "CLA.350", newState)
               })
            observeEvent(input$toggle.chlorophyll.352, ignoreInit=TRUE, {
               newState <- !private$components$CLA.352$visible
               private$components$CLA.352$visible <- newState
               setVisibility(session, htmlContainer="nglShiny_oec_s2", "CLA.352", newState)
               })

            observeEvent(input$center.OEC, ignoreInit=TRUE, {
               center(session, htmlContainer="nglShiny_oec_s2", "OEC AND :A")
               })
            observeEvent(input$center.A, ignoreInit=TRUE, {
               center(session, htmlContainer="nglShiny_oec_s2", ":A")
               })
            observeEvent(input$center.CHL348, ignoreInit=TRUE, {
               center(session, htmlContainer="nglShiny_oec_s2", "[CLA] AND 348 AND :A")
               })
            observeEvent(input$center.CHL349, ignoreInit=TRUE, {
               center(session, htmlContainer="nglShiny_oec_s2", "[CLA] AND 349 AND :A")
               })
            observeEvent(input$center.CHL350, ignoreInit=TRUE, {
               center(session, htmlContainer="nglShiny_oec_s2", "[CLA] AND 350 AND :A")
               })
            observeEvent(input$center.CHL352, ignoreInit=TRUE, {
               center(session, htmlContainer="nglShiny_oec_s2", "[CLA] AND 352 AND :A")
               })

            observeEvent(input$hideAllButton, ignoreInit=TRUE, {
               components <- names(private$components)
               for(component in components){
                  printf("--- hiding %s", component)
                  private$components[[component]]$visible <- FALSE
                  setVisibility(session, htmlContainer="nglShiny_oec_s2", component, FALSE)
                  } # for component
               })

            observeEvent(input$showAllButton, ignoreInit=TRUE, {
               components <- names(private$components)
               for(component in components){
                  if(component == "A.ballStick") next;
                  printf("--- showing %s", component)
                  private$components[[component]]$visible <- TRUE
                  setVisibility(session, htmlContainer="nglShiny_oec_s2", component, TRUE)
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

            output$nglShiny_oec_s2 <- renderNglShiny({
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
   stopifnot(repos$BioCann=="https://bioconductor.org/packages/3.15/data/annotation")
   stopifnot(repos$BioCsoft=="https://bioconductor.org/packages/3.15/bioc")
   stopifnot(repos$CRAN=="https://cran.microsoft.com")
   require(devtools)

   Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS=FALSE)

   install_github("paul-shannon/nglShiny", force=TRUE)
   require(rsconnect)
   deployApp(account="paulshannon",
             appName="OECapp",
             appTitle="Oxygen-evolving complex",
             appFiles=c("oec.R", "oec.html"),
             appPrimaryDoc="oec.R",
             forceUpdate=TRUE
             )

} # deploy
#------------------------------------------------------------------------------------------------------------------------
app.obj <- OECApp$new()
port <- sample(10000:15000, size=1)
if(grepl("hagfish", Sys.info()[["nodename"]]) & !interactive()){
   runApp(shinyApp(app.obj$ui, app.obj$server), port=port)
   } else {
   shinyApp(app.obj$ui, app.obj$server)
   }

