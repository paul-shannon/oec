library(shiny)
library(nglShiny)
library(htmlwidgets)
# library(shinyBS)
# library(yaml)
#----------------------------------------------------------------------------------------------------
#tooltips <- yaml.load_file("tooltips.yaml")
#for(i in 1:length(tooltips)) tooltips[[i]]$text <- paste(tooltips[[i]]$text, collapse=" ")
#printf("length of tooltips read: %d", length(tooltips))
#----------------------------------------------------------------------------------------------------
printf <- function(...)print(noquote(sprintf(...)))
#----------------------------------------------------------------------------------------------------
#                 PAS 116–222 95 0.81
#                 GAF 234–432 185 0.73
#                 PHY 480–610 80 1.8
# PHY without hairpin 480–610 43 0.97
#             Hairpin 560–591
#----------------------------------------------------------------------------------------------------
# from https://www.uniprot.org/uniprot/P14713
components.1s5l=list(
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
    ) # components.1s5l



nglRepresentations = c('angle', 'axes', 'ball+stick', 'backbone', 'base', 'cartoon', 'contact',
                       'dihedral', 'distance', 'helixorient', 'licorice', 'hyperball', 'label',
                       'line', 'surface', 'point', 'ribbon', 'rocket', 'rope', 'spacefill', 'trace', 'unitcell',
                       'validation')
nglColorSchemes <- c('residueIndex', 'chainIndex', 'entityType', 'entityIndex')
defaultRepresentation <- "cartoon"
defaultColorScheme <- "residueIndex"
options.1s5l <- list(pdbID="1S5L", htmlContainer="nglShiny_1s5l", namedComponents=components.1s5l)
ngl.1s5l <- nglShiny(options.1s5l, 300, 300) #, elementId="nglShiny_1s5l")

# gnl selection language https://nglviewer.org/ngl/api/manual/usage/selection-language.html
# javascript navigations:
# x = stage.getComponentsByName("1S5L").list[0]
# x.removeAllRepresentations()
# x.addRepresentation("ball+stick", {sele: "34-41"})
# x.addRepresentation("ball+stick", {sele: "1-40028"})
# x.addRepresentation("cartoon", {sele: ":A"})
# x.addRepresentation("surface", {sele: ".MN1 .MN2 .MN3 .MN4"})
# x.addRepresentation("line", {sele: "[OEC] AND :A"})   just the OEC in chain A
#----------------------------------------------------------------------------------------------------
# 1RQK, 3I4D: Photosynthetic reaction center from rhodobacter sphaeroides 2.4.1
# crambin, 1crn: https://bmcbiophys.biomedcentral.com/articles/10.1186/s13628-014-0008-0
# addResourcePath("www", "www");

ui = shinyUI(fluidPage(

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
                                        #actionButton("toggleChromophoreBVisibilityButton", "Chromophore B"),
                                        #actionButton("toggleBChainVisibilityButton", ":B"),
                                        #actionButton("togglePASdomainVisibilityButton", "PAS"),
                                        #actionButton("toggleGAFdomainVisibilityButton", "GAF"),
                                        #br(); br()
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
  ) # fluidPage

) # shinyUI
#----------------------------------------------------------------------------------------------------
server = function(input, output, session) {

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
     newState <- !components.1s5l$A$visible
     components.1s5l$A$visible <<- newState
     setVisibility(session, htmlContainer="nglShiny_1s5l", "A", newState)
     })

   observeEvent(input$toggleA.ballStick.VisibilityButton, ignoreInit=TRUE, {
     newState <- !components.1s5l$A.ballStick$visible
     components.1s5l$A.ballStick$visible <<- newState
     setVisibility(session, htmlContainer="nglShiny_1s5l", "A.ballStick", newState)
     })

   observeEvent(input$toggleOEC.spacefill.VisibilityButton, ignoreInit=TRUE, {
     newState <- !components.1s5l$OEC.spacefill$visible
     components.1s5l$OEC.spacefill$visible <<- newState
     setVisibility(session, htmlContainer="nglShiny_1s5l", "OEC.spacefill", newState)
     })

   observeEvent(input$toggleOEC.line.VisibilityButton, ignoreInit=TRUE, {
     newState <- !components.1s5l$OEC.line$visible
     components.1s5l$OEC.line$visible <<- newState
     setVisibility(session, htmlContainer="nglShiny_1s5l", "OEC.line", newState)
     })

  observeEvent(input$hideAllButton, ignoreInit=TRUE, {
     components.1s5l$A$visible <<- FALSE
     components.1s5l$A.ballStick$visible <<- FALSE
     components.1s5l$OEC.line$visible <<- FALSE
     components.1s5l$OEC.spacefill$visible <<- FALSE
     setVisibility(session, htmlContainer="nglShiny_1s5l", "A", FALSE)
     setVisibility(session, htmlContainer="nglShiny_1s5l", "A.ballStick", FALSE)
     setVisibility(session, htmlContainer="nglShiny_1s5l", "OEC.line", FALSE)
     setVisibility(session, htmlContainer="nglShiny_1s5l", "OEC.spacefill", FALSE)
     })

  observeEvent(input$showAllButton, ignoreInit=TRUE, {
     components.1s5l$A$visible <<- TRUE
     components.1s5l$A.ballStick$visible <<- TRUE
     components.1s5l$OEC.line$visible <<- TRUE
     components.1s5l$OEC.spacefill$visible <<- TRUE
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

  output$value <- renderPrint({input$action})

  output$nglShiny_1s5l <- renderNglShiny({
    printf("--- rendering nglShiny")
    ngl.1s5l
    })

} # server
#----------------------------------------------------------------------------------------------------
deploy <- function()
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
             appTitle="Oxygen-evolving coples",
             appFiles=c("1S5L.R"),
             appPrimaryDoc="1S5L.R",
             forceUpdate=TRUE
             )
} # deploy
#----------------------------------------------------------------------------------------------------
#runApp(shinyApp(ui=ui, server=server), port=5671)
runApp(shinyApp(ui=ui, server=server))


