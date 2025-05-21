#' Shiny app to launch an interactive session
#'
#' @description
#' Interactive Shiny App for MoveRakeR visualisation
#' This app is still in beta testing development.
#'
#' @details
#' More to add here, placeholder text
#'
#' @returns
#' A nice Shiny browser of goodness
#'
#' @export
RakeRvis <- function(data = NULL,
                       Providers = c("OpenStreetMap", "GoogleEarth", "Esri.OceanBasemap", "Esri.WorldImagery"),
                       browser = TRUE,
                       TagID = NULL,
                       col = NULL,
                       col_anim = NULL,
                       plot_w = "100%",
                       plot_h = 800,
                       shapes = NULL,
                       pcol = "black",
                       scol = "black",
                       points=NULL,
                       PathOut = NULL,
                       legend = TRUE,
                       lines4fixes = TRUE,
                       fixes = TRUE) {

  #print(shapes)
  PKGENVIR2 <<- new.env(parent=emptyenv())
  PKGENVIR2$TagID <- TagID
  PKGENVIR2$Providers <- Providers
  PKGENVIR2$col <- col
  PKGENVIR2$col_anim <- col_anim
  PKGENVIR2$plot_w <- plot_w
  PKGENVIR2$plot_h <- plot_h
  PKGENVIR2$pcol <- pcol
  PKGENVIR2$scol <- scol
  PKGENVIR2$points <- points
  PKGENVIR2$PathOut <- PathOut
  PKGENVIR2$legend <- legend
  PKGENVIR2$lines4fixes <- lines4fixes
  PKGENVIR2$fixes <- fixes

  require(MoveRakeR)

  # ------------------------------------------------------------------------------ #
  ### initial things - now moved from Global file as 'data' is a function and falls over
  if(!is.null(data)){

    if(MoveRakeR::is_Track(data)){
      if(!is.null(TagID)){
        data <- data[data$TagID %in% TagID,]
      }
      #onedata <- data
      PKGENVIR2$onedata <- data

    }

    if(MoveRakeR::is_TrackStack(data)){
      if(!is.null(TagID)){
        data <- subset_TMS(data,TagIDs = TagID)
      }
      #onedata <- do.call('rbind',data)
      data <- MoveRakeR::TrackStack2Track(data)
      PKGENVIR2$onedata <- data
    }

    if(MoveRakeR::is_TrackMultiStack(data)){
      data <- MoveRakeR::subset_TMS(data,TagIDs = TagID)
      data <- MoveRakeR::TrackMultiStack2Track(data)
      #onedata <- data
      PKGENVIR2$onedata <- data
      #stop("Calls to leaflet plotting are not yet implemented for TMS objects")
    }

    if(!is(data, "Track")){ # using the generic is S3 method, will check if Track, TrackStack or TrackMultiStack now
      stop("Data are not a valid Track, TrackStack or TrackMultiStack object")
    }

    # ------------------------------ #
    # check if some variables exist for plotting in the leaflet popup clicker table
    # if they don't add them in the data, as otherwise it's a pain
    if(!exists("speed_2d",data)){PKGENVIR2$onedata$speed_2d <- NA}
    if(!exists("speed_3d",data)){PKGENVIR2$onedata$speed_3d <- NA}
    if(!exists("altitude",data)){PKGENVIR2$onedata$altitude <- NA}
    if(!exists("altitude_agl",data)){PKGENVIR2$onedata$altitude_agl <- NA}
    if(!exists("satellites_used",data)){PKGENVIR2$onedata$satellites_used <- NA}

    # if only altitude is provided not _agl...
    if(exists("altitude",data) & !exists("altitude_agl",data)){PKGENVIR2$onedata$altitude_agl <- onedata$altitude}

    PKGENVIR2$onedataexist <- 1

  } else{
    PKGENVIR2$onedata <- NULL
    PKGENVIR2$nedataexist <- 0
  }

  if(!is.null(shapes)){

    PKGENVIR2$shapes <- list()
    for(i in 1:length(shapes)){
      #print(i)
      PKGENVIR2$shapes[[i]] <- sf::st_transform(shapes[[i]], crs = 4326)
    }
  }

  # ------------------------------------------------------------------------------ #
  # Browser choice
  if(browser){
    brow = TRUE
  }else{
    brow = getOption("shiny.launch.browser", interactive())
  }

  # ------------------------------------------------------------------------------ #
  # # # # # For real deployment on Github
  appDir <- system.file("myapp", package = "RakeRvis")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)
  }

  # ------------------------------------------------------------------------------ #
  ###### FOR DEV TESTING
  #appDir <- "Y:/R packages/RakeRvis/inst/myapp"
  #source(file.path(appDir,"global.R"), local = TRUE, chdir = TRUE)
  #source(file.path(appDir,"ui.R"), local = TRUE, chdir = TRUE)
  #source(file.path(appDir,"server.R"), local = TRUE, chdir = TRUE)

  # ------------------------------------------------------------------------------ #
  # final call to run the app
  shiny::runApp(appDir, display.mode = "normal", launch.browser = brow)

}

PKGENVIR <- new.env(parent=emptyenv())

