#options(shiny.fullstacktrace=FALSE)

server <- function(input, output, session){

  ##################################################################
  # SET UP PATHNAMES FOR MODULE OUTPUTS
  ##################################################################
  # # # # # REAL DEPLOYMENT ON GITHUB
  appDir <- paste0(system.file("", package = "RakeRvis"),"/myapp/")

  # # # # # DEV TESTING
  #appDir <- "Y:/R packages/RakeRvis/inst/myapp"

  ##################################################################
  ##################################################################
  ##################################################################
  ##################################################################
  # BASE ELEMENTS MODULE
  ##################################################################
  ##################################################################
  ##################################################################
  ##################################################################

  output$res <- renderText({
    req(input$sidebarItemExpanded)
    paste("Expanded menuItem:", input$sidebarItemExpanded)
  })

  # switches for the overall tabs for the user back to main map and automatic switches from processes
  shiny::observeEvent({
    input$switchtab
  }, {
    newtab <- switch(input$tabs,
                     "cleaner" = "map",
                     "thin" = "map",
                     "trips" = "map",
                     "access" = "map",
                     "explorer" = "map",
                     "utilisation" = "map",
                     "data_analytics" = "map"

    )
    shinydashboard::updateTabItems(session, "tabs", newtab)
    updateTabsetPanel(session, inputId = "tab_being_displayed", selected = "Main map")
  })

  shiny::observeEvent(ignoreNULL = FALSE, ignoreInit = TRUE,{
    input$switchtab2
    check_twodata$switch
    check_threedata$switch
    check_MBdata$switch1
    check_MBdata$switch2
    check_UvAdata$switch1
    check_UvAdata$switch2
  }, {

    if(input$tabs!= "map"){
      newtab <- switch(input$tabs,
                       "cleaner" = "map",
                       "thin" = "map",
                       "trips" = "map",
                       "access" = "map",
                       "explorer" = "map",
                       "utilisation" = "map",
                       "data_analytics" = "map"

      )
      shinydashboard::updateTabItems(session, "tabs", newtab)
    }

    if(!is.null(check_twodata$switchtab2)){updateTabsetPanel(session, inputId = "tab_being_displayed", selected = "2")}
    if(!is.null(check_twodata$switch)){updateTabsetPanel(session, inputId = "tab_being_displayed", selected = "2")}
    if(!is.null(check_threedata$switch)){updateTabsetPanel(session, inputId = "tab_being_displayed", selected = "2")}
    if(!is.null(check_MBdata$switch1)){updateTabsetPanel(session, inputId = "tab_being_displayed", selected = "2")}
    if(!is.null(check_MBdata$switch2)){updateTabsetPanel(session, inputId = "tab_being_displayed", selected = "2")}
    if(!is.null(check_UvAdata$switch1)){updateTabsetPanel(session, inputId = "tab_being_displayed", selected = "2")}
    if(!is.null(check_UvAdata$switch2)){updateTabsetPanel(session, inputId = "tab_being_displayed", selected = "2")}


    #shinydashboard::updateTabItems(session, "tabs", newtab)
  })


  ######################################################
  ############## USER INTERFACE ELEMENTS ###############
  ######################################################

  #########################
  ######### clear #########
  #########################
  output$clear <- shiny::renderUI({

    htmltools::tagList(
      shiny::actionButton(inputId = "clear", label = "Clear")

    )
  })


  ###################################################################################################################################
  ###################################################################################################################################
  ###################################################################################################################################
  ##### animal GPS data stuff ###
  ###################################################################################################################################
  ###################################################################################################################################
  ###################################################################################################################################

  ##############################################################
  ###################### REACTIVE VALUES  ######################
  ##############################################################
  onedat <- shiny::reactiveValues(onedata = NULL, onedata_original = NULL,  onedata2 = NULL)

  # THIS IS FOR THE VARIABLES THAT ARE AVAILABLE TO THE USER TO PLOT LOCATIONS BY
  # STARTS OFF AS TAGID
  pal_use <- reactiveValues(this = NULL, anim = NULL, variable = NULL, type = NULL, this4lines = NULL)

  # actual plotby data used in the mapping
  plotdata <- reactiveValues(data = NULL, data2 = NULL, data3 = NULL, TagID = NULL, plot_by = NULL, plotby_var = NULL)

  ##### NULLIFY "v" choices for saving map, set up blank values
  v <- reactiveValues()
  v$fix <- NULL
  v$colour <- NULL
  v$TagID <- TagID
  v$shape <- NULL
  v$ud <- NULL
  v$line <- NULL
  v$ltog <- TRUE
  v$ptog <- TRUE
  v$radius <- NULL
  v$add.points <- NULL
  #v$add.points.popup <- NULL # not needed
  v$leg.val <- NULL
  v$leg.tit <- NULL
  v$update <- 0 # whether to update map if time slider returns no data
  v$pal <- NULL
  v$val <- NULL
  v$linedone <- FALSE
  v$mapdone <- 0

  # nullify existence of onedata, twodata
  check_onedata <- reactiveValues(d=NULL)
  check_twodata <- reactiveValues(d=NULL, switch = 0)
  check_threedata <- reactiveValues(d=NULL, switch = 0)
  check_MBdata <- reactiveValues(d=NULL, switch1 = 0, switch2 = 0)
  check_UvAdata <- reactiveValues(d=NULL, switch1 = 0, switch2 = 0)

  # trips
  check_fourdata <- reactiveValues(d=NULL)
  check_tripdata <- reactiveValues(d=NULL)

  # reactive value storage: buffers and current SPA shape for bespoke site-specific GIS
  # tbh not sure this is actually needed as all starts as NULL anyway - not sure where I read this.
  cds = shiny::reactiveValues(
                              plot_h = plot_h, # plot_h comes from main package env
                              plot_w = plot_w,  # plot_w comes from main package env

  )

  #####################################################
  # ONEDATA STARTER
  ###################################################
  # FIRST OBSERVER RUNS DATA ONLY ONCE IF NEW onedata_original is discovered (e.g. new MB/UvA read in)
  # or if data supplied at the start
  # DEFAULTS INITIALLY TO PLOTBY TAGID
  # NOTE UPDATE 13/04/23 to keep hold of any onedata at the start, to avoid overwrite
  # for new read in (DatasetModule)
  mappy_data <- reactiveValues(initialval = 0, dataexist = 0, deletion = 0, df_update = 0, df_counter = 0)

  # for storing separate trip datasets for plotting alongside the GPS data, we need to remember them not overwrite them!
  mappy_data_trips <- reactiveValues(initialval = 0)

  notificationData = shiny::reactiveValues(data = NULL, sel = NULL, new = 0, new_nm = "onedata_1", notificationData1 = NULL, notificationData2 = NULL)

  observe({

    if(!is.null(onedata)){
      attr(onedata, "source") <- "Supplied" # for use in the reactive table to display on map

      # set any originally supplied data source or newly sourced data to the reactive onedata_original
      onedat$onedata_original <- onedata

      onedata <- NULL
    } #else{
    #onedat$onedata_original <- NULL # if NULL then then observeEvent onedat$onedata_original does not fire off, so no need for NULL capture there for original data
    #}

  })

  table_clicked <- reactiveValues(click = 0)
  slider_check_base = reactiveValues(st = NULL, en = NULL)

  shiny::observeEvent({
    onedat$onedata_original
  },{

    mappy_data$df_update = 1
    mappy_data$df_counter = mappy_data$df_counter + 1
    #notificationData$new <- 1 # new run through activated

    # # # # # # # # # # # #
    # 13/04/2023 PATCH
    # make the first time any onedata is fed through, init = 1
    # if the data from the DatasetModule reactive table ALREADY EXISTS
    # i.e. you are just replotting not bringing in a new dataset, then
    # we must skip this step otherwise a duplicate dataset is generated
    # A bit laborious repeating same code for the zero condition

    if(!is.null(onedat$onedata_original)){
      nanim_ <- length(unique(onedat$onedata_original$TagID))
      source_ <- attr(onedat$onedata_original, "source")
      #nfix_ <- attr(onedat$onedata_original, "nfix")
      #id_ <- attr(onedat$onedata_original, "id")

      # for use inthe trps table really
      attr(onedat$onedata_original, "nfix") <- nrow(onedat$onedata_original)
      attr(onedat$onedata_original, "id") <- mappy_data$df_counter

    } else{
      nanim_ <- 0
      source_ <- "none"
      #nfix_ <- 0
      #id_ <- 0
    }

    if(mappy_data$initialval > 0){

      if(table_clicked$click == 0){

        mappy_data$initialval <- mappy_data$initialval + 1
        mappy_data$dataexist = 1 # need a way of saying data now exists for the Dataset module

        # current newly named dataset:
        notificationData$new_nm <- paste0("nm_onedata_", mappy_data$initialval)

        # then store a version of that dataset_init as a reactive dataset
        mappy_data[[paste0("onedata_", mappy_data$initialval)]] <- onedat$onedata_original
        mappy_data[[paste0("onedata_downl_", mappy_data$initialval)]] <- as.character(Sys.time())
        mappy_data[[paste0("nm_onedata_", mappy_data$initialval)]] <- paste0("onedata_", mappy_data$initialval)
        mappy_data[[paste0("nanim_", mappy_data$initialval)]] <- nanim_
        mappy_data[[paste0("source_", mappy_data$initialval)]] <- source_
        mappy_data[[paste0("nfix_", mappy_data$initialval)]] <- nrow(onedat$onedata_original)
        mappy_data[[paste0("id_", mappy_data$initialval)]] <- mappy_data$df_counter

      }
    } else if(mappy_data$initialval == 0){

      # first time run, always generate the initial data table, even if NULL
      mappy_data$initialval <- mappy_data$initialval + 1
      mappy_data$dataexist = 1

      # current newly named dataset:
      notificationData$new_nm <- paste0("nm_onedata_", mappy_data$initialval)

      # then store a version of that dataset_init as a reactive dataset
      mappy_data[[paste0("onedata_", mappy_data$initialval)]] <- onedat$onedata_original
      mappy_data[[paste0("onedata_downl_", mappy_data$initialval)]] <- as.character(Sys.time())
      mappy_data[[paste0("nm_onedata_", mappy_data$initialval)]] <- paste0("onedata_", mappy_data$initialval)
      mappy_data[[paste0("nanim_", mappy_data$initialval)]] <- nanim_
      mappy_data[[paste0("source_", mappy_data$initialval)]] <- source_
      mappy_data[[paste0("nfix_", mappy_data$initialval)]] <- nrow(onedat$onedata_original)
      mappy_data[[paste0("id_", mappy_data$initialval)]] <- mappy_data$df_counter

    }

    # put back to zero again in case new data read in, in which case add to the dataset list
    table_clicked$click = 0

    # # # # # # # # # # # #

    # if onedata is NULL on input then none of the following fires off, but map still generated
    # for further read in from remote repos, otherwise populate map with supplied data

    if(!is.null(onedat$onedata_original)){ # onedata here is still user-defined via function

      # ----------------------------------------------------------------------- #
      # NULLIFYING ALL REACTIVES AT THE START OF ANY NEW DATA READ IN

      updateTextInput(session=session, inputId = "colour_pal_leaf_plotby", value = NULL)
      updateTextInput(session=session, inputId = "colour_pal_leaf", value = NULL)

      plotdata$TagID <- plotdata$plot_by <- plotdata$plotby_var <- plotdata$data <- plotdata$data2 <- NULL
      pal_use$this <- pal_use$this4lines <- NULL
      onedat$onedata2 <- NULL
      onedat$pal <- NULL
      pal_use$variable <- pal_use$anim <- pal_use$this <- pal_use$type <- pal_use$this4lines <- NULL

      #cl_reactive$current_values <- NULL
      slider_check_base$st <- slider_check_base$en <- NULL
      reactivetest$pal2do <- reactivetest$palette_type <- reactivetest$inputs <- NULL

      # set the new original data to onedat$onedata reactive and get rid of original data
      onedat$onedata <- onedat$onedata_original
      onedat$onedata_original <- NULL # i.e. stop the main observer running EVERY time

      # ----------------------------------------------------------------------- #

      if(!is.null(onedat$onedata)){

        # ------------------------------- #
        # first fire off the plot_by variable choices and the select inputs
        # USING TAGID at the outset

        picker_levelsA <- as.character(unique(onedat$onedata[,which(names(onedat$onedata) == "TagID")]))
        picker_levelsA <- c(picker_levelsA, 'All')

        # the original TAGID, that we want to retain
        output$tid <- shiny::renderUI({

          shinyWidgets::pickerInput(inputId = "TagID",
                                    label = NULL,
                                    choices = picker_levelsA,
                                    selected = picker_levelsA[[1]],
                                    multiple = input$multianimal

          )

        })

        #SECOND PICKER
        picker_levelsB <- as.character(unique(onedat$onedata[,which(names(onedat$onedata) == "TagID")]))
        picker_levelsB <- c(picker_levelsB, 'All')

        output$plotby_var <- shiny::renderUI({

          shinyWidgets::pickerInput(inputId = "plotby_var",
                                    label = NULL,
                                    choices = picker_levelsB,
                                    selected = picker_levelsB[[1]],
                                    multiple = TRUE

          )

        })

        # ------------------------------- #
        # then assess further variables available that could replace TagID if the user wishes so
        # and make the selectInput

        ####################################
        # WITH LOTS OF DATA THIS WAS RUNNING SLOW AT BOOT UP OF THE APP

        output$plot_by <- shiny::renderUI({

          dat_classes = t(lapply(onedat$onedata, function(x) paste0(class(x), collapse = ', ')) %>% data.frame())

          nms_use = names(onedat$onedata)

          # drop logical or posixct classes
          if(any(grepl("POSIX",dat_classes[,1]))){

            w = grep("POSIX",dat_classes[,1])

            nm_rm <- names(dat_classes[w,1])
            nms_use = nms_use[!nms_use %in% nm_rm]
          }
          if(any(dat_classes[,1] %in% "logical")){

            w = which(dat_classes[,1] %in% "logical")
            nm_rm <- names(dat_classes[w,1])
            nms_use = nms_use[!nms_use %in% nm_rm]
          }


          shiny::selectInput(inputId = "plot_by",label = "Plot locations by:",
                             #choices = names(onedat$onedata),
                             choices = nms_use,
                             selected =  "TagID", multiple = FALSE)
        })

        # --------------------------------------------- #
        # colours need defining on first time run:
        # duplicate code to below

        if(is.null(col)){
          pal <- leaflet::colorFactor(palette = "RdYlGn", domain = onedat$onedata[,which(names(onedat$onedata) == "TagID")])
        } else{

          if(length(col) == 1){col <- rep(col,length(unique(onedat$onedata[,which(names(onedat$onedata) == "TagID")])))}

          if(length(col) != length(unique(onedat$onedata[,which(names(onedat$onedata) == "TagID")]))){

            paste0(
              "Colour length = ", length(col),
              " != ",
              length(
                unique(
                  onedat$onedata[,which(names(onedat$onedata) == "TagID")]
                )
              ),
              " taking appropriate no. of colours from that supplied"
            )

            col0 <- col[length(unique(onedat$onedata[,which(names(onedat$onedata) == "TagID")]))]

            pal <- leaflet::colorFactor(palette = col0, domain = onedat$onedata[,which(names(onedat$onedata) == "TagID")])

          } else{
            # so if colouor arguments met, then plot by custom selection
            pal <- leaflet::colorFactor(palette = col, domain = onedat$onedata[,which(names(onedat$onedata) == "TagID")])

          }

        }

        # ------------------------ #
        # animal colour palette to start with

        if(is.null(col_anim)){

          pal_anim <- leaflet::colorFactor(palette = "RdYlGn", domain = onedat$onedata$TagID)

        } else{

          if(length(col_anim) == 1){col_anim <- rep(col_anim, length(unique(onedat$onedata$TagID)))}

          if(length(col_anim) != length(unique(onedat$onedata$TagID))){

            paste0(
              "Colour length = ", length(col_anim),
              " != ",
              length(unique(onedat$onedata$TagID)),
              " taking appropriate no. of colours from that supplied"
            )

            # this isn't quite right yet, i.e. if specifying more than actually available will fall over
            col_anim0 <- col_anim[length(unique(onedat$onedata$TagID))]
            pal_anim <- leaflet::colorFactor(palette = col_anim0, domain = onedat$onedata$TagID)

          } else{
            # so if colouor arguments met, then plot by custom selection
            pal_anim <- leaflet::colorFactor(palette = col_anim, domain = onedat$onedata$TagID)

          }

        }

        pal_use$variable <- pal
        pal_use$anim <- pal_anim
        onedat$pal <- pal_anim # for data explorer

        attr(pal_use, "plot_by") <- plotdata$plot_by

        pal_use$this <-  pal_anim
        pal_use$type <-  "animal"

        pal_use$this4lines <- leaflet::colorFactor(palette = "lightgrey", domain = onedat$onedata[,which(names(onedat$onedata) == "TagID")])

      }

    }

  })


  #########################
  ######### shapes ########
  #########################

  ##### reactive shape display

  output$shape <- shiny::renderUI({

    if(!is.null(shapes)){

      shpno <- length(shapes)
      choicesS <- paste("shape",1:shpno)

      choicesS <- c('All', choicesS)
      #htmltools::tagList(
      shiny::selectInput(inputId = "shape", label = "shape", choices = choicesS, selected = "All")
      #)

    } else{
      return(
        shiny::renderText("No shape")
      )
    }
  })

  ##############################################################################################################################
  ###############                                 REACTIVE MAP ELEMENTS                                         ################
  ##############################################################################################################################
  # e.g. tiles, zoom etc

  # ------------------------------------------- #
  # leaflet map reactive
  map_reactive <- reactive({

    if("GoogleEarth" %in% Providers){
      Providers_GE <- "GoogleEarth"
      Providers <- Providers[Providers != "GoogleEarth"]
    }

    m = leaflet::leaflet() %>% leaflet::leaflet(
      options = leaflet::leafletOptions(
        preferCanvas = TRUE, zoomControl = FALSE,  attributionControl=FALSE
      )
    )

    # ADDITIONAL
    # moving the +- button: https://stackoverflow.com/questions/71013017/move-zoom-controls-in-leaflet-for-r-shiny
    # this is to allow the trips selector menu to sit more neatly at the top left of the map
    if(input$zoom_control){
      m <- m %>%
        htmlwidgets::onRender( # java script code goes in the JsCode arg
          "function(el, x) {
                              L.control.zoom({position:'topright'}).addTo(this);
                          }")

    }

    # --------------------------- #
    # Layer tile options
    # --------------------------- #

    # add Google Earth Layer if exists, and make sure layer names reflect this addition
    if(exists("Providers_GE")){
      m <- m %>% leaflet::addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",
                                   attribution = 'Google',
                                   options = leaflet::providerTileOptions(updateWhenIdle = TRUE, updateWhenZooming = FALSE))  %>%
        leafem::addMouseCoordinates()
    }
    for(i in 1:length(Providers)){
      m <- m %>% leaflet::addProviderTiles(Providers[i], group = Providers[i],
                                           options = leaflet::providerTileOptions(
                                             updateWhenIdle = FALSE,
                                             updateWhenZooming = FALSE,
                                             providerTileOptions(zIndex=-10), # because TIA raster overlays do not work otherwise: https://stackoverflow.com/questions/34159578/raster-image-goes-below-base-layer-while-markers-stay-above-xindex-is-ignored
                                             updateWhenZooming = FALSE)
      )
    }
    if(exists("Providers_GE")){
      Providers <- c(Providers, "GoogleEarth")
    }
    m <- m %>% leaflet::addLayersControl(
      baseGroups = Providers,
      #overlayGroups = "overlay",
      options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>% # autoZIndex because TIA raster overlays do not work otherwise
      #leaflet::setView(lat = 55, lng = -4, zoom = 9) %>%
      leaflet::mapOptions(zoomToLimits = "never") %>%
      leaflet::addScaleBar(position = "bottomleft")

    ########## Look into this
    #?leaflet.extras2::antpathOptions

    if(!is.null(onedat$onedata)){

      m <- m %>% leaflet::setView(lat = mean(onedat$onedata$latitude), lng = mean(onedat$onedata$longitude), zoom = 9)
    } else{
      m <- m %>% leaflet::setView(lat = 56.1, lng = -3.2, zoom = 7)
    }


    # add right mouse click
    # https://stackoverflow.com/questions/60750953/r-shiny-leaflet-right-click-context-menu
    m <- m  %>%
      htmlwidgets::onRender("
    function(el,x) {
        mymap = this;
        mymap.on('contextmenu', function(e) {
        var coords = {lng: e.latlng.lng, lat: e.latlng.lat}
        Shiny.setInputValue('mymap_right_click', coords);
    });
    }
    ")

    m$dependencies <- c(m$dependencies, leaflet:::leafletAwesomeMarkersDependencies())

    #m <- m %>%   addEasyprint(options = easyprintOptions( exportOnly = TRUE ))

    m

  })

  # ------------------------------------------- #
  # not really needed but rembering a right clight if needed for something in future
  shiny::observeEvent(input$map_right_click, {
    output$myLng<- renderText({ input$map_right_click$lng })
  })

  # ------------------------------------------- #
  # reactive listener for size adjustments (requires map redrawing)
  output$map_height <- shiny::renderUI({
    shiny::textInput(inputId = "map_height",
                     label= "Map height:",
                     width = '50%',
                     value = cds$plot_h)  # cds is updated automatically below in next reactive
  })
  output$map_width <- shiny::renderUI({
    shiny::textInput(inputId = "map_width",
                     label= "Map width:",
                     width = '50%',
                     value = cds$plot_w)  # cds is updated automatically below in next reactive
  })

  # ------------------------------------------- #
  # Final map rendering outputs

  ##### there is a well known issue with mapdeck if you re-render it is not easy to destroy the content so F12
  ##### inspection produces a load of too many active webgl contexts
  ##### current set up here is most stable so far - although you still have to click reset map after the switch between MB and LF but it's
  ##### NO ERRORS in F12

  # original....  option for user at the start on map fire off
  needed <- reactiveValues(first = NULL, mapOutput_md = NULL, mapOutput_lf = NULL) # needed$first strarts off as NULL on initial fire.

  mymapExists <- reactiveVal(FALSE)

  #### initial reactive map set up for output
  output$mymap <- leaflet::renderLeaflet({

    mymapExists(TRUE)

    map_reactive()

  })

  output$mymap_md <- mapdeck::renderMapdeck({
    if(!is.null(onedat$onedata)){
      m <- m <- mapdeck::mapdeck(style = mapdeck_style("light"), location = c(mean(onedat$onedata$longitude), mean(onedat$onedata$latitude)), zoom = 9, max_pitch = 60)
    } else{
      m <- m <- mapdeck::mapdeck(style = mapdeck_style("light"), location = c(0.4, 52.5), zoom = 7)
    }
    m
  })

  # specifying as a UI store here for feeding back to UI DOES NOT PLOT THE GPS LINES
  #output$mapOutput_lf <- shiny::renderUI({
  needed$mapOutput_lf <- shiny::renderUI({
    leafgl::leafglOutput("mymap", width = cds$plot_w, height = cds$plot_h) #"100%"
  })

  # specifying as a UI store here for feeding back to UI DOES NOT PLOT THE GPS LINES
  #output$mapOutput_md <- shiny::renderUI({
  needed$mapOutput_md <- shiny::renderUI({
    mapdeck::mapdeckOutput(outputId = "mymap_md", width = cds$plot_w, height = cds$plot_h)
  })

  which_one <- eventReactive(input$mapdeck_switch,{

    #browser()

    if(input$mapdeck_switch == "Leaflet"){

      if(!is.null(needed$mapOutput_lf)){
        return(needed$mapOutput_lf)
      }
    }
    if(input$mapdeck_switch == "Mapdeck"){

      if(!is.null(needed$mapOutput_md)){
        mymapExists(TRUE)
        return(needed$mapOutput_md)
      }

    }

  })

  output$mapOutput <- renderUI({
    which_one()
  })


  ###############################################
  # other options tested
  # OPTION 1
  # lines for mapdeck not plotting

#  observeEvent(input$mapdeck_switch, {
#    if(input$mapdeck_switch == "Leaflet"){
#      print("test Leaflet")
#      #mm <- leafgl::leafglOutput("mymap", width = cds$plot_w, height = cds$plot_h) #"100%"
#      output$mymap <- leaflet::renderLeaflet({
#        map_reactive()
#      })
#      output$mapOutput <- shiny::renderUI({
#        leafgl::leafglOutput("mymap", width = cds$plot_w, height = cds$plot_h) #"100%"
#      })
#      #output$mapOutput <- NULL
#    } else{
#      print("test Mapdeck")
#      #mm <- mapdeck::mapdeckOutput(outputId = "mymap_md", width = cds$plot_w, height = cds$plot_h)
#      output$mymap_md <- mapdeck::renderMapdeck({
#        if(!is.null(onedat$onedata)){
#          m <- m <- mapdeck::mapdeck(style = mapdeck_style("light"), location = c(mean(onedat$onedata$longitude), mean(onedat$onedata$latitude)), zoom = 9)
#        } else{
#          m <- m <- mapdeck::mapdeck(style = mapdeck_style("light"), location = c(0.4, 52.5), zoom = 7)
#        }
#        m
#      })
#      output$mapOutput <- shiny::renderUI({
#        mapdeck::mapdeckOutput(outputId = "mymap_md", width = cds$plot_w, height = cds$plot_h)
#      })
#      #output$mapOutput <- NULL
#    }
 # }, priority = 10)


  ###############################################

  # OPTION 2 - this has issues with mapdeck not loading up the line layer on first map fire off!
#  output$mymap <- leaflet::renderLeaflet({
#    map_reactive()
#  })
#
#  output$mymap_md <- mapdeck::renderMapdeck({
#    if(!is.null(onedat$onedata)){
#      m <- m <- mapdeck::mapdeck(style = mapdeck_style("light"), location = c(mean(onedat$onedata$longitude), mean(onedat$onedata$latitude)), zoom = 9)
#    } else{
#      m <- m <- mapdeck::mapdeck(style = mapdeck_style("light"), location = c(0.4, 52.5), zoom = 7)
#    }
#    m
#  })
#
#  output$mapOutput <- shiny::renderUI({
#    if(input$mapdeck_switch == "Leaflet"){
#      print("test Leaflet")
#      x <- leafgl::leafglOutput("mymap", width = cds$plot_w, height = cds$plot_h) #"100%"
#    } else{
#      print("test Mapdeck")
#      x <- mapdeck::mapdeckOutput(outputId = "mymap_md", width = cds$plot_w, height = cds$plot_h)
#    }
#    return(x)
#  })



  ##############################################
  # OPTION 3
  # reactive use, unstable

#  map_ <- eventReactive(input$mapdeck_switch,{
#    if(input$mapdeck_switch == "Leaflet"){
#      #print("test Leaflet")
#      #mm <- leafgl::leafglOutput("mymap", width = cds$plot_w, height = cds$plot_h) #"100%"
#      mm <- leaflet::renderLeaflet({
#        map_reactive()
#      })
#    }
#    if(input$mapdeck_switch == "Mapdeck"){
#      mm <- mapdeck::renderMapdeck({
#        if(!is.null(onedat$onedata)){
#          m <- m <- mapdeck::mapdeck(style = mapdeck_style("light"), location = c(mean(onedat$onedata$longitude), mean(onedat$onedata$latitude)), zoom = 9)
#        } else{
#          m <- m <- mapdeck::mapdeck(style = mapdeck_style("light"), location = c(0.4, 52.5), zoom = 7)
#        }
#        m
#      })
#    }
#    mm
#  })
#
#  UI_output <- reactive({
#    if(input$mapdeck_switch == "Leaflet"){
#      print("test Leaflet")
#      x <- leafgl::leafglOutput("mymap", width = cds$plot_w, height = cds$plot_h) #"100%"
#    } else{
#      print("test Mapdeck")
#      x <- mapdeck::mapdeckOutput(outputId = "mymap_md", width = cds$plot_w, height = cds$plot_h)
#    }
#    return(x)
#  })
#
#  output$mapOutput <- shiny::renderUI({
#    if(input$mapdeck_switch == "Leaflet"){
#      output$mymap <- map_()
#    }
#    if(input$mapdeck_switch == "Mapdeck"){
#      output$mymap_md <- map_()
#    }
#    UI_output()
#  })


  ############################################################################################
  ############################################################################################
  ############################################################################################
  ############################################################################################
  ############################################################################################
  # MAP RESETTER
  ############################################################################################
  ############################################################################################
  ############################################################################################
  ############################################################################################
  ############################################################################################

  observeEvent({

    input$reset_map
  },{

    #skipPlot(1)
    #skipPlot2(1)

    if(!grepl("%", input$map_height)){
      cds$plot_h <- as.numeric(input$map_height)
    } else{
      cds$plot_h <-input$map_width
    }

    if(!grepl("%", input$map_width)){
      cds$plot_w <- as.numeric(input$map_width)
    } else{
      cds$plot_w <-input$map_width
    }


    if(!is.null(input$mapdeck_switch)){

      if(input$mapdeck_switch == "Leaflet"){

        reactivetest$leaflet_legno = reactivetest$leaflet_legno + 1 # resetting the mapbox legend ticker otherwise will not plot after map refreshed

        #print("output mymap leaflet: map_reactive()")

        output$mymap <- leaflet::renderLeaflet({
          map_reactive()
        })

        #leaflet::leafletProxy("mymap")

      } else if(input$mapdeck_switch == "Mapdeck"){

        reactivetest$mapbox_legno = reactivetest$mapbox_legno + 1 # resetting the mapbox legend ticker otherwise will not plot after map refreshed

        #print("output mymap_md mapdeck:renderMapdeck")

      }

    }

  #print("end of reset map observer")

  }, ignoreInit = TRUE) #, priority = 10

  ############################################################################################
  ############################################################################################
  ############################################################################################
  ############################################################################################
  ############################################################################################
  # MAP SAVING
  ############################################################################################
  ############################################################################################
  ############################################################################################
  ############################################################################################
  ############################################################################################

  ################ MAP SAVING OPTIONS AN EASYPRINT FROM LEAFLET.EXTRAS2
  ### update easyprint options if using leaflt to save map
  ### so if changing he position of the easyprint, as it is rendered initially in the reactive element, it needs removing and re-adding

  observeEvent({

    input$mapsave_position
    input$mapsave_filename
    input$HideControlContainer
    input$hidden

  },{

    # this works to add the Easyprint here because taking advantage of the fact the observer here is fired off
    # at the start, so no point having easyprint in the reactive above (although works) as it would render twice
    if(!is.null(input$mapsave_position)){
      if(!is.null(input$mapsave_filename)){
        if(!is.null(input$HideControlContainer)){

          if(input$mapdeck_switch == "Leaflet"){

            leafletProxy("mymap") %>% removeEasyprint() %>%
              addEasyprint(options = easyprintOptions(
                title = 'Give me that map',
                position = input$mapsave_position,
                exportOnly = TRUE,
                #hideClasses = list("leaflet-control-zoom"), #leaflet-overlay-pane", "leaflet-popup",
                hidden = input$hidden,
                hideControlContainer = input$HideControlContainer,
                filename = input$mapsave_filename,
                tileLayer = "OpenStreetMap",
                tileWait = 5000,
                defaultSizeTitles = list(
                  "CurrentSize" = "The current map extent",
                  "A4Landscape" = "A4 (Landscape) extent with w:1045, h:715",
                  "A4Portrait" = "A4  (Portrait) extent with w:715, h:1045"
                ),
                # sizeModes = c("A4Portrait","A4Landscape"),
                sizeModes = list("CurrentSize" = "CurrentSize",
                                 "A4Landscape" = "A4Landscape",
                                 "A4Portrait" = "A4Portrait",
                                 "Custom Landscape"=list(
                                   width= 1800,
                                   height= 700,
                                   name = "Custom landscape (w:1800, h:700)",
                                   className= 'customCssClass'),
                                 "Custom Portrait"=list(
                                   width= 700,
                                   height= 1800,
                                   name = "Custom portrait (w:700, h:1800)",
                                   className= 'customCssClass1')
                ),
                customWindowTitle = "Some Fancy Title",
                customSpinnerClass = "shiny-spinner-placeholder",
                spinnerBgColor = "#b48484"))

            #  leafletProxy("map") %>%
            #    clearControls()
          }

        }
      }


    }


  })

  observeEvent({

    input$savemap

  },{

    if(input$mapdeck_switch == "Leaflet"){
      leafletProxy("mymap") %>%

        #leaflet::addScaleBar(position = input$scalebar_pos,  options = scaleBarOptions(metric = input$scalebar_met, imperial = input$scalebar_imp)) %>%
        addEasyprint(options = easyprintOptions(hidden = TRUE,  exportOnly = TRUE, hideControlContainer = FALSE)) %>%
        easyprintMap(sizeModes = input$user_map_size, filename =  input$mapsave_filename)

    }

  })


  ######################################################
  ############## DYNAMIC SERVER ELEMENTS ###############
  ######################################################
  # e.g clearing map, points slider, toggles

  # --------------------------- #
  # CLEAR MAP
  # --------------------------- #
  # shape layer IDs
  layerId2 = paste("foo",1:(length(shapes)+1))

  shiny::observeEvent({
    input$clear
  },{

    #search any names called foo followed by a number - lines get out of sync sometimes!

    if(!is.null(input$mapdeck_switch)){

      if(input$mapdeck_switch == "Leaflet"){
        leaflet::leafletProxy("mymap") %>%
          leaflet::clearMarkers() %>%
          leaflet::clearControls() %>%
          leafgl::removeGlPoints("two")

        # for polyline switch of methods
        leaflet::leafletProxy("mymap") %>%
          #leafgl::removeGlPolylines(layerId = cl_reactive$current_layers)
          leafgl::removeGlPolylines(layerId = paste0("foo", 1:10000)) %>%
          leaflet::removeShape(layerId = paste0("foo", 1:10000))

      }

      if(input$mapdeck_switch == "Mapdeck"){

        mapdeck::mapdeck_update(map_id = "mymap_md") %>%
          mapdeck::clear_path(layer_id = "linesplotted") %>%
          mapdeck::clear_pointcloud(layer_id = "scatter_layer") %>%
          mapdeck::clear_trips(layer_id = paste0("animated_lines_")) # it is likely that clear_trips() works best in the most recent version of mapdeck

      }

    }

    shinyWidgets::updateAwesomeCheckbox(session=session, inputId="ptog", label = "Fixes", value = FALSE)
    shinyWidgets::updateAwesomeCheckbox(session=session, inputId="ltog", label = "Lines", value = FALSE)

  })


  ####################################################################################################################
  ####################################################################################################################
  ####################################################################################################################
  ####################################################################################################################
  # COLOUR PALETTE SWITCHES
  ####################################################################################################################
  ####################################################################################################################
  ####################################################################################################################
  ####################################################################################################################

  ############################## THE COLOUR MODULE ##########################################

  #inputs = input$builtin_pal, palette_type = "animal"
  colour_module <- function(ID=NULL, fun = NULL, inputs = NULL, palette_type = "animal", ...) {

    moduleServer(
      ID,
      ## Below is the module function

      function(input, output, session) {

        #print(paste0("Observer: colour_pal_leaf (+ plotby) choices from user for ", palette_type))

        # get the current palette being used before any box changes etc on the front end
        if(palette_type == "animal"){
          pal_anim <- pal_use$animal
        }
        if(palette_type == "variable"){
          pal_anim <- pal_use$variable
          attr(pal_use, "plot_by") <- plotdata$plot_by
        }
        if(palette_type == "lines"){
          pal_anim <- pal_use$this4lines
        }

        reactivetest$pal2do <- pal_anim
        reactivetest$palette_type <- palette_type
        reactivetest$inputs <- inputs # this will be the input$colour_pal_leaf and input$colour_pal_leaf_plotby and line plot equivalent

        if(palette_type == "animal"){
          DOMAIN = onedat$onedata$TagID
        }
        if(palette_type == "variable"){

          if(!is.null(input$rescale_plotby)){
            if(!is.null(plotdata$TagID)){
              if(input$rescale_plotby){

                #if(plotdata$TagID == reactivetest$TagID){
                  # if TRUE, then also look at the animals selected for the scale to plot
                  DOMAIN = onedat$onedata[onedat$onedata$TagID %in% plotdata$TagID,]
                  DOMAIN = DOMAIN[,which(names(DOMAIN) == input$plot_by)]
                #} else{
                #  DOMAIN = onedat$onedata[,which(names(onedat$onedata) == input$plot_by)]
                #}

              } else{
                DOMAIN = onedat$onedata[,which(names(onedat$onedata) == input$plot_by)]
              }
            } else{
              DOMAIN = onedat$onedata[,which(names(onedat$onedata) == input$plot_by)]
            }

          } else{
            DOMAIN = onedat$onedata[,which(names(onedat$onedata) == input$plot_by)]
          }

        }

        if(palette_type == "lines"){

          if(is.null(plotdata$plot_by)){
            DOMAIN = onedat$onedata$TagID
          } else{

            if(plotdata$plot_by == "TagID"){
              DOMAIN = onedat$onedata$TagID
            } else{
              DOMAIN = onedat$onedata[,which(names(onedat$onedata) == input$plot_by)]
            }

          }

        }

        ##############################################################################################
        # 02/05/2025: need to assess character/factor or numeric of the variable being plotted
        # best as a switch to be made by the use
        # although only even plot factors as such and integer has option of being both

        # input$colour_factor is from the radio button choice either colorFactor or colorNumeric

        # note what do do for...DateTime...and other types??


        #x = DOMAIN
        Use4cf <- function(x){

          if(class(x)[1] %in% c("character", "factor")){

            use4cf <- "colorFactor" # always plotting as a leaflet::colorFactor()

          }

          if(class(x)[1] %in% c("numeric")){

            use4cf <- "colorNumeric" # if user chose factor but variable is numeric, have to plot as numeric realistically....
          }


          if(class(x)[1] %in% c("integer")){

            if(length(unique(x)) > 50){ # arbitrary max cutoff
              use4cf <- "colorNumeric"
            } else{
              use4cf <- input$colour_factor # integer is the one where it could be either...
            }
          }

          if(class(x)[1] %in% c("POSIXct","POSIXt")){
            use4cf <- "colorNumeric"
          }

          if(!class(x)[1] %in% c("character","numeric","factor","integer","POSIXct","POSIXt")){
            use4cf <- "colorFactor" # not 100% sure
          }

          # if x = DOMAIN = TagID THIS HAS TO BE character
          # error occurs when doing css below using PAL stared palette if numeric will not work as that
          # always needs to be the TagID and character


          if(!is.null(input$plot_by)){
            if(input$colour_factor == "colourNumeric" & input$plot_by == "TagID"){

              # this will not work with the animal set up
              # this arises if the plot_by variable is selected and is numeric
              # need to set back to factor
              use4cf <- "colorFactor"
            }
          }
          return(use4cf)
        }

        use4cf <- Use4cf(DOMAIN)

        colorRampMR <- function(x, domain, Palette){
          if(x == "colorFactor"){
            if(Palette[1] == "UvA"){
              return(leaflet::colorFactor(palette = c("wheat","purple","green3","red","orange","blue","brown","yellow2","cyan","grey", "darkgrey"), domain = domain))
            } else if(Palette[1] == "Okabe-Ito"){
              return(leaflet::colorFactor(palette = palette(), domain = domain))
            } else{
              return(leaflet::colorFactor(palette = Palette, domain = domain))
            }
          }
          if(x == "colorNumeric"){

            if(Palette[1] == "UvA"){
              return(leaflet::colorNumeric(palette = c("wheat","purple","green3","red","orange","blue","brown","yellow2","cyan","grey", "darkgrey"), domain = domain))
            } else if(Palette[1] == "Okabe-Ito"){
              return(leaflet::colorNumeric(palette = palette(), domain = domain))
            } else{
              return(leaflet::colorNumeric(palette = Palette, domain = domain))
            }
          }

        }

        ##############################################################################################

        if(is.null(inputs)){
          #pal_anim <- leaflet::colorFactor(palette = "RdYlGn", domain = DOMAIN)
          pal_anim <- colorRampMR(use4cf, Palette = "RdYlGn", domain = DOMAIN)

        }

        #print(paste0("Test2, inputs = ", inputs))

        # check for any spaces, make commas, but some special cases where we don't want that to happen for palette usage, stupid issue with spaces and numeric the way I have done it
        pal2use <- NULL

        if(grepl("Dark 2", inputs)){pal2use <- "Dark 2"}
        if(grepl("Pastel 1", inputs)){pal2use <- "Pastel 1"}
        if(grepl("Pastel 2", inputs)){pal2use <- "Pastel 2"}
        if(grepl("Set 1", inputs)){pal2use <- "Set 1"}
        if(grepl("Set 2", inputs)){pal2use <- "Set 2"}
        if(grepl("Set 3", inputs)){pal2use <- "Set 3"}
        if(grepl("Tableau 10",inputs)) {pal2use <- "Tableau 10"}
        if(grepl( "Classic Tableau", inputs)){pal2use <- "Classic Tableau"}
        if(grepl("Polychrome 36", inputs)){pal2use <- "Polychrome 36"}
        vars <- inputs
        vars <- gsub(" ",",",inputs)
        vars <- strsplit(vars, ",")

        #print(paste0("TESTING",vars[[1]]))

        #print(vars)

        if(length(vars[[1]]) > 0){

          ###### set names
          if(all(vars[[1]] %in% colors() | vars[[1]] %in% c("viridis","RdYlGn","magma","inferno", "plasma",
                                                            "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys",
                                                            "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples",
                                                            "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd","UvA","Okabe-Ito"
          ))){

            #pal_anim <- leaflet::colorFactor(palette =  vars[[1]], domain = DOMAIN)
            pal_anim <- colorRampMR(use4cf, Palette = vars[[1]], domain = DOMAIN)

          }

          ###### hex options
          if(any(grepl("#",vars))){

            # get which vals are hex
            vars0 <- vars[[1]]
            #vars0 <- c("#FF0000", "#FF1256", "red", "#526", "#FF000011")
            #vars0 <- c("#FF0000", "#FF1256", "#526", "#FF000011")

            # only take either 6 or 8 hex values
            # which are hex
            hex = which(grepl("#",vars0) & (nchar(vars0) == 7 | nchar(vars0) == 9))

            # which are NOT hex and ARE in the colour list
            colsinlist = which(vars0 %in% colors())

            # keep hex 7 or 9 nchar AND colours in list
            vars0 <- vars0[sort(c(hex,colsinlist))]

            if(length(vars0) > 0){
              #pal_anim <- leaflet::colorFactor(palette =  vars0, domain = DOMAIN)
              pal_anim <- colorRampMR(use4cf, Palette = vars0, domain = DOMAIN)

            }

          }

          ###### topo.colors etc reeeallly bad repetition
          if(any(grepl("topo.colors",vars[[1]]))){

            if(any(grepl("[[:digit:]]", vars[[1]]))){

              numval = as.numeric(gsub("\\D", "", vars[[1]])) # javascript \D meta character! Ok

              #pal_anim <- leaflet::colorFactor(palette = topo.colors(numval), DOMAIN)
              pal_anim <- colorRampMR(use4cf, Palette = topo.colors(numval), domain = DOMAIN)

            }

          }
          if(any(grepl("heat.colors",vars[[1]]))){

            if(any(grepl("[[:digit:]]", vars[[1]]))){

              numval = as.numeric(gsub("\\D", "", vars[[1]])) # javascript \D meta character! Ok

              #pal_anim <- leaflet::colorFactor(palette = heat.colors(numval), DOMAIN)
              pal_anim <- colorRampMR(use4cf, Palette = heat.colors(numval), domain = DOMAIN)
            }

          }
          if(any(grepl("rainbow",vars[[1]]))){

            if(any(grepl("[[:digit:]]", vars[[1]]))){

              numval = as.numeric(gsub("\\D", "", vars[[1]])) # javascript \D meta character! Ok

              #pal_anim <- leaflet::colorFactor(palette = rainbow(numval), DOMAIN)
              pal_anim <- colorRampMR(use4cf, Palette = rainbow(numval), domain = DOMAIN)

            }

          }
          if(any(grepl("terrain.colors",vars[[1]]))){

            if(any(grepl("[[:digit:]]", vars[[1]]))){

              numval = as.numeric(gsub("\\D", "", vars[[1]])) # javascript \D meta character! Ok

              #pal_anim <- leaflet::colorFactor(palette = terrain.colors(numval), DOMAIN)
              pal_anim <- colorRampMR(use4cf, Palette = terrain.colors(numval), domain = DOMAIN)


            }

          }
          if(any(grepl("cm.colors",vars[[1]]))){

            if(any(grepl("[[:digit:]]", vars[[1]]))){

              numval = as.numeric(gsub("\\D", "", vars[[1]])) # javascript \D meta character! Ok

              #pal_anim <- leaflet::colorFactor(palette = cm.colors(numval), domain = DOMAIN)
              pal_anim <- colorRampMR(use4cf, Palette = cm.colors(numval), domain = DOMAIN)

            }

          }

          #######################
          # "palette.colours" e.g. palette.colors(4, "Tableau 10")
          # check if user tried to feed an alpha - would be numeric and mess up the way I handle things here
          # plus we have that anyway on the plotting
          if(any(grepl("palette.colors",vars[[1]]))){

            #print(paste("palette.colors test"))

            vars0 <- vars[[1]]

            if(is.null(pal2use)){
              # first check if a pallette was given
              check_pals <- list()
              for(i in 1:length(palette.pals())){
                check_pals[[i]] <- grepl(palette.pals()[i],vars0)
              }
              check_pals = do.call('rbind',check_pals)
              test = apply(check_pals,MARGIN = 1, function(x){(any(x))})
              test = lapply(test, function(x){
                if(length(x) == 0) "none"
                else x
              })
              check_pals = do.call('rbind',test)
              pal2use <- palette.pals()[which(check_pals)]

              if(length(pal2use) > 2){
                warning("Taking only the first palette given")
                pal2use <- pal2use[1]
              }

              if(length(pal2use) > 0){
                # strip out the palette, i.e. some are with numbers such as R3 so would mess up search for other number
                vars0 <- gsub(pal2use,"",vars0)
              }
              if(length(pal2use) == 0){
                pal2use <- "Okabe-Ito"
              }
            }


            # search for number
            if(any(grepl("[[:digit:]]", vars0)[1])){

              numval = as.numeric(gsub("\\D", "", vars0))
              numval <- as.numeric(na.omit(numval))

              if(length(numval) > 0){

                if(use4cf <- "colorFactor"){
                  pal_anim <- leaflet::colorFactor(palette = palette.colors(n = numval[1], palette = pal2use), domain = DOMAIN)
                } else if(use4cf <- "colorNumeric"){
                  pal_anim <- leaflet::colorNumeric(palette = palette.colors(n = numval[1], palette = pal2use), domain = DOMAIN)
                }


              } else{

                if(use4cf <- "colorFactor"){
                  pal_anim <- leaflet::colorFactor(palette = palette.colors(n = NULL, palette = pal2use), domain = DOMAIN)
                } else if(use4cf <- "colorNumeric"){
                  pal_anim <- leaflet::colorNumeric(palette = palette.colors(n = NULL, palette = pal2use), domain = DOMAIN)
                }
              }


            } else{
              if(use4cf <- "colorFactor"){
                pal_anim <- leaflet::colorFactor(palette = palette.colors(n = NULL, palette = pal2use), domain = DOMAIN)
              } else if(use4cf <- "colorFactor"){
                pal_anim <- leaflet::colorNumeric(palette = palette.colors(n = NULL, palette = pal2use), domain = DOMAIN)
              }
            }

          }

          ##### "palette" only
          if(!any(grepl("palette.colors",vars[[1]]))){

            if(any(grepl("palette",vars[[1]]))){

              #if(!grepl("[[:digit:]]", vars[[1]]) ){
              # get value between brackets character assuming this is one of the defaults
              # cheating here https://statisticsglobe.com/extract-characters-between-parantheses-r

              x_extract1 <- gsub("[\\(\\)]",          # Extract characters within parentheses
                                 "",
                                 regmatches(vars[[1]],
                                            gregexpr("\\(.*?\\)",
                                                     vars[[1]]))[[1]])


              if(is.null(pal2use) & length(x_extract1) > 0){

                # same-ish code as above for color.palette
                check_pals <- list()
                for(i in 1:length(palette.pals())){
                  check_pals[[i]] <- grepl(palette.pals()[i],vars[[1]])
                }
                check_pals = do.call('rbind',check_pals)
                test = apply(check_pals,MARGIN = 1, function(x){(any(x))})
                test = lapply(test, function(x){
                  if(length(x) == 0) "none"
                  else x
                })
                check_pals = do.call('rbind',test)
                pal2use <- palette.pals()[which(check_pals)]

              }

              if(length(x_extract1) > 0){

                if(x_extract1 == ""){

                  #pal_anim <- leaflet::colorFactor(palette = palette(), domain = DOMAIN) # default
                  pal_anim <- colorRampMR(use4cf, Palette = palette(), domain = DOMAIN)

                } else{

                  #pal_anim <- leaflet::colorFactor(palette = palette(pal2use), domain = DOMAIN)
                  pal_anim <- colorRampMR(use4cf, Palette = palette(pal2use), domain = DOMAIN)
                }
              }
              if(length(x_extract1) == 0){

                #pal_anim <- leaflet::colorFactor(palette = palette(), domain = DOMAIN) # default
                pal_anim <- colorRampMR(use4cf, Palette = palette(), domain = DOMAIN)
              }

            }

          }

        }

        if(palette_type == "animal"){

          pal_use$anim <- pal_anim # not sure if still needed! storing twice at the moment...hangup from previously
          pal_use$this  <-  pal_anim
          pal_use$type <-  palette_type
        }

        if(palette_type == "variable"){

          pal_use$variable <- pal_anim
          pal_use$this  <-  pal_anim
          pal_use$type <-  palette_type

          attr(pal_use, "plot_by") <- plotdata$plot_by
        }

        if(palette_type == "lines"){
          pal_use$lines <- pal_anim
          pal_use$this4lines  <-  pal_anim # have to have separate "this" for the line paletter


          # not updating anything type-wise with lines ? I don't think....confusing
        }

        # but if lines, then say we switch to variable sat_used, this line colour will be used = not right
        # so also need to work with "palette_type"

        reactivetest$curr_pal_type <- palette_type # palette type last used on function run
        reactivetest$TagID <- plotdata$TagID # grab the TagIDs last used on function run

        reactivetest$use4cf <- use4cf

        if(palette_type != "lines"){

          #### SAVE THE CURRENT input that was last used...if not lines
          reactivetest$curr_pal_input_points <- inputs

        } else{
          reactivetest$curr_pal_input_lines <- inputs
        }

      }

    )
  }

  # ---------------------------------------------------------------------------- #
  # THE BOXES IN THE DROP DOWN MENU
  # UI: shinyWidgets::dropdown |> shiny::uiOutput("colour_pal_leaf") and shiny::uiOutput("colour_pal_leaf_plotby")
  # ---------------------------------------------------------------------------- #

  # colour plotter
  reactivetest <- reactiveValues(pal2do = NULL, palette_type = NULL, inputs = NULL, mapbox_legno = 1, leaflet_legno = 1, mapdeck_line_no = 1) #colour_pal_leaf_plotby = c("red yellow green pink"), colour_pal_leaf = "red blue green"

  ############################################################################################
  # build reactive UI server-side for text boxes (see below for line colour palette text box input "colour_lines")

  # This is for a custom COLOUR RAMP to feed into leaflet::colorFactor
  # or code that is accepted by the function and will be parsed
  output$colour_pal_leaf <- shiny::renderUI({

    # if col_anim is given by the user, have that feed into the text box, else red, blue, green

    if(!is.null(col_anim)){
      shiny::textInput(inputId = "colour_pal_leaf", value = col_anim, label= "Custom paletter", width = "100%")  # cds is updated automatically below in next reactive
    } else{
      shiny::textInput(inputId = "colour_pal_leaf", value = c("red","blue","green"), label= "Custom paletter", width = "100%")  # cds is updated automatically below in next reactive
    }

  })

  output$colour_pal_leaf_plotby <- shiny::renderUI({

    # if col_anim is given by the user, have that feed into the text box, else red, blue, green

    if(!is.null(col)){
      shiny::textInput(inputId = "colour_pal_leaf_plotby", value = col, label= "Custom paletter", width = "100%")  # cds is updated automatically below in next reactive
    } else{
      shiny::textInput(inputId = "colour_pal_leaf_plotby", value = c("red","yellow","green", "pink"), label= "Custom paletter", width = "100%")  # cds is updated automatically below in next reactive
    }

  })

  #################################### THE TEXT BOXES ########################################
  #### 1. colour_pal_leaf
  ####### observing colours for TagID via the colour_module
  observeEvent({
    input$colour_pal_leaf
    #reactivetest$colour_pal_leaf

  },{


    if(!is.null(input$colour_pal_leaf)){
      if(!is.null(reactivetest$init_cpl)){ # if the initial init value of the drop down box for colour_pal_leaf is NULL, then do NOTHING ON THE FIRST RENDER
        colour_module(inputs = input$colour_pal_leaf, palette_type = "animal") #input$colour_pal_leaf
      }

    }

    if(input$lcol_samepoints){
      pal_use$this4lines <- pal_use$this
    }

    reactivetest$init_cpl <- 1 # change this from NULL so next time run, it will work as intended
  })

  #### 2. colour_pal_leaf_plotby
  #### observing colours for the plotby variable also using the colour module
  observeEvent({
    input$colour_pal_leaf_plotby
    #reactivetest$colour_pal_leaf_plotby

  },{

    if(!is.null(input$colour_pal_leaf_plotby)){
      if(!is.null(reactivetest$init_cplpb)){ # if the initial init value of the drop down box for colour_pal_leaf_plotby is NULL, then do NOTHING ON THE FIRST RENDER
        colour_module(inputs = input$colour_pal_leaf_plotby, palette_type = "variable")
      }

    }

    if(input$lcol_samepoints){
      pal_use$this4lines <- pal_use$this
    }

    reactivetest$init_cplpb <- 1

  })

  ################################# RADIO BUTTON PRESENTS ###################################
  # 3. listen also to radio presets - maxes for both variable and TagID as so easy to change

  use4cf <- eventReactive(input$colour_factor,{

    if(pal_use$type == "animal"){
      domain = onedat$onedata$TagID
    }
    if(pal_use$type == "variable"){
      domain = onedat$onedata[,which(names(onedat$onedata) == plotdata$plot_by)]
    }

    if(class(domain) %in% c("character", "factor")){

      use4cf <- "colorFactor" # always plotting as a leaflet::colorFactor()

    }

    if(class(domain) %in% c("numeric")){

      use4cf <- "colorNumeric" # if user chose factor but variable is numeric, have to plot as numeric realistically....
    }

    if(class(domain) %in% c("integer")){

      if(length(unique(domain)) > 50){ # arbitrary max cutoff
        use4cf <- "colorNumeric"
      } else{
        use4cf <- input$colour_factor # integer is the one where it could be either...
      }
    }

    if(class(domain) %in% c("POSIXct","POSIXt")){
      use4cf <- "colorNumeric"
    }

    if(!class(domain) %in% c("character", "factor","integer","POSIXct","POSIXt")){
      use4cf <- "colorFactor" # not 100% sure
    }
    use4cf
  })

  observeEvent({
    input$builtin_pal
  },{

    if(!is.null(input$plot_by)){
      if(input$plot_by != "TagID"){pal_use$type <- "variable"} else{pal_use$type <- "animal"}
    }

    if(!is.null(pal_use$type)){
      if(pal_use$type == "animal"){
        colour_module(inputs = input$builtin_pal, palette_type = "animal")
        }
      if(pal_use$type == "variable"){
        colour_module(inputs = input$builtin_pal, palette_type = "variable")
        }
    } else{

      pal_anim <- leaflet::colorFactor(palette = c("wheat","purple","green3","red","orange","blue","brown","yellow2","cyan","grey", "darkgrey"), domain = onedat$onedata$TagID)
      pal_use$anim <- pal_anim
      pal_use$this  <-  pal_anim
      reactivetest$curr_pal_input_points <- "UvA"
    }

    # update lines if same4fixes is selected

    if(input$lcol_samepoints){
      pal_use$this4lines <- pal_use$this
    }

    updateRadioButtons(session, inputId="builtin_pal2", selected = character(0))

  })

  observeEvent({
    input$builtin_pal2
  },{

    if(!is.null(input$plot_by)){
      if(input$plot_by != "TagID"){pal_use$type <- "variable"} else{pal_use$type <- "animal"}
    }
    if(!is.null(pal_use$type)){

      if(pal_use$type == "animal"){
        colour_module(inputs = input$builtin_pal2, palette_type = "animal")
      }
      if(pal_use$type == "variable"){
        colour_module(inputs = input$builtin_pal2, palette_type = "variable")
      }
    } else{

      pal_anim <- leaflet::colorFactor(palette = c("wheat","purple","green3","red","orange","blue","brown","yellow2","cyan","grey", "darkgrey"), domain = onedat$onedata$TagID)
      pal_use$anim <- pal_anim
      pal_use$this  <-  pal_anim
      reactivetest$curr_pal_input_points <- "UvA"
    }

    if(input$lcol_samepoints){
      pal_use$this4lines <- pal_use$this
    }

    updateRadioButtons(session, inputId="builtin_pal", selected = character(0))

  })

  # Still need, point line widths, point shapes, point line colours

  ############################################################################
  # LINE COLOURS
  # box to take the colours for lines for custom selection

  output$colour_lines <- shiny::renderUI({

    shiny::textInput(inputId = "colour_lines", value = "grey", label= "Custom paletter", width = "100%")  # cds is updated automatically below in next reactive

  })

  #############################################################################
  # 02/05/2025
  # option to choose between plotting as a factor and numeric via a switch
  # output$colour_factor # rendered UI side as with other radio buttons
  # check out what happens when the nn4 dropdown is first fired off...
  # this is odd so if it is NULL and then the INITIAL values in the boxes
  # we want the colour_module to NOT fire off, when the menu is first initiated.
  # so we would have to remember the value before and the current one and test both?
  # i.e. if NULL before and now NOT NULL then DO NOTHING...

  observeEvent(input$nn4, {

    reactivetest$init_cplpb <- input$colour_pal_leaf_plotby
    reactivetest$init_cpl <- input$colour_pal_leaf
    reactivetest$init_cl <-input$colour_lines

  })

  # Issue highlighted through mapdeck for plotting line colours.
  #use4cf() # if this is numeric but say then you select an integer variable, mapdeck
  # is trying to use a NUMERIC line paletter, as
  # pal_use$this4lines <- pal_use$this
  # is effectively chosen.... how?? we ALWAYS want to keep the line palette as the TagID surely?

  ####### observing colours for TagID via the colour_module
  observeEvent({
    input$colour_lines
    input$lcol_samepoints # if switch activated as TRUE
    #use4cf() # does this also need to monitor a change in the input$colour_factor?
  },{

    if(input$lcol_samepoints){
      # use same palette from current points selection
      pal_use$this4lines <- pal_use$this
    } else{

      if(!is.null(input$colour_lines)){
        if(!is.null(reactivetest$init_cl)){ # if the initial init value of the drop down box for colour_lines is NULL, then do NOTHING ON THE FIRST RENDER
          colour_module(inputs = input$colour_lines, palette_type = "lines")
        }

      } else{
        pal_use$this4lines <- pal_use$this
      }

    }

    reactivetest$init_cl <- 1
  })


  ###############################################################################
  #### Then monitor the change in input$plot_by, which is the user picking of the variable to plot

  shiny::observeEvent({
    input$plot_by
  },{

    ############################################################################################
    # PICKER VARIABLE FOR PLOT BY, e.g. choosing to plot 4 satellites only
    #
    # COLOURATION OF POINTS ACCORDING TO VARIABLE CHOSEN AND LEVELS THEREIN
    # col can be supplied as a palette or list of colours matching length of levels
    # Inefficient as the observer will have to do this

    # also pointless having this plotting if the plot by variable is numeric, unless you can
    # pick a range to plot as a slider!
    # turning this option off for now though with numeric plotting

    if(!is.null(onedat$onedata)){

      onedata0 <- onedat$onedata # work with the original data - note in any further updates of onedata eg. in read ins this also needs accounting for, i.e. create a new onedata NOT reactive but in open session
      plotbycol <- onedata0[,which(names(onedata0) == input$plot_by)]

      # if the input color_factor is specified as colorFactor then try and render the plotby_var picker
      if(input$colour_factor == "colorFactor"){

        # but... if the variable you are trying to plot is numeric or has a long vector length
        # i.e. if the user had mis-specified the column as character in a numeric format then
        # disable the plot by var picker? But then how long is a piece of string. perhaps not overly code the second length element

        var_class <- class(plotbycol)

        if(!var_class[1] %in% c("numeric")){

          if(any(is.na(plotbycol))){

            plotbycol <- ifelse(is.na(plotbycol), "NoLab", as.vector(plotbycol))

            choices = c("All", unique(as.character(plotbycol)))

          } else{
            choices = c("All",unique(as.vector(plotbycol)))
          }

          # only building the second variable picker if plot_by is anything other than TagID

          if(input$plot_by != "TagID"){

            # for some reason update version of the picker doesn't work
            output$plotby_var <- shiny::renderUI({

              shinyWidgets::pickerInput(inputId = "plotby_var",
                                        label = NULL,
                                        choices = choices,
                                        selected = "All",
                                        multiple = TRUE)

            })

          } else{
            output$plotby_var <- NULL

          }
        } else{
          output$plotby_var <- NULL

        }

      } else{
        output$plotby_var <- NULL

      }

      onedat$onedata <- onedata0

    }

    ############################################################################################
    # When we initially switch from TagID default to plotby
    # there is initially no colour palette defined
    # we need a better way of assessing the CURRENT palette and if switched, we keep whatever
    # is currently selected

    # if != TagID ...
    if(input$plot_by != "TagID"){
      if(is.null(reactivetest$nonTag)){ # this will NULL at first
        # then take the previous plot palette FOR POINTS

        if(reactivetest$curr_pal_input_points == "UvA"){
          pal_anim <- leaflet::colorFactor(palette = c("wheat","purple","green3","red","orange","blue","brown","yellow2","cyan","grey", "darkgrey"), domain = onedat$onedata[,which(names(onedat$onedata) == input$plot_by)])
        } else{
          colour_module(inputs = reactivetest$curr_pal_input_points, palette_type = "variable")
        }

        if(reactivetest$curr_pal_input_points == "Okabe-Ito"){
          pal_anim <- leaflet::colorFactor(palette = palette(), domain = onedat$onedata[,which(names(onedat$onedata) == input$plot_by)])
        } else{
          colour_module(inputs = reactivetest$curr_pal_input_points, palette_type = "variable")
        }

      }
      reactivetest$nonTag <- 1 # this will NULL at first, so set to 1 so above process can never run again
    }

    #############################################################################################
    # if the user switches to a plotby variable and then back to TagID, on return, the
    # lines palette will STILL be looking for the previous domain as it will not be updated
    # so as above, we need to re evaluate the line palette
    if(!is.null(input$colour_lines)){
      colour_module(inputs = input$colour_lines, palette_type = "lines")
    }

    ############################################################################################
    # we also need to trigger colour changes even if the palette is the same
    # otherwise the palette carried through will be on a different scale for
    # the previous plotby variable

    # this is the last known colour palette used

    if(pal_use$type == "animal"){
      colour_module(inputs = reactivetest$curr_pal_input_points, palette_type = "animal")
    }
    if(pal_use$type == "variable"){
      colour_module(inputs = reactivetest$curr_pal_input_points, palette_type = "variable")
    }

  })


  ###############################################################################
  # If plotting with a by variable and rescaling the colour ramp within the TagID
  # when switching back to another TagID the colour ramp is not updated again following
  # a TagID switch
  # unfortunately I think that may need another separate listener...
  # i.e. use the current colour palette but scale for the new TagID
  # BUT..... currently this is working AFTER the interim step of first trying to plot the
  # old bird with the NEW palette!

  observeEvent({
    input$TagID # monitoring this as it is never not NULL
    input$rescale_plotby
    },{

      if(!is.null(input$rescale_plotby)){
        if(!is.null(input$TagID)){
          if(!is.null(input$plot_by)){

            # CURRENT PALETTE TYPE: reactivetest$curr_pal_type <- palette_type
            # CURRENT PALETTE: reactivetest$curr_pal_input_points

            # we only want to do this for the plotby condition:
            if(input$plot_by != "TagID"){
              colour_module(inputs = reactivetest$curr_pal_input_points, palette_type = reactivetest$curr_pal_type)

            }

          }

        }
      }

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # A trick to make initial mapping happen with NULL inputs (pass to reactive values)
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # not convinced about this original idea with options to ignoreinit etc

  observe({

    if(is.null(plotdata$data)){
      plotdata$data <- onedat$onedata
    }

    if(is.null(input$TagID)){
      plotdata$TagID <- unique(plotdata$data$TagID)[1]
    } else{
      plotdata$TagID <- input$TagID
    }

    if(is.null(input$plot_by)){
      plotdata$plot_by <- "TagID"
    } else{
      plotdata$plot_by <- input$plot_by
    }

    if(is.null(input$plotby_var)){
      plotdata$plotby_var <- "All"
    } else{
      plotdata$plotby_var <- input$plotby_var
    }

  })

  ############################################################################
  #########                     SLIDER                                ########
  ############################################################################

  shiny::observeEvent({
    plotdata$TagID
    plotdata$plot_by
    plotdata$plotby_var

    onedat$onedata
  },{

    TAGID <- plotdata$TagID
    PLOTBY <- plotdata$plot_by
    PLOTBY_VAR <- plotdata$plotby_var

    if(!is.null(PLOTBY)){
      if(PLOTBY == "TagID"){PLOTBY_VAR <- "All"}
    } else{
      PLOTBY <- "TagID"
      PLOTBY_VAR <- "All"
    }

    if(!is.null(TAGID)){

      if(any(TagID != 'All')){
        test <- onedat$onedata[onedat$onedata$TagID %in% TAGID, ]
      } else{
        test <- onedat$onedata
      }
      test$TagID <- as.factor(test$TagID)

      # which plotby variable are we looking at?
      if(!is.null(PLOTBY) && PLOTBY %in% onedat$onedata[,which(names(onedat$onedata) %in% PLOTBY)]){

        # and what level of that variable?
        if(!is.null(PLOTBY_VAR)){

          if(any(PLOTBY_VAR != 'All')){

            # AND then we have to make sure if any NAs these are labelled as NoLab
            PLOTBY_VAR <- ifelse(PLOTBY_VAR == "NoLab", NA, PLOTBY_VAR)

            #test <- indata[indata[,which(names(indata) %in% PLOTBY)] %in% PLOTBY_VAR, ]
            test <- onedat$onedata[onedat$onedata[,which(names(onedat$onedata) %in% PLOTBY)] %in% PLOTBY_VAR, ]
          }
          # otherwise we select the full data for all levels of the plotby variable used, i.e. do no subsetting

        }

      }

      if(!is.null(onedat$onedata)){

        sliderParams <- reactiveValues(
          max = as.Date(max(test$DateTime, na.rm = TRUE)),
          min = as.Date(min(test$DateTime, na.rm = TRUE)),
          value = as.Date(range(test$DateTime, na.rm = TRUE)))

        if(!is.null(slider_check_base$st) && !is.null(slider_check_base$en)){

          # check if the current sldier range is within the bird range

          if(slider_check_base$st >= min(test$DateTime) &
             slider_check_base$en <= max(test$DateTime)
          ){
            # then value of slider updated for this range rather than bird max min
            sliderParams$value <- c(slider_check_base$st, slider_check_base$en)

          }

        }

      } else if(is.null(onedat$onedata)){

        # placeholder range if nothing specified
        sliderParams <- reactiveValues(
          max = as.Date("2014-05-01 12:00:00"),
          min = as.Date("2014-07-01 12:00:00"),
          value = as.Date(c("2014-05-01 12:00:00","2014-07-01 12:00:00")))
      }

      output$slider <- shiny::renderUI({

        #sliderInput or updateSliderInput?
        shiny::sliderInput(inputId = "slider",
                           label = "Select time range",
                           value = sliderParams$value,
                           min = sliderParams$min,
                           max = sliderParams$max,
                           width = "100%")

      })

    }

  })

  # ---------------------------------------------- #
  # If the user opts to display fixes as a timeline - this will over-ride the normal fix and line plotting
  # and the timeline - but should be memory conservative only displaying x fixes per y
  # https://github.com/trafficonese/leaflet.extras2/blob/master/inst/examples/playback_app_divicon.R
  # Given awesome markers is limited on the colours allowed, we need a reactive css
  # that can respond to the current colour palette to then feed through to the
  # markers displayed in the playback() routine
  # I think this sort of example is probably best: https://stackoverflow.com/questions/65585282/change-css-properties-based-on-shiny-input
  # refraining at the moment from using separate www/css files
  # here, uiOutput("css_markers"), is in the ui
  # and we need to have the colours generated for whatever combination of
  # animals we have and the current reactive colour palette
  # the colours passed through to L.divIcon instead and can be called anything
  # as this will get picked up through the "CssClass"
  # so.... first need to work out what TagIDs are currently displayed
  # and from the colouor pallette, work out their hex colours
  # also note for use in the way intended with the switch, it needs to be character, so appending X to the start
  #css_color <- reactiveValues(nms = paste0("X",plotdata$TagID[1]), cols = "red")

  JS_code_last <- reactiveValues(use = "
        }
        return {
          icon: L.divIcon({
            html: '<div class= \"divicon2 ' + CssClass + '\">' + text + '</div>'
          })
        }
        }")


  observeEvent({
    input$marker_type

  },{

    if(input$marker_type == "pin"){
      JS_code_last$use <- "
              }
              return {
                icon: L.divIcon({
                  html: '<div class= \"divicon ' + CssClass + '\">' + text + '</div>',
                  iconAnchor: [-5, 5]
                })
              }
              }"
    }
    if(input$marker_type == "circle"){
      JS_code_last$use <- "
              }
              return {
                icon: L.divIcon({
                  html: '<div class= \"divicon2 ' + CssClass + '\">' + text + '</div>'
                })
              }
              }"
    }


  })

  # style4markers is a reactive function dependent on the current plotdata2 settings and colour palette selected
  style4markers <- reactive({

    # note if "All" selected, this is taken care of elsewhere and results in the full data in the reactive plotdata$data2

    # guessing this may be needed as reactive could fire off at the start and find this NULL
    if(!is.null(onedat$onedata$TagID)){

      #if(is.null(pal_use$this)){
      if(is.null(reactivetest$curr_pal_input_points)){
        PAL <- leaflet::colorFactor(palette =  c("red","blue","green"), domain = onedat$onedata$TagID)
      } else{
        #PAL <- pal_use$this
        PAL <- leaflet::colorFactor(palette =  reactivetest$curr_pal_input_points, domain = onedat$onedata$TagID)

      }
     #thedata <- thedata()
     thedata_ <- check_onedata$d

      #if(!is.null(plotdata$data2)){
      if(!is.null(thedata_)){

        # automatically detect the animals and colours

        #cols2use <- unique(data.frame(cols = PAL(plotdata$data2$TagID),
        #                              TagID = plotdata$data2$TagID))

        cols2use <- unique(data.frame(cols = PAL(thedata_$TagID),
                                      TagID = thedata_$TagID))

        #cols2use$cols <- gsub("#","0x",cols2use$cols)
        cols2use$cols <- tolower(cols2use$cols)
        cols2use$TagID <- paste0("X",cols2use$TagID) # this was line 1577 above.... not sure why error happened but app seems OK and in animation/colours!
        cols2use$css_nm <- paste0("color", 1:nrow(cols2use)) # note dot not needed for CssClass name

        # and store this is its own reactive? Actually not sure if that is necessary as it is built afresh in the observer too
        # loop through the TagIDs to dynamically build the css

        # first main style part
        style_part1 <- "
                     .leaflet-div-icon {
                         background: transparent !important;
                         border: unset !important;
                     }
                     "

        if(input$marker_type == "circle"){
          marker_icon <-
            "
            .divicon2 {
              border: 1px solid #666;
              border-radius: 50%;
              font-size: 14px;
              color: black;
              text-align: center;
              width: 20px !important;
              height: 20px !important;
              padding-top: 3px;
              font-weight: 800;
            }
           "

        }
        if(input$marker_type == "pin"){
          marker_icon <-
            "
            .divicon {
              width: 20px;
              height: 20px;
              border-radius: 50% 50% 50% 0;
              background: #c30b82;
              position: absolute;
              transform: rotate(-45deg);
              margin: -15px 0 0 -15px;
            }
            "
          #left: 50%;
          #top: 50%;

        }
        style_part1 <- paste0(style_part1,marker_icon)

        #.leaflet-div-icon {
        #  background: transparent !important;
        #  border: unset !important;
        #}
        #.divicon {
        #  border: 1px solid #666;
        #  border-radius: 50%;
        #  font-size: 14px;
        #  color: black;
        #  text-align: center;
        #  width: 20px !important;
        #  height: 20px !important;
        #  padding-top: 3px;
        #  font-weight: 800;
        #}

        # build the dynamic part per animal and color needed
        for(i in 1:nrow(cols2use)){
          if(i == 1){
            css2add <- paste0( paste0( ".color", i ), "{\n ", "background-color: ",cols2use$cols[i], ";\n ", "}\n ")
          } else{
            css2add <- paste0(css2add, paste0( paste0( ".color", i ), "{\n ", "background-color: ",cols2use$cols[i], ";\n ", "}\n ") )
          }
        }

        # build full css
        css2use <- paste0(
          style_part1, "\n \n", css2add
        )

        #tags$head(tags$style(css2use))

        return(css2use)

      }

    }

  })

  # output the css for the markers based on the style4markers() reactive
  output$css_markers <- renderUI({
    tags$head( # line 1672 in error above
      tags$style(
        #HTML(
        #  paste0(c(style4markers(), css), collapse = "\n")
        #)
        style4markers()

      )
    )
  })

  # then in the observeEvent for the playback(), we need to grab the css_color reactiveValues
  # to build the js part of the script dynamically as well!

  observeEvent({

    input$switch2timeline
    #plotdata$data2
    thedata()
    input$timeline_speed
    pal_use$this
    input$marker_type
    input$maxInterpolationTime
    input$tickLen

    # further Mapdeck elements
    pal_use$this4lines
    input$trail_length
    input$stroke_width
    input$stroke_opacity
    input$mapdeck_switch

  },{

    if(input$switch2timeline){
      thedata_ <- isolate(thedata())

      shinybusy::add_busy_spinner(spin = "fading-circle")

      if(!is.null(thedata_)){
        # source the reactive data2 element, carry out some basic data prep
        dat_here <- thedata_

        dat_here2 <- MoveRakeR::simplify(dat_here)
        names(dat_here2)[which(names(dat_here2) == "DateTime")] <- "time"
        dat_here2$label <- paste0("Time: ", dat_here2$time)
        dat_here2$popup = sprintf("<b>TagID</b>: %s<br><b>DateTime</b>: %s",
                                  dat_here2$TagID, dat_here2$time)
        dat_here2$TagID <- paste0("X",dat_here2$TagID)
        row.names(dat_here2) <- 1:nrow(dat_here2)

        # make the POINTS object to plot
        pts0 = sf::st_as_sf(dat_here2, coords = c("longitude", "latitude"), crs = 4326)
        if(length(unique(dat_here2$TagID)) > 1){
          pts <- split(pts0, f = pts0$TagID)
        } else{
          pts = pts0
        }

        # so here we are using the CssClass name that will have a matching hex colour in the dynamically
        # built css in the ui, and we can grab the colours

        # names of the TagIDs will be carried through from the pts point layer into the "case" argument
        cols2use <- unique(data.frame(cols = pal_use$this(dat_here2$TagID), TagID = dat_here2$TagID))
        #cols2use$cols <- gsub("#","0x",cols2use$cols)
        cols2use$cols <- tolower(cols2use$cols)
        cols2use$css_nm <- paste0("color", 1:nrow(cols2use))

        JS_code_1st <- paste0("function(feature) {
                                                 var CssClass = 'blue';
                                                 var text = null;
                                                 switch (feature.name) {\n ")


        for(i in 1:nrow(cols2use)){
          if(i == 1){
            js2add <- paste0( "case '", cols2use$TagID[i], "':\n ", "CssClass = '", cols2use$css_nm[i], "';\n  text = '';\n break;\n ")
          } else{
            js2add <- paste0(js2add, paste0( "case '", cols2use$TagID[i], "':\n ", "CssClass = '", cols2use$css_nm[i], "';\n  text = '';\n break;\n ") )
          }
        }

        JS_code <- paste0(JS_code_1st, js2add, JS_code_last$use)

        if(!is.null(input$mapdeck_switch)){
          if(input$mapdeck_switch == "Leaflet"){

          leaflet::leafletProxy("mymap") %>%
            leaflet.extras2::addPlayback(data = pts,
                                         popup = ~popup,
                                         label = ~label,
                                         name = ~TagID,
                                         popupOptions = popupOptions(offset=c(0,-35)),
                                         #labelOptions  = labelOptions(offset=c(8,-10)),
                                         options = playbackOptions(radius = 3,
                                                                   tickLen = input$tickLen,
                                                                   speed = input$timeline_speed,
                                                                   marker =  htmlwidgets::JS(JS_code),
                                                                   maxInterpolationTime = input$maxInterpolationTime,
                                                                   tracksLayer = FALSE,
                                                                   transitionpopup = TRUE,
                                                                   transitionlabel = FALSE,
                                                                   playCommand = "Animate",
                                                                   stopCommand = "Stop"),
                                         pathOpts = pathOptions(weight = 5))
          }


          if(input$mapdeck_switch == "Mapdeck"){

            dat_here_sf <- sf::st_as_sf(dat_here, coords = c("longitude", "latitude"), crs = 4326)
            dt <- st_coordinates(dat_here_sf)
            dt <- data.table::as.data.table(dt)

            dt$M <- dat_here$DateTime
            dt$TagID <- dat_here$TagID

            if(input$elev_toggle){

              if(exists("altitude_agl",dat_here)){
                dt$Z  <- dat_here$altitude_agl  # agl DEM uva
              }
              if(exists("altitude",dat_here)){
                dt$Z <- dat_here$altitude # general uva
              }
              if(exists("height.above.msl",dat_here)){
                dt$Z  <- dat_here$height.above.msl # Movebank
              }
              if(!exists("altitude_agl",dat_here) & !exists("altitude",dat_here) & !exists("height.above.msl",dat_here)){
                dt$Z  <- 0
              }
              if(all(is.na(dat_here$altitude))){
                dt$Z  <- 0
              }
            } else{
              dt$Z  <- 0
            }

            # first remove the previous lines
            mapdeck::mapdeck_update(map_id = "mymap_md") %>%
              mapdeck::clear_trips(layer_id = paste0("animated_lines_"))

            # turn on the points the same as lines for this
            shinyWidgets::updateAwesomeCheckbox(session=session, inputId="lcol_samepoints", label = "Lines", value = TRUE)

            lines2plot_animated <- sfheaders::sf_linestring(
              obj = dt
              , x = "X"
              , y = "Y"
              , z = "Z"
              , m = "M"
              , linestring_id = "TagID"
            )
            lines2plot_animated <- sf::st_set_crs(lines2plot_animated, 4326)

            pal2use4lines <- pal_use$this4lines
            COL <- pal2use4lines(dat_here[,which(names(dat_here) == "TagID")])
            #COL = colorspace::adjust_transparency(COL, alpha = input$stroke_opacity)
            # need to add the alphas to the hex for mapdeck (?)

            lines2plot_animated$COL <- unique(COL)

            shinyWidgets::updateAwesomeCheckbox(session=session, inputId="ptog", label = "Fixes", value = FALSE)
            shinyWidgets::updateAwesomeCheckbox(session=session, inputId="ltog", label = "Lines", value = FALSE)

            #session
            mapdeck::mapdeck_update(map_id = "mymap_md") %>%
              mapdeck::add_trips(
                data = lines2plot_animated
                , animation_speed = input$timeline_speed
                , trail_length = input$trail_length
                , stroke_width = input$stroke_width*5
                , opacity = input$stroke_opacity
                , stroke_colour = "COL"
                , layer_id = paste0("animated_lines_")
                , palette = NULL
              )

          }

        }

      }

    }


    if(!input$switch2timeline){

      if(!is.null(input$mapdeck_switch)){

        if(input$mapdeck_switch == "Leaflet"){
          leaflet::leafletProxy("mymap") %>%
            leaflet.extras2::removePlayback()
        }

        if(input$mapdeck_switch == "Mapdeck"){

          mapdeck::mapdeck_update(map_id = "mymap_md") %>%
            mapdeck::clear_trips(layer_id = paste0("animated_lines_"))

          shinyWidgets::updateAwesomeCheckbox(session=session, inputId="ptog", label = "Fixes", value = TRUE)
          shinyWidgets::updateAwesomeCheckbox(session=session, inputId="ltog", label = "Lines", value = TRUE)

        }

      }

    }

  })


  shiny::observeEvent({
    input$switch2timeline
    input$mapdeck_switch

  },{

    if(!is.null(input$mapdeck_switch)){
      if(input$mapdeck_switch == "Mapdeck"){

      if(!input$switch2timeline){

        # in previous mapdeck version I had trouble removing the animated_lines, despite clear_trips existing as a function
        # but this works using the latest version, so the non-Cran version may be the best option here

        mapdeck::mapdeck_update(map_id = "mymap_md") %>%
          mapdeck::clear_trips(layer_id = "animated_lines") #, update_view = FALSE)

        #shinyWidgets::updateAwesomeCheckbox(session=session, inputId="ptog", label = "Fixes", value = TRUE)
        #shinyWidgets::updateAwesomeCheckbox(session=session, inputId="ltog", label = "Lines", value = TRUE)

        }

      }

    }

  },ignoreInit = TRUE)


  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  # FILTER
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################

  # these whould probably be separate modules in their own right
  # idea is to filter by something in the data, e.g. five sats, AND etc etc YEAR
  # which lends itself more to an SQL query!
  # so is that neater to have an sql box pop up that the user
  # can enter SQL into? treating the data read in as a database??
  # this would then have to be the current data selected

  # Is there a way of also assessing levels to subset data by in a filter way rather plot colouring way?
  # sort of already done in our plotby option but could have to option to assess
  # character variables in data and filter by a, b, c, variables
  # so year could be one level, then sat number, then something else and so forth?
  # like Excel's addlevel option
  # so two boxes side-by-side, one drop down list to select variables, then another to show the levels of the variable
  # an addlevel button pressed adds another row of the two boxes, that take the same data from above, and subset again

  # inspiration: https://stackoverflow.com/questions/54352046/how-to-get-the-correct-inputid-while-using-insertui-in-shiny
  # set up reactive to capture number for id increments

  filt_query <- reactiveValues(num = 0, current_data = isolate(check_onedata$d)) #isolate(plotdata$data2)

  # removing UI
  observeEvent(input$removelevel, ignoreNULL = TRUE, ignoreInit = TRUE, {

    num <- filt_query$num

    # Don't let the user remove the very first Row
    #if (num == 1) {
    #  return()
    #}

    removeUI(selector = paste0("#filter", num))
    filt_query$num <- filt_query$num - 1

    # if removing via the div selector as above
    #removeUI(selector = "#level2do")
  })

  # adding UI element
  # note SHiny training - can you do this? I didn't realise!! i.e. adding ignoreNULL etc so no
  # if(NULL) nonesense I get tied up in??
  observeEvent(input$addlevel, ignoreNULL = FALSE, ignoreInit = TRUE, {

    # assess levels of the data (like we do anyway for the plotby) - but this has to be cascading so as needing to see if
    # any of the added select inputs have chosen one of the variables

    # first add one to the reactive value
    filt_query$num <- filt_query$num + 1
    num <- filt_query$num

    shiny::insertUI(
      immediate = TRUE,
      selector = "#addlevel",
      where = "afterEnd", #afterEnd

      # can then use split layout to arrange boxes - should have done that in the trips tab
      # we need only two boxes nothing too fancy, two drop downs, one for the variable and then the choices of levels

      ui = shiny::splitLayout(
        cellWidths = c("50%", "50%"),
        cellArgs = list(style = "padding: 1px"),
        id = paste0("filter", num),

        # then the two elements i.e. 50-50
        shinyWidgets::pickerInput(inputId = paste0("filt_", num),label = "", choices = names(isolate(newdata())), selected =  "", multiple = FALSE), # names(plotdata$data2)

        # but how do we make this also reactive to the elements of the first?
        shinyWidgets::pickerInput(inputId = paste0("filt_lev_", num),label = "", choices = "", selected =  "", multiple = FALSE),

        # this is needed to make sure the dropdown size is not over-ridden
        tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                              }
                              ")))

      )

      # this was an example wrapping up the ui in a div with an ID than can be used to removeUI via the id (in separate observeEvent)
      #ui =
      #  div(
      #    #textInput("txt", "Insert some text"),
      #    shiny::selectInput(inputId = "filt_txt",label = "", choices = names(plotdata$data2), selected =  NULL, multiple = FALSE),
      #    id="level2do"
      #  )

    )

  })

  # as the user may want to change boxes already displayed in the stack, I don't know how to register changes in them
  # other than looping over and seraching for a change! Probably a much neater clever reactive way to do this...

  observe({

    if(filt_query$num > 0){

      for(i in 1:filt_query$num){

        # observe each filt_num id in turn and if changed....do stuff
        observeEvent(input[[paste0("filt_", i)]],{

          if(i == 1){

            #filt_query$current_data <- plotdata$data2
            #filt_query$current_data <- newdata()
            filt_query$current_data <- check_onedata$d

          }

          #filt_query$current_data <- filt_query$current_data[names(filt_query$current_data) == input[[paste0("filt_", i)]],]

          # get levels of the variable
          val <- which(names(filt_query$current_data) == input[[paste0("filt_", i)]])

          var_levs <- unique(filt_query$current_data[,val])

          #print(var_levs)

          # update second drop down for levels
          shinyWidgets::updatePickerInput(session = session, inputId =  paste0("filt_lev_", i),label = "", choices = var_levs, selected =  "")

        })

        # then cycle over the choices
        #print(input[[paste0("filt_lev_", i)]])
        #observeEvent(input[[paste0("filt_", i)]],{
        #
        #})

      }

    }

  })

  # Need to observe changes in ANY of the filt_lev_i's and subset the data for plotting
  # keeping as a separate observe so as not to confuse above input updating

  observe({

    if(filt_query$num == 0){

      #dat <- plotdata$data2
      #dat <- newdata()

      dat <- check_onedata$d
    }

    if(filt_query$num > 0){

      # actually get the values of all the levels
      #dat <- plotdata$data2
      dat <-  newdata()

      for(i in 1:filt_query$num){

        if(!is.null(input[[paste0("filt_lev_", i)]])){

          if(input[[paste0("filt_lev_", i)]] != ""){

            if(nrow(dat[dat[,which(names(dat) %in% input[[paste0("filt_", i)]])] %in% input[[paste0("filt_lev_", i)]], ]) > 0){

              dat <- dat[dat[,which(names(dat) %in% input[[paste0("filt_", i)]])] %in% input[[paste0("filt_lev_", i)]], ]

            }
          }
        }
      }

      plotdata$data3 <- dat

    }

  })

  # ----------------------------------------------- #
  #  # Quick filter by year/month/day
  #  # first assess years in the dataset
  # ----------------------------------------------- #
  # select inputs year_ month_ and day_
  # these need populating for choices from the data we may currently have in plotdata$data2 or data3
  # but they have to be interlinked so one choice of a year limits the available options for

  filter_vals <- reactiveValues(datyear = NULL, datmonth = NULL, datday = NULL, years = NULL, month = NULL, days = NULL)

  output$filter_year <- shiny::renderUI({

    #newdata <- newdata()
    newdata <- check_onedata$d

    #if(!is.null(plotdata$data2)){
    if(!is.null(newdata)){

      #years <- c("none",unique(lubridate::year(plotdata$data2$DateTime)))
      years <- c("none",unique(lubridate::year(newdata$DateTime)))

    }
    #if(!is.null(plotdata$data3)){
    #  years <- unique(lubridate::year(plotdata$data2$DateTime))
    #}
    filter_vals$years <- years
    #shiny::selectInput(inputId = "year_",label = "", choices = years, selected = "", multiple = TRUE)
    shiny::selectInput(inputId = "year_",label = "", choices = years, selected = "", multiple = TRUE)

  })

  output$filter_month <- shiny::renderUI({

    #newdata <- newdata()
    newdata <- check_onedata$d

    #if(!is.null(plotdata$data2)){
    if(!is.null(newdata)){

      #months <- c("none",unique(lubridate::month(plotdata$data2$DateTime)))
      months <- c("none",unique(lubridate::month(newdata$DateTime)))

    }
    #if(!is.null(plotdata$data3)){
    #  months <- unique(lubridate::month(plotdata$data2$DateTime))
    #}
    filter_vals$months <- months
    shiny::selectInput(inputId = "month_", label = "", choices = months, selected =  "", multiple = TRUE)
  })

  output$filter_day <- shiny::renderUI({

    #newdata <- newdata()
    newdata <- check_onedata$d

    #if(!is.null(plotdata$data2)){
    if(!is.null(newdata)){
      #days <- c("none",unique(lubridate::day(plotdata$data2$DateTime)))
      days <- c("none",unique(lubridate::day(newdata$DateTime)))
    }
    #if(!is.null(plotdata$data3)){
    #  days <- unique(lubridate::day(plotdata$data2$DateTime))
    #}
    filter_vals$days <- days
    shiny::selectInput(inputId = "day_", label = "", choices = days, selected =  "", multiple = TRUE)
  })

  ########
  # update the select inputs for any current data selected

  observeEvent({
    input$year_

  },{

    #if(!is.null(filter_vals$dat)){
    #  # then use whatever data is in the reactive
    #  dat <- filter_vals$dat
    #}

    #newdata <- newdata()
    newdata <- check_onedata$d

    #if(!is.null(plotdata$data2)){
    if(!is.null(newdata)){

      # first time use - is there any plotdata2 data?
      #dat <- plotdata$data2
      dat <- newdata

      if(!is.null(dat)){

        if(!is.null(input$year_)){

          if(!any(input$year_ == "none")){
            # subset data by user selection and look for available days and hours - update

            # at initial load not even data2 is populated so need this to only work if that is also not NULL from last if else
            dat <- dat[lubridate::year(dat$DateTime) %in% input$year_,]

            #if(nrow(dat) > 0){
            #  #filter_vals$dat <- dat
            #
            #  months2 <- unique(lubridate::month(dat$DateTime))
            #  #shiny::updateSelectInput(inputId = "month_", label = "", choices = months2)
            #
            #}
          } else{
            #shiny::updateSelectInput(inputId = "year_", label = "", choices =  filter_vals$years, selected = "none")
            #dat <- plotdata$data2
            dat <- newdata
          }

        } else{

          #dat <- plotdata$data2
          dat <- newdata

        }
        filter_vals$datyear <- dat
      }

      if(nrow(dat) > 0){
        #print(dat)
        plotdata$data3 <- dat

      }

    }

  })

  observeEvent({
    input$month_

  },{

    if(!is.null(filter_vals$datyear)){
      # first time use - is there any plotdata2 data?
      dat <- filter_vals$datyear


      if(!is.null(dat)){

        if(!is.null(input$month_)){

          if(!any(input$month_ == "none")){

            dat <- dat[lubridate::month(dat$DateTime) %in% input$month_,]

          } else{

            dat <- filter_vals$datyear

          }

        } else{
          dat <- filter_vals$datyear

        }

        filter_vals$datmonth <- dat

      }

      if(nrow(dat) > 0){

        plotdata$data3 <- dat

      }

    }
    ##else{
    ##  dat <- plotdata$data2
    ##}

  })

  observeEvent({
    input$day_

  },{

    if(!is.null(filter_vals$datmonth)){
      # first time use - is there any plotdata2 data?
      dat <- filter_vals$datmonth

      if(!is.null(dat)){

        if(!is.null(input$day_)){

          if(!any(input$month_ == "none")){

            dat <- dat[lubridate::day(dat$DateTime) %in% input$day_,]

          } else{

            dat <- filter_vals$datmonth

          }

        } else{
          dat <- filter_vals$datmonth

        }

        filter_vals$datday <- dat

      }

      if(nrow(dat) > 0){
        #print(dat)
        plotdata$data3 <- dat

      }

    }
    ##else{
    ##  dat <- plotdata$data2
    ##}

  })

  # ----------------------- #
  # remove the year filter

  observeEvent(input$reset_year_filter,{

    #plotdata$data3 <- plotdata$data2
    #plotdata$data3 <- newdata()
    plotdata$data3 <- check_onedata$d

    shiny::updateSelectInput(inputId = "year_", label = "", choices =  filter_vals$years, selected = "")
    shiny::updateSelectInput(inputId = "month_", label = "", choices =  filter_vals$months, selected = "")
    shiny::updateSelectInput(inputId = "day_", label = "", choices =  filter_vals$days, selected = "")

  })

  # ----------------------------------------------- #
  # observe a change in the textarea box, and if so, implement the code
  observeEvent({
    input$go_sql
  },{

    output$bad_code <- NULL
    #plotdata$data2_prev <- plotdata$data2

    # also a quick year filter! Or maybe assess month/years available?
    # And then select?
    # listen to go buttton press - check language also

    # ------------------------------------------------- #
    # SQL / R
    # ------------------------------------------------- #

    if(input$go_sql){

      #DATA <- plotdata$data2
      #DATA <- newdata()
      DATA <- check_onedata$d

      if(input$lang_tog == "SQL"){
        first_try = try(sqldf::sqldf(input$sql_filter), silent = TRUE)
      }
      if(input$lang_tog == "R"){
        first_try = try(eval(parse(text = input$sql_filter)), silent = TRUE) # frowned upon but surely OK in these circumstances?
      }

      #DATA[lubridate::year(DATA$DateTime) == 2019,]

      if(!is.null(first_try)){

        #DATA <- rename_TagID(TrackMultiStack2Track(LBBGWalB201416))
        #plotdata$data3 <- sqldf::sqldf('select max(TagID) from data_filt')
        #first_try = try(sqldf::sqldf("select * from DATA where TagID == (253 AND 506)"), silent = TRUE)

        if(any(substr(first_try, 1,5) == "Error")){
          plotdata$data3 <- NULL
          #output$bad_code <- renderText("Invalid sql code")

          output$bad_code <- renderUI(
            HTML(as.character(div(style = "color: red;", "Invalid code")))
          )

          #first_try <- integer(0)
        } else if(nrow(first_try) == 0){
          plotdata$data3 <-  NULL
          #output$bad_code <- renderText("No data")

          output$bad_code <- renderUI(
            HTML(as.character(div(style = "color: red;", "No data")))
          )

        } else{
          # THIS IS WHERE GOOD CODE MAKES IT THROUGH
          first_try <- first_try[!is.na(first_try$longitude),] # safety net for baffoonery
          first_try <- first_try[!is.na(first_try$latitude),]
          if(nrow(first_try) > 0){
            plotdata$data3 <- first_try
            output$bad_code <- NULL
          } else{
            output$bad_code <- renderUI(
              HTML(as.character(div(style = "color: red;", "No data")))
            )
          }

        }

      } else{
        plotdata$data3 <-  NULL
        output$bad_code <- renderUI(
          HTML(as.character(div(style = "color: red;", "No R code entered")))
        )
      }

    }

  })

  observeEvent({
    input$reset_sql
  },{

    if(input$reset_sql){
      plotdata$data3 <-  NULL
      output$bad_code <- NULL
    }

  })


  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  # Day and night distinction
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  # sun periods - dawn/dusk

  shiny::observeEvent(input$daynight_go,{

    # use the MoveRakeR suncalc function
    #test = MoveRakeR::sun_calc(data_in)

    # get the full data
    onedat$onedata <- MoveRakeR::sun_calc(onedat$onedata)
    #thedata() # reactive should update if onedata changes??

    # now check for the presence of input$daynight
    vals = input$daynight # these are to include in day category

    onedat$onedata$daynight <- "night"

    # always class "DA" as day, obviously
    onedat$onedata$daynight <- ifelse(onedat$onedata$dnt %in% ("DA"), "day", onedat$onedata$daynight)

    if("Sunset" %in% vals){
      onedat$onedata$daynight <- ifelse(onedat$onedata$dnt %in% ("SS"), "day", onedat$onedata$daynight)
    }
    if("Sunrise" %in% vals){
      onedat$onedata$daynight <- ifelse(onedat$onedata$dnt %in% ("SR"), "day", onedat$onedata$daynight)
    }
    if("Civil Twilight" %in% vals){
      onedat$onedata$daynight <- ifelse(onedat$onedata$dnt %in% ("CTe"), "day", onedat$onedata$daynight)
      onedat$onedata$daynight <- ifelse(onedat$onedata$dnt %in% ("CTm"), "day", onedat$onedata$daynight)
    }
    if("Nautical Twilight" %in% vals){
      onedat$onedata$daynight <- ifelse(onedat$onedata$dnt %in% ("NTe"), "day", onedat$onedata$daynight)
      onedat$onedata$daynight <- ifelse(onedat$onedata$dnt %in% ("NTm"), "day", onedat$onedata$daynight)
    }
    if("Astronomical Twilight" %in% vals){
      onedat$onedata$daynight <- ifelse(onedat$onedata$dnt %in% ("ATe"), "day", onedat$onedata$daynight)
      onedat$onedata$daynight <- ifelse(onedat$onedata$dnt %in% ("ATm"), "day", onedat$onedata$daynight)
    }

    # update the picker input for plotby

    if(is.null(input$plot_by)){
      selected = "TagID"
    } else{
      selected = input$plot_by
    }

    # for some reason update version of the picker doesn't work
    #output$plot_by <- shiny::renderUI({
    #  shiny::selectInput(inputId = "plot_by",
    #                     label = "Plot locations by:",
    #                     choices = names(onedat$onedata),
    #                     selected = selected,
    #                     multiple = FALSE)
    #})

    # drop any variables that are posixct
    #class(onedat$onedata)

    dat_classes = t(lapply(onedat$onedata, function(x) paste0(class(x), collapse = ', ')) %>% data.frame())

    nms_use = names(onedat$onedata)

    # drop logical or posixct classes
    if(any(grepl("POSIX",dat_classes[,1]))){

      w = grep("POSIX",dat_classes[,1])

      nm_rm <- names(dat_classes[w,1])
      nms_use = nms_use[!nms_use %in% nm_rm]
    }
    if(any(dat_classes[,1] %in% "logical")){

      w = which(dat_classes[,1] %in% "logical")
      nm_rm <- names(dat_classes[w,1])
      nms_use = nms_use[!nms_use %in% nm_rm]
    }


    updateSelectInput(session, inputId = "plot_by",
                      label = "Plot locations by:",
                      #choices = names(onedat$onedata),
                      choices = nms_use,
                      selected = selected
    )

    output$mapOutput <- shiny::renderUI({
      if(input$mapdeck_switch == "Leaflet"){

        x <- leafgl::leafglOutput("mymap", width = cds$plot_w, height = cds$plot_h) #"100%"
        needed$first <- TRUE
      } else{

        x <- mapdeck::mapdeckOutput(outputId = "mymap_md", width = cds$plot_w, height = cds$plot_h)
        needed$first <- TRUE
      }
      return(x)
    })



  })

  # Could also add open weather tiles? requires API, but may be possible
  # tricky though to feed off a single time slider!
  #leaflet::leafletProxy("mymap") %>% addOpenweatherTiles()

  # tides, moon phases?

  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  # Reactive for TagID, and slider range selection
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  # Select TagID and slide range to give current data and update the time slider
  # Note, this does mean a new bird selection above will always reset the slider to the range
  # of that individual... perhaps should have something more reactive to check
  # if that new bird selected can be used for a reactive time selection by the user??
  # THIS GETS plotdata$data2
  # is this better as a reactive

  thedata <- reactive({

    shinybusy::add_busy_spinner(spin = "fading-circle")

  # previous idea cumbersome
  #shiny::observeEvent({
  #  onedat$onedata
  #  plotdata$TagID
  #  plotdata$plot_by
  #  plotdata$plotby_var
  #  input$slider
  #},{

    thedata_ <- NULL

    #thedata <- NULL


    if(all(plotdata$plotby_var %in% "NoLab")){
      PLOTBY_VAR <- NA
    } else{
      PLOTBY_VAR <- plotdata$plotby_var
    }

    # NULL TESTS - note now TaID can't be NULL at the outset
    if(!is.null(onedat$onedata) & !is.null(plotdata$TagID) & !is.null(input$slider) ){

      # is the input TagID in the data we are trying to plot? icluding ALL possibility?
      #(plotdata$TagID == "All" || plotdata$TagID %in% onedat$onedata$TagID) &&
      if(any(plotdata$TagID %in% onedat$onedata$TagID) | any(plotdata$TagID == "All")){

        # is the min value of the slider >= to min of the bird, and same for max value
        if(input$slider[1] >= as.Date(min(onedat$onedata$DateTime)) && input$slider[2] <= as.Date(max(onedat$onedata$DateTime))){

          ## THEN DO STUFF ...

          ## subset the TagID based on the choice
          if(!"All" %in% plotdata$TagID){

            data_2 <- onedat$onedata[onedat$onedata$TagID %in% plotdata$TagID, ]
          } else{

            data_2 <- onedat$onedata
          }

          # further - if there is a subsetter for the other variable column.... and it is NOT "ALL"
          # if all that is true, then make the reactive slider values euqal to the input of the slider
          # if the current plotby subset level can be found in the current plotby variable, then subset it
          # else DON'T

          if(nrow(data_2[data_2[,which(names(data_2) %in% plotdata$plot_by)] %in% PLOTBY_VAR, ]) > 0){

            if(!"All" %in% PLOTBY_VAR){

              data_3 <- data_2[data_2[,which(names(data_2) %in% plotdata$plot_by)] %in% PLOTBY_VAR, ]

            } else{
              data_3 <- data_2 #no subset selected if "All"
            }

            #updateSliderInput(session=session, inputId = "slider",
            #                  value = c(as.Date(min(data_2$DateTime)), as.Date(max(data_2$DateTime))),
            #                  min = as.Date(min(data_2$DateTime)), max = as.Date(max(data_2$DateTime)),
            #)


          } else{
            # otherwise the TagID may have been reselected from a previous subset of a plotby variable, so go back one step higher

            if(!"All" %in% plotdata$TagID){

              data_3 <- onedat$onedata[onedat$onedata$TagID %in% plotdata$TagID, ]
            } else{

              data_3 <- onedat$onedata
            }
          }


          # ----------------------------------------------------------------------------------- #
          # TIME SLIDER OPTIONS
          # I know this is really verbose with repeated options but helped visualise
          # condition 1: are slider choices for st and en BOTH within the current TagID data?
          if(input$slider[1] >= as.Date(min(data_3$DateTime)) && input$slider[2] <= as.Date(max(data_3$DateTime))){

            data_4 <- data_3[as.Date(data_3$DateTime) >= input$slider[1] & as.Date(data_3$DateTime) <= input$slider[2],]
            #plotdata$data2 <- data_4
            thedata_ <- data_4

          }
          # -------------- #
          # Condition 2: start of slider falls outside the data start range, but upper slider value is still within the data range
          if(input$slider[1] < as.Date(min(data_3$DateTime)) && input$slider[2] <= as.Date(max(data_3$DateTime)) && input$slider[2] >= as.Date(min(data_3$DateTime))){

            data_4 <- data_3[as.Date(data_3$DateTime) >= input$slider[1] & as.Date(data_3$DateTime) <= input$slider[2],]
            #plotdata$data2 <- data_4
            thedata_ <- data_4

            updateSliderInput(session=session, inputId = "slider",
                              #value = c(as.Date(min(data_2$DateTime)), as.Date(max(data_2$DateTime))),
                              min = as.Date(min(data_2$DateTime)), max = NULL,
            )
          }

          # -------------- #
          # Condition 3: start of slider falls inside the data start range, but upper slider value is outside
          if(input$slider[1] >= as.Date(min(data_3$DateTime)) && input$slider[1] <= as.Date(max(data_3$DateTime)) && input$slider[2] > as.Date(max(data_3$DateTime))){

            data_4 <- data_3[as.Date(data_3$DateTime) >= input$slider[1] & as.Date(data_3$DateTime) <= input$slider[2],]
            #plotdata$data2 <- data_4
            thedata_ <- data_4

            updateSliderInput(session=session, inputId = "slider",
                              #value = c(as.Date(min(data_2$DateTime)), as.Date(max(data_2$DateTime))),
                              min = NULL, max = as.Date(max(data_2$DateTime)),
            )
          }

          # --------------- #
          # (conditions 4 and 5 result in NO data from time slider choices)

          # Condition 4: both start and end are less than the min date in the data
          if(input$slider[1] < as.Date(min(data_3$DateTime)) && input$slider[2] < as.Date(min(data_3$DateTime))){

            # therefore revert to full data, or last data plotted, i.e. NO UPDATE PASSED ON?
            #plotdata$data2 <- data_3

            updateSliderInput(session=session, inputId = "slider",
                              value = c(as.Date(min(data_2$DateTime)), as.Date(max(data_2$DateTime))),
                              min = as.Date(min(data_2$DateTime)), max = as.Date(max(data_2$DateTime)),
            )

          }

          # Condition 5: both start and end are greater than the max date in the data
          if(input$slider[1] > as.Date(max(data_3$DateTime)) && input$slider[2] > as.Date(max(data_3$DateTime))){

            # therefore revert to full data, or last data plotted, i.e. NO UPDATE PASSED ON?
            #plotdata$data2 <- data_3

            updateSliderInput(session=session, inputId = "slider",
                              value = c(as.Date(min(data_2$DateTime)), as.Date(max(data_2$DateTime))),
                              min = as.Date(min(data_2$DateTime)), max = as.Date(max(data_2$DateTime)),
            )

          }
          # Condition 6, forgot about this one, but start is before data and end is after, so technically in the range

          if(input$slider[1] < as.Date(min(data_3$DateTime)) && input$slider[2] > as.Date(max(data_3$DateTime))){

            data_4 <- data_3[as.Date(data_3$DateTime) >= input$slider[1] & as.Date(data_3$DateTime) <= input$slider[2],]
            #plotdata$data2 <- data_4
            thedata_ <- data_4

            updateSliderInput(session=session, inputId = "slider",
                              #value = c(as.Date(min(data_2$DateTime)), as.Date(max(data_2$DateTime))),
                              min = as.Date(min(data_2$DateTime)), max = as.Date(max(data_2$DateTime)),
            )

          }
          # do we even need to update the slide then if not passing on new data to plotter if outside the range?????

          #check_onedata$d=plotdata$data2 #define it ouside

          if(!exists("data_4")){
            check_onedata$d=data_3 #define it ouside
          } else{
            check_onedata$d=data_4 #define it ouside
          }

          # add the plotby variable as an attribute - for lines to know what we are segmenting by

          #if(!is.null(plotdata$data2)){
          if(!is.null(thedata_)){
            #attr(plotdata$data2,"plotby") <- plotdata$plot_by

            attr(thedata_,"plotby") <- plotdata$plot_by

          }


          #plotdata$data2 <- data_3

          slider_check_base$st = input$slider[1]
          slider_check_base$en = input$slider[2]

        } else{
          thedata_ <- NULL
        }

      } else{
        thedata_ <- NULL
      }

    } else{
      thedata_ <- NULL
    }

    return(thedata_)



  })

  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  # GPS LINES
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################


  skipPlot <- reactiveVal(1) # lines
  skipPlot2 <- reactiveVal(1) # points

  #### first observe event of TagID, slider AND the shapes now (still need to do additional point layer in same way)
  shiny::observeEvent({

    input$mapdeck_switch
    thedata()
    mymapExists()
    #plotdata$data2

    plotdata$data3
    input$ltog
    input$line_opacity
    input$line_width
    #input$leaflet_method
    input$leaflet_method

    input$reset_map
    #skipPlot()
    pal_use$this4lines
    input$elev_toggle # specific to mapdeck for elevation toggle
    input$reset_lines
  },{

    # OK original kudos to this example: https://stackoverflow.com/questions/65839072/forcing-render-order-in-r-shiny
    # This basically skips the very first time the observer fires off even after init and NULL with all the things we are monitoring
    # but this allows the mapdeck base map to load BEFORE the lines plot...
    # but actually found a better way around this now

    #if (isolate(skipPlot()==1)) {
    #  # skip first reactive sequence
    #  skipPlot(0)
    #  # launch next reactive sequence
    #  invalidateLater(1,session)
    #} else {
      #invalidateLater(4000,session)
      #linesplotted(FALSE)

      shinybusy::add_busy_spinner(spin = "fading-circle")

      if(mymapExists()){

      thedata_ <- isolate(thedata())

      if(input$ltog){

        #if(!is.null(plotdata$data2)){
        if(!is.null(thedata_)){

          pal2use4lines <- pal_use$this4lines

          #########################
          # if all that is TRUE, then take the plot data from time slider subsetting above

          if(!is.null(plotdata$data3)){
            thedata_ <- plotdata$data3
          } else{
            #thedata <- plotdata$data2

          }
          onedata3 <- thedata_[order(thedata_$TagID, thedata_$DateTime),]

          #getplotby <- attr(plotdata$data2,"plotby")
          getplotby <- attr(onedata3,"plotby")

          # if error in user-entered custom-paletter
          if(is.null(pal2use4lines)){

            VAL <- onedata3[,which(names(onedata3) %in% getplotby)]

            pal2use4lines <- leaflet::colorFactor(palette =  c("red","blue","green"), domain = VAL)

            pal_use$this4lines<- pal2use4lines

          }


          # ------------------------------------------------ #
          # lines
          # ------------------------------------------------ #

          if(dim(onedata3)[1] > 0){

            if(!is.null(input$mapdeck_switch)){

              if(input$mapdeck_switch == "Mapdeck"){

                # this needs the object to be an sf multi line string object by TagID, or rather plotby
                onedata3_sf <- sf::st_as_sf(onedata3, coords = c("longitude", "latitude"), crs = 4326)
                dt <- sf::st_coordinates(onedata3_sf)
                dt <- data.table::as.data.table(dt)

                if(input$elev_toggle){

                  if(exists("altitude_agl",onedata3)){
                    dt$Z  <- onedata3$altitude_agl  # agl DEM uva
                  }
                  if(exists("altitude",onedata3)){
                    dt$Z <- onedata3$altitude # general uva
                  }
                  if(exists("height.above.msl",onedata3)){
                    dt$Z  <- onedata3$height.above.msl # Movebank
                  }
                  if(!exists("altitude_agl",onedata3) & !exists("altitude",onedata3) & !exists("height.above.msl",onedata3)){
                    dt$Z  <- 0
                  }
                  if(all(is.na(onedata3$altitude))){
                    dt$Z  <- 0
                  }
                }

                if(!input$elev_toggle){
                  dt$Z <- 0
                }

                dt$M <- onedata3$DateTime
                dt$TagID <- onedata3$TagID

                # first remove the previous lines
                mapdeck::mapdeck_update(map_id = "mymap_md") %>%
                  #mapdeck::clear_path(layer_id = "linesplotted")
                  mapdeck::clear_path(layer_id =  paste0("linesplotted",reactivetest$mapdeck_line_no)) %>%
                  mapdeck::clear_path(layer_id =  paste0("linesplotted",reactivetest$mapdeck_line_no -1 ))

                lines2plot <- sfheaders::sf_linestring(
                  obj = dt
                  , x = "X"
                  , y = "Y"
                  , z = "Z"
                  , m = "M"
                  , linestring_id = "TagID"
                )
                lines2plot <- sf::st_set_crs(lines2plot, 4326)

                # palette to hex per level
                # debugged 06/05/2025: if the plotby variable is not TagID, then the pal2use4lines()
                # function we currently have will be looking for whatever the plotby variable is
                # but if we have anything other than TagID then it will not work for: pal2use4lines(as.character(sp_lines$TagID)
                # there has to be a way of saying if it is TagID the colour the lines by the paletter with domain = TagID
                # else just plot by the current box-entered value in input$colour_lines

                if(input$plot_by != "TagID"){
                  if(is.null(input$colour_lines)){
                    COL = "grey"
                  } else{
                    COL = input$colour_lines # current text entered into box - i.e. how do you plot the lines anyway for the line segments n-1 from points? not coded here
                  }
                } else{
                  clines = pal2use4lines
                  COL <- clines(onedata3[,which(names(onedata3) == "TagID")])
                }

                # need to add the alphs to the hex for madeck (!!)
                COL2 = colorspace::adjust_transparency(COL, alpha = input$line_opacity)

                lines2plot$COL2 <- unique(COL2)

                #m <- mapdeck::mapdeck(style = mapdeck_style("light"), location = c(0.4, 52.5), zoom = 7)
                #m %>%  mapdeck::add_path(
                #  data = lines2plot
                #  #, stroke_opacity = opacity # not used when specifying hex
                #  , layer_id = "linesplotted"
                #  #, palette = pal2use4lines
                #  #, elevation = "z"
                #  , stroke_colour =  "COL2"
                #  , update_view = FALSE
                #  , stroke_width = input$line_width*100
                #)
                #delay_time = 1000
                # output$mapOutput_md  IS NOT LOADED BY THE TIME THIS EVENT FIRES OFF!

                mapdeck::mapdeck_update(map_id = "mymap_md") %>%
                  mapdeck::add_path(
                    data = lines2plot
                    #, stroke_opacity = opacity # not used when specifying hex
                    #, layer_id = "linesplotted"
                    , layer_id = paste0("linesplotted",reactivetest$mapdeck_line_no),
                    #, palette = pal2use4lines
                    #, elevation = "z"
                    , stroke_colour =  "COL2"
                    , update_view = FALSE
                    , stroke_width = input$line_width*100,
                    #transitions = list(
                    #  stroke_colour = delay_time,
                    #  stroke_width = delay_time,
                    #  position = delay_time
                    #)
                  )

              }

              #------------------ #

              #####################################################
              ## plot the subsetted data, clear previous points and lines only

              if(input$mapdeck_switch == "Leaflet"){

                ### LINES FROM THE POINTS SELECTED
                z <- droplevels(data.frame(long = onedata3$longitude, lat = onedata3$latitude, DateTime = onedata3$DateTime, TagID = onedata3$TagID, var = onedata3[,which(names(onedata3) %in% getplotby)]))

                ## sfheaders for multilines
                sp_lines <- sfheaders::sf_multilinestring(z, x = "long", y = "lat", multilinestring_id = "TagID")
                sf::st_agr(sp_lines) = "constant"

                sp_lines$TagID <- unique(z$TagID)

                #pal2use4lines(as.character(sp_lines$TagID))

                # SEPARATE OUTPUT LAYER FOR SAVING (defunct!)
                v$TagID <- unique(onedata3$TagID)
                v$line <- sp_lines
                v$ltog <- input$ltog
                v$update <- 1

                ### this isn't as efficient as it could be as this plots all the lines back regardless of before
                ### this whole operation could be better as a reactive!

                leaflet::leafletProxy("mymap") %>%
                  #leafgl::removeGlPolylines(layerId = cl_reactive$current_layers)
                  leafgl::removeGlPolylines(layerId = paste0("foo", 1:10000)) %>%
                  leaflet::removeShape(layerId = paste0("foo", 1:10000))

                # debugged 06/05/2025: if the plotby variable is not TagID, then the pal2use4lines()
                # function we currently have will be looking for whatever the plotby variable is
                # but if we have anything other than TagID then it will not work for: pal2use4lines(as.character(sp_lines$TagID)
                # there has to be a way of saying if it is TagID the colour the lines by the paletter with domain = TagID
                # else just plot by the current box-entered value in input$colour_lines

                if(input$plot_by != "TagID"){
                  if(is.null(input$colour_lines)){
                    clines = "grey"
                  } else{
                    clines = input$colour_lines # current text entered into box - i.e. how do you plot the lines anyway for the line segments n-1 from points? not coded here
                  }
                } else{
                  clines = pal2use4lines(as.character(sp_lines$TagID))
                }

                if(!is.null(sp_lines)){

                  ###### addPolylines
                  if(input$leaflet_method == "leaflet"){

                    layerID <- paste0("foo",1:nrow(sp_lines))

                    leaflet::leafletProxy("mymap") %>%
                      #leaflet::addMapPane("line_order", zIndex = 430) %>% # shown below circles
                      #leaflet::addMapPane("circles_order", zIndex = 420) %>% # shown above lines
                      #  #leaflet::addPolylines(data = yy, weight = 2, color = "grey", opacity = 0.5, fillOpacity = 0.5, layerId = "foo")
                      leaflet::addPolylines(data = sp_lines,
                                            color = clines, # modified line for feeding names of sf object back to palt2use4lines
                                            opacity = input$line_opacity,
                                            weight = input$line_width,
                                            #options = leaflet::pathOptions(pane = "line_order"),
                                            layerId = layerID)
                  }

                  if(input$leaflet_method == "leafgl"){

                    layerID <- paste0("foo",1)
                    sf::st_agr(sp_lines) = "constant"
                    sp_lines = sf::st_cast(sp_lines, "LINESTRING")

                    ###### addGlPolylines NOT WORKING WELL WITH LOTS OF LINES, SOME DROP OF THE MAP WIERDLY
                    leaflet::leafletProxy("mymap") %>%
                      #leaflet::addMapPane("line_order", zIndex = 430) %>% # shown below circles
                      #leaflet::addMapPane("circles_order", zIndex = 420) %>% # shown above lines
                      leafgl::removeGlPolylines(layerId = paste0("foo", 1)) %>%
                      leafgl::addGlPolylines(data = sp_lines,
                                             color = clines,
                                             opacity = input$line_opacity,
                                             weight = input$line_width,
                                             #options = leaflet::pathOptions(pane = "line_order"),
                                             layerId = layerID
                      )

                  }

                }
              }

            }

          } #else {v$update <- 0} # ONLY PLOT IF THE TIME SLIDER HAS SELECTED DATA <----- redundant now?

        }

      }

      if(!is.null(input$mapdeck_switch)){
        if(input$mapdeck_switch == "Leaflet"){

          if(!input$ltog){

            leaflet::leafletProxy("mymap") %>%
              #leafgl::removeGlPolylines(layerId = cl_reactive$current_layers)
              leafgl::removeGlPolylines(layerId = paste0("foo", 1:10000)) %>%
              leaflet::removeShape(layerId = paste0("foo", 1:10000))


          }
        }

        if(input$mapdeck_switch == "Mapdeck"){
          if(!input$ltog){

            mapdeck::mapdeck_update(map_id = "mymap_md") %>%
              #mapdeck::clear_path(layer_id = "linesplotted")
              mapdeck::clear_path(layer_id =  paste0("linesplotted",reactivetest$mapdeck_line_no)) %>%
              mapdeck::clear_path(layer_id =  paste0("linesplotted",reactivetest$mapdeck_line_no -1 ))



          }
        }

      }

    }

  }, ignoreInit = TRUE, ignoreNULL = TRUE)


  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  # GPS POINTS
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################


  #### first observe event of TagID, slider AND the shapes now (still need to do additional point layer in same way)
  shiny::observeEvent({

    input$mapdeck_switch
    mymapExists()
    #plotdata$data2
    thedata()
    plotdata$data3 # sqlfilter
    input$ptog
    input$Slider_radius
    input$Slider_opacity
    input$leaflet_method
    input$reset_map
    pal_use$this
    input$elev_toggle # specific to mapdeck for elevation toggle

  },{

    # similar to the lines plot above, initially skipper for mapdeck friends
    #if (isolate(skipPlot2()==1)) { #& isolate(pointsPlot() == 1)
    #  # skip first reactive sequence
    #  skipPlot2(0)
    #  # launch next reactive sequence
    #  invalidateLater(1,session)
    #} else {
      #invalidateLater(5000,session)

      if(mymapExists()){

      shinybusy::add_busy_spinner(spin = "fading-circle")
      thedata_ <- isolate(thedata())

      # capture the palette function as it currently is in the reactive, calling something sensible for use in this observer
      pal <-  pal_use$this

      #if(!is.null(plotdata$data2)){
      if(!is.null(thedata_)){

        #if(nrow(plotdata$data2) > 0){
        if(nrow(thedata_) > 0){

          #########################
          # if all that is TRUE, then take the plot data from time slider subsetting above
          if(!is.null( plotdata$data3 )){
            onedata2 <- plotdata$data3
          } else{
            onedata2 <- thedata_
            #onedata2 <- plotdata$data2
          }

          onedata2$group <- "two"
          onedata2 <- onedata2[order(onedata2$TagID, onedata2$DateTime),]

          ### debugging tests
          #input$plot_by # current user plotby
          #plotdata$plot_by # current reactively stored plotby
          #pal_use$this # overall palette
          #pal_use$variable # variable palette
          # identical(pal_use$this, pal_use$variable) # TRUE
          #reactivetest$curr_pal_type # what is the current option for plotting? variable
          #input$colour_factor # colorFactor # current numeric or factor for palettes
          #reactivetest$use4cf # colorNumeric the adjusted input$colorFactor based on the data we are trying to plot, e.g. if user selects numeric and it's a factor that we have
          #current_variable_type = attr(pal_use$variable, "colorType")
          #current_variable_type <- ifelse(current_variable_type == "factor", "character", current_variable_type)
          #current_data_type = class(onedata2[,which(names(onedata2) == plotdata$plot_by)])
          #if (current_variable_type != current_data_type) {
          #stop("Mismatching variable types in palette")
          # need a way of capturing a mismatching condition that occurs
          # possibly reactive orders
          # to do with dnt variable creation and then trying to plot a numeric or vice versa
          # something gets out of sync somewhere
          #}
          ### test:
          #if(plotdata$plot_by != "TagID"){
          #  print("testing why observer firing...")
          #  cols_used = unique(pal_use$this(plotdata$data2[,which(names(plotdata$data2) == plotdata$plot_by)]))
          #  df = data.frame(nrowsdata2 = nrow(plotdata$data2),
          #             TagID = paste(unique(plotdata$data2$TagID), collapse = ", "),
          #             current_patette = paste(reactivetest$curr_pal_type, collapse = ", "),
          #             current_patette2 = paste(reactivetest$curr_pal_input_points, collapse = ", "),
          #             current_lines = paste(input$colour_lines, collapse = ", "),
          #             plotby = plotdata$plot_by,
          #             ptog =  input$ptog,
          #             Slider_radius = input$Slider_radius,
          #             Slider_opacity = input$Slider_opacity,
          #             leaflet_method = input$leaflet_method,
          #             reset_map = input$reset_map,
          #             n_cols_used = length(cols_used))#
          #  print(df)
          #  }

          # ------------------------------------------------ #
          # plotting

          #print(onedata2[onedata2$TagID == 5377 & onedata2$DateTime > datetime("2018-06-18 02:49:05") & onedata2$DateTime < datetime("2018-06-18 05:49:05"),])

          VAL <- onedata2[,which(names(onedata2) %in% plotdata$plot_by)]

          # if colorFactor choice selected, the earlier constraint is to STILL plot
          # data as numeric if you have a numeric variable
          # if colorNumeric is specified and you have a character/factor variable then default is to
          # still plot as factor/character, the in between situation is integer

          if(any(is.na(VAL))){
            if(class(VAL) %in% "numeric"){
              # if numeric class then we do not want to add in NoLab as we were doing for character/factor classes
              VAL <- ifelse(is.na(VAL), NA, as.vector(VAL))
            } else if(class(VAL) %in% "integer"){
              if(input$colour_factor %in% "colorFactor"){
                VAL <- ifelse(is.na(VAL), "NoLab", as.vector(VAL))
              } else{
                VAL <- ifelse(is.na(VAL), NA, as.vector(VAL))
              }
            } else{
              VAL <- ifelse(is.na(VAL), "NoLab", as.vector(VAL))
            }
          }

          TITLE <- plotdata$plot_by

          # if error in user-entered custom-paletter
          #if(is.null(pal)){
          #  pal <- leaflet::colorFactor(palette =  c("red","blue","green"), domain = VAL)
          #  pal_use$this <- pal
          #  pal_use$anim <- pal
          #}
          #print(pal)

          # ------------------------------------ #
          # LEAFLET COLOURING OF POINTS - whether custom colours or colour palette defined earlier
          # this is also dependent on whether using TagID or the plot_by for switching variables
          # get the row-specific colour to be plotted based on the pal function in turn from pal_use$this
          # use the  pal_use$type to check if animal, i.e. use the TagID colour pallete
          # or, "variable" for the specific colouration for a variable in the data.frame
          # calling COL now for verbose coding
          # and could probably be combined with above onedata selection

          # selecting TagID at the start or further switches to TagID for plotting
          if(plotdata$plot_by == "TagID"){

            #browser()

            # first use the animal colour palette
            pal <-  pal_use$anim

            if(!is.null(pal)){
              COL <- pal(onedata2[,which(names(onedata2) == "TagID")])
            } else{

              if(!is.null(input$builtin_pal)){
                colour_module(inputs = input$builtin_pal, palette_type = "animal")
                pal <-  pal_use$anim
              } else if(!is.null(input$builtin_pal2)){
                colour_module(inputs = input$builtin_pal2, palette_type = "animal")
                pal <-  pal_use$anim
              } else{
                pal <- leaflet::colorFactor(palette =  c("red","blue","green"), domain = onedata2[,which(names(onedata2) == "TagID")])
              }

              COL <- pal(onedata2[,which(names(onedata2) == "TagID")])
            }

          }

          # if not TagID for level plotting by variable...
          if(plotdata$plot_by != "TagID"){

            # first use the variable-specific colour palette
            pal <-  pal_use$variable

            if(!is.null(pal)){
              COL <- pal(onedata2[,which(names(onedata2) %in% plotdata$plot_by)])
            } else{

              if(!is.null(input$builtin_pal)){
                colour_module(inputs = input$builtin_pal, palette_type = "variable")
                pal <-  pal_use$anim
              } else if(!is.null(input$builtin_pal2)){
                colour_module(inputs = input$builtin_pal2, palette_type = "variable")
                pal <-  pal_use$anim
              } else{
                pal <- leaflet::colorFactor(palette =  c("red","blue","green"), domain = onedata2[,which(names(onedata2) %in% plotdata$plot_by)])
              }

              COL <- pal(onedata2[,which(names(onedata2) %in% plotdata$plot_by)])

            }

            COL <- pal(onedata2[,which(names(onedata2) == plotdata$plot_by)])
            #COL <- pal(onedata2[,which(names(onedata2) == input$plot_by)])

          }

          if(exists("altitude_agl",onedata2)){
            popup_altitude <- onedata2$altitude_agl

            if(all(is.na(onedata2$altitude_agl))){
              popup_altitude <- onedata2$altitude
            }
          } else{
            popup_altitude <- onedata2$altitude
          }

          #if(plotdata$plot_by == "TagID"){
          if(!is.null(input$option_read_track_MB) && input$option_read_track_MB == "BTOTT"){

            POPUP <- paste("<b>", onedata2$TagID,"</b>", "<br>",
                           "<b>",paste0(plotdata$plot_by,":"), "</b>", onedata2[,which(names(onedata2) == plotdata$plot_by)], "<br>",
                           "<b>","DateTime:", "</b>", onedata2$DateTime, "<br>",
                           "<b>","Latitude:", "</b>", round(onedata2$latitude,5), "<br>",
                           "<b>","Longitude:", "</b>", round(onedata2$longitude,5), "<br>",
                           "<b>","Altitude (m):", "</b>", popup_altitude, "<br>",
                           "<b>","Satellites used:", "</b>", onedata2$satellites_used, "<br>",
                           "<b>","Speed (m/s):","</b>", round(onedata2$speed_2d,5))

          } else if(!is.null(input$option_read_track_MB) && input$option_read_track_MB == "BTOTT_MB"){
            POPUP <- paste("<b>", onedata2$TagID,"</b>", "<br>",
                           "<b>",paste0(plotdata$plot_by,":"), "</b>", onedata2[,which(names(onedata2) == plotdata$plot_by)], "<br>",
                           "<b>","DateTime:", "</b>", onedata2$DateTime, "<br>",
                           "<b>","Latitude:", "</b>", round(onedata2$latitude,5), "<br>",
                           "<b>","Longitude:", "</b>", round(onedata2$longitude,5), "<br>",
                           "<b>","Altitude (m):", "</b>", popup_altitude, "<br>"
            )
          }

          #####################################################
          ## plot the subsetted data, clear previous points and lines only (not polys)
          # if fixes themselves to be displayed from selection in function by user AND if the toggle is TRUE

          if(!is.null(input$mapdeck_switch)){
            if(input$mapdeck_switch == "Leaflet"){

              COL2 <- COL
              ### if the input toggle ptog is FALSE, then set points to NULL, i.e. turn off
              if(!is.null(input$ptog)){

                if(!input$ptog){
                  # if input ptog is false, clear the map
                  pp <- NULL
                  leaflet::leafletProxy("mymap") %>%
                    leaflet::clearGroup(group = "two") %>%
                    leafgl::removeGlPoints(layerId = "two")

                }

                if(input$ptog){

                  # if input ptog is true, plot
                  pp <- data.frame(latitude = onedata2$latitude, longitude = onedata2$longitude, group = "two")
                  pp$id <- 1:nrow(pp)

                  #df1 = data.frame(id = 1:n,
                  #                 x = rnorm(n, 10, 3),
                  #                 y = rnorm(n, 49, 1.8))

                  pts = sf::st_as_sf(pp, coords = c("longitude", "latitude"), crs = 4326)

                  if(input$leaflet_method == "leafgl"){

                    leaflet::leafletProxy("mymap") %>%
                      leaflet::clearGroup(group = "two") %>%
                      leafgl::removeGlPoints(layerId = "two") %>%
                      #leaflet::addMapPane("line_order", zIndex = 430) %>% # shown below circles
                      #leaflet::addMapPane("circles_order", zIndex = 420) %>% # shown above lines
                      leafgl::addGlPoints(data = pts,
                                          layerId = "two",
                                          radius= input$Slider_radius,
                                          fillOpacity = input$Slider_opacity,
                                          stroke = FALSE,
                                          fillColor=COL,
                                          #options = pathOptions(pane = "circles_order"),
                                          popup = POPUP)

                  }

                  if(input$leaflet_method == "leaflet"){

                    leaflet::leafletProxy("mymap") %>%
                      leaflet::clearGroup(group = "two") %>%
                      leafgl::removeGlPoints(layerId = "two") %>%
                      #leaflet::addMapPane("line_order", zIndex = 430) %>% # shown below circles
                      #leaflet::addMapPane("circles_order", zIndex = 420) %>% # shown above lines
                      leaflet::addCircleMarkers(group = onedata2$group,
                                                lat = onedata2$latitude,
                                                lng = onedata2$longitude,
                                                radius= input$Slider_radius,
                                                fillOpacity = input$Slider_opacity,
                                                stroke = FALSE,
                                                color=COL,
                                                #options = pathOptions(pane = "circles_order"),
                                                popup = POPUP)
                  }


                }

              } else{

                leaflet::leafletProxy("mymap") %>%
                  leaflet::clearGroup(group = "two") %>%
                  leafgl::removeGlPoints(layerId = "two")
              }

            }

            # ------------------------------ #
            if(input$mapdeck_switch == "Mapdeck"){

              if(!is.null(input$ptog)){

                if(!input$ptog){
                  pp <- v$fix
                  COL2 <- v$colour

                  mapdeck::mapdeck_update(map_id = "mymap_md") %>%
                    mapdeck::clear_pointcloud(layer_id = "scatter_layer")

                }

                if(input$ptog){

                  pp <- data.frame(latitude = onedata2$latitude, longitude = onedata2$longitude, TagID = onedata2$TagID)
                  pp$id <- 1:nrow(pp)

                  if(exists("altitude_agl",onedata2)){

                    pp$altitude <- onedata2$altitude_agl  # agl DEM uva
                  }
                  if(exists("altitude",onedata2)){


                    pp$altitude <- onedata2$altitude # general uva
                  }
                  if(exists("height.above.msl",onedata2)){

                    pp$altitude <- onedata2$height.above.msl # Movebank
                  }

                  if(!exists("altitude_agl",onedata2) & !exists("altitude",onedata2) & !exists("height.above.msl",onedata2)){

                    pp$altitude <- 0
                  }
                  if(all(is.na(onedata2$altitude))){
                    pp$altitude <- 0
                  }

                  # make sf object
                  onedata2_sf <- sf::st_as_sf(pp, coords = c("longitude", "latitude"), crs = 4326)

                  # first remove the previous lines
                  mapdeck::mapdeck_update(map_id = "mymap_md") %>%
                    mapdeck::clear_path(layer_id = "scatter_layer")

                  #mapdeck::mapdeck_update(map_id = "mymap_md") %>%
                  #  mapdeck::add_sf(
                  #    data = onedata2_sf
                  #    , fill_colour = COL
                  #    #, elevation = 'height.above.msl'
                  #    , layer_id = "scatter_layer"
                  #    , update_view = FALSE
                  #    #, palette = pal
                  #    , radius = input$Slider_radius
                  #    , stroke_opacity = input$Slider_opacity
                  #  )

                  COL2 = colorspace::adjust_transparency(COL, alpha = input$Slider_opacity)
                  onedata2_sf$COL2 <- COL2

                  onedata2_sf$POPUP <- POPUP

                  if(input$elev_toggle){
                    mapdeck::mapdeck_update(map_id = "mymap_md") %>%
                      mapdeck::add_pointcloud(
                        data = onedata2_sf
                        , fill_colour = "COL2"
                        , elevation = "altitude"
                        , layer_id = "scatter_layer"
                        , update_view = FALSE
                        #, palette = pal
                        , radius = input$Slider_radius
                        , fill_opacity = input$Slider_opacity
                        , tooltip = "POPUP"
                        #, legend = js
                      )
                  } else{
                    onedata2_sf$altitude <- 0

                    mapdeck::mapdeck_update(map_id = "mymap_md") %>%
                      mapdeck::add_pointcloud(
                        data = onedata2_sf
                        , fill_colour = "COL2"
                        , elevation = "altitude"
                        , layer_id = "scatter_layer"
                        , update_view = FALSE
                        #, palette = pal
                        , radius = input$Slider_radius
                        , fill_opacity = input$Slider_opacity
                        , tooltip = "POPUP"
                        #, legend = js
                      )
                  }

                }

              }

            }
          }

          # SEPARATE OUTPUT LAYER FOR SAVING (not strictly used now!)
          v$fix = pp
          v$colour = COL2
          v$TagID <- unique(onedata2$TagID)
          v$ptog <- input$ptog
          v$leg.val <- VAL
          v$pal <- pal
          v$leg.tit <- TITLE
          v$update <- 1
          v$radius <- input$Slider_radius
          v$opacity <- input$Slider_opacity

          # first run of the map - display the legend

          if(!is.null(input$mapdeck_switch)){
            if(input$mapdeck_switch == "Leaflet"){

              # values come through here as character...! if numeric then that's not going to work - not quite sure why that is happening tbh
              if(input$colour_factor == "colorNumeric"){
                VAL <- as.numeric(VAL)
              }

              if(input$leg){
                leaflet::leafletProxy("mymap") %>%
                  leaflet::addLegend(pal = pal, position = input$legend_radio, values = VAL,
                                     opacity = 1, title = TITLE, layerId = paste0("legend",reactivetest$leaflet_legno))
              }

            }
          }

        }

      }

    }

  }, ignoreInit = TRUE, ignoreNULL = TRUE)



  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  # Toggle the legend (instead of having legend in the reactive with GPS points)
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################

  shiny::observeEvent({
    # any changesin the palette TagID selector
    pal_use$this
    input$leg # if the legend toggle on/off then redraw legend
    input$legend_radio
    v$leg.val
    #input$mapdeck_switch
  },{

    ## similar to the lines and points plot above, initially skipper for mapdeck friends
    #if (isolate(skipPlot2()==1)) {
    #  # skip first reactive sequence
    #  skipPlot2(0)
    #  # launch next reactive sequence
    #  invalidateLater(1,session)
    #} else {
      #if(v$mapdone > 0){

      if(!is.null(input$mapdeck_switch)){

        if(input$mapdeck_switch == "Leaflet"){

          if(input$leg){

            thedata_ <- isolate(thedata())

            if(!is.null(thedata_)){

              VAL <- thedata_[,which(names(thedata_) %in% plotdata$plot_by)]

              # values come through here as character...! if numeric then that's not going to work - not quite sure why that is happening tbh
              if(reactivetest$use4cf == "colorNumeric"){
                #VAL <- as.numeric(v$leg.val)
                VAL <- as.numeric(VAL)
              } #else{
              #  VAL <- v$leg.val
              #}

              #pal = v$pal
              pal <- pal_use$this

              #title = v$leg.tit
              title = plotdata$plot_by

              leaflet::leafletProxy("mymap") %>%
                #leaflet::removeControl("legend") %>%
                leaflet::removeControl(paste0("legend",reactivetest$leaflet_legno-1)) %>%
                leaflet::removeControl(paste0("legend",reactivetest$leaflet_legno)) %>%

                leaflet::addLegend(pal = pal,
                                   position = input$legend_radio,
                                   values = VAL,
                                   opacity = 1,
                                   title = title,
                                   #layerId = "legend"
                                   layerId = paste0("legend",reactivetest$leaflet_legno)
                                   )
            }

          }

          if(!input$leg){

            leaflet::leafletProxy("mymap") %>%
              #leaflet::removeControl("legend") %>%
              leaflet::removeControl(paste0("legend",reactivetest$leaflet_legno-1)) %>%
              leaflet::removeControl(paste0("legend",reactivetest$leaflet_legno))
          }
        }


        if(input$mapdeck_switch == "Mapdeck"){
          if(input$leg){

            ## note, an add_layer function does seem to now exist in mapdeck that was not there
            ## at time of writing
            # https://rdrr.io/github/SymbolixAU/mapdeck/man/add_legend.html
            # as well as clear_legend()

            thedata_ <- isolate(thedata())

            if(!is.null(thedata_)){

              # Get the plotby variable class
              VAL <- thedata_[,which(names(thedata_) %in% plotdata$plot_by)]
              class_val <- class(VAL)[1]

              # remembering the last use4cf
              variable_type <- ifelse(reactivetest$use4cf == "colorFactor","category","gradient")

              ##### remembering also, the bird ID probably plots first....(on initial run)
              variable_type2 <- ifelse(reactivetest$use4cf == "colorFactor","character","numeric")

              if(class_val != variable_type2){
                if(class_val == "integer" & variable_type2 == "character"){
                  VAL <- as.character(VAL) # could really just use the reactive data selection here
                  variable_type <- "category"
                }
                if(class_val == "integer" & variable_type2 == "numeric"){
                  VAL <- as.numeric(VAL)
                  variable_type <- "gradient"
                }

                if(class_val != "integer" &  class_val != "numeric"){
                  variable_type <- "category"
                }

              }

              pal <- pal_use$this

              if(is.null(pal)){
                pal <- leaflet::colorFactor(palette =  c("red","blue","green"), domain = v$TagID)

              }


              # ---------------------------------------------- #
              # Build legend
              # ---------------------------------------------- #


              if(variable_type == "category"){

                # sort the values if integer, otherwise orders are all messed up (as they are also on leaflet actually)
                uniq_vals = unique(VAL)

                if(class_val == "integer"){
                  uniq_vals <- sort(as.numeric(uniq_vals))
                }

                check_pal_type = ifelse(attr(pal,"colorType") == "factor","character",attr(pal,"colorType"))
                check_data_type = class(VAL)[1]

                if(check_pal_type != check_data_type){

                  # recreate palette?
                  if(plotdata$plot_by != "TagID"){
                    colour_module(inputs = input$builtin_pal, palette_type = "variable")
                  } else{
                    colour_module(inputs = input$builtin_pal, palette_type = "animal")
                  }
                  pal <- pal_use$this

                  if(check_pal_type == "character" & check_data_type == "numeric"){
                    variable_type <- "numeric" # should be

                    # then we need numeric and something is out of sync
                    mn = min(VAL, na.rm=TRUE)
                    mx = max(VAL,na.rm=TRUE)
                    ra = mx-mn
                    levs = c(0,0.2,0.4,0.6,0.8,1)
                    vals2use = (levs * ra)+mn
                    cols = pal(vals2use)
                    #vals2use_print <- round(vals2use, 2)
                    vals2use_print <- sprintf(paste0("%.",2,"f"), round(vals2use,2))

                    l1 <- mapdeck::legend_element(
                      variables = vals2use_print
                      , colours = cols
                      , colour_type = "fill"
                      , variable_type = variable_type
                      , title = plotdata$plot_by
                    )
                  } else{

                    # otherwise, continue character checks

                    # still an issue with mismatching reactive length? posixct issues
                    if(length(uniq_vals) != length(unique(pal(uniq_vals)))){

                      if(check_data_type == "POSIXct"){
                        # plot as numeric ramp for now

                        mn = min(VAL, na.rm=TRUE)
                        mx = max(VAL,na.rm=TRUE)
                        ra = mx-mn
                        levs = c(0,0.2,0.4,0.6,0.8,1)
                        vals2use = (levs * ra)+mn
                        cols = pal(vals2use)

                        vals2use_print <- as.Date(vals2use)

                        l1 <- mapdeck::legend_element(
                          variables = vals2use_print
                          , colours = cols
                          , colour_type = "fill"
                          , variable_type = variable_type
                          , title = plotdata$plot_by
                        )

                      } #else{
                      # DO NOTHING

                      #}
                    }

                  }

                } else{

                  # category
                  l1 <- mapdeck::legend_element(
                    variables = uniq_vals
                    , colours = unique(pal(uniq_vals))
                    , colour_type = "fill"
                    , variable_type = variable_type
                    , title = plotdata$plot_by
                  )
                }

              } else if(variable_type == "gradient"){

                mn = min(VAL, na.rm=TRUE)
                mx = max(VAL,na.rm=TRUE)
                ra = mx-mn
                levs = c(0,0.2,0.4,0.6,0.8,1)
                vals2use = (levs * ra)+mn
                cols = pal(vals2use)
                #vals2use_print <- round(vals2use, 2)
                vals2use_print <- sprintf(paste0("%.",2,"f"), round(vals2use,2))

                l1 <- mapdeck::legend_element(
                  variables = vals2use_print
                  , colours = cols
                  , colour_type = "fill"
                  , variable_type = variable_type
                  , title = plotdata$plot_by
                )
              }

              #browser()
              js <- mapdeck_legend(l1)
              v$js <- js
              # ---------------------------------------------- #
              # there is a mapdeck function now in the development version for adding legends to maps and clearing them
              # but it's not the latest one that's on Cran. Check the current installed version and if not found,
              # using a hammy workaround
              # ---------------------------------------------- #

              xx = as.character(packageVersion("mapdeck"))
              xx2 = strsplit(xx,"[.]")[[1]]
              xx3 = as.numeric(paste(xx2, collapse = '', sep = ''))

              # Use add_legend if Cran v > 0.3.5
              if(xx3 >= 35){

                ### WARNING THIS REQURES THE DEV VERSION OF mapdeck 0.3.6+ from github - the one on Cran as of 16/05/2025 does not have add_legend() and clear_legend()
                mapdeck::mapdeck_update(map_id = "mymap_md") %>%
                  mapdeck::clear_legend(layer_id = paste0("legend_id",reactivetest$mapbox_legno-1)) %>%
                  mapdeck::clear_legend(layer_id = paste0("legend_id",reactivetest$mapbox_legno)) %>%
                  mapdeck::add_legend(legend = js, layer_id = paste0("legend_id",reactivetest$mapbox_legno))

                # Otherwise plot as a point cloud
              } else{

                fake_data = v$fix[1,] # ok still to use this presumably, mostly thedata() reactive derivation above now
                fake_data$longitude = NA
                fake_data$latitude = NA

                mapdeck::mapdeck_update(map_id = "mymap_md") %>%
                  mapdeck::add_pointcloud(
                    data = fake_data
                    , lon = "longitude"
                    , lat = "latitude"
                    , update_view = FALSE
                    , layer_id = paste0("legend_id",reactivetest$mapbox_legno)
                    , legend = js
                  )

              }
            } # is null the data_?

          }

          if(!input$leg){

            xx = as.character(packageVersion("mapdeck"))
            xx2 = strsplit(xx,"[.]")[[1]]
            xx3 = as.numeric(paste(xx2, collapse = '', sep = ''))

            if(xx3 >= 35){ # Cran current version is 0.3.5
              mapdeck::mapdeck_update(map_id = "mymap_md") %>%
                mapdeck::clear_legend(layer_id = paste0("legend_id",reactivetest$mapbox_legno-1)) %>%
                mapdeck::clear_legend(layer_id = paste0("legend_id",reactivetest$mapbox_legno))
            } else{
              mapdeck::mapdeck_update(map_id = "mymap_md") %>%
                mapdeck::clear_pointcloud(layer_id = paste0("legend_id",reactivetest$mapbox_legno)-1) %>%
                mapdeck::clear_pointcloud(layer_id = paste0("legend_id",reactivetest$mapbox_legno))
            }

          }

        }


      }

  })



  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  # Reset lines - Mapdeck
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################

  # try and reset the lines mapdeck issues?
  observeEvent(input$reset_lines,{

    if(is.null(reactivetest$mapdeck_line_no)){
      reactivetest$mapdeck_line_no <- 1
    }
    #if(is.null(reactivetest$leaflet_legno)){
    #  reactivetest$leaflet_line_no <- 1
    #}

  })



  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  # Reset legend (if issues with mapbox after map re-load)
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################


  observeEvent(input$reset_legend,{

    if(is.null(reactivetest$mapbox_legno)){
      reactivetest$mapbox_legno <- 1
    }
    if(is.null(reactivetest$leaflet_legno)){
      reactivetest$leaflet_legno <- 1
    }

    if(input$mapdeck_switch == "Leaflet"){
      reactivetest$leaflet_legno = reactivetest$leaflet_legno + 1

      if(input$leg){

        thedata_ <- isolate(thedata())

        if(!is.null(thedata_)){

          VAL <- thedata_[,which(names(thedata_) %in% plotdata$plot_by)]

          # values come through here as character...! if numeric then that's not going to work - not quite sure why that is happening tbh
          if(reactivetest$use4cf == "colorNumeric"){
            #VAL <- as.numeric(v$leg.val)
            VAL <- as.numeric(VAL)
          } #else{
          #  VAL <- v$leg.val
          #}

          #pal = v$pal
          pal <- pal_use$this

          #title = v$leg.tit
          title = plotdata$plot_by

          leaflet::leafletProxy("mymap") %>%
            #leaflet::removeControl("legend") %>%
            leaflet::removeControl(paste0("legend",reactivetest$leaflet_legno-1)) %>%
            leaflet::removeControl(paste0("legend",reactivetest$leaflet_legno)) %>%

            leaflet::addLegend(pal = pal,
                             position = input$legend_radio,
                             values = VAL,
                             opacity = 1,
                             title = title,
                             #layerId = "legend"
                             layerId = paste0("legend",reactivetest$leaflet_legno)
          )
        }

      }

    }

    if(input$mapdeck_switch == "Mapdeck"){
      reactivetest$mapbox_legno = reactivetest$mapbox_legno + 1

      #if(input$leg){
      #  xx = as.character(packageVersion("mapdeck"))
      #  xx2 = strsplit(xx,"[.]")[[1]]
      #  xx3 = as.numeric(paste(xx2, collapse = '', sep = ''))
      #  # Use add_legend if Cran v > 0.3.5
      #  if(xx3 >= 35){
      #    ### WARNING THIS REQURES THE DEV VERSION OF mapdeck 0.3.6+ from github - the one on Cran as of 16/05/2025 does not have add_legend() and clear_legend()
      #    mapdeck::mapdeck_update(map_id = "mymap_md") %>%
      #      mapdeck::clear_legend(layer_id = paste0("legend_id",reactivetest$mapbox_legno-1)) %>%
      #      mapdeck::clear_legend(layer_id = paste0("legend_id",reactivetest$mapbox_legno)) %>%
      #      mapdeck::add_legend(legend = v$js, layer_id = paste0("legend_id",reactivetest$mapbox_legno))
      #    # Otherwise plot as a point cloud
      #  } else{
      #    mapdeck::mapdeck_update(map_id = "mymap_md") %>%
      #      mapdeck::clear_pointcloud(layer_id = paste0("legend_id",reactivetest$mapbox_legno)-1) %>%
      #      mapdeck::clear_pointcloud(layer_id = paste0("legend_id",reactivetest$mapbox_legno))
      #    fake_data = v$fix[1,] # ok still to use this presumably, mostly thedata() reactive derivation above now
      #    fake_data$longitude = NA
      #    fake_data$latitude = NA
      #    mapdeck::mapdeck_update(map_id = "mymap_md") %>%
      #      mapdeck::add_pointcloud(
      #        data = fake_data
      #        , lon = "longitude"
      #        , lat = "latitude"
      #        , update_view = FALSE
      #        , layer_id = paste0("legend_id",reactivetest$mapbox_legno)
      #        , legend = v$js
      #      )
      #  }
      #}

      if(input$leg){

        thedata_ <- isolate(thedata())

        if(!is.null(thedata_)){

          # Get the plotby variable class
          VAL <- thedata_[,which(names(thedata_) %in% plotdata$plot_by)]
          class_val <- class(VAL)

          # remembering the last use4cf
          variable_type <- ifelse(reactivetest$use4cf == "colorFactor","category","gradient")

          ##### remembering also, the bird ID probably plots first....(on initial run)
          variable_type2 <- ifelse(reactivetest$use4cf == "colorFactor","character","numeric")

          if(class_val != variable_type2){
            if(class_val == "integer" & variable_type2 == "character"){
              VAL <- as.character(VAL) # could really just use the reactive data selection here
              variable_type <- "category"
            }
            if(class_val == "integer" & variable_type2 == "numeric"){
              VAL <- as.numeric(VAL)
              variable_type <- "gradient"
            }

            if(class_val != "integer" &  class_val != "numeric"){
              variable_type <- "category"
            }

          }

          pal <- pal_use$this

          if(is.null(pal)){
            pal <- leaflet::colorFactor(palette =  c("red","blue","green"), domain = v$TagID)
          }

          # ---------------------------------------------- #
          # Build legend
          # ---------------------------------------------- #

          if(variable_type == "category"){

            # sort the values if integer, otherwise orders are all messed up (as they are also on leaflet actually)
            uniq_vals = unique(VAL)

            if(class_val == "integer"){
              uniq_vals <- sort(as.numeric(uniq_vals))
            }

            check_pal_type = ifelse(attr(pal,"colorType") == "factor","character",attr(pal,"colorType"))
            check_data_type = class(VAL)[1]

            if(check_pal_type != check_data_type){

              # recreate palette?
              if(plotdata$plot_by != "TagID"){
                colour_module(inputs = input$builtin_pal, palette_type = "variable")
              } else{
                colour_module(inputs = input$builtin_pal, palette_type = "animal")
              }
              pal <- pal_use$this

            }

            # still an issue with mismatching reactive length?
            if(length(uniq_vals) != length(unique(pal(uniq_vals)))){

            }

            l1 <- mapdeck::legend_element(
              variables = uniq_vals
              , colours = unique(pal(uniq_vals))
              , colour_type = "fill"
              , variable_type = variable_type
              , title = plotdata$plot_by
            )
          } else if(variable_type == "gradient"){

            mn = min(VAL, na.rm=TRUE)
            mx = max(VAL,na.rm=TRUE)
            ra = mx-mn
            levs = c(0,0.2,0.4,0.6,0.8,1)
            vals2use = (levs * ra)+mn
            cols = pal(vals2use)
            #vals2use_print <- round(vals2use, 2)
            vals2use_print <- sprintf(paste0("%.",2,"f"), round(vals2use,2))

            l1 <- mapdeck::legend_element(
              variables = vals2use_print
              , colours = cols
              , colour_type = "fill"
              , variable_type = variable_type
              , title = plotdata$plot_by
            )

          }

          js <- mapdeck_legend(l1)
          v$js <- js
          # ---------------------------------------------- #
          # there is a mapdeck function now in the development version for adding legends to maps and clearing them
          # but it's not the latest one that's on Cran. Check the current installed version and if not found,
          # using a hammy workaround
          # ---------------------------------------------- #

          xx = as.character(packageVersion("mapdeck"))
          xx2 = strsplit(xx,"[.]")[[1]]
          xx3 = as.numeric(paste(xx2, collapse = '', sep = ''))

          # Use add_legend if Cran v > 0.3.5
          if(xx3 >= 35){

            ### WARNING THIS REQURES THE DEV VERSION OF mapdeck 0.3.6+ from github - the one on Cran as of 16/05/2025 does not have add_legend() and clear_legend()
            mapdeck::mapdeck_update(map_id = "mymap_md") %>%
              mapdeck::clear_legend(layer_id = paste0("legend_id",reactivetest$mapbox_legno-1)) %>%
              mapdeck::clear_legend(layer_id = paste0("legend_id",reactivetest$mapbox_legno)) %>%
              mapdeck::add_legend(legend = js, layer_id = paste0("legend_id",reactivetest$mapbox_legno))

            # Otherwise plot as a point cloud
          } else{

            fake_data = v$fix[1,] # ok still to use this presumably, mostly thedata() reactive derivation above now
            fake_data$longitude = NA
            fake_data$latitude = NA

            mapdeck::mapdeck_update(map_id = "mymap_md") %>%
              mapdeck::add_pointcloud(
                data = fake_data
                , lon = "longitude"
                , lat = "latitude"
                , update_view = FALSE
                , layer_id = paste0("legend_id",reactivetest$mapbox_legno)
                , legend = js
              )

          }
        } # is null the data_?

      }

    }

  })


  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  # layer controls
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################

  shiny::observeEvent({
    input$layer_control
    input$mapdeck_switch
  },{

    if(!is.null(input$mapdeck_switch)){

      if(input$mapdeck_switch == "Leaflet"){

        if(input$layer_control == FALSE){

          leaflet::leafletProxy("mymap") %>% leaflet::removeLayersControl()

        }

        if(input$layer_control == TRUE){

          # as in the original map render: Prividers are specified by the user at the outset
          # better in future to have some sort of user selection in the app

          if("GoogleEarth" %in% Providers){
            Providers_GE <- "GoogleEarth"
            Providers <- Providers[Providers != "GoogleEarth"]
          }

          # add Google Earth Layer if exists, and make sure layer names reflect this addition
          if(exists("Providers_GE")){
            leaflet::leafletProxy("mymap") %>% leaflet::addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google', options = leaflet::providerTileOptions(updateWhenIdle = TRUE, updateWhenZooming = FALSE))
          }

          for(i in 1:length(Providers)){
            leaflet::leafletProxy("mymap") %>% leaflet::addProviderTiles(Providers[i], group = Providers[i],
                                                                         options = leaflet::providerTileOptions(
                                                                           updateWhenIdle = TRUE,
                                                                           providerTileOptions(zIndex=-10),
                                                                           updateWhenZooming = FALSE)
            )
          }

          if(exists("Providers_GE")){
            Providers <- c(Providers, "GoogleEarth")
          }

          leaflet::leafletProxy("mymap") %>% leaflet::addLayersControl(
            baseGroups = Providers,
            options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>%
            leaflet::mapOptions(zoomToLimits = "never")

          #%>%
          #leaflet::addScaleBar(position = "bottomleft")


        }
      }

    }

  })

  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  # Toggle attribution
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################

  # doesn't work!
  # license as I understand does allow you to remove the credit
  # but this needs more inspection

  shiny::observeEvent({
    input$map_attribution
    input$mapdeck_switch
  },{

    if(!is.null(input$mapdeck_switch)){
      if(input$mapdeck_switch == "Leaflet"){
        leaflet::leafletProxy("mymap") %>% leaflet::leaflet(
          options = leaflet::leafletOptions(attributionControl=input$map_attribution
          )
        )

      }

    }

  })

  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  # Toggle scale bar stuff
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################

  shiny::observeEvent({
    input$scalebar_toggle
    input$scalebar_pos
    input$scalebar_met
    input$scalebar_imp
    input$mapdeck_switch
  },{

    if(!is.null(input$mapdeck_switch)){

      if(input$mapdeck_switch == "Leaflet"){

          if(input$scalebar_toggle){

            leaflet::leafletProxy("mymap") %>% leaflet::addScaleBar(position = input$scalebar_pos,  options = scaleBarOptions(metric = input$scalebar_met, imperial = input$scalebar_imp))

          }

          if(!input$scalebar_toggle){

            leaflet::leafletProxy("mymap") %>% removeScaleBar()

          }

      }
    }

  })

  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
   # mapdeck (Mapdeck) specific stuff
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################


  #radio buttons to observe style changes for basemap

  shiny::observeEvent({
    input$md_style
    input$mapdeck_switch
  },{

    if(!is.null(input$mapdeck_switch)){

      if(input$mapdeck_switch == "Mapdeck"){
        mapdeck::mapdeck_update(map_id = "mymap_md") %>%
          mapdeck::update_style(style = mapdeck_style(input$md_style))

      }
    }

  })

  # snap to north
  # not sure how to do this yet! without redrawing the map!

  ###################################################################################################################################

  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  # External points
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################


  shiny::observeEvent({
    input$point
    input$ext_point_col # for Mapdeck version external points (but also could be for leaflet style by swapping out pcol!)
    input$Slider_opacity_p

    # also fire off this observer if changing to leafgl
    input$leaflet_method
    input$mapdeck_switch
  },{

    # points to add to the map? assess how many sets of points, and check for named lat long colums
    if(!is.null(points)){

      v$point <- input$point

      points <- lapply(points,function(x){
        names(x) <- tolower(names(x))
        if(any(!c("latitude","longitude") %in% names(x))) { # any NOT TRUE?
          stop("needs latitude and longitude named columns")
        }
        #x <- subset(x,select=c(latitude,longitude))
        return(x)
      })

      # if retaining all columns beyong lat long, then
      # need separate point plots and popups per list of points
      # previously was rbinded together so much simpler

      #points <- do.call('rbind',points)

      POPUP_POINTS <- list()
      p = 1
      for(p in 1:length(points)){

        # first do the lat long that should be present given check above
        POPUP_POINTS[[p]] <- paste(
          "<b>","Latitude:", "</b>", round(points[[p]]$latitude,5), "<br>",
          "<b>","Longitude:", "</b>", round(points[[p]]$longitude,5), "<br>"

        )

        # Assess extra columns to add to popup

        col = sf::st_drop_geometry(points[[p]])

        nms = names(col)
        nms = nms[!nms %in% c("latitude","longitude")]

        # cycle over and add to the popup
        n = 1
        for(n in 1:length(nms)){

          w = which(names(col) == nms[n])

          col_0 = data.frame(col[,w])

          names(col_0) <- nms[n]

          if(class(col_0[[1]])[1] == "numeric"){
            col_0 <- round(col_0,5)
          }

          POPUP_POINTS[[p]] <- paste(POPUP_POINTS[[p]],paste("<b>",names(col_0),"</b>", col_0[[1]], "<br>"))
        }


      }

      v$add.points <- points # save as reactive for mapshot output
      #v$add.points.popup <- POPUP_POINTS # NOT NEEDED FOR OUTPUT save popup for each list of points

      # could do in loop above but keeping separate for clarity
      # cycle over each points list separately i.e. given they may have different popups of columns etc

      #message("test1")
      if(input$point){

        # not sure why I did this nested observer thing...
        shiny::observeEvent({
          input$Slider_radius_p
          input$Slider_opacity_p
        },{

          v$radius_p <- input$Slider_radius_p
          v$opacity_p <- input$Slider_opacity_p


          if(!is.null(input$mapdeck_switch)){
            #if(!Mapdeck){
            if(input$mapdeck_switch == "Leaflet"){

              leaflet::leafletProxy("mymap") %>% clearGroup(group = "one")

              #message("test2")
              #i = 1
              for(p in 1:length(points)){

                points[[p]]$group = "one"

                if(input$leaflet_method == "leafgl"){

                  # now using leafgl::addGlPoints which is much faster
                  pts_ext = sf::st_as_sf(points[[p]], crs = 4326)

                  leaflet::leafletProxy("mymap") %>%
                    leafgl::addGlPoints(data = pts_ext,
                                        group = "one",
                                        radius= input$Slider_radius_p,
                                        fillOpacity = input$Slider_opacity_p,
                                        stroke = FALSE,
                                        fillColor = colorspace::adjust_transparency(input$ext_point_col, alpha = input$Slider_opacity_p),
                                        popup = POPUP_POINTS[[p]])



                }

                if(input$leaflet_method == "leaflet"){

                  leaflet::leafletProxy("mymap")  %>%
                    leaflet::addCircleMarkers(group = points[[p]]$group, lat = points[[p]]$latitude,
                                              lng = points[[p]]$longitude,
                                              radius= input$Slider_radius_p,
                                              fillOpacity = input$Slider_opacity_p,
                                              stroke = FALSE,
                                              color = colorspace::adjust_transparency(input$ext_point_col, alpha = input$Slider_opacity_p),
                                              popup = POPUP_POINTS[[p]])
                }

              }

            }

            if(input$mapdeck_switch == "Mapdeck"){

              for(p in 1:length(points)){

                # first remove the previous lines
                mapdeck::mapdeck_update(map_id = "mymap_md") %>%
                  mapdeck::clear_pointcloud(layer_id = paste0("external_points_layer", p))

                pts_ext = sf::st_as_sf(points[[p]], coords = c("longitude", "latitude"), crs = 4326)

                EXT_COL2 = colorspace::adjust_transparency(input$ext_point_col, alpha = input$Slider_opacity_p)
                pts_ext$EXT_COL2 <- EXT_COL2

                pts_ext$POPUP <- POPUP_POINTS[[p]]

                pts_ext$altitude <- 0

                mapdeck::mapdeck_update(map_id = "mymap_md") %>%
                  mapdeck::add_pointcloud(
                    data = pts_ext
                    , fill_colour = "EXT_COL2"
                    , elevation = "altitude"
                    , layer_id = paste0("external_points_layer", p)
                    , update_view = FALSE
                    #, palette = pal
                    , radius = input$Slider_radius_p
                    #, fill_opacity = input$Slider_opacity
                    #, tooltip = "POPUP_POINTS"

                  )

              }
            }


          }


        })

      } else{

          if(!is.null(input$mapdeck_switch)){
            if(input$mapdeck_switch == "Leaflet"){
              leaflet::leafletProxy("mymap") %>% leaflet::clearGroup(group = "one")
            }

            if(input$mapdeck_switch == "Mapdeck"){
              for(p in 1:length(points)){
                mapdeck::mapdeck_update(map_id = "mymap_md") %>%
                  mapdeck::clear_pointcloud(layer_id = paste0("external_points_layer", p))

              }
            }

          }

      }

    }

  })

  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  # External shapes
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################

  shiny::observeEvent({
    input$shape
  },{

    # # # # # # # #
    # subset the shape selected

    if(!is.null(shapes)){ # i.e. from the original function specification

      SHAPE = input$shape # user selected
      ##SHAPE <- "Shape 1"

      if(SHAPE != "All"){
        shpn <- as.numeric(strsplit(SHAPE, " ")[[1]][2]) # get the numeric value of order of shape
        shapes2 <- shapes[[shpn]]
        poly_col <- "black"
        layerId = paste("foo",shpn)

      } else{ # ALL

        # new sf way, note may need a union
        shapes2 <- sf::st_combine(do.call('rbind',shapes))

        # old sp way
        #shapes2 <- rgeos::gUnaryUnion(do.call('rbind',shapes))
        #shapes2 <- do.call('rbind',shapes)
        #shapes2 <- sp::SpatialPointsDataFrame(shapes2, data = data.frame(1))
        #col <- rep("black",length(shapes2))
        #layerId <- rep(paste0("foo ",length(shapes)+1),length(shapes2))

        poly_col <- "black"
        layerId <- paste0("foo ",length(shapes)+1)


      }

      # if the input selected is a specific shape, just add that shape
      leaflet::leafletProxy("mymap")  %>%
        leaflet::removeShape(layerId = layerId2) %>%
        leaflet::addPolygons(data = shapes2,
                             color = poly_col,
                             fill = FALSE,
                             weight = 1,
                             smoothFactor = 0.5,
                             opacity = 1.0,
                             fillOpacity = 0.5, layerId = layerId)

      v$shape <- SHAPE

    }

    # # # # # # # #

  })

  ##################################################################
  ##################################################################
  ##################################################################
  ##################################################################
  # DATA READ-IN MODULE
  ##################################################################
  ##################################################################
  ##################################################################
  ##################################################################
  source(file.path(appDir,"DataReadModule.R"), local = TRUE, chdir = TRUE)
  DataReadModule()

  ##################################################################
  ##################################################################
  ##################################################################
  ##################################################################
  # DATA EXPLORER MODULE
  ##################################################################
  ##################################################################
  ##################################################################
  ##################################################################
  source(file.path(appDir,"DataExplorerModule.R"), local = TRUE, chdir = TRUE)
  DataExplorerModule()

  ##################################################################
  ##################################################################
  ##################################################################
  ##################################################################
  # DATA ANALYTICS MODULE
  ##################################################################
  ##################################################################
  ##################################################################
  ##################################################################
  #source(file.path(appDir,"DataAnalyticsModule.R"), local = TRUE, chdir = TRUE)
  #DataAnalyticsModule()

  ##################################################################
  ##################################################################
  ##################################################################
  ##################################################################
  # DATASET MODULE
  ##################################################################
  ##################################################################
  ##################################################################
  ##################################################################
  source(file.path(appDir,"DatasetModule.R"), local = TRUE, chdir = TRUE)
  DatasetModule()

}
