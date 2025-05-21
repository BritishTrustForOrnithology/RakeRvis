DataExplorerModule <- function(ID=NULL, fun = NULL, ...) {

  moduleServer(
    ID,
    ## Below is the module function

    function(input, output, session) {

      ########################################
      ########################################
      ########################################
      ########################################
      # EXPLORE THE DATA
      ########################################
      ########################################
      ########################################
      ########################################

      # This is similar to the UvA approach to visualise acc'n data
      # which produces stacked plots of variables over time

      # e.g. altitude, speed, voltage etc etc. PLUS a leaflet (or cesian) map imbedded
      # this allows you to spatially visualise alongside covariates

      # this doesn't actually use any BTOTT functions

      # render a second TagID selection specific to this tab
      # and allow a reactive value to capture the data we are worling with
      # separate to other processes so as not to slow them down

      explore_data <- reactiveValues(d = NULL)

      ##### THIS PICKS THE LATEST DATA YOU HAVE AVAILABLE, i.e. onedata as loaded from start, or thinned etc etc
      observe({

        if(!is.null(onedat$onedata)){
          #print("Explorer: using onedata for starters")
          explore_data$d <- onedat$onedata

          # overwite the data we use with others that may have been generated in the workflow
          if(!is.null(check_twodata$d)){
            #print("Explorer: Using twodata")
            explore_data$d <- check_twodata$d
          }
          if(!is.null(check_threedata$d)){
            explore_data$d <- check_threedata$d
            #print("Explorer: Using threedata")
          }
          if(!is.null(check_fourdata$d)){
            explore_data$d <- check_fourdata$d
            #print("Explorer: Using fourdata")
          }
          if(is.null(onedat$onedata)){
            message("Explorer: No data supplied")
          }


          tids = unique(explore_data$d$TagID)


          output$TagID2 <- shiny::renderUI({
            shiny::selectInput(inputId = "TagID2",label = "TagID", tids)
          })

        }

        #react$TagID2 <- output$TagID2

      })

      # if explore data changes, then we need to nullify the
      # picker value check within bird DT range?
      # AND also TagID2 - needs to be updated
      # with the new tagID found in the data???
      # Or have TagID2 as a reactive element?
      #react <- reactiveValues(TagID2 = NULL)


      observeEvent(explore_data$d,{

        if(!is.null(explore_data$d)){

          slider_check$st <- NULL
          slider_check$en <- NULL

          # update TagID
          #tids = unique(explore_data$d$TagID)
          #shiny::updateSelectInput(session = session, inputId = "TagID2",label = "TagID", tids)

        }

      })


      # render a graph type button to switch between ggplot and plotly
      output$switch_graph_type <- shiny::renderUI({
        shiny::selectInput(inputId = "plot_format",label = "Graph type", c("ggplot","plotly"), selected = "ggplot")
        })


      picker <- reactiveValues(choices = NULL, final_select = NULL)

      # This next section is to render a drop down to select covariates for plotting
      # need to first detect the covariates available to the user in the data
      observe({

        if(!is.null(explore_data$d)){

          getvars = explore_data$d # i.e. anything but TagID DateTime, latitude longitude x, X, y, Y and anything not a factor? Not sure about latter

          #getvars = data_1800[[1]]
          #apply(test,MARGIN = 2,typeof)
          out <- list()
          for(i in 1:length(names(getvars))){
            out[[i]] <- data.frame(nm = names(getvars)[i], class = class(getvars[,i]))
          }
          out <- do.call('rbind', out)
          out <- out[!out$nm %in% c("TagID", "DateTime"),]
          out <- out[out$class != "character" & out$class != "logical",]

          # select key ones

          st <- NULL
          if(any(grepl("alt", out$nm))){
            st <- c(st,out$nm[grep("alt", out$nm)])
          }
          if(any(grepl("height", out$nm))){
            st <- c(st,out$nm[grep("height", out$nm)])
          }
          if(any(grepl("speed", out$nm))){
            st <- c(st,out$nm[grep("speed", out$nm)])
          }
          if(any(grepl("temperature", out$nm))){
            st <- c(st,out$nm[grep("temperature", out$nm)])
          }
          if(any(grepl("sat", out$nm))){
            st <- c(st,out$nm[grep("sat", out$nm)])
          }
          if(any(grepl("pressure", out$nm))){
            st <- c(st,out$nm[grep("pressure", out$nm)])
          }

          # UvA loads of speeds slim down
          st = st[!st %in% c("speed_accuracy", "x_speed", "y_speed", "z_speed")]

          # if altitude AND altitude agl present - pick altitude agl by default
          if(any(grepl("altitude_agl", st)) && any(grepl("altitude", st))){
            st = st[!st %in% "altitude"]
          }

          # pick speed 3d over speed 2d
          if(any(grepl("speed_2d", st)) && any(grepl("speed_3d", st))){
            st = st[!st %in% "speed_2d"]
          }

          # are any of the columns all nulls or NAs?
          getvars2 <- subset(getvars, select = names(getvars) %in% st)
          na_checker = apply(getvars2, MARGIN = 2, function(x){ all(is.na(x)) })
          getvars2 = getvars2[,which(as.vector(!na_checker))]

          final_select = names(getvars2)

          #print(final_select)

          picker$choices <- out$nm
          picker$final_select <- final_select

          output$switch_covariate <- shiny::renderUI({
            shiny::selectInput(inputId = "switch_cov",label = "Covariates", choices = out$nm, selected = final_select, multiple = TRUE)
          })

        }


      })




      #class(test$DateTime)

      # or have a UvA-style or Movetech style?
      # smart or default option to plot anything with altitude in there? Or first one it finds,
      # and same for "speed" or "ve



      ########################################################################
      # remember the current data
      # Select TagID from the original explore data read in, then remembers the
      # selection per TagID and updates the time slider
      # so the further reactive below can then select time slider range within this bound
      # then later in turn this can be used in xy geo plots and DateTime plots
      # returns current_data$d

      # initial reactives
      Cls <- reactiveValues(COL = NULL, COLS = NULL)
      current_data <- reactiveValues(d = NULL)

      # this is so hard to get right - we have to listen for the changes in TagID
      # to update the time selector range AND then have the user select the time period
      # AND then store that current selection AND listen for the covariates

      #######################################################################
      ### DATETIME SLIDER
      ### Separate so it does not interfere with the other map(s)
      #######################################################################

      # also new idea to plot NEW tagIDs within the given slider range
      # IF IT IS POSSIBLE. This may need further reactive stores
      # to snatch the current values and perform logic test if found
      # in the range, i.e. use if found, if not, default to the TagID max min range

      slider_check = reactiveValues(st = NULL, en = NULL)

      shiny::observeEvent({
        input$TagID2
        explore_data$d
        },{

          #message(print(explore_data$d))
          if(!is.null(input$TagID2) && input$TagID2 %in% explore_data$d$TagID){

            if(any(input$TagID2 != 'All')){
              test_ <- explore_data$d[explore_data$d$TagID %in% input$TagID2, ]
            } else{
              test_ <- explore_data$d
            }
            test_$TagID <- as.factor(test_$TagID)

            #print(input$TagID2)
            #print(test_)

            if(!is.null(explore_data$d)){

              sliderParams_ <- reactiveValues(
                max = as.Date(max(test_$DateTime, na.rm = TRUE)),
                min = as.Date(min(test_$DateTime, na.rm = TRUE)),
                value = as.Date(range(test_$DateTime, na.rm = TRUE)))

              if(!is.null(slider_check$st) && !is.null(slider_check$en)){

                # check if the current sldier range is within the bird range

                #print(slider_check$st)

                if(slider_check$st >= min(test_$DateTime) &
                   slider_check$en <= max(test_$DateTime)
                ){
                  # then value of slider updated for this range rather than bird max min
                  sliderParams_$value <- c(slider_check$st, slider_check$en)

                }

              }

            } else if(is.null(explore_data$d)){
              # placeholder range if nothing specified
              sliderParams_ <- reactiveValues(
                max = as.Date("2014-05-01 12:00:00"),
                min = as.Date("2014-07-01 12:00:00"),
                value = c("2014-05-01 12:00:00","2014-07-01 12:00:00"))
            }

            #sliderParams2 <- reactiveValues(
            #  value = c(input$slider[1],input$slider[2]))

            output$slider2 <- shiny::renderUI({

              #sliderInput or updateSliderInput?
              shiny::sliderInput(inputId = "slider2",
                                 label = "Select time range",
                                 value = sliderParams_$value,
                                 min = sliderParams_$min,
                                 max = sliderParams_$max,
                                 width = "100%")

            })
          }




      })


      ###############################################
      # As with main base module, select TagID and slide range to give current data and update the time slider
      # Note, this does mean a new bird selection above will always reset the slider to the range
      # of that individual... perhaps should have something more reactive to check
      # if that new bird selected can be used for a reactive time selection by the user??

      shiny::observeEvent({
        explore_data$d
        input$TagID2
        input$slider2
      },{

        #req(explore_data$d, input$TagID2, input$slider2)

        #print(input$TagID2)
        #print(input$slider2[1])
        #print(input$slider2[2])

        # mad if statement - basically if the data is updated, then
        # the input$TagID2 and input$slider2 are both potentially still
        # pointing to references that will nolonger be found in the new data
        # no matter how many updateinput commands or reactive captures you implement
        # May be better perhas will some form of eventreactive maybe

        if(!is.null(explore_data$d) && !is.null(input$TagID2) && input$TagID2 %in% explore_data$d$TagID &&
           !is.null(input$slider2) && input$slider2[1] >= min(explore_data$d$DateTime) && input$slider2[2] <= max(explore_data$d$DateTime)){

          slider_check$st = input$slider2[1]
          slider_check$en = input$slider2[2]

          #print(slider_check$st)


          if(length(input$TagID2) == 1){

            data_ <- explore_data$d
            data_$TagID <- as.factor(data_$TagID)
            pal <-  onedat$pal
            data_$COL <- pal(data_$TagID) # full colouor pallette across TagIDs we have in full data

            TAGID2 = input$TagID2

            ## subset the data based on the choice
            data_2 <- data_[data_$TagID %in% TAGID2, ]

            # store colours as reactive values
            #Cls$COLS = unique(Cls$COL) #??

            #print(Cls$COL)
            #print(unique(data_2$COL))

            data_3 <- data_2[data_2$DateTime >= input$slider2[1] & data_2$DateTime <= input$slider2[2],]

            # BELOW IS NOT NEEDED NOW I THINK - IT CAUSES
            # AN INFINITE LOOP ON GRAPHICS THAT ISOLATE CANNOT SOLVE
            # plus we re-build the slider above anyway so that is the conflict

            ### ignore.....
            ### then update the time slider for this range
            ### note had originally had as an oberve in an oberve! which caused issues
            ###val <- input$slider2
            #isolate({updateSliderInput(session, "slider2", value = val)})
            ### stop ignoring ...

            current_data$d <- data_3

            #print(current_data$d)

          }



        }

      })


      #############################################################
      # the scatter plots and xy plotting
      # this eventReactive takes the current subsetted TagID and DateTime selected data
      # and subsets it for whatever covaraite variables we have selected from input$switch_cov
      #https://tbradley1013.github.io/2018/08/10/create-a-dynamic-number-of-ui-elements-in-shiny-with-purrr/

      # had confusion here - it needs the input covariates to fire off this reative
      # not JUST the current data - i.e. the current data may not have changed and then if you want to
      # select different variables from that unchanged data, this wasn't firing and new variables couldn't be added
      # to the plot stack! So it has to listen for changes in the variables you want to plot
      # PLUS this function also needs to fire off with TagID and the slider!

      # weird issue with switch between plotly and ggplot broken
      # ! now fixed as conflict in UI naming on output downstream ! :)

      wq_data <- reactiveValues(output = NULL)

      #wq_data <- eventReactive(input$submit_cov, {
      #wq_data <- eventReactive({
      observeEvent({
        input$switch_cov
        input$TagID2
        input$slider2
        #input$plot_format
        },{

        #req(current_data$d, input$switch_cov)
        req(current_data$d)

          if(!is.null(explore_data$d) && !is.null(input$TagID2) && input$TagID2 %in% explore_data$d$TagID &&
             !is.null(input$slider2) && input$slider2[1] >= min(explore_data$d$DateTime) && input$slider2[2] <= max(explore_data$d$DateTime)){

            # select the covariate
            if(!is.null(input$switch_cov)){

              if(!is.null(current_data$d)){

                #print(current_data$d)

                output <- current_data$d

                #output <- data_1800[[1]]
                #cov <- c("DateTime","TagID", "altitude_agl", "speed_3d")
                cov <- c("DateTime","TagID","COL",input$switch_cov)

                output = tibble::tibble(output[,which(names(output) %in% cov)]) # keep TagID, DateTime AND whetever covariates are selected
                #names(output)[2] <- "result"

                #print(output)

                wq_data$Output <- output

              }

              #return(output)
            }

          }

      })

      ### DEV note still need to align the plot y axes given varying
      ### y axis numeric digits per plot

      ## functions
      # plotly
      plotly_mine <- function(data,
                              #variable = "altitude_agl",
                              mode = "lines+markers",
                              margin = list(l = 10, r = 2, b = 2, t = 2, pad = 2),  #m2 <- list(l = 10, r = 2, b = 10, t = 2, pad = 2)
                              #yaxnm = "Altitude AGL (m)",
                              xaxnm = "", #DateTime (UTC)
                              tick_cex = 4, title_cex = 4, cex = 4, lwd = 0.5,
                              showticklabels = TRUE){

        #print(data)

        yaxnm = unique(data$parameter2)

        # a way of making sure any light colour choices show up on white background
        use_col = colorspace::darken(unique(data$COL), 0.2)

        ep <- plotly::plot_ly(
          data = data,
          color = ~TagID,
          colors = use_col,
          type = 'scatter',
          mode = mode,
          showlegend = FALSE,
          marker = list(size = 4),
          line = list(width = 0.1),
          #text = ~tripNo,
          text = ~TagID,
          hovertemplate = paste('<b>TagID: </b>: %{text}',
                                '<br><b>Y</b>: %{y:.2f}<br>',
                                '<b>DateTime: </b>: %{x:.%H:%M:%S}</br>'),
          x = ~DateTime,
          #y = ~altitude_agl
          #y = data[,variable]
          y = ~result

        ) %>%
          # posthoc layout changes
          plotly::layout(
            #autosize = FALSE,
            #plot_bgcolor = "#e5ecf6",
            margin = margin,

            xaxis = list(
              showticklabels = showticklabels,
              title = xaxnm,
              type = 'date',
              value = "%H:%M m",
              tickfont = list(size = tick_cex),
              titlefont = list(size = title_cex)
            ),
            yaxis = list(
              showticklabels = showticklabels,
              title = yaxnm,
              tickfont = list(size = tick_cex),
              titlefont = list(size = title_cex)
            )
            #, legend = list(title=list(text='<b> Species of Iris </b>'))
          )

        return(ep)

      }


      # ggplot
      ggplot_mine <- function(data,
                              #variable = "altitude_agl",
                              #yaxnm = "Altitude (m)",
                              xaxnm = NULL, #"DateTime (UTC)"
                              title_cex = 4,
                              tick_cex = 4
                              ){

        #print(data)

        use_col = colorspace::darken(unique(data$COL), 0.2)
        yaxnm = unique(data$parameter2)

        g <- ggplot2::ggplot(data, ggplot2::aes(x = DateTime,
                                                #y = data[,variable],
                                                y = result,
                                                color = TagID)) +
          ggplot2::geom_point(size = 2, shape = 21) +
          ggplot2::scale_fill_manual(values = "white") +
          ggplot2::scale_color_manual(values = use_col) +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.position = "none",
                         axis.text =  ggplot2::element_text(size = tick_cex),
                         axis.title =  ggplot2::element_text(size = title_cex)) +
          ggplot2::scale_y_continuous(name = yaxnm)

        if(!is.null(xaxnm)){
          g <- g + ggplot2::ylab("DateTime (UTC)")
        } else{
          g <- g + ggplot2::theme(axis.title.x = ggplot2::element_blank())
        }

        return(g)

      }

      # if the plot format button is pressed direct the process to
      # use the correct function
      fun <- reactiveValues(use = NULL)

      observeEvent(input$plot_format,{

        # select the function depending on user selection of plot type
        if(input$plot_format == "ggplot"){fun$use = ggplot_mine}
        if(input$plot_format == "plotly"){fun$use = plotly_mine}


      })



      ###### This next reactive listens to the current data to change covriates
      ###### and RECALCULATES A VALUE - i.e. event Reactive rather than observeEvent
      ###### here using tidyverse to map across column variables, given input
      ###### again fires off on the submit button, and requires wq_data() FUNCTION given selected covariates
      ###### note in teh example following for this, they have a grou_by parameter, which I think is a
      ###### single column in their data - we have to do by columns instead as that is where our covariates are
      ###### i.e. not in long format
      ###### Downsie to this is it will always map the whole set
      ###### even if we just want to tack another graph on the end


      #graphs <- eventReactive(input$submit_cov, {
      graphs <- eventReactive({
        wq_data$Output
        input$plot_format # listen for change in plot type to direct to type of plot wanted
        input$cov_title_size
        input$cov_tick_size
      },{

        #if(!is.null(wq_data$Output)){
        #  output$graphs_ui <- NULL
        #}

        #print(input$switch_cov)
        #print(current_data$d)
        #print(input$plot_format)

        #req(wq_data()) # requiring also switch covariates , input$switch_cov

        if(!is.null(wq_data$Output) && !is.null(input$switch_cov) && !is.null(input$plot_format) && !is.null(input$cov_title_size) && !is.null(input$cov_tick_size)){

          # this is not ideal to pivot_longer because it creates quite a long dataset perhaps unneccessarily if there is another way of doing this, but memory wise may be no different!
          #wq_data() %>%
          wq_data$Output %>%
            tidyr::pivot_longer(cols = input$switch_cov,
                                  names_to = "parameter", values_to = "result") %>%
            dplyr::mutate(parameter2 = parameter) %>%
            dplyr::group_by(parameter) %>%
            tidyr::nest() %>%
            dplyr::mutate(
              graphs = purrr::map(data, fun$use, tick_cex = input$cov_tick_size, title_cex = input$cov_title_size)  # this applies the function of either ggplot or plotly
            ) %>%
            dplyr::arrange(parameter) %>%
            dplyr::pull(graphs) # pull out final graph element calculations, which is the stored plotly or ggplot, which will be a function technically i.e. graphs()

            #rm(test)
            #test <<- wq_data()
            #print(test)
        }

      })


      #observeEvent(input$submit_cov, {
      observeEvent({
        wq_data$Output
        input$plot_format
        input$cov_title_size
        input$cov_tick_size
        },{

       # print("test_100")

        #print(wq_data$Output)

        req(graphs())

          #test <<- graphs()
          #print(test)

          if(!is.null(wq_data$Output) && !is.null(input$switch_cov) && !is.null(input$plot_format) && !is.null(input$cov_title_size) && !is.null(input$cov_tick_size)){

            if(input$plot_format == "plotly"){
              purrr::iwalk(graphs(), ~{
                output_name_p <- paste0("ep", .y)
                output[[output_name_p]] <- plotly::renderPlotly(.x)
              })
            }
            if(input$plot_format == "ggplot"){
              #print("ggplot_test2")

              purrr::iwalk(graphs(), ~{
                output_name_g <- paste0("eg", .y)
                output[[output_name_g]] <- shiny::renderPlot(.x)
              })
            }

          }

      })

      #input$plot_format
      #input$switch_cov # the variables the user wishes to plot

      #### Now do the actual plotting of the maps via purrr:imp of the listed graph elements produced above
      #### also wrapped up in an observeEvent to listen for changes to map height in settings

      observeEvent({
        input$cov_setting
        input$cov_title_size
        input$cov_tick_size
        },{

          if(!is.null(input$cov_setting)){

            output$graphs_ui <- renderUI({

              req(graphs()) #,◘ input$switch_cov

              if(!is.null(wq_data$Output) && !is.null(input$switch_cov) && !is.null(input$plot_format) && !is.null(input$cov_title_size) && !is.null(input$cov_tick_size)){
                if(input$plot_format == "plotly"){

                  #print("plotly_test")
                  #print(graphs())

                  plots_list <- purrr::imap(graphs(), ~{
                    htmltools::tagList(
                      plotly::plotlyOutput(
                        outputId = paste0("ep", .y),
                        #height = "100px"
                        height = input$cov_setting
                      ),
                      br()
                    )

                  })

                }

                if(input$plot_format == "ggplot"){

                  #print("ggplot_test")

                  #print("ggplot_test3")
                  plots_list <- purrr::imap(graphs(), ~{
                    htmltools::tagList(
                      shiny::plotOutput(
                        outputId = paste0("eg", .y),
                        #height = "100px"
                        height = input$cov_setting
                      ),
                      br()
                    )
                  })

                }
              }
              tagList(plots_list)
              #print(plots_list_plotly)
            })

          }

      })



      #####################################################################################
      # set up a separate module to call the reactive leaflet
      # in turn to be observed for fire up in presence of particular dataset orders
      # Note this is the same as the trip version so could be grouped as a single function across all maps

      map_explorer_reactive <- reactiveValues(d = NULL)

      ExplorerReactiveMapModule <- function(ID = NULL, fun = NULL, data_use = NULL, ...){
        moduleServer(ID,
                     function(input,output,session){

                       # get centre of the data for centring the map

                       if(is.null(data_use)){
                         xc <- -3.2
                         yc <- 54
                       } else{
                         xc = mean(data_use$longitude,na.rm = TRUE)
                         yc = mean(data_use$latitude,na.rm = TRUE)
                       }

                       map_reactive_explorer <- reactive({

                         if("GoogleEarth" %in% Providers){
                           Providers_GE <- "GoogleEarth"
                           Providers <- Providers[Providers != "GoogleEarth"]
                         }
                         m = leaflet::leaflet() %>% leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE, zoomControl = FALSE)) %>%

                           # ADDITIONAL BIT
                           # moving the +- button: https://stackoverflow.com/questions/71013017/move-zoom-controls-in-leaflet-for-r-shiny
                           # this is to allow the trips selector menu to set more neatly at the top left of the map
                           htmlwidgets::onRender( # java script code coes in the JsCode arg
                             "function(el, x) {
                              L.control.zoom({position:'topright'}).addTo(this);
                          }")

                         # THIS IS THE SAME AS THE ORIGINAL MAP SEE COMMENTS THERE:
                         if(exists("Providers_GE")){
                           m <- m %>% leaflet::addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google', options = leaflet::providerTileOptions(updateWhenIdle = TRUE, updateWhenZooming = FALSE))
                         }
                         for(i in 1:length(Providers)){
                           m <- m %>% leaflet::addProviderTiles(Providers[i], group = Providers[i],
                                                                options = leaflet::providerTileOptions(updateWhenIdle = TRUE, updateWhenZooming = FALSE)
                           )
                         }
                         if(exists("Providers_GE")){
                           Providers <- c(Providers, "GoogleEarth")
                         }
                         m <- m %>% leaflet::addLayersControl(
                           baseGroups = Providers,
                           options = leaflet::layersControlOptions(collapsed = TRUE)) %>%
                           leaflet::mapOptions(zoomToLimits = "never") %>%
                           leaflet::addScaleBar(position = "bottomleft")
                         #m <- m %>% leaflet::setView(lat = mean(check_fourdata$d$latitude), lng = mean(check_fourdata$d$longitude), zoom = 9)
                         m <- m %>% leaflet::setView(lat = yc, lng = xc, zoom = 9)

                         # add right mouse click (not sure needed)
                         m = m  %>%
                           htmlwidgets::onRender("
              function(el,x) {
              mymap = this;
              mymap.on('contextmenu', function(e) {
              var coords = {lng: e.latlng.lng, lat: e.latlng.lat}
              Shiny.setInputValue('mymap_trips_right_click', coords);
              });
              }
          ")
                         m
                       })
                       map_explorer_reactive$d <- map_reactive_explorer
                     })

      }

      observe({
        ExplorerReactiveMapModule(data_use = explore_data$d)
      })

      output$mymap_explorer <- leaflet::renderLeaflet({
        map_explorer_reactive <- map_explorer_reactive$d
        map_explorer_reactive()
      })


      ############################################################################
      ############################################################################
      # Plot on the leaflet map
      ############################################################################
      ############################################################################
      # only one tagID at a time in this at the moment

      observeEvent(current_data$d,{

        if(!is.null(current_data$d)){

          use_dat2 <- current_data$d

          # colours (points use the full column, lines use vectorlength == TagID)
          COL = colorspace::darken(unique(current_data$d$COL), 0.2)
          COLS <- unique(COL)

          use_dat2$group <- "six"

          leaflet::leafletProxy("mymap_explorer") %>%
            leaflet::clearMarkers() %>%
            leaflet::clearControls() %>%
            leaflet::clearGroup(group = "six") #%>%
          #leaflet::removeShape(layerId = "expl_point") %>%
          #leaflet::removeMarker(layerId = "expl_point")

          if(dim(use_dat2)[1] > 0){

            ### LINES FROM THE POINTS SELECTED
            ta <- data.frame(long = use_dat2$longitude, lat = use_dat2$latitude, TagID = use_dat2$TagID)

            if(length(unique(ta$TagID)) == 1){
              taa <- points_to_line(ta, "long","lat")
            } else{
              taa <- points_to_line(ta, "long","lat", id_field = "TagID")
            }

            # LEAFLET COLOURING OF POINTS - whether custom colours or colour palette defined earlier
            POPUP_TRIP <- paste("<b>", use_dat2$TagID,"</b>", "<br>",
                                "<b>","Trip Number:", "</b>", use_dat2$tripNo, "<br>",
                                "<b>","DateTime:", "</b>", use_dat2$DateTime, "<br>",
                                "<b>","Latitude:", "</b>", round(use_dat2$latitude,5), "<br>",
                                "<b>","Longitude:", "</b>", round(use_dat2$longitude,5), "<br>"
                                #,
                                #"<b>","Altitude (m):", "</b>", use_dat2$altitude_agl, "<br>",
                                #"<b>","Satellites used:", "</b>", use_dat2$satellites_used, "<br>",
                                #"<b>","Speed (m/s):","</b>", round(use_dat2$speed_2d,5)
                                )


            #################################################
            ### LINES
            # if lines for fixes selected in function by user AND if the toggle is TRUE

            store_foo_ <- list()
            if(!is.null(taa)){

              for(i in 1:length(taa)){

                leaflet::leafletProxy("mymap_explorer") %>%
                  leaflet::addPolylines(data = taa[i], weight = 2, color = COLS[i], opacity = 0.5, fillOpacity = 0.5, layerId = paste0("foo_","_",i))

                # for remembering last selection
                store_foo_[[i]] = paste0("foo_","_",i)

              }
              store_foo_$d = do.call('rbind',store_foo_)[,1]

            } else{

              if(!is.null(store_foo_$d)){

                for(i in 1:length(store_foo_$d)){

                  leaflet::leafletProxy("mymap_explorer") %>%
                    leaflet::removeShape(layerId = store_foo_$d[i])

                }
              }

            }

            #################################################
            ### POINTS
            leaflet::leafletProxy("mymap_explorer") %>%
              leaflet::addCircleMarkers(layerId = use_dat2$DateTime, group = use_dat2$group, lat = use_dat2$latitude, lng = use_dat2$longitude,
                                        radius= 5, fillOpacity = 0.5, stroke = FALSE, color=COL,
                                        popup = POPUP_TRIP)



          }


        }


      })


      ##############################################################################################
      # Battery voltage
      # this is tricky as the UvA way of doing this is tag by tag, which I like
      # but at the moment the setup is not coded for all TagIDs
      # perhaps it should be
      # The reason I didn't do that is because if the user load in a load of 10 s data from several individuals
      # and uses plotly, the app will take forever to plot!
      # So I am not too sure how to handle that - or to somehow assess the memory size and prevent plotting??
      #
      # Movetech has the battery voltage in the file already but as far as I can see not th solar charge??









      ##################################
      # store the current ggplot or plotly elements for adding vertical lines to on marker click
      #
      ##### for storing the map outputs for adding vertical lines to!
      #vlines <- shiny::reactiveValues(ep1 = NULL, ep2 = NULL, ep3 = NULL, ep4 = NULL, ep5 = NULL)
      #
      #vlines$ep1 <- ep1
      #vlines$ep2 <- ep2
      #vlines$ep3 <- ep3
      #vlines$ep4 <- ep4
      #vlines$ep5 <- ep5
      #
      # Add a vertical line on xy plots for the point hovered over on leaflet? Or easier for click
      #     click_explorer <- shiny::reactiveValues(d = NULL)
      #
      #     ### but really don't want to replot the whole darn map! so in that case would
      #      ### ALSO NEED TO HAVE THE MAP ELEMENTS STORED AS REACTIVE VALUES
      #      # AS THEY ARE BUILT UNDER GGPLOT AND PLOTLY SO CAN BE RE-ACCESSED AND ADDED TO!
      #      # so do in one go when marker is clicked, get the DateTime, and add it to the current maps
      #
      #      observeEvent({
      #        input$mymap_explorer_marker_click
      #      },{
      #
      #        # prob not needing to be a reactive element now? e.g. could acess and plot the input directly?
      #        click_explorer$d <- input$mymap_explorer_marker_click$id
      #
      #        DateTime <- as.POSIXct(click_explorer$d, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      #
      #        print(DateTime)
      #
      #        if(!is.null(input$plot_format)){
      #
      #         # then do the ggplot adding of the line
      #          if(input$plot_format == "ggplot"){
      #
      #            # get the latest plots the user generated:
      #            # this is terrible and should be a funciton but for now.....
      #            g1 <- vlines$ep1
      #            g2 <- vlines$ep2
      #            g3 <- vlines$ep3
      #            g4 <- vlines$ep4
      #            g5 <- vlines$ep5
      #
      #            g1 <- g1 + ggplot2::geom_vline(xintercept = DateTime, colouor = "black", linetype = "dashed", size = 1)
      #            g2 <- g2 + ggplot2::geom_vline(xintercept = DateTime, colouor = "black", linetype = "dashed", size = 1)
      #            g3 <- g3 + ggplot2::geom_vline(xintercept = DateTime, colouor = "black", linetype = "dashed", size = 1)
      #            g4 <- g4 + ggplot2::geom_vline(xintercept = DateTime, colouor = "black", linetype = "dashed", size = 1)
      #            g5 <- g5 + ggplot2::geom_vline(xintercept = DateTime, colouor = "black", linetype = "dashed", size = 1)
      #
      #            # this may completely defeat the purpose and re-generating the whole plot...althoght split in parts
      #            output$ep1 <- shiny::renderUI({
      #              output$ggplot_altitude <- shiny::renderPlot(g1)
      #              shiny::plotOutput("ggplot_altitude", height = "100px")
      #            })
      #            output$ep2 <- shiny::renderUI({
      #              output$ggplot_speed3d <- shiny::renderPlot(g2)
      #              shiny::plotOutput("ggplot_speed3d", height = "100px")
      #            })
      #            output$ep3 <- shiny::renderUI({
      #              output$ggplot_traj <- shiny::renderPlot(g3)
      #              shiny::plotOutput("ggplot_traj", height = "100px")
      #            })
      #            output$ep4 <- shiny::renderUI({
      #              output$ggplot_nsat <- shiny::renderPlot(g4)
      #              shiny::plotOutput("ggplot_nsat", height = "100px")
      #            })
      #            output$ep5 <- shiny::renderUI({
      #              output$ggplot_temp <- shiny::renderPlot(g5)
      #              shiny::plotOutput("ggplot_temp", height = "100px")
      #            })
      #          }
      #        }
      #
      #      })








#      # testing in open code
#      test_data_0 <- tibble(data_1800[[1]])
#      cov <- c("DateTime","TagID", "altitude_agl", "speed_3d")
#      test_data = test_data_0[,which(names(test_data_0) %in% cov)] # keep TagID, DateTime AND whetever covariates are selected
#      #names(output)[2] <- "result"
#
#      cov2 <- c("altitude_agl", "speed_3d")
#
#      plotly_mine = function(data){
#        print("hello")
#      }
#
#      #group_by(altitude_agl, speed_3d) %>%
#      cov2 <- c("altitude_agl", "speed_3d")
#      test_data %>%
#        tidyr::pivot_longer(cols = cov2,
#          names_to = "parameter", values_to = "result") %>%
#        group_by(parameter) %>%
#        tidyr::nest() %>%
#        mutate(
#          graphs = purrr::map(data,plotly_mine)
#        ) %>%
#        arrange(altitude_agl, speed_3d) %>%
#        pull(graphs)
#
#      # their example is in long format
#      test_data1 <- test_data_0[,which(names(test_data_0) %in% c("DateTime","TagID", "altitude_agl"))]
#      test_data2 <- test_data_0[,which(names(test_data_0) %in% c("DateTime","TagID", "speed_3d"))]
#      names(test_data1)[3] <- "result"
#      test_data1$parameter = "altitude_agl"
#
#      names(test_data2)[3] <- "result"
#      test_data2$parameter = "speed_3d"
#
#      test_data <- rbind(test_data1, test_data2)
#
#      test_data %>%
#        group_by(parameter) %>% ############## <<<<-------- this is my pinchpoint - needing to feed through here but we have columns not stacked parameter to select on
#        tidyr::nest() %>%
#        mutate(
#          graphs = purrr::map(data, plotly_mine)  # this applies the function of either ggplot or plotly
#        ) %>%
#        arrange(parameter) %>%
#        pull(graphs)








    }

  )

}







