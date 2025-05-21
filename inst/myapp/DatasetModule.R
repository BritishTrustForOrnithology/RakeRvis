DatasetModule <- function(ID=NULL, fun = NULL, ...){

  shiny::moduleServer(
    ID,
    ## Below is the module function

    function(input, output, session) {

      # observe a change in the value in mappy_data (in the server)
      # if advances, then we update the sortable list
      onedata_labels = shiny::reactiveValues(labs = NULL, downl = NULL, nanim = NULL, source = NULL, df2use = NULL)

      plotted = shiny::reactiveValues(test = NULL)

      ################################################################################
      ###################### SOURCING IN DATA
      #dataframe <- reactive({
      # not sure we really need this as a reactive if defining new read in data from the input

      observeEvent(input$datafile,{

        if(is.null(input$datafile)){
          return(NULL)
        } else{

          data <- read.csv(input$datafile$datapath, sep = ",")
          #data<- data %>% group_by(C) %>% mutate(A) %>%
          #  mutate(B) %>% mutate(add = (A+B)) %>% mutate(sub = (A-B))

          # check for likely Movebank formatted data

          #if(exists('individual.local.identifier', data)){
          #  data$TagID <- data$'individual.local.identifier'
          #}
          #if(exists('location.long', data)){
          #  data$longitude <- data$'location.long'
          #}
          #if(exists('location.lat', data)){
          #  data$latitude <- data$'location.lat'
          #}
          #if(exists('timestamp', data)){
          #  data$timestamp <- gsub(".000", "", data$timestamp)
          #  data$DateTime <- MoveRakeR::datetime(data$timestamp)
          #}

          dropsat = 0
          flt_switch = FALSE
          mindata = 0

          .finalise_tracks <- function(data, option){

            #print(unique(data$individual.local.identifier))
            # proceed with normal processing
            # this is always needed

            if(exists("timestamp",data)){

              data$timestamp <- gsub(".000", "", data$timestamp)
              data$DateTime <- MoveRakeR::datetime(data$timestamp)

              #data$DateTime <- as.POSIXct(data$timestamp,format = "%Y-%m-%d %H:%M:%S",tz="UTC")
            } else{
              stop("MB data should have 'timestamp' if you want to read MB data into BTOTT a 'Track' format")
            }
            if(exists("location.long",data)){
              data$longitude <- data$location.long
            } else{
              stop("MB data should have 'location.long' if you want to read MB data into BTOTT a 'Track' format")
            }
            if(exists("location.lat",data)){
              data$latitude <- data$location.lat
            } else{
              stop("MB data should have 'location.lat' if you want to read MB data into BTOTT a 'Track' format")
            }
            if(exists("individual.local.identifier",data)){
              data$TagID <- data$individual.local.identifier
            } else{
              stop("MB data should have 'individual.local.identifier' if you want to read MB data into BTOTT a 'Track' format")
            }

            #### continue further processing for BTOTT
            if(option == "BTOTT"){

              # this is not ideal if both are present!! In which case would break out of BTOTT formatting....
              if(exists("height.above.ellipsoid",data)){
                names(data)[which(names(data) == "height.above.ellipsoid")] <- "altitude"
              }
              if(exists("height.above.msl",data)){
                names(data)[which(names(data) == "height.above.msl")] <- "altitude"
              }
              if(exists("ground.speed",data)){
                names(data)[which(names(data) == "ground.speed")] <- "ground.speed.MT"
              }
              if(exists("gps.satellite.count",data)){
                names(data)[which(names(data) == "gps.satellite.count")] <- "satellites_used"
              }
              if(exists("event.id",data)){
                names(data)[which(names(data) == "event.id")] <- "location"
              }
              if(exists("ground.speed",data)){
                names(data)[which(names(data) == "ground.speed")] <- "speed_2d"
              }
              if(exists("location.error.numerical",data)){
                names(data)[which(names(data) == "location.error.numerical")] <- "h_accuracy"
              }
              if(exists("vertical.error.numerical",data)){
                names(data)[which(names(data) == "vertical.error.numerical")] <- "v_accuracy"
              }
              if(exists("heading",data)){
                names(data)[which(names(data) == "heading")] <- "direction"
              }
              if(exists("external.temperature",data)){
                names(data)[which(names(data) == "external.temperature")] <- "temperature"
              }
              if(exists("barometric.pressure",data)){
                names(data)[which(names(data) == "barometric.pressure")] <- "pressure"
              } else{
                data$pressure <- NA # don't think there are any pressure measurements in the MT data yet, although is in example of Daniel's for Tysties
              }

              data$Type <- "MoveBank"
              data$gps.pdop <- NA   # in UvA but not Movetech
              data$altitude_agl <- NA # in UvA but not Movetech
              data$speed_3d <- NA
              data$gps_fixtime <- NA
              data$x_speed <- NA
              data$y_speed <- NA
              data$z_speed <- NA
              data$speed_accuracy <- NA

              # further in MB not in UVA (unless merged)
              if(exists("tag.voltage",data)){
                names(data)[which(names(data) == "tag.voltage")] <- "tag_voltage"
              } else{
                data$tag_voltage <- NA
              }
              if(exists("battery.charging.current",data)){
                names(data)[which(names(data) == "battery.charging.current")] <- "battery_charging_current"
              } else{
                data$battery_charging_current <- NA
              }

            }

            #dataVo <- read_voltage_UvA(TagID, start=start, end=end)

            # common to all options
            data <- data[!is.na(data$latitude),]
            data <- data[!is.na(data$longitude),]

            if(nrow(data) > 0){

              data_sf <- sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
              data_sf <- sf::st_transform(data_sf, 3857)
              coords = sf::st_coordinates(data_sf)
              data$X <- coords[,1]
              data$Y <- coords[,2]
              data <- data[!duplicated(data$DateTime),]

              # TRAJ SPEED AND dist/dt
              dataXa <- adehabitatLT::as.ltraj(subset(data,select=c(X,Y)),data$DateTime,id=1)
              dataXa  <- dataXa[[1]]
              data$dist <- dataXa$dist
              data$dt <- dataXa$dt
              data$traj.speed <- data$dist / data$dt # in m/s

              # drop data with less or equal to than X satellites
              if(dropsat == TRUE){

                if(option == "BTOTT"){
                  if(exists("satellites_used",data)){
                    data <- data[data$satellites_used > dropsats,]
                  } else{
                    warning("No satellites_used column found, no further dropping of no. satellites < dropsat made")
                  }

                }

                if(option == "BTOTT_MB"){

                  if(exists("gps.satellite.count",data)){
                    data <- data[data$gps.satellite.count > dropsats,]
                  } else{
                    warning("No gps.satellite.count column found, no further dropping of no. satellites < dropsat made")
                  }
                }

              }

              flt = FALSE
              if(flt_switch){

                if(exists("flt.switch",data)){

                  # note this assumes flt.switch column -99 will always be the formatting used in MB
                  flt = TRUE # set to TRUE TO NOT FILL WITH ZEROS BELOW
                  ##### switch != 0 removed - I think this was covered anyway but making more explicit here
                  data$flt.switch <- ifelse(is.na(data$flt.switch),-99,data$flt.switch)
                  data$flt.switch <- ifelse(data$flt.switch != 0,-99,data$flt.switch)
                  #data <- data[data$flt.switch != -99,]

                  if(option == "BTOTT"){ # silly underscore renaming!
                    names(data)[which(names(data) == "flt.switch")] <- "flt_switch"
                  }

                } else{
                  warning("No flt.switch column found, no assessment of flt.switch values made")
                }

              }

              if(dim(data)[1] > 0){ # by removing duff data from MB via flt_switch can reduce some tag data to none

                data <- data[!is.na(data$latitude),]
                data <- data[!duplicated(data$DateTime),]

                if(option == "BTOTT"){
                  # reduce dataframe to a more manageable number of columns
                  # BUT also need to check we have the columns from the renaming above. And return a warning
                  # maybe .... although that would be a repeated warning across animals...

                  BTOTT_nm_check <- c("Type","TagID","DateTime","speed_2d","speed_3d","traj.speed","latitude","longitude","X","Y","dist","dt","satellites_used","gps_fixtime","gps.pdop","gps.hdop","altitude","altitude_agl","h_accuracy","v_accuracy","pressure","temperature","x_speed","y_speed","z_speed","speed_accuracy","direction","location","flt_switch","tag_voltage","battery_charging_current")

                  miss <- which(!BTOTT_nm_check %in% names(data))
                  if(length(miss) > 0){
                    nm_miss <- BTOTT_nm_check[miss]
                    # add as NA to the data

                    addin <- data.frame(matrix(NA, nrow = nrow(data), ncol = length(nm_miss)))
                    names(addin) <- nm_miss
                    data <- cbind(data,addin)

                  }

                  data <- subset(data,select = c(Type,TagID,DateTime,speed_2d,speed_3d,traj.speed,latitude,longitude,X,Y,dist,dt,satellites_used,gps_fixtime,gps.pdop,gps.hdop,altitude,altitude_agl,h_accuracy,v_accuracy,pressure,temperature,x_speed,y_speed,z_speed,speed_accuracy,direction,location,flt_switch,tag_voltage,battery_charging_current))

                  # flt_switch column is ALWAYS returned even if user doesn't want it OR if there is no flt.switch column in MB dataset!
                  # so if that's the case, force acceptance of all row values under BTOTT formatting
                  if(!flt){data$flt_switch <- 0}

                }


              } else{
                warning(paste0("Too little or no data for ", unique(data$TagID), " so excluded"))
                data <- NULL
                return(data)
              }


              if(!is.null(data)){
                if(dim(data)[1] < mindata){
                  warning(paste0("Too little or no data for ", TagID, " so excluded"))
                  data <- NULL
                  return(data)
                } # if less than X data points set data to NULL and skip it
                return(Track(data)) # both BTOTT and BTOTT_MB options give this
              }
            } else{
              return(NULL)
            }

          }

          data <- .finalise_tracks(data, option = "BTOTT")

          if(exists('TagID', data) & exists('longitude', data) & exists('latitude', data) & exists('DateTime', data)){

            attr(data, "source") <- "Local" # for use in the reactive table to display on map

            onedat$onedata_original <- data

          } else{
            warning("Data are not in Track format")
          }

          #dataDT = DT::datatable(
          #  data,
          #  options = list(
          #    initComplete = htmlwidgets::JS(
          #      "function(settings, json) {",
          #      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          #      "}"),
          #    searching = FALSE,
          #    pageLength = 5,
          #    lengthMenu = c(5, 10, 15, 20),
          #    scrollX = TRUE
          #  ) # close list for options
          #) # close datatable

          #return(dataDT)

        }

      })

      #output$table <- DT::renderDataTable({
      #  dataframe()
      #})

      ############################################################################################################
      ############################################################################################################
      ############################################################################################################
      ############################################################################################################
      # DOWNLOADING DATA
      ############################################################################################################
      ############################################################################################################
      ############################################################################################################
      ############################################################################################################

      thedata_dl <- reactive({

        # get the current selection and associated data:

        data_out <- current_selection_made$data
        data_out <- data_out[data_out$pick == TRUE,]

        if(nrow(data_out) > 0){ ## changed 02/05/25 to nrow() > 0; without re-running as surely that makes more sense?

          if(input$downloadData_view == "yes"){
            # if plotdata3 exists....use this, i.e. you may have filtered further...

            if(!is.null( plotdata$data3 )){
              onedata2 <- plotdata$data3
            } else{
              onedata2 <- thedata()
              #onedata2 <- plotdata$data2
            }
            current_data <- onedata2
          }
          if(input$downloadData_view == "no"){
            current_data <- mappy_data[[data_out$labels]]
          }

          return(current_data)

        } else{

          return(NULL)
        }

      })

      # Downloadable csv of selected dataset ----
      output$downloadData <- shiny::downloadHandler(

        filename = function() {
          paste0("myfile.csv")
        },
        content = function(file) {

          if(!is.null(thedata_dl())){
            shiny::withProgress(value = 1, message = 'downloading',{

              # this is a fake progress bar as I can't for now get this to work by file size - silly really!
              for(i in 1:10){
                   shiny::incProgress(1/15)
              #     Sys.sleep(0.25)
               }
              write.csv(thedata_dl(), file, row.names = FALSE)

            })

          }
        }
      )


      ###############################################################################################################
      ###############################################################################################################
      # NOTIFICATION: display the current data plotted
      ###############################################################################################################
      ###############################################################################################################

      # Use a notification menu to plot data and select data. The select data
      # will be blank if nothing is selected, but the selected data is KEY for
      # feeding that dataset through other processes, e.g. thinner, cleaner
      # so allows for a different plotted dataset to that fed through the process,
      # for full flexibility
      # plotted$test is the picked data from the reactive table when the display or combined buttons are pressed.
      # current_selection_made$data is solely that as highlighted in the table

      shiny::observeEvent({
        plotted$test
        current_selection_made$data
      },{

        if(mappy_data$deletion == 0){

          ########### THE DATA AS VISUALISED IN THE MAIN MAP
          # if more than one dataset is plotted in combination...loop over plotted_labels
          # build the notificationData menu for Dataset PLOTTED (from display and cobine buttono pushes) or initial fire up...

          # initialise the list to capture names of datasets
          notificationData$notificationData1 <- list()

          # If there is something IN the reactive dataset table, proceed, otherwise don't, i.e. table can be blank as as user can delete
          if(mappy_data$initialval > 0){

            # then first run will be NULL, if there is a dataset loaded already, use this
            # as it will have been plotted straight off

            if(is.null(plotted$test)){

              if(mappy_data$dataexist == 1){

                notificationData$notificationData1 <- data.frame(Data_plotted = paste("Dataset plotted:", mappy_data[[paste0("nm_onedata_", mappy_data$initialval)]]))
                #notificationData1 <- data.frame(Data_plotted = notificationData$new_nm)

              }

            }

            # if test is then populated through display after user interaction:
            # if not NULL, and nrow > 0
            if(!is.null(plotted$test)){

              if(nrow(plotted$test) > 0){

                for(i in 1:nrow(plotted$test)){
                  notificationData$notificationData1[[i]] = data.frame(Data_plotted = paste("Dataset plotted:", plotted$test$labels[i]))
                }
                notificationData$notificationData1 <- do.call('rbind', notificationData$notificationData1)

              }

              # if a new run through has been activated, e.g. through new data download or the cleaner, thinner or trips...
              # need some way of over-riding the current selections, so perhaps if the
              # notificationData1 does not match the last data run through label in server when table_clicked$click == 0
              # so even when the table has something selected an new data generated, the notification needs to update to latest in list
              # AS PLOTTED, because that is what happens, even for cleaned data!

              #if(nrow(notificationData1) == 1){
              #  if(gsub("Dataset plotted: ","",notificationData1) != notificationData$new_nm){
              #    notificationData1  <- data.frame(Data_plotted = paste("Dataset plotted:", notificationData$new_nm))
              #  }
              #}


            }

          }


          # transfer out of the reactive
          #notificationData1 <- notificationData$notificationData1

          ########### THE DATA SELECTED, i.e. for other parts of the app
          # build the notificationData menu for Dataset SELECTED
          selected_data <- current_selection_made$data

          notificationData$notificationData2 <- list()

          if(nrow(selected_data) > 0){

            selected_data <- selected_data[selected_data$pick == TRUE,]

            # if there is a current selection from the TRUE flag....
            if(nrow(selected_data) > 0){

              for(i in 1:length(selected_data$labels)){
                notificationData$notificationData2[[i]] = data.frame(Data_selected = paste("Dataset selected:", selected_data$labels[i]))
              }
              notificationData$notificationData2 <- do.call('rbind', notificationData$notificationData2)

            }

          }


          if(length(notificationData$notificationData1) == 0){

            # then nothing is able to be selected for further processing...
            notificationData2 <- list()
          }

          # remember the current notification2 SELECTED - a bit round about
          if(length(notificationData$notificationData2) > 0){
            if(length(notificationData$notificationData2) == 1){

              dataset_label <- gsub("Dataset selected: ","", notificationData$notificationData2$Data_selected)

              ##### >>>>> this next line is for use in the CLEANER, THINNER AND TRIPS, i.e. this is the dataset as selected to use <<<<< #####
              notificationData$Data <- mappy_data[[dataset_label[1]]]
              notificationData$sel <- dataset_label[1] # reactive here as also need to be updated following fresh run through of processes! even when not selected by the user, the dataset changes on new download or run of cleaner, thinner or trips

            }
          } else{

            notificationData$Data <- NULL
            notificationData$sel <- 0
          }

          #output$notificationoutput = renderText({
          #  if(is.null(input$linkClicked)){
          #    notificationitemid = "a"
          #  }else{
          #    notificationitemid = input$linkClicked
          #  }
          #  return(notificationitemid)
          #})

        } else{
          mappy_data$deletion <- 0
        }

      })


      # ------------------------------------------------------------------------- #
      # observe changes in the notifications 1 (plotted) and 2 (selected)

      shiny::observeEvent({
        notificationData$notificationData1
        notificationData$notificationData2

      },{

        # remember the current notification2 SELECTED - a bit round about
        if(length(notificationData$notificationData2) > 0){

          if(length(notificationData$notificationData2) == 1){

            dataset_label <- gsub("Dataset selected: ","", notificationData$notificationData2$Data_selected)

            ##### >>>>> this next line is for use in the CLEANER, THINNER AND TRIPS, i.e. this is the dataset as selected to use <<<<< #####
            notificationData$Data <- mappy_data[[dataset_label[1]]]
            notificationData$sel <- dataset_label[1] # reactive here as also need to be updated following fresh run through of processes! even when not selected by the user, the dataset changes on new download or run of cleaner, thinner or trips

          }
        } else{

          notificationData$Data <- NULL
          notificationData$sel <- 0
        }

        #### build message menu
        output$messageMenu <- shinydashboard::renderMenu({

          # USING NOTIFICATIONS
          # Code to generate each of the messageItems here, in a list. This assumes
          # that messageData is a data frame with two columns, 'from' and 'message'.
          #msgs <- apply(messageData, 1, function(x) {
          #  shinydashboard::messageItem(from = x[["from"]], message = x[["message"]])
          #})
          #tsks <- apply(taskData, MARGIN = 1, function(x) {
          #  shinydashboard::taskItem(text = x[["dataset"]])
          #})

          # data plotted
          if(length(notificationData$notificationData1) == 0){
            notificationData$notificationData1 <- data.frame(Data_plotted = "None plotted")
          }
          # data selected
          if(length(notificationData$notificationData2) == 0){
            notificationData$notificationData2 <- data.frame(Data_selected = "None selected")
          }

          ntfs1 <- apply(notificationData$notificationData1, MARGIN = 1, function(x) {
            shinydashboard::notificationItem(text = x[["Data_plotted"]], icon = shiny::icon("th-list"), status = "success")
          })

          ntfs2 <- apply(notificationData$notificationData2, MARGIN = 1, function(x) {

            if(gsub("Dataset selected: ","", x[["Data_selected"]]) == notificationData$sel){
              txt = paste0(x[["Data_selected"]],"*")
            } else{
              txt = x[["Data_selected"]]
            }
            shinydashboard::notificationItem(text = txt, icon = shiny::icon("th-list"), status = "warning")
          })

          # This is equivalent to calling:
          #shinydashboard::dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
          #shinydashboard::dropdownMenu(type = "tasks", .list = tsks)
          shinydashboard::dropdownMenu(type = "notifications", .list = c(ntfs1, ntfs2),
                                       icon = shiny::icon("cog"))
        })

      })


      ###############################################################################################################
      ###############################################################################################################
      # START MAIN PROCESS
      ###############################################################################################################
      ###############################################################################################################

      shiny::observeEvent({
        #mappy_data$df_update
        mappy_data$initialval
      },{

        #if(!is.null(onedat$onedata_original)){
        if(mappy_data$dataexist == 1){

          # assess the labels

          names_onedata <- list()
          downl_onedata <- list()
          anim_onedata <- list()
          source_onedata <- list()
          nfix_onedata <- list()
          id_onedata <- list()

          # if deletion happens this can take it back to zero and the table then falls over
          if(mappy_data$initialval > 0){

            for(i in 1:mappy_data$initialval){
              names_onedata[[i]] <- mappy_data[[paste0("nm_onedata_", i)]]
              downl_onedata[[i]] <- mappy_data[[paste0("onedata_downl_", i)]]
              anim_onedata[[i]] <- mappy_data[[paste0("nanim_", i)]]
              source_onedata[[i]] <- mappy_data[[paste0("source_", i)]]
              nfix_onedata[[i]] <- mappy_data[[paste0("nfix_", i)]]
              id_onedata[[i]] <- mappy_data[[paste0("id_", i)]]
            }

            # the names of the data will be as in the labels
            # so if ticked just get() the named data

            # not sure if we need this as reactive but for now....
            onedata_labels$labs <- do.call('rbind',names_onedata)[,1]
            onedata_labels$downl <- do.call('rbind',downl_onedata)[,1]
            onedata_labels$nanim <- do.call('rbind',anim_onedata)[,1]
            onedata_labels$source <- do.call('rbind',source_onedata)[,1]
            onedata_labels$nfix <- do.call('rbind',nfix_onedata)[,1]
            onedata_labels$id <- do.call('rbind',id_onedata)[,1]

            # additional summary to data
            reworked_table <- data.frame(labels = onedata_labels$labs, created = onedata_labels$downl, n_anim = onedata_labels$nanim, n_fix = onedata_labels$nfix, id = as.character(onedata_labels$id), source = onedata_labels$source)

            quantity <- 1:nrow(reworked_table) #id <-
            label <- paste0("lab","-",quantity)
            reworked_table_ <- data.frame(pick = FALSE, reworked_table, quantity = quantity, label = label, stringsAsFactors = FALSE) # id = id

            onedata_labels$df2use <- reworked_table_

          } else if(mappy_data$initialval == 0){

            onedata_labels$df2use <- NULL
            reworked_table_ <- onedata_labels$df2use

          }

          if(mappy_data$df_update == 1){

            if(!is.null(onedata_labels$df2use[nrow(onedata_labels$df2use),]$pick)){
              onedata_labels$df2use[nrow(onedata_labels$df2use),]$pick <- TRUE

              notificationData$notificationData1 <- data.frame(Data_plotted = paste("Dataset plotted:", "nm_onedata_", mappy_data$initialval))

            } else{
              notificationData$notificationData1 <- data.frame(Data_plotted = "None plotted")
              notificationData$notificationData2 <- data.frame(Data_selected = "None selected")

            }

            mappy_data$df_update = 0

          }

        }

      })

      # I like R package sortable and the bucket list example was tested but not successfully at this point in time
      # May be better as an R handsontable - tick boxes then "display"
      # can also show when downloaded and fix count no animals

      current_selection_made =  shiny::reactiveValues(data = NULL)
      selData <- ""

      shiny::observeEvent({
        input$gogo
      },{

        if(!is.null(input$gogo) ){

          isolate({

            reworked_table_ <- onedata_labels$df2use
            df_ <- rhandsontable::hot_to_r(input$gogo)

            current_selection_made$data = df_
            mappy_data$current_selection <- current_selection_made$data

            index <- which(df_$pick==TRUE)
            if(length(index)==0){
              return()
            } else{

              labs <- reworked_table_$label[index]
              #print(labs) #"lab-4" "lab-6" "lab-9"

              iter <- length(labs)
              valLabs <- sapply(1:iter, function(i) {
                if(is.null(input[[paste0("testd",labs[i])]] )) {
                  0
                } else {  as.numeric(input[[paste0("testd",labs[i])]])  }

              })

            }

          })
        }

      })

      # ------------------------------------------------------------------ #

      rds_load <-  shiny::reactive({

        reworked_table_ <- onedata_labels$df2use  #MB_table_output$tid

        return(reworked_table_)

      })

      output$gogo  <-  rhandsontable::renderRHandsontable({

        if(is.null(rds_load() )) return()
        #if(is.null(onedata_labels$df2use() )) return()

        df_ <- rds_load()

        df_ <- df_[with(df_,order(as.numeric(id))),]

        rhandsontable::rhandsontable(df_, readOnly = FALSE, rowHeaders= NULL, useTypes= TRUE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE)

      })

      # ------------------------------------------------------------------ #
      # if the Display_data button is pressed, then feed into below to show on map

      shiny::observeEvent({
        input$Display_data
      },{

        # indicate you have clicked the table, so as not to add to the table again on onedata_orig observer in main server file
        table_clicked$click = 1
        test = current_selection_made$data
        test = test[test$pick,]

        if(!is.null(test)){

          # if button combine is pressed with NO TICK, then zero row length table arises, needs error catcher to allow nothing to happen if that is done
          if(nrow(test) > 0){

            if(nrow(test) == 1){

              onedat$onedata_original <- mappy_data[[test$labels]]

              # remember the labels plotted
              plotted$test <- test

            } else{

              # if displaying two data or more, you want to combine them but not save as a separate combined dataset as in the combined data button push

              # remember the labels plotted
              plotted$test <- test

              data2combine <- sapply(test$labels, function(x){
                mappy_data[[x]]
              })

              # have to check for columns in which both datasets have in common to combine...
              colsInCommon = colnames(data2combine[[1]])
              for (q in 2:length(data2combine)){
                colsInCommon = intersect(colsInCommon, colnames(data2combine[[q]])) # base R intsesect function I was not familiar with, neater than what I was thinking of: https://stackoverflow.com/questions/51288257/common-column-names-among-data-sets-in-r
              }
              #data2combine[[1]][,names(data2combine[[1]]) %in% colsInCommon]

              # then retain only those columns in each dataset
              slimdown <- function(x){
                return(x[,names(x) %in% colsInCommon])
              }
              colsret <- lapply(data2combine, slimdown)
              colsret = do.call('rbind',colsret)

              onedat$onedata_original <- colsret

            }

          }

          # pput back to zero again in case new data read in, in which case add to the dataset list, now in main server
          #table_clicked$click = 0
        }

      })

      # ------------------------------------------------------------------ #
      # separate processfor displaying cleaned and thinnned data

      shiny::observeEvent(
        input$Display_clean_data,{

          # indicate you have clicked the table, so as not to add to the table again on onedata_orig observer in main server file
          table_clicked$click = 1
          test = current_selection_made$data
          test = test[test$pick,]

          if(!is.null(test)){

            # if button combine is pressed with NO TICK, then zero row length table arises, needs error catcher to allow nothing to happen if that is done
            if(nrow(test) > 0){

              if(nrow(test) == 1){

                check_twodata$d <- mappy_data[[test$labels]]

              }

            }
          }

        })


      shiny::observeEvent(
        input$Display_thinned_data,{

          # indicate you have clicked the table, so as not to add to the table again on onedata_orig observer in main server file
          table_clicked$click = 1
          test = current_selection_made$data
          test = test[test$pick,]

          if(!is.null(test)){

            # if button combine is pressed with NO TICK, then zero row length table arises, needs error catcher to allow nothing to happen if that is done
            if(nrow(test) > 0){

              if(nrow(test) == 1){

                check_threedata$d <- mappy_data[[test$labels]]

              }

            }
          }

        })

      # ------------------------------------------------------------------ #
      # combine data as a separate dataset for further analysis (rather than just display)

      shiny::observeEvent({
        input$Combine_data
      },{

        mappy_data$df_update <- 1
        # indicate you have clicked the table, so as not to add to the table again on onedata_orig observer in main server file
        table_clicked$click = 1

        test = current_selection_made$data
        test = test[test$pick,]

        # remember the labels plotted
        plotted$test <- test

        if(!is.null(test)){

          # if button combine is pressed with NO TICK, then zero row length table arises, needs error catcher to allow nothing to happen if that is done
          if(nrow(test) > 0){

            if(nrow(test) == 1){

              # nothing to combine

            } else{

              # same process as above for just displaying, but this time we want to store as a separate dataset
              data2combine <- lapply(test$labels, function(x){
                mappy_data[[x]]
              })

              colsInCommon = colnames(data2combine[[1]])
              for (q in 2:length(data2combine)){
                colsInCommon = intersect(colsInCommon, colnames(data2combine[[q]])) # base R intsersect function I was not familiar with, neater than what I was thinking of: https://stackoverflow.com/questions/51288257/common-column-names-among-data-sets-in-r
              }
              slimdown <- function(x){
                return(x[,names(x) %in% colsInCommon])
              }
              colsret <- lapply(data2combine, slimdown)
              colsret = do.call('rbind',colsret)

              if(!is.null(colsret)){
                attr(colsret, "source") <- "User_combined"

                # as with the onedata_original process in main server
                mappy_data$initialval <- mappy_data$initialval + 1
                mappy_data$df_counter <- mappy_data$df_counter + 1
                mappy_data$dataexist = 1

                mappy_data[[paste0("onedata_", mappy_data$initialval)]] <- colsret
                mappy_data[[paste0("onedata_downl_", mappy_data$initialval)]] <- as.character(Sys.time())
                mappy_data[[paste0("nm_onedata_", mappy_data$initialval)]] <- paste0("onedata_", mappy_data$initialval)
                mappy_data[[paste0("nanim_", mappy_data$initialval)]] <- length(unique(colsret$TagID))
                mappy_data[[paste0("source_", mappy_data$initialval)]] <- "User-combined"
                mappy_data[[paste0("nfix_", mappy_data$initialval)]] <- nrow(colsret)
                mappy_data[[paste0("id_", mappy_data$initialval)]] <- mappy_data$df_counter

                onedat$onedata_original <- colsret
              }

            }

          }

          # pput back to zero again in case new data read in, in which case add to the dataset list, now in main server
          #table_clicked$click = 0
        }

      })

      # ------------------------------------------------------------------ #
      # Delete dataset from the table

      shiny::observeEvent({
        input$Delete_data
      },{

        if(!any(notificationData$notificationData2$Data_selected %in% "None selected")){
          showModal(dataModal_delete())
        }

      })

      ######
      # are you sure you want to delete? Dialogue pop up

      #dataModal_delete <- function() { # SAME THING!
      dataModal_delete <- reactive({
        # display a modal dialog with a header, textinput and action buttons
        modalDialog(
          tags$h2('Are you sure you want to delete these data?'),
          shiny::radioButtons(inputId = "radio_delete", label = "",
                              choiceNames = c("yes","no"),
                              choiceValues = c("yes","no"),
                              selected = "yes"),

          footer=tagList(
            modalButton(label = 'Cancel_delete'),
            actionButton('ok_delete', 'OK') #submit

          )
        )
      })

      ######
      # Observe the delete - this is quite complex as have to extract the reactive data
      # to a list in normal R process. Plus other quirks
      # but then there is another situation if you delete a dataset, then add another,
      # the new data read in will not come into the list because
      # mappy_data$initialval has not altered!!

      observeEvent(input$ok_delete, {

        if(input$radio_delete == "yes"){

          # indicate you have clicked the table, so as not to add to the table again on onedata_orig observer in main server file
          table_clicked$click = 1

          test = current_selection_made$data
          test = test[test$pick,]
          ids <- test$quantity

          # remember the table for messaging
          plotted$test <- test

          # need to reverse the mappy_data$initialval by how many datasets are being deleted

          for(m in ids){

            mappy_data[[paste0("onedata_", m)]] <- NULL
            mappy_data[[paste0("onedata_downl_", m)]] <- NULL
            mappy_data[[paste0("nm_onedata_", m)]] <-NULL
            mappy_data[[paste0("nanim_", m)]] <- NULL
            mappy_data[[paste0("source_", m)]] <- NULL
            mappy_data[[paste0("id_", m)]] <- NULL
            mappy_data[[paste0("nfix_", m)]] <- NULL # VERBOSE! if anything ever added to the table needs several coding lines to do so


          }

          ##################
          # need to take out the NULLs - situation can arise if use selects one row ahead of another for deletion
          # counter goes back one, and then in the above first observer at start, it would only select up to that revised
          # counter which would find a NULL and crashes
          # so before you take X away from the counter, cycle over the reactive elements and remove NULLs
          # (a bit cumbersome) might be a better way ...
          # convert to convetional list and re-assign the mappy reactive (no easy way of collapsing NULLs in reactives?)
          # https://stackoverflow.com/questions/41246017/how-to-delete-element-from-reactivevalues-in-shiny
          # but note if reduced to NO data, we don't do this....
          # (also not really needed id taking out the last in the list i.e. NULLING last row)
          # NOT IF YOU ADD TO THE TABLE IT WILL ALL NEED ADDING EXTRA INFO IN FOR THOSE ROWS IN BELOW LOGIC!

          # Coded originally before I knew about reactiveValuesToList!! All very verbose but helped think around the problem

          if(mappy_data$initialval - length(ids) > 0){

            # translate into conventional lists
            list1 = list(); list2 = list(); list3 = list(); list4 = list(); list5 = list(); list6 = list(); list7 = list()

            i = 2
            for(i in 1:mappy_data$initialval){

              #<ReactiveValues>
              #  Values:    dataexist, initialval, nanim_1, nanim_2, nm_onedata_1, nm_onedata_2, onedata_1, onedata_2, onedata_downl_1, onedata_downl_2, source_1, source_2

              if(is.null(mappy_data[[paste0("onedata_", i)]])){
                list1[[i]] <- NULL
              } else{
                datagrab <- mappy_data[[paste0("onedata_", i)]]
                list1[[i]] <- datagrab
              }
              if(is.null(mappy_data[[paste0("onedata_downl_", i)]])){
                list2[[i]] <- NULL
              } else{
                datagrab2 <- mappy_data[[paste0("onedata_downl_", i)]]
                list2[[i]] <- datagrab2
              }
              if(is.null(mappy_data[[paste0("nm_onedata_", i)]])){
                list3[[i]] <- NULL
              } else{
                datagrab3 <- mappy_data[[paste0("nm_onedata_", i)]]
                list3[[i]] <- datagrab3
              }
              if(is.null(mappy_data[[paste0("nanim_", i)]])){
                list4[[i]] <- NULL
              } else{
                datagrab4 <- mappy_data[[paste0("nanim_", i)]]
                list4[[i]] <- datagrab4
              }
              if(is.null(mappy_data[[paste0("source_", i)]])){
                list5[[i]] <- NULL
              } else{
                datagrab5 <- mappy_data[[paste0("source_", i)]]
                list5[[i]] <- datagrab5
              }
              if(is.null(mappy_data[[paste0("nfix_", i)]])){
                list6[[i]] <- NULL
              } else{
                datagrab6 <- mappy_data[[paste0("nfix_", i)]]
                list6[[i]] <- datagrab6
              }
              if(is.null(mappy_data[[paste0("id_", i)]])){
                list7[[i]] <- NULL
              } else{
                datagrab7 <- mappy_data[[paste0("id_", i)]]
                list7[[i]] <- datagrab7
              }

            }

            if(any(sapply(list1, is.null))){
              list1 <- list1[-which(sapply(list1, is.null))]
            }
            if(any(sapply(list2, is.null))){
              list2 <- list2[-which(sapply(list2, is.null))]
            }
            if(any(sapply(list3, is.null))){
              list3 <- list3[-which(sapply(list3, is.null))]
            }
            if(any(sapply(list4, is.null))){
              list4 <- list4[-which(sapply(list4, is.null))]
            }
            if(any(sapply(list5, is.null))){
              list5 <- list5[-which(sapply(list5, is.null))]
            }
            if(any(sapply(list6, is.null))){
              list6 <- list6[-which(sapply(list6, is.null))]
            }
            if(any(sapply(list7, is.null))){
              list7 <- list7[-which(sapply(list7, is.null))]
            }

            # HOWEVER...NOTE THAT THE ORIGINAL IDs of the datasets will SHIFT!
            # SO we also need to update the id of the dataset otherwise weird over-writing stuff happens

            for(j in 1:length(list3)){
              list3[[j]] <- paste0("onedata_", j)
            }

            ##################
            # THEN take away the length(ids) so should equal minus NULLs
            # as with the onedata_original process in main server
            # don't think you can recreate the same reactive and then have it listened to in the same way! So will have to NULL out all the
            # reactive elements, and have NULLs at the end... this is terrible

            for(i in 1:mappy_data$initialval){

              mappy_data[[paste0("onedata_", i)]] <- NULL
              mappy_data[[paste0("onedata_downl_", i)]] <- NULL
              mappy_data[[paste0("nm_onedata_", i)]] <- NULL
              mappy_data[[paste0("nanim_", i)]] <- NULL
              mappy_data[[paste0("source_", i)]] <- NULL
              mappy_data[[paste0("nfix_", i)]] <- NULL
              mappy_data[[paste0("id_", i)]] <- NULL

            }

            for(i in 1:(mappy_data$initialval - length(ids))){

              mappy_data[[paste0("onedata_", i)]] <- list1[[i]]
              mappy_data[[paste0("onedata_downl_", i)]] <- list2[[i]]
              mappy_data[[paste0("nm_onedata_", i)]] <- list3[[i]]
              mappy_data[[paste0("nanim_", i)]] <- list4[[i]]
              mappy_data[[paste0("source_", i)]] <- list5[[i]]
              mappy_data[[paste0("nfix_", i)]] <- list6[[i]]
              mappy_data[[paste0("id_", i)]] <- list7[[i]]

            }

            # Rebuild mappy data
            #mappy_data <- reactiveValues(initialval = 0, dataexist = 0, deletion = 0, df_update = 0, df_counter = 0)

            # update counter
            mappy_data$initialval <- mappy_data$initialval - length(ids)

          } else{

            mappy_data$initialval <- mappy_data$initialval - length(ids)

          }

          ##################
          # suggest also deleting the current data plotted on the map - as technically IS still remembered "under the hood"
          # as plotdata$data2 and data3 etc...

          leaflet::leafletProxy("mymap") %>%
            leaflet::clearMarkers() %>%
            leaflet::clearControls() %>%
            leafgl::removeGlPoints("two")

          # for polyline switch of methods
          leaflet::leafletProxy("mymap") %>%
            #leafgl::removeGlPolylines(layerId = cl_reactive$current_layers)
            leafgl::removeGlPolylines(layerId = paste0("foo", 1:10000)) %>%
            leaflet::removeShape(layerId = paste0("foo", 1:10000))

          plotdata$data2 <- plotdata$data3 <- onedat$onedata_original <- NULL # this isn't working at the moment....data is still retained from previously even after dataset deleted from table

        }

        # this needs setting back to zero (i.e. not selected any more if deleted), to force the update of the table in the main server ca line 166: shiny::observeEvent({ onedat$onedata_original ... which then flows through the onedata process and feeds the mappy_data$initialval observe in this Dataset module
        table_clicked$click <- 0

        # some hacking here because another observer above always runs and generates the notification data from scratch
        # so had to code in some workaround to stop that happening IF the notifications were updated here following a deletion

        if(mappy_data$initialval == 0){
          notificationData$notificationData1 <- data.frame(Data_plotted = "None plotted")
          notificationData$notificationData2 <- data.frame(Data_selected = "None selected")

          mappy_data$deletion <- 1

        } else{
          notificationData$notificationData1 <- data.frame(Data_plotted = paste("Dataset plotted:", "nm_onedata_", mappy_data$initialval))

          #if(length(notificationData$notificationData2) > length(notificationData$notificationData1)){
          #
          #  dp = notificationData$notificationData1$Data_plotted
          #  dp = paste0(gsub("Dataset plotted: ","",dp),"*")
          #  notificationData$notificationData2 <- data.frame(Data_selected = dp)
          #}

          # if a higher number is selected than now available, set back to data_plotted number
          dp1 = as.numeric(gsub("\\D", "", notificationData$notificationData1$Data_plotted))
          dp2 = as.numeric(gsub("\\D", "", notificationData$notificationData2$Data_selected))

          if(max(dp2) > dp1){
            dp3 = notificationData$notificationData1$Data_plotted
            notificationData$notificationData2$Data_selected <- paste0("Dataset selected: onedata_",dp1)
          }

          mappy_data$deletion <- 1
        }

        removeModal()

      })

    }

  )
}


