DataReadModule <- function(ID=NULL, fun = NULL, ...) {

  moduleServer(
    ID,
    ## Below is the module function

    function(input, output, session) {

      ########################################
      ########################################
      ########################################
      ########################################
      # READING IN DATA
      ########################################
      ########################################
      ########################################
      ########################################

      # ------------------------ #
      # ------------------------ #
      # ------------------------ #
      # Movebank
      # ------------------------ #
      # ------------------------ #
      # ------------------------ #

      #if(is.null(onedata)){

      MB_table_output <- reactiveValues(df = NULL, ndat = NULL, tid = NULL)

      l <- reactiveValues()

      #l$username <- NULL
      #l$password <- NULL

      dataModal <- function(failed = FALSE) {

        # display a modal dialog with a header, textinput and action buttons
        modalDialog(
          tags$h2('Please enter your username and password for Movebank'),
          textInput('username', 'Username'),
          textInput('password', 'Password'),

          if(failed)
            div(tags$b("Invalidity", style = "color: red;")),

          footer=tagList(
            modalButton(label = 'Cancel'),
            actionButton('ok', 'OK') #submit

          )
        )
      }

      observeEvent(input$goButton3, { # = show

        cat("Sourcing MB data")

        # clear the map for previous markers from any data supplied
        # actually that's not possible anyway if onedata is false!!
        leaflet::leafletProxy("mymap") %>%
          leaflet::clearGroup(group = "two") %>%
          #leaflet::removeShape(layerId = "foo")
          leaflet::removeShape(layerId = c("1","2","3","foo","foo2","foo3",layerId2)) # ,  cl_reactive$current_layers

        # OK we need a model dialogue option to prompt the user for
        # login information, and return error if not given
        # that feeds into the 'login' value required for read_track_MB
        # https://stackoverflow.com/questions/60022116/how-to-create-a-popup-window-for-user-to-input-inforamation-in-r-shiny
        # check for existence of 'login' in global env
        # if not found runt he dialogue for user to enter

        if(!exists("login")){

          showModal(dataModal())

        } else{
          if(!inherits(login, "MovebankLogin")){
            stop("'login' object found but it is not a MovebankLogin class")
          } else{
            l$login <- login
          }

        }


      })

      observeEvent(input$ok, { # ok
        # only activated through data model OK button puch ans not diplayed anyway if login found in global env
        if(!is.null(input$username) &&
           !is.null(input$password)){

          l$login <- move::movebankLogin(input$username, input$password)

          removeModal()

        } else{
          l$login <- NULL
          showModal(dataModal(failed = TRUE))
        }


      })

      observeEvent(l$login,{

        if(!is.null(l$login)){

          login <<-  l$login

          # NULL character needs translating to actual NULL the function can understand
          if(input$start_MB == "NULL" | input$start_MB == ""){
            start_MB <- NULL
          } else{
            start_MB <- input$start_MB
          }
          if(input$end_MB == "NULL" | input$end_MB == ""){
            end_MB <- NULL
          } else{
            end_MB <- input$end_MB
          }

          if(input$TagID_MB == "NULL" | input$TagID_MB == ""){
            TagID_MB <- NULL
          } else{
            TagID_MB <- trimws(unlist(strsplit(input$TagID_MB, ",")))
          }

          dataMB <- MoveRakeR::read_track_MB(
            TagID = TagID_MB, # this is needed as the text input box needs translating to vector
            repo = input$repo_MB,
            start = start_MB,
            end = end_MB,
            option = input$option_read_track_MB, #"BTOTT_MB",
            dropsat = input$dropsat_MB,
            dropsats = input$dropsats_MB,
            flt_switch = input$flt_switch_MB,
            mindata = input$mindata_MB,
            p4s = 3035,
            verbose = TRUE
          )


          #dataMB <- TrackStack2Track(dataMB) # because it is delivered as a stack

          attr(dataMB, "source") <- "Movebank" # for use in the reactive table to display on map

          MB_table_output$ndat <- nrow(dataMB)

          #dataMB <<- dataMB

          #Sys.sleep(0.5)
          #shiny::incProgress(1)

          #})
          MB_table_output$df <- dataMB
          check_MBdata$d=dataMB

          #onedat$onedata <- dataMB # was previously this but now we capture original data differently in the main map, so see line below
          onedat$onedata_original <- dataMB # Really important code line
          #onedata <- dataMB # Really important code line


          # the above sets onedata from the read in of MB data, overriding any
          # data provion by the user

          l$login <- NULL # needs nullifying otherwise it will not re run

          # As with UvA I think we also need to nullify the previous CLEAN and THIN
          # data as they are still reactive and listen to for subsetting
          # Tag ID and sliders so if they still are NOT NULL
          # and new data is sourced, it will crash

          #check_twodata$d=NULL
          #check_threedata$d=NULL
          if(is.null(check_MBdata$switch1)){check_MBdata$switch1 <- 0}
          check_MBdata$switch1 <- check_MBdata$switch1 + 1

        }

      })

      #shiny::withProgress(message = "Running clean_GPS", {

      # on go button, build the data table
      observeEvent(input$showdata, {

        if(!is.null(MB_table_output$df)) {

          MB_table_output$df = MB_table_output$df %>% dplyr::mutate_if(is.numeric, round, 3)

          output$MB_table <- DT::renderDataTable({

            DT::datatable(
              MB_table_output$df,
              options = list(
                initComplete = htmlwidgets::JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                  "}"),
                searching = FALSE,
                pageLength = 5,
                lengthMenu = c(5, 10, 15, 20),
                scrollX = TRUE
              ) # close list for options
            ) # close datatable

          })

          output$nrow_MB <- shiny::renderText({
            paste0("Total of: ", MB_table_output$ndat, " fixes read in.")
          })

          # also update map, removing the clean fix layer previously displayed
          #leaflet::leafletProxy("mymap") %>%
          #  leaflet::clearGroup(group = "two") %>%
          #  #leaflet::removeShape(layerId = "foo")
          #  leaflet::removeShape(layerId = c("1","2","3","foo","foo2","foo3",layerId2,  cl_reactive$current_layers))

          leaflet::leafletProxy("mymap") %>%
            leafgl::removeGlPoints("two") %>%
            leaflet::clearGroup(group = "two") %>%
            leafgl::removeGlPolylines(layerId = paste0("foo", 1:10000)) %>%
            leaflet::removeShape(layerId = paste0("foo", 1:10000))

        }

        if(is.null(MB_table_output$df)) {
          #leaflet::leafletProxy("mymap") %>%
          #  leaflet::clearGroup(group = "two") %>%
          #  #leaflet::removeShape(layerId = "foo")
          #  leaflet::removeShape(layerId = c("1","2","3","foo","foo2","foo3",layerId2,  cl_reactive$current_layers))

          leaflet::leafletProxy("mymap") %>%
            leafgl::removeGlPoints("two") %>%
            leaflet::clearGroup(group = "two") %>%
            leafgl::removeGlPolylines(layerId = paste0("foo", 1:10000)) %>%
            leaflet::removeShape(layerId = paste0("foo", 1:10000))




        }
      })

      # reset
      observeEvent(input$reset_MB, { #### WE EMPTY THE PLOT AREA HERE

        MB_table_output <- reactiveValues(df = NULL, ndat = NULL)
        output$MB_table <- renderDataTable({})

        output$nrow_MB <- NULL
        check_MBdata$d <- NULL
        MB_table_output$df <- NULL
        #onedat$onedata <- NULL

        leaflet::leafletProxy("mymap") %>%
          leaflet::clearGroup(group = "two") %>%
          #leaflet::removeShape(layerId = "foo")
          leaflet::removeShape(layerId = c("1","2","3","foo","foo2","foo3",layerId2)) #cl_reactive$current_layers


      })


      ############################################
      # Check what animals the user has available for downloading
      # data from chosen repo

      ll <- reactiveValues()

      dataModal2 <- function(failed = FALSE) {

        # display a modal dialog with a header, textinput and action buttons
        modalDialog(
          tags$h2('Please enter your username and password for Movebank'),
          textInput('username2', 'Username'),
          textInput('password2', 'Password'),

          if(failed)
            div(tags$b("Invalidity", style = "color: red;")),

          footer=tagList(
            modalButton(label = 'Cancel'),
            actionButton('ok2', 'OK') #submit

          )
        )
      }

      observeEvent(input$goButton4, { # = show

        cat("Checking tags in specified MB repo")

        if(!exists("login")){

          showModal(dataModal2())

        } else{
          if(!inherits(login, "MovebankLogin")){
            stop("'login' object found but it is not a MovebankLogin class")
          } else{
            ll$login <- login
          }

        }


      })

      observeEvent(input$ok2, { # ok
        # only activated through data model OK button puch ans not diplayed anyway if login found in global env
        if(!is.null(input$username2) &&
           !is.null(input$password2)){

          ll$login <- move::movebankLogin(input$username2, input$password2)

          removeModal()

        } else{
          ll$login <- NULL
          showModal(dataModal2(failed = TRUE))
        }


      })

      #############################################################################################################
      ### Check the TagIDs in the MB repository
      # actually this renders first for what the user has available so may need
      # to double check this doesn't fire off e.g. if someone has NOT GOT HE ACCESS TO THE START UP REPO!
      # but a first table is initially required....
      observeEvent(ll$login,{

        if(!is.null(ll$login)){

          login <<- ll$login

          rt <- try(move::getMovebankAnimals(study = input$repo_MB,login = login),silent = TRUE)

          if(substr(rt[1],1,5) != "Error"){

            #cv <- move::getMovebankAnimals(study="BTO - North West England 2016 - Lesser Black-backed Gull",login=login)
            #names(cv)

            reworked_table <- data.frame(animalName = rt$local_identifier, #rt$animalName,
                                         timestamp_start = gsub(".000","",rt$timestamp_start),
                                         timestamp_end = gsub(".000","",rt$timestamp_end),
                                         number_of_events = rt$number_of_events,
                                         taxon_canonical_name = rt$taxon_canonical_name,
                                         individual_id = rt$individual_id,
                                         tag_id = rt$tag_id)

            # MB table name = "reworked_table"
            quantity <- id <- 1:nrow(reworked_table)
            label <- paste0("lab","-",quantity)
            reworked_table_ <- data.frame(pick = FALSE, reworked_table, id = id, quantity = quantity, label = label, stringsAsFactors = FALSE)

            MB_table_output$tid <- reworked_table_
            #reworked_table_ <<- reworked_table_

            #output$MB_table_tid <- DT::renderDataTable({
            #
            #  DT::datatable(
            #    MB_table_output$tid,
            #    options = list(
            #      initComplete = htmlwidgets::JS(
            #        "function(settings, json) {",
            #        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            #        "}"),
            #      searching = FALSE,
            #      pageLength = 10,
            #      lengthMenu = c(5, 10, 15, 20, 30),
            #      scrollX = TRUE,
            #      scrollY = TRUE
            #    ) # close list for options
            #  ) # close datatable
            #
            #})

            ll$login <- NULL # needs nullifying otherwise it will not re run

          } else{
            ll$login <- NULL
            reworked_table <- reworked_table_ <- MB_table_output$tid <- NULL
          }

        }

      })

      #--------------------
      ###### THIS NEXT SECTION IS ALL ABOUT REACTIVITY OF THE TAG IDs FROM THE MB TABLE
      ###### AND USING THAT AS A CHECK BOX SELECTOR TO THEN DO DOWNLOADING
      # KUDOS TO THIS EXAMPLE:
      # https://stackoverflow.com/questions/30733573/get-selected-rows-of-rhandsontable

      current_selection = reactiveValues(data = NULL)
      selData <- ""

      observeEvent({
        input$demTb
      },{

        if(!is.null(input$demTb) ){

          isolate({
            reworked_table_ <- MB_table_output$tid
            df_ <- rhandsontable::hot_to_r(input$demTb)

            current_selection$data = df_

            index <- which(df_$pick==TRUE)
            if(length(index)==0) return()
            labs <- reworked_table_$label[index]

            #print(labs) #"lab-4" "lab-6" "lab-9"

            iter <- length(labs)
            valLabs <- sapply(1:iter, function(i) {
              if(is.null(input[[paste0("testd",labs[i])]] )) {
                0
              } else {  as.numeric(input[[paste0("testd",labs[i])]])  }

            })

          })
        }

      })

      #--------------------
      # this will be the section where we do stuff with the current selections,
      # if the download button is pressed. i.e. picking up the tags selected and the start
      # ends to then feed into the function to get the MB data

      observeEvent({
        input$DonwloadMBdata
      },{

        test = current_selection$data
        test = test[test$pick,]

        if(length(test$pick) > 0){
          if(!is.null(test)){

            dataMB <- MoveRakeR::read_track_MB(
              TagID = test$animalName,
              repo = input$repo_MB,
              start = test$timestamp_start,
              end = test$timestamp_end,
              option = input$option_read_track_MB, #"BTOTT_MB",
              dropsat = input$dropsat_MB,
              dropsats = input$dropsats_MB,
              flt_switch = input$flt_switch_MB,
              mindata = input$mindata_MB,
              p4s = 3035,
              verbose = TRUE
            )

            #dataMB <- TrackStack2Track(dataMB) # because it is delivered as a stack
            attr(dataMB, "source") <- "Movebank" # for use in the reactive table to display on map
            MB_table_output$ndat <- nrow(dataMB)

            MB_table_output$df <- dataMB
            check_MBdata$d=dataMB

            #onedat$onedata <- dataMB # was previously this but now we capture original data differently in the main map, so see line below
            onedat$onedata_original <- dataMB # Really important code line
            #onedata <- dataMB # still needed though with one_data original??

            if(is.null(check_MBdata$switch2)){check_MBdata$switch2 <- 0}
            check_MBdata$switch2 <- check_MBdata$switch2 + 1
        }



        } #else{
          #MB_table_output$ndat <- MB_table_output$df <- check_MBdata$d <- NULL
        #}


      })

      #--------------------
      rds <- reactive({

        reworked_table_ <- MB_table_output$tid

        return(reworked_table_)
        selData <- "reworked_table" # this will be your result of the tags as listed from the MB checker

        df_ <- hot_to_r(input$demTb)

        isolate({

          index <- which(df_$pick==TRUE)
          if(length(index)==0) return(df_)
          labs <- reworked_table_$label[index]
          iter <- length(labs)

        }) # end isolate

        valLabs <- sapply(1:iter, function(i) {
          if(is.null(input[[paste0("testd",labs[i])]] )) {
            0
          } else {
            as.numeric(input[[paste0("testd",labs[i])]])/100
          }
        })

        dft_ <- data.frame(label=labs, multi=valLabs, stringsAsFactors = FALSE)
        dft_ <- merge(reworked_table_,dft_,by="label", all.x=TRUE)

        dft_$quantity <- sapply(1:length(dft_$quantity), function(z) {
          if( is.na( dft_$multi[z]) ) {
            dft_$quantity[z]
          } else { reworked_table_$quantity[z]*(1 + dft_$multi[z]) }
        })
        dft_[with(dft_,order(as.numeric(id))),]
        df_[with(df_,order(as.numeric(id))),]

        df_$quantity <- df_$quantity
        return(df_)
      })

      output$demTb  <-  rhandsontable::renderRHandsontable({

        if(is.null(rds() )) return()

        df_ <- rds()

        df_ <- df_[with(df_,order(as.numeric(id))),]

        rhandsontable::rhandsontable(df_, readOnly = FALSE, rowHeaders= NULL, useTypes= TRUE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE)


      })



      ##################################################################################
      ##################################################################################
      ##################################################################################
      ##################################################################################
      ##################################################################################
      ##################################################################################
      ##################################################################################


      # ------------------------ #
      # ------------------------ #
      # ------------------------ #
      # UvA-BiTS
      # ------------------------ #
      # ------------------------ #
      # ------------------------ #
      if(exists("db", envir = globalenv())){rm(db, envir = globalenv())}

      nn <- shiny::reactiveValues(db = NULL, db2 = NULL)

      # need to test for the existence of the
      # db_file, and return a message saying the user
      # should set it up if not found
      # directing to how to do so
      # goButton5 (in UI) ----> triggers dataModal3() which has 'ok3' ----> if OK triggers download
      #l$username <- NULL
      #l$password <- NULL

      dataModal3 <- function(failed = FALSE) {

        # display a modal dialog with a header, textinput and action buttons
        modalDialog(
          htmltools::tags$h2("Please enter ODBC user Data Source Name (DSN) to link to UvA-BiTS PostgreSQL"),
          shiny::textInput(inputId = 'dsn', label = "DSN", value = 'GPS'),
          shinyBS::bsTooltip("dsn", "For example what you called the DSN on setup e.g. GPS",
                             "right", options = list(container = "body")),
          #https://wiki.e-ecology.nl/index.php/How_to_access_the_e-Ecology_database
          if(failed)
            htmltools::div(tags$b("No ODBC DSN found, see https://wiki.e-ecology.nl/index.php/How_to_access_the_e-Ecology_database", style = "color: red;")),

          footer=htmltools::tagList(
            shiny::modalButton('Cancel'),
            shiny::actionButton('ok3', 'OK') #submit

          )
        )
      }

      #dbtest <- reactiveValues(dbexist = 0)

      observeEvent(input$goButton5, { # = show

        # activate modal dialogue (gobutton) and then --> uva access (ok) --> download

        cat("Sourcing UvA data")

        # clear the map for previous markers from any data supplied
        # actually that's not possible anyway if onedata is false!!
        # this also clears currently if you have Movebank already loaded
        # need a way of combining UvA and MB data!

        #leaflet::leafletProxy("mymap") %>%
        #  leaflet::clearGroup(group = "two") %>%
        #  #leaflet::removeShape(layerId = "foo")
        #  leaflet::removeShape(layerId = c("1","2","3","foo","foo2","foo3",layerId2,  cl_reactive$current_layers))

        leaflet::leafletProxy("mymap") %>%
          leafgl::removeGlPoints("two") %>%
          leaflet::clearGroup(group = "two") %>%
          leafgl::removeGlPolylines(layerId = paste0("foo", 1:10000)) %>%
          leaflet::removeShape(layerId = paste0("foo", 1:10000))


        # check for existence of 'db' in global env
        # if not found runt he dialogue for user to enter
        if(!exists("db")){

          showModal(dataModal3())

        } else{
          if(!inherits(db, "RODBC")){
            stop("'db' object found but it is not a RODBC class")
          } else{
            nn$db <- db
          }

        }

      })

      observeEvent(input$ok3, { # ok

        # if OK button is pressed - use the user-supplied data source name in textInput
        dsn = input$dsn # this is from the dialogue box as entered by user
        db <<- try(RODBC::odbcConnect(dsn),silent=TRUE) # currently required "db" to be named globally

        if(inherits(db, "RODBC")){
          nn$db <- db

          # all fine if it is an RODBC
          removeModal()

        } else{
          nn$db <- NULL
          showModal(dataModal(failed = TRUE))
        }

      })

      shiny::observeEvent(nn$db,{

        if(!is.null(nn$db)){

          db <<-  nn$db

          # NULL character needs translating to actual NULL the function can understand
          if(input$start_UvA == "NULL" | input$start_UvA == ""){
            start_UvA <- NULL
          } else{
            start_UvA <- input$start_UvA
          }
          if(input$end_UvA == "NULL" | input$end_UvA == ""){
            end_UvA <- NULL
          } else{
            end_UvA <- input$end_UvA
          }

          if(input$TagID_UvA == "NULL" | input$TagID_UvA == ""){
            TagID_UvA <- NULL
          } else{
            TagID_UvA <- trimws(unlist(strsplit(input$TagID_UvA, ",")))
          }

          dataUvA <- MoveRakeR::read_track_UvA(
            TagID = TagID_UvA, # this is needed as the text input box needs translating to vector
            start = start_UvA,
            end = end_UvA,
            dropsat = input$dropsat_UvA,
            dropsats = input$dropsats_UvA,
            pressure = input$pressure,
            mindata = input$mindata_UvA,
            p4s = sp::CRS("+init=epsg:27700"),
            verbose = TRUE
          )


          if(MoveRakeR::is_TrackStack(dataUvA)){
            dataUvA <- MoveRakeR::TrackStack2Track(dataUvA)
          }

          attr(dataUvA, "source") <- "UvA-BiTS" # for use in the reactive table to display on map

          # because it is delivered as a stack
          UvA_table_output$ndat <- nrow(dataUvA)

          #Sys.sleep(0.5)
          #shiny::incProgress(1)

          #})
          UvA_table_output$df <- dataUvA
          check_UvAdata$d <- dataUvA


          #onedat$onedata <- dataUvA
          onedat$onedata_original <- dataUvA  # for the main leaflet map display
          #onedata <- dataUvA # Really important code line

          nn$db <- NULL # needs nullifying otherwise it will not re run

          # I think we also need to nullify the previous CLEAN and THIN
          # data as they are still reactive and listen to for subsetting
          # Tag ID and sliders so if they still are NOT NULL
          # and new data is sourced, it will crash

          #check_twodata$d=NULL
          #check_threedata$d=NULL
          if(is.null(check_UvAdata$switch1)){check_UvAdata$switch1 <- 0}
          check_UvAdata$switch1 <- check_UvAdata$switch1 + 1

        }

      })

      #shiny::withProgress(message = "Running clean_GPS", {

      # on go button, build the data table
      observeEvent(input$showdata_UvA, {

        if(!is.null(UvA_table_output$df)) {

          UvA_table_output$df = UvA_table_output$df %>% dplyr::mutate_if(is.numeric, round, 3)

          output$UvA_table <- DT::renderDataTable({

            DT::datatable(
              UvA_table_output$df,
              options = list(
                initComplete = htmlwidgets::JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                  "}"),
                searching = FALSE,
                pageLength = 5,
                lengthMenu = c(5, 10, 15, 20),
                scrollX = TRUE
              ) # close list for options
            ) # close datatable

          })

          output$nrow_UvA <- shiny::renderText({
            paste0("Total of: ", UvA_table_output$ndat, " fixes read in.")
          })

          # also update map, removing the clean fix layer previously displayed
          #leaflet::leafletProxy("mymap") %>%
          #  leaflet::clearGroup(group = "two") %>%
          #  #leaflet::removeShape(layerId = "foo")
          #  leaflet::removeShape(layerId = c("1","2","3","foo","foo2","foo3",layerId2,  cl_reactive$current_layers))

          leaflet::leafletProxy("mymap") %>%
            leafgl::removeGlPoints("two") %>%
            leaflet::clearGroup(group = "two") %>%
            leafgl::removeGlPolylines(layerId = paste0("foo", 1:10000)) %>%
            leaflet::removeShape(layerId = paste0("foo", 1:10000))

        }

        if(is.null(MB_table_output$df)) {
          #leaflet::leafletProxy("mymap") %>%
          #  leaflet::clearGroup(group = "two") %>%
          #  #leaflet::removeShape(layerId = "foo")
          #  leaflet::removeShape(layerId = c("1","2","3","foo","foo2","foo3",layerId2,  cl_reactive$current_layers))

          leaflet::leafletProxy("mymap") %>%
            leafgl::removeGlPoints("two") %>%
            leaflet::clearGroup(group = "two") %>%
            leafgl::removeGlPolylines(layerId = paste0("foo", 1:10000)) %>%
            leaflet::removeShape(layerId = paste0("foo", 1:10000))

        }
      })

      # reset
      observeEvent(input$reset_UvA, { #### WE EMPTY THE PLOT AREA HERE

        UvA_table_output <- reactiveValues(df = NULL, ndat = NULL)
        output$UvA_table <- renderDataTable({})

        output$nrow_UvA <- NULL
        check_UvAdata$d <- NULL
        UvA_table_output$df <- NULL
        #onedat$onedata <- NULL

        #leaflet::leafletProxy("mymap") %>%
        #  leaflet::clearGroup(group = "two") %>%
        #  #leaflet::removeShape(layerId = "foo")
        #  leaflet::removeShape(layerId = c("1","2","3","foo","foo2","foo3",layerId2,  cl_reactive$current_layers))

        leaflet::leafletProxy("mymap") %>%
          leafgl::removeGlPoints("two") %>%
          leaflet::clearGroup(group = "two") %>%
          leafgl::removeGlPolylines(layerId = paste0("foo", 1:10000)) %>%
          leaflet::removeShape(layerId = paste0("foo", 1:10000))



      })

      #############################################################################################################
      #############################################################################################################
      #############################################################################################################
      #############################################################################################################
      #############################################################################################################

      #######################################################################
      #######################################################################
      #######################################################################
      # STEP 1
      # Query the UvA database to get the station names available to you
      #######################################################################
      #######################################################################
      #######################################################################

      # need two reactives - first to display the table of available station names from "get_UvA_station_name"
      # from which we can pick rows, to then feed in the station name to "get_UvA_Animals"
      # to then in turn pick data and display on the map - allows display of birds from multiple colonies!

      UvA_table_output <- shiny::reactiveValues(df = NULL, ndat = NULL, station = NULL,  tid = NULL)
      current_selection_uva = reactiveValues(station = NULL, data = NULL)
      selDataStation <- ""
      selDataAnimal <- ""

      # first bring up the list of available colonies visible to the user:
      # observing the database reactive element
      # UvAStationData (in UI) ----> triggers dataModal4() which has 'ok4' ----> if OK triggers download

      dataModal4 <- function(failed = FALSE) {

        # display a modal dialog with a header, textinput and action buttons
        modalDialog(
          htmltools::tags$h2("Please enter ODBC user Data Source Name (DSN) to link to UvA-BiTS PostgreSQL"),
          shiny::textInput(inputId = 'dsn', label = "DSN", value = 'GPS'),
          shinyBS::bsTooltip("dsn", "For example what you called the DSN on setup e.g. GPS",
                             "right", options = list(container = "body")),
          #https://wiki.e-ecology.nl/index.php/How_to_access_the_e-Ecology_database
          if(failed)
            htmltools::div(tags$b("No ODBC DSN found, see https://wiki.e-ecology.nl/index.php/How_to_access_the_e-Ecology_database", style = "color: red;")),

          footer=htmltools::tagList(
            shiny::modalButton('Cancel'),
            shiny::actionButton('ok4', 'OK') #submit

          )
        )
      }

      shiny::observeEvent({
        input$UvAStationData
        },{

          if(!exists("db")){

            showModal(dataModal4())

          } else{
            if(!inherits(db, "RODBC")){
              stop("'db' object found but it is not a RODBC class")
            } else{
              nn$db2 <- db
            }

          }

      })


      shiny::observeEvent({
        input$ok4
      },{

        dsn = input$dsn # this is from the dialogue box as entered by user
        db <<- try(RODBC::odbcConnect(dsn),silent=TRUE) # currently required "db" to be named globally

        if(inherits(db, "RODBC")){
          nn$db2 <- db

          # all fine if it is an RODBC
          removeModal()

        } else{
          nn$db2 <- NULL
          showModal(dataModal(failed = TRUE))
        }

      })

      shiny::observeEvent(nn$db2,{

        if(!is.null(nn$db2)){

          #print("TEST")
            db <<-  nn$db2

            ###### get station names
            reworked_table_uva_station <- MoveRakeR::get_UvA_station_name() # BTOTT PART!
            quantity <- id <- 1:nrow(reworked_table_uva_station)
            label <- paste0("lab","-",quantity)
            reworked_table_uva_station_ <- data.frame(pick = FALSE, reworked_table_uva_station, id = id, quantity = quantity, label = label, stringsAsFactors = FALSE)
            reworked_table_uva_station_ <- reworked_table_uva_station_[reworked_table_uva_station_$key_name != "ANNOTATION",]

            # # # # # patch # # # # #
            # There is an issue in the way I have separated out at two specific SE England sites
            # These went in as the same site code but I have separated out the two in BTOTT codes.
            # We needed an extra row to also show for the second site, so the user can click this

            reworked_table_uva_station_0 <- reworked_table_uva_station_[reworked_table_uva_station_$station_name != "Havergate",]
            id_0 <- max(reworked_table_uva_station_0$id)

            if("Orfordness" %in% reworked_table_uva_station_$station_name){

              extra_row <- data.frame(pick = FALSE, key_name = "LBBG_ORFORDNESS", station_name = "Havergate", project_id = "11", id = as.character(id_0+1), quantity = as.character(id_0+1), label = paste0("lab-",id_0+1), stringsAsFactors = FALSE)

              reworked_table_uva_station_ <- rbind(reworked_table_uva_station_, extra_row)
              reworked_table_uva_station_ <- unique(reworked_table_uva_station_)

            }

            UvA_table_output$station <- reworked_table_uva_station_   # in case of failure - this observe had "reworked_table_uvastation_" INSTEAD BUT STILL WORKED....

          }

      })


      observeEvent({
        input$demTb_uva_station
      },{

        if(!is.null(input$demTb_uva_station) ){

          isolate({
            reworked_table_uva_station_ <- UvA_table_output$station
            df_station_ <- rhandsontable::hot_to_r(input$demTb_uva_station)
            #print(df_station_)

            current_selection_uva$station = df_station_

            index_station <- which(df_station_$pick==TRUE)
            if(length(index_station) == 0) return()
            labs_station <- reworked_table_uva_station_$label[index_station]

            #print(labs) #"lab-4" "lab-6" "lab-9"

            iter_station <- length(labs_station)
            valLabs_station <- sapply(1:iter_station, function(i) {
              if(is.null(input[[paste0("testd",labs_station[i])]] )) {
                0
              } else {  as.numeric(input[[paste0("testd",labs_station[i])]])  }

            })

          })
        }

      })


      #--------------------
      rds_uva_station <- reactive({

        reworked_table_uva_station_ <- UvA_table_output$station

        return(reworked_table_uva_station_)
        selDataStation <- "reworked_table_uva_station"

        df_station_ <- hot_to_r(input$demTb_uva_station)

        isolate({

          index_station <- which(df_station_$pick == TRUE)
          if(length(index_station)==0) return(df_station_)
          labs_station <- reworked_table_uva_station_$label[index_station]
          iter_station <- length(labs_station)

        }) # end isolate

        valLabs_station <- sapply(1:iter_station, function(i) {
          if(is.null(input[[paste0("testd",labs_station[i])]] )) {
            0
          } else {
            as.numeric(input[[paste0("testd",labs_station[i])]])/100
          }
        })

        dft_station_ <- data.frame(label=labs_station, multi=valLabs_station, stringsAsFactors = FALSE)
        dft_station_ <- merge(reworked_table_uva_station_, dft_station_, by="label", all.x=T)

        dft_station_$quantity <- sapply(1:length(dft_station_$quantity), function(z) {
          if( is.na( dft_station_$multi[z]) ) {
            dft_station_$quantity[z]
          } else { reworked_table_uva_station_$quantity[z]*(1 + dft_station_$multi[z]) }
        })
        dft_station_[with(dft_station_,order(as.numeric(id))),]
        df_station_[with(df_station_,order(as.numeric(id))),]

        df_station_$quantity <- df_station_$quantity
        return(df_station_)
      })


      output$demTb_uva_station  <-  rhandsontable::renderRHandsontable({

        if(is.null(rds_uva_station())){

          #output$UvAGetTagIDs <- NULL

          return()

        } else{

          #output$UvAGetTagIDs <- shiny::renderUI({
          #    shiny::actionButton("UvAGetTagIDs", "Get TagIDs", styleclass = "success")
          #})

          df_station_ <- rds_uva_station()

          df_station_ <- df_station_[with(df_station_,order(as.numeric(id))),]

          rhandsontable::rhandsontable(df_station_, readOnly = FALSE, rowHeaders= NULL, useTypes= TRUE) %>%
            rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE)

        }

      })


      #--------------------
      # Do stuff with the current selection of stations - i.e. will be used to build a second table
      # of animals actually available, which itself needs to be a reactive table to select from!
      # Not perfect this as keeping clicking the button keeps changing the selection
      # swap buttons over - not ideal

      station_button_clicked <- reactiveValues(val = NULL)

      observeEvent({
        input$UvAStationData
      },{

        if(is.null(station_button_clicked$val)){

          station_button_clicked$val <- "clicked"
          current_selection_uva$station

        }

      })


      #######################################################################
      #######################################################################
      #######################################################################
      # STEP 2
      # use the current reactive values selected from the station table to feed into further reactive
      # elements to download data specific to TagIDs
      #######################################################################
      #######################################################################
      #######################################################################

      observeEvent({
        input$UvAGetTagIDs
      },{

        if(!is.null(station_button_clicked$val)){

          selected_names = current_selection_uva$station
          selected_names = selected_names[selected_names$pick == TRUE,]

          # now get the TagIDs - this feeds into the next reactive table!
          #current_selection_uva$data <- get_UvA_Animals(selected_names$station_name)

          ###### get station names
          reworked_table_uva_animal <- MoveRakeR::get_UvA_Animals(selected_names$station_name) # THE BTOTT PART OF ALL THIS!

          reworked_table_uva_animal$DateTime_start <- as.character(reworked_table_uva_animal$DateTime_start)
          reworked_table_uva_animal$DateTime_end <- as.character(reworked_table_uva_animal$DateTime_end)

          #reworked_table_uva_animal <- subset(reworked_table_uva_animal,select = c("TagID","DateTime_start","DateTime_end","station_name"))

          if(nrow(reworked_table_uva_animal) > 0){
            reworked_table_uva_animal$DateTime_end <- ifelse(reworked_table_uva_animal$DateTime_end == "9999-12-31 00:00:00", as.character(Sys.time()),reworked_table_uva_animal$DateTime_end)
            reworked_table_uva_animal$DateTime_end <- ifelse(reworked_table_uva_animal$DateTime_end == "9999-12-31", as.character(Sys.time()),reworked_table_uva_animal$DateTime_end)

            quantity <- id <- 1:nrow(reworked_table_uva_animal)
            label <- paste0("lab","-",quantity)
            reworked_table_uva_animal_ <- data.frame(pick = FALSE, reworked_table_uva_animal, id = id, quantity = quantity, label = label, stringsAsFactors = FALSE)
            UvA_table_output$tid <- reworked_table_uva_animal_
          } else{
            UvA_table_output$tid <- NULL
          }

        } else{

          print("Need to see what stations you have first")
        }

      })

      observeEvent({
        input$demTb_uva_tid
      },{

        if(!is.null(input$demTb_uva_tid) ){

          isolate({
            reworked_table_uva_animal_ <- UvA_table_output$tid

            df_animal_ <- rhandsontable::hot_to_r(input$demTb_uva_tid)

            current_selection_uva$tid = df_animal_

            index_animal <- which(df_animal_$pick==TRUE)
            if(length(index_animal) == 0) return()
            labs_animal <- reworked_table_uva_animal_$label[index_animal]

            iter_animal <- length(labs_animal)
            valLabs_animal <- sapply(1:iter_animal, function(i) {
              if(is.null(input[[paste0("testd",labs_animal[i])]] )) {
                0
              } else {  as.numeric(input[[paste0("testd",labs_animal[i])]])  }

            })

          })
        }

      })


      #--------------------
      rds_uva_animal <- reactive({

        reworked_table_uva_animal_ <- UvA_table_output$tid

        return(reworked_table_uva_animal_)
        selDataAnimal <- "reworked_table_uva_animal"

        df_animal_ <- hot_to_r(input$demTb_uva_tid)

        isolate({

          index_animal <- which(df_animal_$pick == TRUE)
          if(length(index_animal)==0) return(df_animal_)
          labs_animal <- reworked_table_uva_animal_$label[index_animal]
          iter_animal <- length(labs_animal)

        }) # end isolate

        valLabs_animal <- sapply(1:iter_animal, function(i) {
          if(is.null(input[[paste0("testd",labs_animal[i])]] )) {
            0
          } else {
            as.numeric(input[[paste0("testd",labs_animal[i])]])/100
          }
        })

        dft_animal_ <- data.frame(label=labs_animal, multi=valLabs_animal, stringsAsFactors = FALSE)
        dft_animal_ <- merge(reworked_table_uva_animal_, dft_animal_, by="label", all.x = TRUE)

        dft_animal_$quantity <- sapply(1:length(dft_animal_$quantity), function(z) {
          if( is.na( dft_animal_$multi[z]) ) {
            dft_animal_$quantity[z]
          } else { reworked_table_uva_animal_$quantity[z]*(1 + dft_animal_$multi[z]) }
        })
        dft_animal_[with(dft_animal_,order(as.numeric(id))),]
        df_animal_[with(df_animal_,order(as.numeric(id))),]

        df_animal_$quantity <- df_animal_$quantity
        return(df_animal_)
      })


      output$demTb_uva_tid  <-  rhandsontable::renderRHandsontable({

        if(is.null(rds_uva_animal())){
          output$UvAGetdata <- NULL

          return()

        } else{

          output$UvAGetdata <- shiny::renderUI({
            htmltools::tagList(
              shiny::actionButton("UvAGetdata", "Download data", styleclass = "success")

            )
          })

          dft_animal_ <- rds_uva_animal()

          dft_animal_ <- dft_animal_[with(dft_animal_,order(as.numeric(id))),]

          rhandsontable::rhandsontable(dft_animal_, readOnly = FALSE, rowHeaders= NULL, useTypes= TRUE) %>%
            rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE)

        }

      })

      #######################################################################
      #######################################################################
      #######################################################################
      # STEP 3
      # Use the current selectios to go and download from the UvA database
      #######################################################################
      #######################################################################
      #######################################################################

      # render download data button only if table is populated
      #--------------------
      # this will be the section where we do stuff with the current selections,
      # if the download button is pressed. i.e. picking up the tags selected and the start
      # ends to then feed into the function to get the MB data

      observeEvent({
        input$UvAGetdata
      },{

        if(!is.null(current_selection_uva$tid)){

          testuva = current_selection_uva$tid
          testuva = testuva[testuva$pick,]

          # NULL character needs translating to actual NULL the function can understand
          #if(input$start_UvA == "NULL" | input$start_UvA == ""){
          #  start_UvA <- NULL
          #} else{
          #  start_UvA <- input$start_UvA
          #}
          #if(input$end_UvA == "NULL" | input$end_UvA == ""){
          #  end_UvA <- NULL
          #} else{
          #  end_UvA <- input$end_UvA
          #}
          #
          #if(input$TagID_UvA == "NULL" | input$TagID_UvA == ""){
          #  TagID_UvA <- NULL
          #} else{
          #  TagID_UvA <- trimws(unlist(strsplit(input$TagID_UvA, ",")))
          #}

          testuva$DateTime_start

          dataUvA <- MoveRakeR::read_track_UvA(
            TagID = testuva$TagID, # this is needed as the text input box needs translating to vector
            start = testuva$DateTime_start,
            end = testuva$DateTime_end,
            dropsat = input$dropsat_UvA, # still from the boxes
            dropsats = input$dropsats_UvA,
            pressure = input$pressure,
            mindata = input$mindata_UvA,
            p4s = 3035,
            verbose = TRUE
          )

          if(MoveRakeR::is_TrackStack(dataUvA)){
            dataUvA <- MoveRakeR::TrackStack2Track(dataUvA)
          }
          attr(dataUvA, "source") <- "UvA-BiTS" # for use in the reactive table to display on map

          # because it is delivered as a stack
          UvA_table_output$ndat <- nrow(dataUvA)

          #Sys.sleep(0.5)
          #shiny::incProgress(1)

          #})
          UvA_table_output$df <- dataUvA
          check_UvAdata$d <- dataUvA

          onedat$onedata_original <- dataUvA
          #onedata <- dataUvA

          #nn$db <- NULL
          if(is.null(check_UvAdata$switch2)){check_UvAdata$switch2 <- 0}
          check_UvAdata$switch2 <- check_UvAdata$switch2 + 1

        }

      })

    }
  )
}




