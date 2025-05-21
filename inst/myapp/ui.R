#################################################################
header <- shinydashboard::dashboardHeader(
  title = 'RakeRvis',
  titleWidth = 270,
  dropdownMenuOutput("messageMenu"),
  htmltools::tags$li(
            htmltools::a(href = 'https://www.r-project.com',
            htmltools::img(src = "https://www.r-project.org/logo/Rlogo.png",
                title = "", height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"
            ),
          class = "dropdown"
          ),
  htmltools::tags$li(htmltools::a(href = 'https://www.bto.org',
            htmltools::img(src = "www/BTO_logo.jpg",
                title = "", height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"
  ),
  class = "dropdown"
  )
)

#################################################################
sidebar <- shinydashboard::dashboardSidebar(
  shinyjs::useShinyjs(),
  width = 270, # not sure why just seems to work well
  #♣
  shinydashboard::sidebarMenu(id = "tabs",
     shinydashboard::menuItem("Map", tabname = "map", icon = shiny::icon("layer-group"),startExpanded = TRUE, selected = TRUE,
        shinydashboard::menuItem("Plot by", tabName = "map", icon = shiny::icon("map-marked"), startExpanded = TRUE, selected = TRUE,
            shinydashboard::menuItem(shiny::uiOutput("plot_by"),tabName = "map", selected = TRUE)
        ),
        shinydashboard::menuItem("Display relocations and lines", tabName = "map", icon = shiny::icon("map-marker-alt"), startExpanded = FALSE, selected = TRUE,
           # see below comments under external point layers - directly putting in UI rather than server side render
           shinyWidgets::awesomeCheckbox(
             inputId = "ptog",
             label = "Fixes",
             value = TRUE,
             status = "danger"
           ),
           shinyWidgets::awesomeCheckbox(
             inputId = "ltog",
             label = "Lines",
             value = TRUE,
             status = "danger"
           )
        ),
        shinydashboard::menuItem("Clear map", tabName = "map", icon = shiny::icon("folder"), startExpanded = FALSE,
           shinydashboard::menuItem(
            shiny::actionButton("clear"," Clear map", icon = shiny::icon("eraser"))
           )
        ),
        shinydashboard::menuItem("Shapefiles", tabName = "map", icon = shiny::icon("draw-polygon"), startExpanded = FALSE,
          shinydashboard::menuItem(shiny::uiOutput("shape"), tabName = "map", selected = TRUE)
        ),
        shinydashboard::menuItem("External Points", tabName = "map", icon = shiny::icon("draw-polygon"), startExpanded = FALSE,
          if(!is.null(points)){
            shinyWidgets::awesomeCheckbox(
              inputId = "point",
              label = "Points",
              value = TRUE,
              status = "danger"
            )
          } else{
            #shiny::renderText("No external SpatialPoint layers")
            shiny::h6("No external SpatialPoint layers")
          }

        )
     ),
     shinydashboard::menuItem("Data access", tabName = "access", icon = shiny::icon("cloud-download-alt")
                              # for other stuff in the secondY
     ),
     #shinyBS::bsTooltip("access", "Download data from MoveBank or UvA-BiTS here.", "right", options = list(container = "body")),
     shinydashboard::menuItem("Data analytics", tabName = "data_analytics", icon = shiny::icon("chart-pie"), startExpanded = FALSE
     ),
     #shinyBS::bsTooltip("data_analytics", "Not yet implemented.", "right", options = list(container = "body")),
     shinydashboard::menuItem("Explorer ", tabName = "explorer", icon = shiny::icon("wpexplorer")
                              # for other stuff in the secondY
     ),
     shiny::actionButton('switchtab', 'Main map'),
     shinyBS::bsTooltip("switchtab", "Switch to main map.",
                        "right", options = list(container = "body")),
     shiny::br(),
     tags$head(
       tags$style(type="text/css",
                  "
                   #inline2 label{ display: table-cell; text-align: left; vertical-align: left; width: 100px; padding-left: 20px; }
                   #inline2 .form-group { display: table-row; }
                   #inline2 input { height: 30px; width: 80px; }
                   "
       )
     ),
     shiny::actionButton('switchtab2', 'Datasets'),
     shinyBS::bsTooltip("switchtab2", "Switch to tab containing details of datasets loaded.",
                        "right", options = list(container = "body")),
     shiny::br(),
     shiny::column(12,
                   shiny::h4("Mapping"),
                   shiny::h6("Reset map dimensions")
     ),
     tags$div(id = "inline2",
              shiny::uiOutput("map_height")
     ),
     tags$div(id = "inline2",
              shiny::uiOutput("map_width")
     ),
     shiny::actionButton('reset_map', 'Reload map data'),
     shiny::column(12,
                  shiny::h6("Switch between Mapbox and Leaflet")
     ),
     shiny::radioButtons(inputId = "mapdeck_switch", label = "Map type",
                         choiceNames = c("Mapdeck","Leaflet"),
                         choiceValues = c("Mapdeck","Leaflet"),
                         selected = "Leaflet"),
     shinyBS::bsTooltip(id = "mapdeck_switch",
                        title = "After re-setting the map click on Reload map data",
                        placement = "right", trigger = "hover", options = list(container = "body")),
     tags$head(
       tags$style(type="text/css",
                  "
                   #inline_ label{ margin-left: 10px; margin-top: -10px; }
                   "
       )
     ),
     shiny::column(12,
                   shiny::h6("Leaflet method")
     ),
     tags$div(id = "inline_",
              shiny::fluidRow(
                shiny::column(12,
                    shiny::radioButtons(inputId = "leaflet_method",   #inputId = "polyline_method",
                          label = NULL, # label = "Polylines",
                          inline=TRUE,
                          choiceNames = c("leaflet", "leafgl"),
                          choiceValues = c("leaflet", "leafgl"),
                          #selected = character(0)
                          selected = "leaflet"
                      )
                )
              ) # fluid row
     ),
     shinyBS::bsTooltip(id = "leaflet_method", #id = "polyline_method",
                        title = "leafgl is more efficient, but may not plot all line segments for big data! Also use leaflet for map outputs",
                        placement = "right", trigger = "hover", options = list(container = "body")
     ),
     shiny::column(12,
         shiny::h6(shiny::icon("far fa-copyright"), "BTO")
     )
    )
  )

#################################################################
##FF9966 (previous warning colour)
body <- shinydashboard::dashboardBody(
  # light purple for status box colour?
  htmltools::tags$style(htmltools::HTML("
    .box.box-solid.box-primary>.box-header {
      color:#fff;
      background:#b19cd9
    }
    .box.box-solid.box-success>.box-header {
      color:#fff;
      background:#FF8F74
    }
    .box.box-solid.box-info>.box-header {
      color:#00dcdc;
      background:#5abcd8
    }
    .box.box-solid.box-warning>.box-header {
      color:#fff;
      background:#ACE169
    }
    .box.box-solid.box-danger>.box-header {
      color:#fff;
      background:#FFCF1C
    }
    .box.box-solid.box-primary{
      border-bottom-color:#666666;
      border-left-color:#666666;
      border-right-color:#666666;
      border-top-color:#666666;
    }
    .box.box-solid.box-success{
      border-bottom-color:#666666;
      border-left-color:#666666;
      border-right-color:#666666;
      border-top-color:#666666;
    }
    .box.box-solid.box-info{
      border-bottom-color:#666666;
      border-left-color:#666666;
      border-right-color:#666666;
      border-top-color:#666666;
    }
    .box.box-solid.box-warning{
      border-bottom-color:#666666;
      border-left-color:#666666;
      border-right-color:#666666;
      border-top-color:#666666;
    }
    .box.box-solid.box-danger{
      border-bottom-color:#666666;
      border-left-color:#666666;
      border-right-color:#666666;
      border-top-color:#666666;
    }
   .main-sidebar{ font-size: 12px!important; }
   .treeview-menu>li>a { font-size: 12px!important; }

   .leaflet-container {
    cursor: auto !important;
   }

  ")),

  htmltools::tags$div(style = "padding:2px"),
  htmltools::tags$head(
    htmltools::tags$style(htmltools::HTML("hr {border-top: 1px solid #000000;}"))
  ),
  tags$head(tags$style(".easyPrintHolder .customCssClass {
                            background-image: url(data:image/svg+xml;utf8;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iaXNvLTg4NTktMSI/Pgo8IS0tIEdlbmVyYXRvcjogQWRvYmUgSWxsdXN0cmF0b3IgMTguMS4xLCBTVkcgRXhwb3J0IFBsdWctSW4gLiBTVkcgVmVyc2lvbjogNi4wMCBCdWlsZCAwKSAgLS0+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgdmVyc2lvbj0iMS4xIiBpZD0iQ2FwYV8xIiB4PSIwcHgiIHk9IjBweCIgdmlld0JveD0iMCAwIDQ0NC44MzMgNDQ0LjgzMyIgc3R5bGU9ImVuYWJsZS1iYWNrZ3JvdW5kOm5ldyAwIDAgNDQ0LjgzMyA0NDQuODMzOyIgeG1sOnNwYWNlPSJwcmVzZXJ2ZSIgd2lkdGg9IjUxMnB4IiBoZWlnaHQ9IjUxMnB4Ij4KPGc+Cgk8Zz4KCQk8cGF0aCBkPSJNNTUuMjUsNDQ0LjgzM2gzMzQuMzMzYzkuMzUsMCwxNy03LjY1LDE3LTE3VjEzOS4xMTdjMC00LjgxNy0xLjk4My05LjM1LTUuMzgzLTEyLjQ2N0wyNjkuNzMzLDQuNTMzICAgIEMyNjYuNjE3LDEuNywyNjIuMzY3LDAsMjU4LjExNywwSDU1LjI1Yy05LjM1LDAtMTcsNy42NS0xNywxN3Y0MTAuODMzQzM4LjI1LDQzNy4xODMsNDUuOSw0NDQuODMzLDU1LjI1LDQ0NC44MzN6ICAgICBNMzcyLjU4MywxNDYuNDgzdjAuODVIMjU2LjQxN3YtMTA4LjhMMzcyLjU4MywxNDYuNDgzeiBNNzIuMjUsMzRoMTUwLjE2N3YxMzAuMzMzYzAsOS4zNSw3LjY1LDE3LDE3LDE3aDEzMy4xNjd2MjI5LjVINzIuMjVWMzR6ICAgICIgZmlsbD0iIzAwMDAwMCIvPgoJPC9nPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+Cjwvc3ZnPgo=);
                            transform: rotate(90deg);
                        }
                       .easyPrintHolder .customCssClass1 {
                            background-image: url(data:image/svg+xml;utf8;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iaXNvLTg4NTktMSI/Pgo8IS0tIEdlbmVyYXRvcjogQWRvYmUgSWxsdXN0cmF0b3IgMTguMS4xLCBTVkcgRXhwb3J0IFBsdWctSW4gLiBTVkcgVmVyc2lvbjogNi4wMCBCdWlsZCAwKSAgLS0+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgdmVyc2lvbj0iMS4xIiBpZD0iQ2FwYV8xIiB4PSIwcHgiIHk9IjBweCIgdmlld0JveD0iMCAwIDQ0NC44MzMgNDQ0LjgzMyIgc3R5bGU9ImVuYWJsZS1iYWNrZ3JvdW5kOm5ldyAwIDAgNDQ0LjgzMyA0NDQuODMzOyIgeG1sOnNwYWNlPSJwcmVzZXJ2ZSIgd2lkdGg9IjUxMnB4IiBoZWlnaHQ9IjUxMnB4Ij4KPGc+Cgk8Zz4KCQk8cGF0aCBkPSJNNTUuMjUsNDQ0LjgzM2gzMzQuMzMzYzkuMzUsMCwxNy03LjY1LDE3LTE3VjEzOS4xMTdjMC00LjgxNy0xLjk4My05LjM1LTUuMzgzLTEyLjQ2N0wyNjkuNzMzLDQuNTMzICAgIEMyNjYuNjE3LDEuNywyNjIuMzY3LDAsMjU4LjExNywwSDU1LjI1Yy05LjM1LDAtMTcsNy42NS0xNywxN3Y0MTAuODMzQzM4LjI1LDQzNy4xODMsNDUuOSw0NDQuODMzLDU1LjI1LDQ0NC44MzN6ICAgICBNMzcyLjU4MywxNDYuNDgzdjAuODVIMjU2LjQxN3YtMTA4LjhMMzcyLjU4MywxNDYuNDgzeiBNNzIuMjUsMzRoMTUwLjE2N3YxMzAuMzMzYzAsOS4zNSw3LjY1LDE3LDE3LDE3aDEzMy4xNjd2MjI5LjVINzIuMjVWMzR6ICAgICIgZmlsbD0iIzAwMDAwMCIvPgoJPC9nPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+CjxnPgo8L2c+Cjwvc3ZnPgo=);
                            transform: rotate(180deg);
                        }")
  ),
  uiOutput("css_markers"),
  ## for registering an event on click in the notification menu: https://stackoverflow.com/questions/34418993/get-the-most-recently-clicked-notificationitem-of-a-dropdownmenu-in-shinydashboa
  #tags$script(HTML("function clickFunction(link){
  #                 Shiny.onInputChange('linkClicked',link);
  #                   }")),
  #textOutput("notificationoutput"),

  # fluid rows refers to the bootstrap library now ubiquitours in
  # we-programming and sets up multiples of 12 for COLUMNAR structure
  # i.e. 6 and 6 = two columns, so I guess 4, 4, 4 = 3 etc, 3, 3, 3, 3 = 4 etc
  # map is loaded into the server side but first thing we need to do
  # is create a placeholder for that map to be generated
  shinydashboard::tabItems(
    shinydashboard::tabItem(tabName = "map",
      shiny::fluidRow(
        shinydashboard::tabBox(
          title = "Tracking data movement map",
          id = "main",
          #status="primary",
          width = 12,
          #solidHeader = TRUE,

          shiny::tabsetPanel(
            id = "tab_being_displayed", # will set input$tab_being_displayed

          shiny::tabPanel("Main map",

            # ANIMALS
            shiny::fluidRow(

              #tags$div(class = "header",
              #         style="display:inline-block;", #; vertical-align:top; width: 150px;"


            #######################################################################
            div(style="display: inline-block;vertical-align:top; width: 45px;",
              shiny::column(1,
                shinyWidgets::dropdown(
                  shiny::fluidRow(
                    shiny::column(12,
                                  shiny::h5(tags$b("Select animals"))
                    )
                  ),
                  inputId = "nn",
                  shinyWidgets::awesomeCheckbox(
                    inputId = "multianimal",
                    label = "Multi-select",
                    value = FALSE,
                    status = "danger"
                  ),
                  shiny::uiOutput("tid"),
                  #shiny::uiOutput("leg"),
                  style = "unite", icon = icon("earlybirds"),
                  status = "royal", width = "200px"
                )
              )
              ),
              #######################################################################
              # PLOTBY LEVEL SELECTOR
              div(style="display: inline-block;vertical-align:top; width: 45px;",
              shiny::column(1,
                shinyWidgets::dropdown(
                  inputId = "nn2",
                  shiny::h6(tags$b("Plot by levels of non numeric variables other than TagID")),
                  shiny::uiOutput("plotby_var"),
                  #shiny::uiOutput("leg"),
                  style = "unite", icon = icon("map-marked-alt"),
                  status = "royal", width = "200px"
                  )
                )
              ),
              #### map layer, zoom, legend and scale bar
              div(style="display: inline-block;vertical-align:top; width: 45px;",
              shiny::column(1,
                shinyWidgets::dropdown(
                  shiny::h5(tags$b("Configure placement of map controls")),
                  inputId = "nn3",
                  shinybusy::add_busy_spinner(spin = "fading-circle"),
                  shinyWidgets::awesomeCheckbox(
                    inputId = "zoom_control",
                    label = "Zoom control",
                    value = FALSE,
                    status = "danger"
                  ),
                  shinyBS::bsTooltip(id = "zoom_control",
                                     title = "Warning: checking this will reload the map",
                                     placement = "right", trigger = "hover", options = list(container = "body")
                                     ),
                  shinyWidgets::awesomeCheckbox(
                    inputId = "leg",
                    label = "Legend",
                    value = TRUE,
                    status = "danger"
                  ),
                  shinyWidgets::awesomeCheckbox(
                    inputId = "layer_control",
                    label = "Layer display",
                    value = TRUE,
                    status = "danger"
                  ),
                  shiny::radioButtons(inputId = "legend_radio", label = "Legend position",
                    choiceNames = c("topright","topleft","bottomright","bottomleft"),
                    choiceValues = c("topright","topleft","bottomright","bottomleft"),
                        selected = "topright"),
                  shinyWidgets::awesomeCheckbox(
                    inputId = "scalebar_toggle",
                    label = "Scale bar",
                    value = TRUE,
                    status = "danger"
                  ),
                  shiny::radioButtons(inputId = "scalebar_pos", label = "Scale bar position",
                    choiceNames = c("topright","topleft","bottomright","bottomleft"),
                    choiceValues = c("topright","topleft","bottomright","bottomleft"),
                        selected = "topright"),
                  shinyWidgets::awesomeCheckbox(
                    inputId = "scalebar_met",
                    label = "Scale metric",
                    value = TRUE,
                    status = "danger"
                  ),
                  shinyWidgets::awesomeCheckbox(
                    inputId = "scalebar_imp",
                    label = "Scale imperial",
                    value = TRUE,
                    status = "danger"
                  ),
                  shinyWidgets::awesomeCheckbox(
                    inputId = "map_attribution",
                    label = "Map attribution",
                    value = TRUE,
                    status = "danger"
                  ),
                  style = "unite", icon = icon("th-list"),
                  status = "royal", width = "200px"
                  #shiny::uiOutput("leg")
                 )
               )
              ),
              ######################
              # more detailed settings MAP SAVING AND LINES/POINTS leafgl vs leaflet
              # there are issues I've found with glpolylines not rendering as it should - not sure why
              # so would like option to switch to normal polylines, and points, even though points can be much slower
              # there is also then the issue if mixing gl and non gl method that points that are not full opacity = 1, will
              # render always underneath the lines! aghr
              # NOTE: 16/10/2023 easyprint in leaflet does not work with leafgl! (and obviously not with mapdeck)
              div(style="display: inline-block;vertical-align:top; width: 45px;",
              shiny::column(1,
                 shinyWidgets::dropdown(
                   inputId = "nn6",
                    # shiny::fluidRow(
                    #   shiny::column(12,
                    #      shiny::h5(tags$b("Line and point plotting"))
                    #   )
                     #),
                     #shiny::fluidRow(
                    #  shiny::column(6,
                    #   shiny::radioButtons(inputId = "leaflet_method",   #inputId = "polyline_method",
                    #         label = "Leaflet method", # label = "Polylines",
                    #         #inline=TRUE,
                    #         choiceNames = c("leaflet", "leafgl"),
                    #         choiceValues = c("leaflet", "leafgl"),
                    #         selected = "leaflet")
                    #    )
                    #  #,
                    #  #shiny::column(6,
                    #  # shiny::radioButtons(inputId = "point_method", label = "Fixes", #inline=TRUE,
                    #  #       choiceNames = c("leaflet", "leafgl"),
                    #  #       choiceValues = c("leaflet", "leafgl"),
                    #  #       selected = "leaflet")
                    #  #  )
                    #  ), # fluid row
                    #  shinyBS::bsTooltip(id = "leaflet_method", #id = "polyline_method",
                    #    title = "leafgl is more efficient, but may not plot all line segments for big data! Also use leaflet for map outputs",
                    #    placement = "right", trigger = "hover", options = list(container = "body")
                    #  ),
                      #shiny::fluidRow(
                      #  htmltools::tags$small("
                      #       The leafgl method is more efficient, whereas regular leaflet may crash with many thousands of fixes!
                      #       However, addGlPolylines can be unstable for many lines preventing
                      #       the map displaying properly on occasion. Switch to regular leaflet if that's the case.")
                      #),
                      #shiny::fluidRow(
                      #  shiny::br()
                      #),
                     shiny::fluidRow(
                       shiny::column(12,
                        shiny::h5(tags$b("Map save options")),
                        shiny::h6(tags$b("Note: currently this only works with leaflet (not leafgl)"))
                       )
                     ),
                     shiny::textInput("mapsave_filename", label='File name', value = 'output_map', width = '100%'),
                     shiny::radioButtons(inputId = "mapsave_position", label = "Save position", #inline=TRUE,
                                       choiceNames = c("topleft", "topright", "bottomleft", "bottomright"),
                                       choiceValues = c("topleft", "topright", "bottomleft", "bottomright"),
                                       selected = "bottomleft"
                     ),
                     shinyWidgets::awesomeCheckbox(
                         inputId = "HideControlContainer",
                         label = "Hide container",
                         value = TRUE,
                         status = "danger"
                     ),
                     shinyBS::bsTooltip(id = "HideControlContainer",
                                      title = "Keep this ticked to retain scale legend on the saved map",
                                      placement = "right", trigger = "hover", options = list(container = "body")),

                     shinyWidgets::awesomeCheckbox(
                       inputId = "hidden",
                       label = "Hide map save tool",
                       value = TRUE,
                       status = "danger"
                     ),
                     shinyBS::bsTooltip(id = "hidden",
                                      title = "Hides the built-in save widget down arrow for neat map, uncheck this keep containers above to retain scale and/or legend and use custom saving below",
                                      placement = "right", trigger = "hover", options = list(container = "body")),

                     shiny::hr(),
                     shiny::radioButtons(inputId = "user_map_size", label = "User map size",
                                         choiceNames = c("CurrentSize", "A4Landscape", "A4Portrait"),
                                         choiceValues = c("CurrentSize", "A4Landscape", "A4Portrait"),
                                         selected = "bottomleft"
                     ),
                     shiny::actionButton("savemap", "Save map", styleclass = "success"),


                     #shiny::uiOutput("colour_pal_leaf"),
                     style = "unite",
                     #icon = icon("cog"),
                     icon = icon("floppy-disk"),
                     status = "royal", width = "200px"
                  )
                )
              ),
              ######################
              # Animation
              div(style="display: inline-block;vertical-align:top; width: 45px;",
              shiny::column(1,
                  shinyWidgets::dropdown(
                     shiny::fluidRow(
                       shiny::column(12,
                          shiny::h5(tags$b("Track animation"))
                       )
                    ),
                    inputId = "nn3a",
                    shinybusy::add_busy_spinner(spin = "fading-circle"),
                    shinyWidgets::awesomeCheckbox(
                      inputId = "switch2timeline",
                      label = "Animation",
                      value = FALSE,
                      status = "danger"
                    ),
                    shiny::numericInput(inputId = "timeline_speed",
                                        label='Timeline Speed',
                                        value = 20000,
                                        width = '100%'),
                    shiny::numericInput(inputId = "maxInterpolationTime",
                                        label='Max tnterpolation time',
                                        value = 10000000,
                                        width = '100%'),
                    shiny::numericInput(inputId = "tickLen",
                                        label='Tick length',
                                        value = 1000000,
                                        width = '100%'),
                    shiny::radioButtons(inputId = "marker_type",
                                        label = "Marker type",
                                        choiceNames = c("Pin","Circle"),
                                        choiceValues = c("pin","circle"),
                                        selected = "pin"),

                    # additional maxbox elements not for leaflet
                    shiny::h6(tags$b("Further mapbox elements ignored by leaflet:")),
                    shiny::numericInput(inputId = "trail_length",
                                        label='Trail length',
                                        value = 50000,
                                        width = '100%'),
                    shiny::numericInput(inputId = "stroke_width",
                                        label='Stroke width',
                                        value = 50,
                                        width = '100%'),
                    shiny::numericInput(inputId = "stroke_opacity",
                                        label='Stroke opacity',
                                        value = 1,
                                        width = '100%'),

                    style = "unite", icon = icon("route"),
                    status = "royal", width = "150px"
                    )
                )
              ),
              ######################
              # COLOUR PALETTES
              div(style="display: inline-block;vertical-align:top; width: 45px;",
              shiny::column(1,
                  shinyWidgets::dropdown(
                  #shinyWidgets::dropMenu(
                    inputId = "nn4", tooltip = TRUE,
                    right = TRUE,
                    shiny::h4(tags$b("Colours by TagID")),
                    shiny::uiOutput("colour_pal_leaf"),
                    shinyBS::bsTooltip("colour_pal_leaf",
                                       "Either a custom palette or one colour per animal",
                                       "right",options = list(container = "body")
                    ),
                    shiny::h4(tags$b("Colours by variable")),
                    shiny::uiOutput("colour_pal_leaf_plotby"),
                    shiny::h5(tags$b("Palette presets")),
                    shiny::column(6,
                      shiny::radioButtons(inputId = "builtin_pal", label = NULL, inline=FALSE,
                        choiceNames = c("Viridis","RdYlGn","Magma","Inferno", "UvA"),
                        choiceValues = c("viridis","RdYlGn","magma","inferno", "UvA"),
                        selected = "viridis")
                      ),
                    shiny::column(6,
                      shiny::radioButtons(inputId = "builtin_pal2", label = NULL, inline=FALSE,
                        choiceNames = c("Plasma", "Blues", "Greys", "Okabe-Ito", "YlOrRd"),
                        choiceValues = c("plasma", "Blues", "Greys", "Okabe-Ito", "YlOrRd"),
                        selected = character(0))
                      ),
                    shinyBS::bsTooltip("builtin_pal2",
                                       "A few popular palettes",
                                       "right",options = list(container = "body")
                    ),
                    shiny::h4(tags$b("Line colours")),
                    shinyWidgets::awesomeCheckbox(
                      inputId = "lcol_samepoints",
                      label = "Same as fixes",
                      value = FALSE,
                      status = "danger"
                    ),
                    shiny::h4(tags$b("Custom line colour")),
                    shiny::uiOutput("colour_lines"),
                    shiny::h4(tags$b("Leaflet factor or numeric")),
                    shiny::column(6,
                      shiny::radioButtons(inputId = "colour_factor", label = NULL, inline=FALSE,
                                        choiceNames = c("colorFactor", "colorNumeric"),
                                        choiceValues = c("colorFactor", "colorNumeric"),
                                        selected = "colorFactor"
                                        )
                      ),
                    shinyWidgets::awesomeCheckbox(
                      inputId = "rescale_plotby",
                      label = "Rescale plotby within animal(s)",
                      value = TRUE,
                      status = "danger"
                    ),
                    style = "unite", icon = icon("palette"),
                    status = "royal", width = "250px"
                )
               )
              ),
              ######################
              # R/SQL/custom FILTER
              div(style="display: inline-block;vertical-align:top; width: 45px;",
              shiny::column(1,
                  shinyWidgets::dropdown(
                    shiny::fluidRow(
                      shiny::column(12,
                         shiny::h5(tags$b("Manual data filtering"))
                      )
                    ),
                    inputId = "nn5",
                    right = TRUE,
                    #tags$head(
                    #  tags$style(type="text/css", "#lang_tog label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }")
                    #),
                    shiny::radioButtons(inputId = "lang_tog", label = "Query language",
                         choiceNames = c("R","SQL"),
                         choiceValues = c("R","SQL"),
                         inline = TRUE,
                         selected = "R"),
                    shiny::h6( "'DATA' refers to the current object."),
                    shiny::fluidRow(
                      shiny::column(12,
                        tags$style("#sql_filter {font-size:12px;height:20px;}"),
                        shiny::textAreaInput("sql_filter", label="", height = '100px', placeholder = "Enter code here")
                        #shinyBS::bsTooltip("sql_filter",
                        #                   "'DATA' refers to the current object.",
                        #                   "right",options = list(container = "body")
                        #)
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(12,
                        shiny::actionButton("go_sql", "Run"),
                        shiny::actionButton("reset_sql", "Reset")
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(12,
                         #textOutput("bad_code")
                         shiny::uiOutput("bad_code")
                      )
                    ),

                    #############################################
                    # Excel inspired idea
                    # this is the same code as used for the trips, will use a different ID though
                    # but to align boxes inline for the filterings to take place
                    htmltools::tags$h5("Filter by variables"),
                    shiny::fluidRow(
                      #style = "width: 375px", # have to force this for some reason
                      #tags$head(
                      #  tags$style(type="text/css",
                      #      ".selectize-dropdown-content {max-height: 800px; }"
                      #     "
                      #     #inline_filt label{ display: table-cell; text-align: left; vertical-align: left; width: 70px; }
                      #     #inline_filt .form-group { display: table-row; }
                      #     #inline_filt input { height: 30px; width: 100px; }
                      #     "
                      #     #"
                      #     #label.control-label,
                      #     #.form-control.single{ display: table-cell; text-align: center; vertical-align: middle; }
                      #     #.form-group { display: table-row;}
                      #     #"
                      #  )
                      #),
                      shiny::column(12,
                                    #tags$div(id = "inline_filt", shiny::uiOutput("circ_radius"))
                                    actionButton("removelevel", "Remove Level"),
                                    actionButton("addlevel", "Add Level"),
                                    tags$div(id = "addlevel",
                                             style = "border: 1px solid silver;")
                      )
                    ),
                    #############################################
                    # Quick year filter
                    shiny::fluidRow(
                        shiny::column(12,
                            shiny::h5(tags$b("Quick filter by year, month, day")),
                            shiny::splitLayout(
                              cellWidths = c("33%", "33%", "33%"),
                              cellArgs = list(style = "padding: 1px"),
                              id = "filters4date",
                                shiny::uiOutput("filter_year"),
                                shiny::uiOutput("filter_month"),
                                shiny::uiOutput("filter_day"),
                                tags$head(tags$style(HTML("
                                .shiny-split-layout > div {
                                  overflow: visible;
                                }
                                  ")))
                            )
                        )
                     ),
                    shiny::fluidRow(
                      shiny::column(4,
                         actionButton("reset_year_filter", "Reset")
                      )
                    ),
                    style = "unite", icon = icon("filter"),
                              status = "royal", width = "300px"
                  )
                )
              ),

            # # # # # # # # #
            #tags$head(
            #  tags$style(
            #    HTML(".btn-group > .btn.active {
            #       background-color: blue;
            #       color: white;
            #       }
            #     .btn-mygrey {
            #       background-color: grey;
            #       color: white;
            #       }
            #      ")
            #  )
            #),
            # # # # # # # # #
            ############################
            # additional night and day box for selecting night and day fixes
            div(style="display: inline-block;vertical-align:top; width: 45px;",
                shiny::column(1,
                    shinyWidgets::dropdown(
                      inputId = "nn10",
                      #right = TRUE,
                      shiny::h5(tags$b("Day and night labelling")),

                      shiny::checkboxGroupInput(
                        #shinyWidgets::prettyCheckboxGroup(
                        #shiny::h6(tags$b("Select light-level to include as part of day:")),
                        inputId = "daynight",
                        label = "Include in 'day':",
                        choices = c("Sunset","Sunrise","Civil Twilight","Nautight twilight","Astronomical twilight"),
                        selected = c("Sunset", "Sunrise", "Civil Twilight")
                        #,
                        #direction = "vertical"
                      ),
                      shinyBS::bsTooltip(id = "daynight",
                                       title = "Default is for sunset sunrise and civil twilight classed as day",
                                       placement = "top", trigger = "hover", options = list(container = "body")
                                       ),
                      shiny::fluidRow(
                        shiny::column(12,
                            #div(style = "display:inline-block;vertical-align:top;", shiny::actionButton("daynight_go", "Day/night")),
                            shiny::actionButton("daynight_go", "Day/night"),
                            shinybusy::add_busy_spinner(spin = "fading-circle"),
                            shinyBS::bsTooltip(id = "daynight_go",
                                 title = "Adds detailed light-level field (dnt) and broad grouping of (daynight) to data: go to plotby on the left-hand panel (apologies you may need to reset the map)",
                                 placement = "right", trigger = "hover", options = list(container = "body")
                                 )
                            )
                        ),
                        #shiny::fluidRow(
                        #  shiny::column(12,
                        #      #shiny::h6(tags$b("Classify to all light-levels (selections above ignored):")),
                        #      shiny::actionButton("daynight_go2", "All light-levels"),
                        #      shinyBS::bsTooltip(id = "daynight_go2",
                        #           title = "Classifies fixes to full light-level criteria (selections above ignored) and adds dnt field to your data: go to plotby on the left-hand panel",
                        #           placement = "right", trigger = "hover", options = list(container = "body")
                        #           ),
                        #    )
                        #),
                        style = "unite", icon = icon("moon"),
                        status = "royal", width = "200px"
                      )

                )
            ),
                ############################
              # additional mapdeck radio buttons for changing th basemap style
              div(style="display: inline-block;vertical-align:top; width: 45px; margin-left: -5px;",
                shiny::column(1,
                    shinyWidgets::dropdown(
                      inputId = "nn7",
                      #right = TRUE,
                      shiny::radioButtons(inputId = "md_style", label = "Mapbox style",
                                          choiceNames = c("dark", "light", "outdoors", "streets", "satellite", "satellite-streets"),
                                          choiceValues = c("dark", "light", "outdoors", "streets", "satellite", "satellite-streets"),
                                          #inline = TRUE,
                                          selected = "light"),
                      shiny::fluidRow(
                        shiny::column(12,
                          shiny::actionButton("north_snap", "Snap to north")
                        )
                      ),
                      shinyBS::bsTooltip(id = "north_snap",
                                         title = "Not currently implemented (until we figure out how)",
                                         placement = "right", trigger = "hover", options = list(container = "body")),
                      shiny::br(),
                      shinyWidgets::awesomeCheckbox(
                        inputId = "elev_toggle",
                        label = "Toggle elevation",
                        value = FALSE,
                        status = "danger"
                      ),
                      shiny::radioButtons(inputId = "ext_point_col", label = "Ext point col",
                                          choiceNames = c("black", "grey20", "grey40", "grey60", "grey80", "white"),
                                          choiceValues = c("black", "grey20", "grey40", "grey60", "grey80", "white"),
                                          #inline = TRUE,
                                          selected = "black"),
                      style = "unite", icon = htmltools::img(src = "www/mapbox-svgrepo-com_purple.png",height = "30px"),
                      status = "royal", width = "200px"
                    )
                )
              ),
              ####### A final help menu that will contain the rendered html file
              div(style="display: inline-block;vertical-align:top; width: 45px; margin-left: 10px",
                  shiny::column(1,
                      shinyWidgets::dropdown(
                      inputId = "nn14",
                      style = "unite",
                      icon = icon("info-circle"),
                      status = "royal", width = "200px"
                    )
                  )
              ),

              #,
              #shiny::column(1,
              #  shinyWidgets::dropdown(
              #    #shiny::uiOutput("ptog"),
              #    shiny::uiOutput("leg"),
              #    style = "unite", icon = icon("cog"),
              #    status = "royal", width = "200px"
              #  )
              #)
              #) # div close
              #,
              #tags$style("#col-sm-1 {font-size:12px;height:20px;}"),
              #.col-sm-1 label{
              #  max-width: 35%;
              #}
            ),
            #leaflet::leafletOutput("mymap", width = "100%", height = 600),
            #tags$head(tags$style(HTML("
            #    .leaflet-top, .leaflet-bottom {
            #      z-index: unset !important;
            #    }
            #
            #    .leaflet-touch .leaflet-control-layers, .leaflet-touch .leaflet-bar {
            #      z-index: 10000000000 !important;
            #    }
            #  "))),

            # -------------------------------------------------------------- #
            # THE MAP OUTPUT PART
            tags$head(tags$style(".leaflet-top {z-index:999!important;}")),

            shiny::uiOutput("mapOutput"),
            #shiny::uiOutput("mapOutput_md"),
            #mapdeck::mapdeckOutput(outputId = "mymap_md",  width = "100%", height = 800),
            shiny::actionButton('reset_legend', 'Reset legend'),
            shinyBS::bsTooltip("reset_legend", "Load the legend back on if not displayed, or mismatched.",
                               "bottom", options = list(container = "body")),
            shiny::actionButton('reset_lines', 'Reset lines'),
            shinyBS::bsTooltip("reset_lines", "Sometimes line ids can be lost: this reloads them.",
                               "bottom", options = list(container = "body")),
            ##if(!Mapdeck){
            ##  leafgl::leafglOutput("mymap", width = "100%", height = 800)
            ##} else{
            ##}

            selectInput("scene", "Select Scene", choices = c("CurrentSize", "A4Landscape", "A4Portrait")),
            actionButton("print", "Print Map"),

            # ------------------------------------------------------------- #

            shiny::fluidRow(
              shinydashboard::box(title = "Lines and markers", width = 12,
                 status="primary",collapsible = TRUE,
                 solidHeader = TRUE,
                 #background = "navy",
                 shiny::column(12,
                    shiny::uiOutput("slider"),
                      shiny::fluidRow(
                        shiny::column(6,
                          shiny::sliderInput(inputId = "Slider_radius",
                             label = "Fix radius",
                             #value = 8, min = 1, max = 20 # mapbox defaults
                             value = 5, min = 1, max = 20 # leaflet defaults
                          )
                        ),
                        shiny::column(6,
                          shiny::sliderInput(inputId = "Slider_opacity",
                             label = "Fix opacity",
                             value = 0.5, min = 0.1, max = 1
                          )
                        )
                      ),
                      shiny::fluidRow(
                        shiny::column(6,
                          shiny::sliderInput(inputId = "line_opacity",
                             label = "Line opacity",
                             value = 0.5, min = 0.1, max = 1
                          )
                        ),
                        shiny::column(6,
                          shiny::sliderInput(inputId = "line_width",
                             label = "Line width",
                             #value = 0.1, min = 0.005, max = 8 # mapbox defaults
                             value = 1.5, min = 0.005, max = 8 # leaflet defaults
                          )
                        )
                      ),
                      if(!is.null(points)){
                        shiny::fluidRow(
                          shiny::column(6,
                            shiny::sliderInput(inputId = "Slider_radius_p",
                               label = "Circle marker radius",
                               value = 4, min = 1, max = 15
                            )
                          ),
                          shiny::column(6,
                            shiny::sliderInput(inputId = "Slider_opacity_p",
                               label = "Circle marker opacity",
                               value = 0.5, min = 0.1, max = 1
                            )
                          )
                        )
                      }
                    )
              )
            )
          ), # tab panel "Main map"

          ###########################################################################################
          # Tab panel for datasets
          # this is a separate module 'DatasetModule'

          shiny::tabPanel("Datasets", value = "2",

              shiny::fluidRow(
                shiny::column(12,
                  shiny::h4("Track datasets available")
                )
              ),

              shiny::fluidRow(

                  shiny::column(12,

                      #shiny::tags$h4("Visualisation tools"),
                      shiny::tags$b("Visualisation tools"),
                      shiny::tags$br(),
                      shiny::actionButton("Display_data", "Display", styleclass = "success"),
                      shiny::actionButton("Combine_data", "Combine", styleclass = "success"),
                      shiny::actionButton("Delete_data", "Delete", styleclass = "success"),
                      shiny::tags$br(),
                      #shiny::actionButton("Display_clean_data", "Display clean data", styleclass = "success")
                      tags$div(style="display:inline-block;vertical-align:top;",
                        shiny::uiOutput("Display_clean_data")
                      ),
                      tags$div(style="display:inline-block;vertical-align:top;",
                        shiny::uiOutput("Display_thinned_data")
                      ),
                      shiny::br(),
                      shiny::fileInput('datafile', 'Upload CSV file',
                         accept=c('csv', 'comma-separated-values','.csv')),
                     # shiny::h5("Download current dataset"),


                      tags$head(
                        tags$style(type="text/css",
                                   "
                                   #align_right label {float:left; padding-right: 10px;}


                                  "
                         )
                      ),
                      tags$div(id = "align_right",
                               shiny::radioButtons(inputId = "downloadData_view", label = "Displayed data: ",
                                                   choiceNames = c("yes", "no"),
                                                   choiceValues = c("yes", "no"),
                                                   inline = TRUE,
                                                   selected = "no")
                               ),
                     tags$div(style="display:inline-block;vertical-align:top;",
                      shiny::downloadButton("downloadData", "Download .csv")
                     )





                  ),
                  shiny::column(12,
                      rhandsontable::rHandsontableOutput('gogo')
                  )
                  #,
                  #shiny::uiOutput("map_data")
                ),

              shiny::fluidRow(
                shiny::column(12,
                    shiny::uiOutput("Trip_datasets_available")
                )
              ),
              shiny::fluidRow(
                shiny::column(12,
                      rhandsontable::rHandsontableOutput('gogo2') # the list of trip datasets if trip_stats are run off
                )
              ),

              #tableOutput('table')
              DT::dataTableOutput("table")


              #,
              #shiny::fluidRow(
              #  shiny::column(12,
              #    shiny::actionButton("transfer_data", "Transfer", styleclass = "success")
              #  )
              #)


          ), # tab panel end
          selected = "Main map"
          ) # tabsetPanel end
         ) # tab box end
      )
      #,

    ),
    ######################################
    ############# 1. Database access tab
    ######################################
      shinydashboard::tabItem(tabName = "access",
      shiny::fluidRow(
        shinydashboard::box(
        title = htmltools::span("1. Remotely access data.", style="color:black"),
          status="danger",
          collapsible = FALSE,
          width = 12,
          solidHeader = TRUE,
          shiny::tags$div(
            shiny::tags$small(
              shiny::tags$b(
                "Source data from online databases. Currently, only two
                are provided: Movebank and the University of Amsterdam
                Bird-tracking System (UvA-BiTS)")
              ),
            shiny::tags$br() # break
            )
        ),
        ####################
        # MOVEBANK
        ####################
        shiny::tags$hr(),
        shiny::fluidRow(
          shiny::column(12,
            shinydashboard::box(
              shinybusy::add_busy_spinner(spin = "fading-circle"),
              title = htmltools::span(htmltools::img(src ="https://www.movebank.org/cms/img/logo-movebank.png",height = "50px")),
              collapsible = FALSE,
              collapsed = FALSE,
              width = 12,
              status = "danger",
              solidHeader = TRUE,
              shiny::tags$div(
                shiny::tags$small("Access data direct from Movebank. You need to have handy your
                  Movebank login and password, and the name of the repository being accessed.
                  For specific animals, also the IDs in Movebank are needed."),
                shiny::tags$br(),
                shiny::h4("OPTION 1"),
                shiny::tags$small("Manually enter in the boxes below to extract custom bird selections (each box
                                  corresponds to an argument in function read_track_MB."),
                shiny::tags$br()
              ),
              shiny::fluidRow(
                 shiny::column(6,
                    shiny::textInput("TagID_MB", label='TagID', value = 467, width = '100%'),
                      shinyBS::bsTooltip("TagID_MB", "TagID(s) to be downloaded.",
                        "top", options = list(container = "body")),
                    shiny::textInput("start_MB", label='start', value = "2016-05-29 12:29:34", width = '100%'),
                      shinyBS::bsTooltip("start_MB", "Start DateTime (UTC) in the format YYYY-mm-dd hh:mm:ss for beginning of data span.",
                         "top", options = list(container = "body")),
                    shiny::textInput("end_MB", label='end', value = "2016-08-29 20:00:00", width = '100%'),
                      shinyBS::bsTooltip("end_MB", "End DateTime (UTC) in the format YYYY-mm-dd hh:mm:ss for end of the data span.",
                         "top", options = list(container = "body")),
                    shiny::textInput("option_read_track_MB", label='option', value = "BTOTT", width = '100%'),
                      shinyBS::bsTooltip("option_read_track_MB", "Format of read in either BTOTT for standard format or original raw Movebank format BTOTT_MB.",
                                       "top", options = list(container = "body")),
                    shiny::textInput("repo_MB", label='repo', value = "BTO - North West England 2016 - Lesser Black-backed Gull", width = '100%'),
                      shinyBS::bsTooltip("repo_MB", "The name of Movebank repository to be accessed (note only one allowed here).",
                         "bottom", options = list(container = "body")),
                  ),
                  shiny::column(6,
                    shiny::textInput("dropsat_MB", label='dropsat', value = FALSE, width = '100%'),
                      shinyBS::bsTooltip("dropsat_MB", "Boolean whether to allow filtering out number of satellites.",
                        "top", options = list(container = "body")),
                    shiny::numericInput("dropsats_MB", label='dropsats', value = 4, width = '100%'),
                      shinyBS::bsTooltip("dropsats_MB", "Integer value for minimum number of satellites to retain (inclusive).",
                        "top", options = list(container = "body")),
                    shiny::textInput("flt_switch_MB", label='flt_switch', value = FALSE, width = '100%'),
                      shinyBS::bsTooltip("flt_switch_MB", "Boolean whether to retain Movebank flt.switch values marked as erroneous in the system.",
                        "top", options = list(container = "body")),
                    shiny::numericInput("mindata_MB", label='mindata', value = 5, width = '100%'),
                      shinyBS::bsTooltip("mindata_MB", "Minimum number of rows to retain per animal; anything less than this value will be ignored.",
                        "bottom", options = list(container = "body")),
                    shiny::actionButton("goButton3", "Download")
                   )
              ),
              shiny::tags$div(
                shiny::h4("OPTION 2"),
                shiny::tags$small("Assess the animal IDs in the selected repo, and download data through interaction with the returned table."),
                shiny::tags$br() # break
              ),
              shiny::tags$small("Check TagIDs in the Movebank repository"),
              shiny::column(12,
                shiny::actionButton("goButton4", "Check TagIDs", styleclass = "success"),
                shiny::actionButton("DonwloadMBdata", "Download", styleclass = "success")
              ),
              shiny::column(12,
                  rhandsontable::rHandsontableOutput('demTb')
              ),
              shiny::fluidRow(
                shinydashboard::box(title = "Downloaded MB data",
                  collapsible = TRUE,
                  #title = "Downloaded MB data",
                  width = 12,
                  #status = "danger",
                  solidHeader = TRUE,
                  DT::dataTableOutput("MB_table") # create here where we can access it / store that informatuon
                )
              )
            ) # movebank main box
          ) # mvebank column
        ), # Movebank Fluid row
        #
        #
        ####################################
        # UVA
        ####################################
        #
        #
        shiny::fluidRow(
          shiny::column(12,
            shinydashboard::box(
              shinybusy::add_busy_spinner(spin = "fading-circle"),
              title = htmltools::span(htmltools::img(src ="https://www.uva-bits.nl/wp-content/uploads/2025/03/super_resolution_headerblauw-300x42.jpeg",height = "50px")),
              collapsible = FALSE,
              collapsed = FALSE,
              width = 12,
              status = "info",
              solidHeader = TRUE,
              shiny::tags$div(
                shiny::tags$small("Download data direct from UvA-BiTS. This requires set up of
                                  an RODBC driver link, instructions for which are in the R help
                                  file of the function MoveRakeR::read_track_UvA. The default label for the DSN is GPS."),
                htmltools::div(tags$b("see https://wiki.e-ecology.nl/index.php/How_to_access_the_e-Ecology_database", style = "color: red;")),

                shiny::tags$br(),
                shiny::h4("OPTION 1"),
                shiny::tags$small("Manually enter in the boxes below to extract custom bird selections (each box
                                  corresponds to an argument in function read_track_UvA."),
                shiny::tags$br()
              ),
              shiny::column(6,
                  shiny::textInput("TagID_UvA", label='TagID', value = 5845, width = '100%'),
                    shinyBS::bsTooltip("TagID_UvA", "TagID(s) to be downloaded.",
                      "right", options = list(container = "body")),
                  shiny::textInput("start_UvA", label='start', value = "2022-05-15 20:00:00", width = '100%'),
                    shinyBS::bsTooltip("start_UvA", "Start DateTime (UTC) in the format YYYY-mm-dd hh:mm:ss for beginning of data span.",
                       "right", options = list(container = "body")),
                  shiny::textInput("end_UvA", label='end', value = "2022-07-20 20:00:00", width = '100%'),
                    shinyBS::bsTooltip("end_UvA", "End DateTime (UTC) in the format YYYY-mm-dd hh:mm:ss for end of the data span.",
                       "top", options = list(container = "body")),
                  shiny::actionButton("goButton5", "Download")
              ),
              shiny::column(6,
                  shiny::textInput("dropsat_UvA", label='dropsat', value = FALSE, width = '100%'),
                    shinyBS::bsTooltip("dropsat_UvA", "Boolean whether to allow filtering out number of satellites.",
                       "right", options = list(container = "body")),
                  shiny::numericInput("dropsats_UvA", label='dropsats', value = 4, width = '100%'),
                    shinyBS::bsTooltip("dropsats_UvA", "Integer value for minimum number of satellites to retain (inclusive).",
                       "right", options = list(container = "body")),
                  shiny::textInput("pressure", label='pressure', value = FALSE, width = '100%'),
                    shinyBS::bsTooltip("pressure", "Boolean whether to also download pressure data for assessing altitude, note has to be in the UvA-BiTS system for the given TagID.",
                       "right", options = list(container = "body")),
                  shiny::numericInput("mindata_UvA", label='mindata', value = 5, width = '100%'),
                    shinyBS::bsTooltip("mindata_UvA", "Minimum number of rows to retain per animal, anything less than this value will be disregarded (five is the default).",
                       "bottom", options = list(container = "body"))
              ),
              shiny::tags$div(
                shiny::tags$small("Note, these datasets are large so be mindful of how much data you download and display in one go!"),
                shiny::tags$br(),
                shiny::h4("OPTION 2"),
                shiny::tags$small("Assess the animal IDs available for station names, and download data through interaction with the returned tables.")
              ),
              #shiny::column(12,
                shiny::tags$small("Check TagIDs in the UvA repository"),
              #),
              shiny::column(12,
                shiny::actionButton("UvAStationData", "Station data", styleclass = "success"),
                shiny::actionButton("UvAGetTagIDs", "Get TagIDs", styleclass = "success")
              ),
              shiny::column(12,
                rhandsontable::rHandsontableOutput('demTb_uva_station'),
                shiny::tags$br(),
                shiny::uiOutput("UvAGetdata"), # rendered only if not NULL from station selection
                rhandsontable::rHandsontableOutput('demTb_uva_tid')
              ),
              #shiny::column(12,
              #  shiny::fluidRow(
              #    shinydashboard::box(
              #    collapsible = TRUE,
              #    width = 12,
              #    DT::dataTableOutput("UvA_table_tid")
              #  )
              #  )
              #),
              #shiny::column(12,
              #  shiny::actionButton("showdata_UvA", "Show data"),
              #  shiny::actionButton("reset_UvA", "Reset data", styleclass = "success"),
              #  shiny::verbatimTextOutput("nrow_UvA")
              #),
              shiny::fluidRow(
                shinydashboard::box(title = "Downloaded UvA-BiTS data",
                  collapsible = TRUE,
                  #title = "Downloaded MB data",
                  width = 12,
                  #status = "danger",
                  solidHeader = TRUE,
                  DT::dataTableOutput("UvA_table") # create here where we can access it / store that informatuon
                )
              )
            ) # uva main box
          ) # uva column
        ) #  UvA fluid row
      ) # main fluid row
    ), # tab
    ################## DATA EXPLORER TAB
    shinydashboard::tabItem(tabName = "explorer",
     shiny::fluidRow(
      shinybusy::add_busy_spinner(spin = "fading-circle"),
      shinydashboard::box(title = "Data Explorer",
        width = 12, status="warning",
        collapsible = TRUE,
        solidHeader = TRUE,
        shiny::tags$div(
          shiny::h5("Explore the variables you may have in the dataset alongside the tracking data.
                            Typically, this could be things like number of satellites, speeds, altitudes,
                            or battery voltages but can be any other covariates you may have in the data.
                            The current layout allows selection of any number of these variables to be
                            plotted over DateTime for one TagID at a time."),
          #shiny::br(),
          shiny::h5("Click on the bird to display the xy data on the map and then click on the plot icon and
                            tree icon to display covariates for a given plot type. Currently plotly
                            and ggplot are coded that you can switch between. Full screen recommended for best experience.")
          #,
          #shiny::tags$br(), # break
        )
        )
      ),
      shiny::fluidRow(
          shiny::column(5,
             shiny::br(),
              shiny::fluidRow(
                shiny::column(1,
                  shinyWidgets::dropdown(
                    shiny::uiOutput("switch_graph_type"),
                    style = "unite",
                    size = "sm",
                    icon = icon("chart-line"),
                    status = "warning", width = "200px"
                  )
                ),
                shiny::column(1,
                  shinyWidgets::dropdown(
                    shiny::uiOutput("switch_covariate"),
                    style = "unite",
                    size = "sm",
                    icon = icon("tree"),
                    status = "warning", width = "200px"
                  )
                ),
                shiny::column(1,
                  shinyWidgets::dropdown(
                    shiny::textInput(inputId = "cov_setting", label='Graph height', value = "100px", width = '100%'),
                    shiny::numericInput(inputId = "cov_title_size", label='Title size', value = 8, width = '100%'),
                    shiny::numericInput(inputId = "cov_tick_size", label='Tick size', value = 8, width = '100%'),
                    style = "unite",
                    size = "sm",
                    icon = icon("cog"),
                    status = "warning", width = "200px"
                  )
                )
                #,
                #shiny::column(5,
                #  shiny::actionButton(
                #    inputId = "submit_cov",
                #    label = "Plot Covariates",
                #    style = "margin:40px;"
                #  )
                #)
             ),
             # old option of plotting plotly direct in the UI - now done server side for ggplot2 option switcher
             #div(
             #selectInput(inputId = "trip_stat_plotly", label = "Parameter", c("DistMax","TripDur","DistTotal")),
             #plotly::plotlyOutput("plotly_altitude", height = "100px"),
             #plotly::plotlyOutput("plotly_speed3d", height = "100px"),
             #plotly::plotlyOutput("plotly_traj", height = "100px"),
             #plotly::plotlyOutput("plotly_nsat", height = "100px"),
             #plotly::plotlyOutput("plotly_temp", height = "100px")
             #)
             #### graph elements on the left-hand side
             #shiny::uiOutput("ep1"),
             #shiny::uiOutput("ep2"),
             #shiny::uiOutput("ep3"),
             #shiny::uiOutput("ep4"),
             #shiny::uiOutput("ep5")

             shiny::uiOutput("graphs_ui")

          ),
          shiny::column(7,
            shiny::br(),
            shiny::fluidRow(
              shiny::column(1,
                    shinyWidgets::dropdown(
                      shiny::uiOutput("TagID2"),
                      style = "unite",
                      size = "sm",
                      icon = icon("earlybirds"),
                      status = "warning", width = "200px"
                      )
              )
            ),
            # then the map
            #h6("Leaflet placeholder"),

            # then the map
            leaflet::leafletOutput("mymap_explorer",
                                   width = "100%",
                                   height = 400),
            # underneath leaflet map, add time slider
            shiny::fluidRow(
              #shinydashboard::box(title = "Time slider", width = 12,
                #status="warning",collapsible = TRUE,
                #solidHeader = TRUE,
                #background = "navy",
                shiny::column(12,
                  shiny::uiOutput("slider2")
                )
              #)
            )
            #leaflet::leafletOutput("mymap_trips",
            #                       width = "100%",
            #                       height = 400)
          )
        )
    ),
    ################## DATA ANALYTICS TAB
    shinydashboard::tabItem(tabName = "data_analytics",
        shiny::fluidRow(
          shinydashboard::box(title = "Data analytics",
            shinybusy::add_busy_spinner(spin = "fading-circle"),
              status="info",
              collapsible = TRUE,
              width = 12,
              solidHeader = TRUE,
              shiny::h6("This is where you can investigate the spans of data through time per bird, and
                        analyse rates tags were on, get no. fixes per bird etc.")
              #shiny::fluidRow(
              #  shinydashboard::box(title = "Select parameters",
              #    #status="primary",
              #    shiny::column(6,
              #        shiny::numericInput("drop_sats", label='drop_sats', value = 3, width = '100%'),
              #        shinyBS::bsTooltip("drop_sats", "No. of satellite threshold to keep in the data (inclusive). Anything less than this value will be dropped.",
              #       "right", options = list(container = "body")),
              #    ) # close main box
              #  ) #b
              #) # fr
            ) #b
          ) #fr
    ) # close tabname
   ) # close overarching tabItems
) # close main dashboardBody

# initialise the dashboard page - has to be wrapped up because we are using bootstrap
ui <- shiny::bootstrapPage(
  shinydashboard::dashboardPage(header, sidebar, body, skin = "purple")
)






