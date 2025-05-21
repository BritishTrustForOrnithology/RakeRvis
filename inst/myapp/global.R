# from original input variables into function
require(dplyr)
require(shiny)
require(MoveRakeR)

Libs <- c("shiny", "mapdeck","shinydashboard", "shinyBS", "shinybusy", "leafgl","leaflet","leaflet.extras2","DT","sp","sf","sfheaders","raster", "rhandsontable")
for(i in Libs){require(i,character.only = TRUE)}

options("sp_evolution_status"=2)

# use point symbols from base R graphics as icons
pchIcons <- function(pch = 0:14, width = 30, height = 30, ...) {
  n <- length(pch)
  files <- character(n)
  # create a sequence of png images
  for (i in seq_len(n)) {
    f <- tempfile(fileext = ".png")
    png(f, width = width, height = height, bg = "transparent")
    par(mar = c(0, 0, 0, 0))
    plot.new()
    points(.5, .5, pch = pch[i], cex = min(width, height) / 8, ...)
    dev.off()
    files[i] <- f
  }
  files
}

#map_css_behind_textboxes = HTML("
#  .leaflet-top, .leaflet-bottom {
#    z-index: unset !important;
#  }
#  .leaflet-touch .leaflet-control-layers, .leaflet-touch .leaflet-bar {
#    z-index: 10000000000 !important;
#  }
#")

onedata <- PKGENVIR2$onedata
onedataexist <- PKGENVIR2$onedataexist
shapes <- PKGENVIR2$shapes
TagID <- PKGENVIR2$TagID
Providers <- PKGENVIR2$Providers
col <- PKGENVIR2$col
col_anim <- PKGENVIR2$col_anim
plot_w <- PKGENVIR2$plot_w
plot_h <- PKGENVIR2$plot_h
shapes <<- PKGENVIR2$shapes
pcol <- PKGENVIR2$pcol
scol <- PKGENVIR2$scol
points <- PKGENVIR2$points
PathOut <- PKGENVIR2$PathOut
legend <- PKGENVIR2$legend
lines4fixes <- PKGENVIR2$lines4fixes
fixes <- PKGENVIR2$fixes

#if(Mapdeck){

  Mapbox_API_key_public <- "pk.eyJ1IjoiY2hyaXN0aGF4dGVyIiwiYSI6ImNsbTY0OGI1NjMyNXMzbHA2b24xamd6NTQifQ.jTTkeD9-mChso9VbY4y7XA"
  mapdeck::set_token(Mapbox_API_key_public)

#}
