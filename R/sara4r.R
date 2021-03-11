
sara4r <- function() {


veirusGUI <- function(action, INIRENA){
    about <- gtkWindow()
    about["title"] <- "sar4r v0.0.8"

    frame <- gtkFrameNew("    INFO    ")
    about$add(frame)

    vbox <- gtkVBoxNew(FALSE, 8)
    vbox$setBorderWidth(24)
    frame$add(vbox)

    hbox <- gtkHBoxNew(FALSE, 8)
    vbox$packStart(hbox, FALSE, FALSE, 0)

    the.buttons <- gtkHButtonBoxNew()
    the.buttons$setBorderWidth(5)
    vbox$add(the.buttons)
    the.buttons$setLayout("spread")
    the.buttons$setSpacing(20)

    contactButton <- gtkButtonNewWithMnemonic("Contacts and INFO", show = TRUE)
    gSignalConnect(contactButton, "clicked", contact)
    the.buttons$packStart(contactButton,fill=F)

    hbox <- gtkHBoxNew(FALSE, 8)
    vbox$packStart(hbox, FALSE, FALSE, 0)

    the.buttons <- gtkHButtonBoxNew()
    the.buttons$setBorderWidth(5)
    vbox$add(the.buttons)
    the.buttons$setLayout("spread")
    the.buttons$setSpacing(20)

    licenceButton <- gtkButtonNewWithMnemonic("Licences, Terms and Conditions", show = TRUE)
    gSignalConnect(licenceButton, "clicked", licence)
    the.buttons$packStart(licenceButton,fill=F)

}


cn_ii <- function(action, window)
{
  cnii_win <- gtkWindow()
  cnii_win$setTitle ("sara4r v0.0.8")

  frame <- gtkFrameNew("  Get Curve Numbers for the Antecedent Moisture Condition II  ")
  cnii_win$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  hbox <- gtkHBoxNew(FALSE,8)

  label <- gtkLabelNew()
  label$setMarkup("<b>Parameters:</b>")
  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Landuse:   </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  landuse.entry <- gtkEntryNew()
  landuse.entry$setWidthChars(60)
  landuse.entry$setSensitive(FALSE)

  hbox$packStart(landuse.entry,FALSE,FALSE,0)

  land.but <- gtkButton("Browse")
  land.but$setTooltipText("Please, browse the Land use and land cover map")
  hbox$packStart(land.but,FALSE,FALSE,0)

  gSignalConnect(land.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Land use and land cover map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      landuse.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Land use and land covers", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>HSG:          </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  hsg.entry <- gtkEntryNew()
  hsg.entry$setWidthChars(60)
  hsg.entry$setSensitive(FALSE)

  hbox$packStart(hsg.entry,FALSE,FALSE,0)

  hsg.but <- gtkButton("Browse")
  hsg.but$setTooltipText("Please, browse the Hydrologic Soil Groups map")
  hbox$packStart(hsg.but,FALSE,FALSE,0)

  gSignalConnect(hsg.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Hydrologic Soil Group map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      hsg.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Hydrologic Soil Groups", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Index File:</b>")
  hbox$packStart(label,FALSE,FALSE,0)

  index.entry <- gtkEntryNew()
  index.entry$setWidthChars(60)
  index.entry$setSensitive(FALSE)
  hbox$packStart(index.entry,FALSE,FALSE,0)

  index.but <- gtkButton("Browse")
  index.but$setTooltipText("Please, browse the Curve Numbers index file")
  hbox$packStart(index.but,FALSE,FALSE,0)

  gSignalConnect(index.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the CN index file",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      index.entry$setText(filename)

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Comma-separated values (*.csv)")
                   fileFilter$addPattern("*.csv")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  the.buttons <- gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(20)

  buttonOK <- gtkButtonNewWithMnemonic("_Calculate CN II", show = TRUE)
  buttonOK$setTooltipText("This process takes a while. Please, wait until the program finished")
  gSignalConnect(buttonOK, "clicked",
                 f = function(widget, ...) {

                   if ((landuse.entry$getText())=="") return(invisible(NULL))
                   if ((hsg.entry$getText())=="") return(invisible(NULL))
                   if ((index.entry$getText())=="") return(invisible(NULL))

                   landuse_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                   hsg_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                   index_file <-  normalizePath(path.expand(index.entry$getText()),  winslash = "/", mustWork = NA)

                   land <- raster(landuse_file)
                   soil <- raster(hsg_file)

                   landsoil <- (land + soil)
                   dev.new()
                   plot(landsoil, main= "Landsoil classes", xlab="Longitude", ylab="Latitude")
                   writeRaster(landsoil, filename = "./landsoil", format="GTiff", overwrite=TRUE)

                   index <- read.csv(index_file, header = FALSE, sep = ",")

                   cn <- reclassify(landsoil, index, include.lowest=TRUE, right=FALSE)
                   writeRaster(cn, filename = "./cn_amc_ii", format="GTiff", overwrite=TRUE)

                   path <- getwd()
                   dialog <- gtkMessageDialogNew(cnii_win, c("modal", "destroy-with-parent"), "info", "close",
                                                 "Landsoil.tif and CN_amc_ii.tif were stored in\n",
                                                 "the working folder ", path, " ")

                   dialog$formatSecondaryText()
                   gSignalConnect(dialog, "response", gtkWidgetDestroy)

                   cn_rat <- ratify(cn)
                   cn_rat
                   cn_rat@data@attributes
                   cn_types <- cn_rat@data@attributes[[1]]
                   cols <- rainbow(nrow(cn_types))
                   cols[1] <- "darkgreen"
                   dev.new()
                   image(cn_rat, col=cols, main="Curve numbers AMC II",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = cn_types$ID, fill = cols, bty="n", cex = 0.8)

                 })
  the.buttons$packStart(buttonOK,fill=F)

  buttonClear <- gtkButtonNewWithMnemonic("Clea_r", show = TRUE)
  buttonClear$setTooltipText("Clic here to clean all data entries")

  gSignalConnect(buttonClear, "clicked",
                 f = function(widget, ...) {

                   landuse.entry$setText("")
                   hsg.entry$setText("")
                   index.entry$setText("")

                 })

  the.buttons$packStart(buttonClear,fill=F)

  buttonCancel <- gtkButtonNewWithMnemonic("Ca_ncel", show = TRUE)
  gSignalConnect(buttonCancel, "clicked", cnii_win$destroy)
  the.buttons$packStart(buttonCancel,fill=F)

}



sobhani_i <- function(action, window)
{
  cni_win <- gtkWindow()
  cni_win$setTitle ("sara4r v0.0.8")

  frame <- gtkFrameNew("  Get Curve Numbers for the Antecedent Moisture Condition I  ")
  cni_win$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  hbox <- gtkHBoxNew(FALSE,8)
  label <- gtkLabelNew()
  label$setMarkup("<b>Sobhani (1975)</b>")
  label$setTooltipText("Sobhani (1975). A review of selected small watershed design methods for possible adoption to Iranian conditions. M.S. Thesis, Utah State University, Logan, UT")
  vbox$packStart(label,FALSE,FALSE,0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>   CNII</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>CNI =   ---------------------------          </b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>  2.334 - 0.01334*CNII</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  vbox$packStart(hbox, FALSE, FALSE, 0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  hbox <- gtkHBoxNew(FALSE,8)
  label <- gtkLabelNew()
  label$setMarkup("<b>\n Parameters:</b>")
  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Landuse:   </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  landuse.entry <- gtkEntryNew()
  landuse.entry$setWidthChars(60)
  landuse.entry$setSensitive(FALSE)
  hbox$packStart(landuse.entry,FALSE,FALSE,0)

  land.but <- gtkButton("Browse")
  land.but$setTooltipText("Please, browse the Land use and land cover map")
  hbox$packStart(land.but,FALSE,FALSE,0)

  gSignalConnect(land.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Land use and land cover map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      landuse.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Land use and land covers", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>HSG:          </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  hsg.entry <- gtkEntryNew()
  hsg.entry$setWidthChars(60)
  hsg.entry$setSensitive(FALSE)
  hbox$packStart(hsg.entry,FALSE,FALSE,0)

  hsg.but <- gtkButton("Browse")
  hsg.but$setTooltipText("Please, browse the Hydrologic Soil Groups map")
  hbox$packStart(hsg.but,FALSE,FALSE,0)

  gSignalConnect(hsg.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Hydrologic Soil Group map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      hsg.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Hydrologic Soil Groups", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Index File:</b>")
  hbox$packStart(label,FALSE,FALSE,0)

  index.entry <- gtkEntryNew()
  index.entry$setWidthChars(60)
  index.entry$setSensitive(FALSE)

  hbox$packStart(index.entry,FALSE,FALSE,0)

  index.but <- gtkButton("Browse")
  index.but$setTooltipText("Please, browse the Curve Numbers index file")
  hbox$packStart(index.but,FALSE,FALSE,0)

  gSignalConnect(index.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the CN index file",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      index.entry$setText(filename)

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Comma-separated values (*.csv)")
                   fileFilter$addPattern("*.csv")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  the.buttons <- gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(20)

  buttonOK <- gtkButtonNewWithMnemonic("_Calculate CN I", show = TRUE)
  buttonOK$setTooltipText("This process takes a while. Please, wait until the program finished")
  gSignalConnect(buttonOK, "clicked",
                 f = function(widget, ...) {

                   if ((landuse.entry$getText())=="") return(invisible(NULL))
                   if ((hsg.entry$getText())=="") return(invisible(NULL))
                   if ((index.entry$getText())=="") return(invisible(NULL))

                   landuse_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                   hsg_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                   index_file <-  normalizePath(path.expand(index.entry$getText()),  winslash = "/", mustWork = NA)

                   land <- raster(landuse_file)
                   soil <- raster(hsg_file)

                   landsoil <- (land + soil)
                   dev.new()
                   plot(landsoil, main= "Landsoil classes", xlab="Longitude", ylab="Latitude")
                   writeRaster(landsoil, filename = "./landsoil", format="GTiff", overwrite=TRUE)

                   index <- read.csv(index_file, header = FALSE, sep = ",")

                   cn <- reclassify(landsoil, index, include.lowest=TRUE, right=FALSE)
                   writeRaster(cn, filename = "./cn_amc_ii", format="GTiff", overwrite=TRUE)

                   cni <- round((cn/(2.334-(0.01334*cn))))
                   writeRaster(cni, filename = "./cn_sobhani_i", format="GTiff", overwrite=TRUE)

                   cn_rat <- ratify(cn)
                   cn_rat
                   cn_rat@data@attributes
                   cn_types <- cn_rat@data@attributes[[1]]
                   cols <- rainbow(nrow(cn_types))
                   cols[1] <- "darkgreen"
                   dev.new()
                   image(cn_rat, col=cols, main="Curve numbers AMC II",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = cn_types$ID, fill = cols, bty="n", cex = 0.8)

                   cni_rat <- ratify(cni)
                   cni_rat
                   cni_rat@data@attributes
                   cni_types <- cni_rat@data@attributes[[1]]
                   cols <- rainbow(nrow(cni_types))
                   cols[1] <- "darkgreen"
                   dev.new()
                   image(cni_rat, col=cols, main="Curve numbers AMC I",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = cni_types$ID, fill = cols, bty="n", cex = 0.8)

                   path <- getwd()
                   dialog <- gtkMessageDialogNew(cni_win, c("modal", "destroy-with-parent"), "info", "close",
                                                 "Landsoil.tif, CN_amc_ii.tif and CN_sobhani_i.tif were stored in\n",
                                                 "the working folder ", path, " ")

                   dialog$formatSecondaryText()
                   gSignalConnect(dialog, "response", gtkWidgetDestroy)

                 })
  the.buttons$packStart(buttonOK,fill=F)

  buttonClear <- gtkButtonNewWithMnemonic("Clea_r", show = TRUE)
  buttonClear$setTooltipText("Clic here to clean all data entries")

  gSignalConnect(buttonClear, "clicked",
                 f = function(widget, ...) {

                   landuse.entry$setText("")
                   hsg.entry$setText("")
                   index.entry$setText("")

                 })

  the.buttons$packStart(buttonClear,fill=F)

  buttonCancel <- gtkButtonNewWithMnemonic("Ca_ncel", show = TRUE)
  gSignalConnect(buttonCancel, "clicked", cni_win$destroy)
  the.buttons$packStart(buttonCancel,fill=F)

}



hawkins_i <- function(action, window)
{
  cni_win <- gtkWindow()
  cni_win$setTitle ("sara4r v0.0.8")

  frame <- gtkFrameNew("  Get Curve Numbers for the Antecedent Moisture Condition I  ")
  cni_win$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  hbox <- gtkHBoxNew(FALSE,8)
  label <- gtkLabelNew()
  label$setMarkup("<b>Hawkins et al. (1985)</b>")
  label$setTooltipText("Hawkins et al. (1985). Runoff probability, storm depth and curve numbers. Journal of Irrigation and Drainage Engineering, 111(4): 330-340")
  vbox$packStart(label,FALSE,FALSE,0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>   CNII</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>CNI =   ---------------------------          </b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>  2.281 - 0.01281*CNII</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  vbox$packStart(hbox, FALSE, FALSE, 0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  hbox <- gtkHBoxNew(FALSE,8)
  label <- gtkLabelNew()
  label$setMarkup("<b>\n Parameters:</b>")
  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Landuse:   </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  landuse.entry <- gtkEntryNew()
  landuse.entry$setWidthChars(60)
  landuse.entry$setSensitive(FALSE)
  hbox$packStart(landuse.entry,FALSE,FALSE,0)

  land.but <- gtkButton("Browse")
  land.but$setTooltipText("Please, browse the Land use and land cover map")
  hbox$packStart(land.but,FALSE,FALSE,0)

  gSignalConnect(land.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Land use and land cover map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      landuse.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Land use and land covers", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>HSG:          </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  hsg.entry <- gtkEntryNew()
  hsg.entry$setWidthChars(60)
  hsg.entry$setSensitive(FALSE)
  hbox$packStart(hsg.entry,FALSE,FALSE,0)

  hsg.but <- gtkButton("Browse")
  hsg.but$setTooltipText("Please, browse the Hydrologic Soil Groups map")
  hbox$packStart(hsg.but,FALSE,FALSE,0)

  gSignalConnect(hsg.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Hydrologic Soil Group map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      hsg.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Hydrologic Soil Groups", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Index File:</b>")
  hbox$packStart(label,FALSE,FALSE,0)

  index.entry <- gtkEntryNew()
  index.entry$setWidthChars(60)
  index.entry$setSensitive(FALSE)

  hbox$packStart(index.entry,FALSE,FALSE,0)

  index.but <- gtkButton("Browse")
  index.but$setTooltipText("Please, browse the Curve Numbers index file")
  hbox$packStart(index.but,FALSE,FALSE,0)

  gSignalConnect(index.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the CN index file",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      index.entry$setText(filename)

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Comma-separated values (*.csv)")
                   fileFilter$addPattern("*.csv")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  the.buttons <- gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(20)

  buttonOK <- gtkButtonNewWithMnemonic("_Calculate CN I", show = TRUE)
  buttonOK$setTooltipText("This process takes a while. Please, wait until the program finished")
  gSignalConnect(buttonOK, "clicked",
                 f = function(widget, ...) {

                   if ((landuse.entry$getText())=="") return(invisible(NULL))
                   if ((hsg.entry$getText())=="") return(invisible(NULL))
                   if ((index.entry$getText())=="") return(invisible(NULL))

                   landuse_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                   hsg_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                   index_file <-  normalizePath(path.expand(index.entry$getText()),  winslash = "/", mustWork = NA)

                   land <- raster(landuse_file)
                   soil <- raster(hsg_file)

                   landsoil <- (land + soil)
                   dev.new()
                   plot(landsoil, main= "Landsoil classes", xlab="Longitude", ylab="Latitude")
                   writeRaster(landsoil, filename = "./landsoil", format="GTiff", overwrite=TRUE)

                   index <- read.csv(index_file, header = FALSE, sep = ",")

                   cn <- reclassify(landsoil, index, include.lowest=TRUE, right=FALSE)
                   writeRaster(cn, filename = "./cn_amc_ii", format="GTiff", overwrite=TRUE)

                   cni <- round((cn/(2.281-(0.01281*cn))))
                   writeRaster(cni, filename = "./cn_hawkins_i", format="GTiff", overwrite=TRUE)

                   cn_rat <- ratify(cn)
                   cn_rat
                   cn_rat@data@attributes
                   cn_types <- cn_rat@data@attributes[[1]]
                   cols <- rainbow(nrow(cn_types))
                   cols[1] <- "darkgreen"
                   dev.new()
                   image(cn_rat, col=cols, main="Curve numbers AMC II",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = cn_types$ID, fill = cols, bty="n", cex = 0.8)

                   cni_rat <- ratify(cni)
                   cni_rat
                   cni_rat@data@attributes
                   cni_types <- cni_rat@data@attributes[[1]]
                   cols <- rainbow(nrow(cni_types))
                   cols[1] <- "darkgreen"
                   dev.new()
                   image(cni_rat, col=cols, main="Curve numbers AMC I",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = cni_types$ID, fill = cols, bty="n", cex = 0.8)

                   path <- getwd()
                   dialog <- gtkMessageDialogNew(cni_win, c("modal", "destroy-with-parent"), "info", "close",
                                                 "Landsoil.tif, CN_amc_ii.tif and CN_hawkins_i.tif were stored in\n",
                                                 "the working folder ", path, " ")

                   dialog$formatSecondaryText()
                   gSignalConnect(dialog, "response", gtkWidgetDestroy)

                 })
  the.buttons$packStart(buttonOK,fill=F)

  buttonClear <- gtkButtonNewWithMnemonic("Clea_r", show = TRUE)
  buttonClear$setTooltipText("Clic here to clean all data entries")

  gSignalConnect(buttonClear, "clicked",
                 f = function(widget, ...) {

                   landuse.entry$setText("")
                   hsg.entry$setText("")
                   index.entry$setText("")

                 })

  the.buttons$packStart(buttonClear,fill=F)

  buttonCancel <- gtkButtonNewWithMnemonic("Ca_ncel", show = TRUE)
  gSignalConnect(buttonCancel, "clicked", cni_win$destroy)
  the.buttons$packStart(buttonCancel,fill=F)

}



chow_i <- function(action, window)
  {
cni_win <- gtkWindow()
cni_win$setTitle ("sara4r v0.0.8")

frame <- gtkFrameNew("  Get Curve Numbers for the Antecedent Moisture Condition I  ")
cni_win$add(frame)

vbox <- gtkVBoxNew(FALSE, 8)
vbox$setBorderWidth(24)
frame$add(vbox)

hbox <- gtkHBoxNew(FALSE,8)
label <- gtkLabelNew()
label$setMarkup("<b>Chow et al. (1988)</b>")
label$setTooltipText("Chow et al. (1988). Applied hydrology. McGraw-Hill, New York")
vbox$packStart(label,FALSE,FALSE,0)

vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

label <- gtkLabelNew()
label$setMarkup("<b>   4.2*CNII</b>")
vbox$packStart(label,FALSE,FALSE,0)

label <- gtkLabelNew()
label$setMarkup("<b>CNI =   ---------------------------          </b>")
vbox$packStart(label,FALSE,FALSE,0)

label <- gtkLabelNew()
label$setMarkup("<b>  10 - 0.058*CNII</b>")
vbox$packStart(label,FALSE,FALSE,0)

vbox$packStart(hbox, FALSE, FALSE, 0)

vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

hbox <- gtkHBoxNew(FALSE,8)
label <- gtkLabelNew()
label$setMarkup("<b>\n Parameters:</b>")
vbox$packStart(label,FALSE,FALSE,0)
vbox$packStart(hbox, FALSE, FALSE, 0)

label <- gtkLabelNew()
label$setMarkup("<b>Landuse:   </b>")
hbox$packStart(label,FALSE,FALSE,0)

landuse.entry <- gtkEntryNew()
landuse.entry$setWidthChars(60)
landuse.entry$setSensitive(FALSE)
hbox$packStart(landuse.entry,FALSE,FALSE,0)

land.but <- gtkButton("Browse")
land.but$setTooltipText("Please, browse the Land use and land cover map")
hbox$packStart(land.but,FALSE,FALSE,0)

gSignalConnect(land.but, "clicked",
               f = function(widget, ...) {
                 dialog <- gtkFileChooserDialog(title = "Select the Land use and land cover map",
                                                parent = NULL, action = "open",
                                                "gtk-ok", GtkResponseType["ok"],
                                                "gtk-cancel", GtkResponseType["cancel"],
                                                show = FALSE)

                 gSignalConnect(dialog, "response",
                                f = function(dialog, response, data) {
                                  if(response == GtkResponseType["ok"]) {
                                    filename <- dialog$getFilename()
                                    landuse.entry$setText(filename)

                                    dev.new()
                                    veirus_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                                    veirus_plot <- raster(veirus_file)
                                    plot(veirus_plot, main="Land use and land covers", xlab="Longitude", ylab="Latitude")

                                  }

                                  dialog$destroy()
                                })

                 fileFilter <- gtkFileFilter()
                 fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                 fileFilter$addPattern("*.tif")
                 fileFilter$addPattern("*.TIFF")
                 fileFilter$addPattern("*.asc")
                 dialog$addFilter(fileFilter)

                 dialog$run()

               })

hbox <- gtkHBoxNew(FALSE, 8)
vbox$packStart(hbox, FALSE, FALSE, 0)

label <- gtkLabelNew()
label$setMarkup("<b>HSG:          </b>")
hbox$packStart(label,FALSE,FALSE,0)

hsg.entry <- gtkEntryNew()
hsg.entry$setWidthChars(60)
hsg.entry$setSensitive(FALSE)
hbox$packStart(hsg.entry,FALSE,FALSE,0)

hsg.but <- gtkButton("Browse")
hsg.but$setTooltipText("Please, browse the Hydrologic Soil Groups map")
hbox$packStart(hsg.but,FALSE,FALSE,0)

gSignalConnect(hsg.but, "clicked",
               f = function(widget, ...) {
                 dialog <- gtkFileChooserDialog(title = "Select the Hydrologic Soil Group map",
                                                parent = NULL, action = "open",
                                                "gtk-ok", GtkResponseType["ok"],
                                                "gtk-cancel", GtkResponseType["cancel"],
                                                show = FALSE)

                 gSignalConnect(dialog, "response",
                                f = function(dialog, response, data) {
                                  if(response == GtkResponseType["ok"]) {
                                    filename <- dialog$getFilename()
                                    hsg.entry$setText(filename)

                                    dev.new()
                                    veirus_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                                    veirus_plot <- raster(veirus_file)
                                    plot(veirus_plot, main="Hydrologic Soil Groups", xlab="Longitude", ylab="Latitude")

                                  }

                                  dialog$destroy()
                                })

                 fileFilter <- gtkFileFilter()
                 fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                 fileFilter$addPattern("*.tif")
                 fileFilter$addPattern("*.TIFF")
                 fileFilter$addPattern("*.asc")
                 dialog$addFilter(fileFilter)

                 dialog$run()

               })

hbox <- gtkHBoxNew(FALSE, 8)
vbox$packStart(hbox, FALSE, FALSE, 0)

label <- gtkLabelNew()
label$setMarkup("<b>Index File:</b>")
hbox$packStart(label,FALSE,FALSE,0)

index.entry <- gtkEntryNew()
index.entry$setWidthChars(60)
index.entry$setSensitive(FALSE)

hbox$packStart(index.entry,FALSE,FALSE,0)

index.but <- gtkButton("Browse")
index.but$setTooltipText("Please, browse the Curve Numbers index file")
hbox$packStart(index.but,FALSE,FALSE,0)

gSignalConnect(index.but, "clicked",
               f = function(widget, ...) {
                 dialog <- gtkFileChooserDialog(title = "Select the CN index file",
                                                parent = NULL, action = "open",
                                                "gtk-ok", GtkResponseType["ok"],
                                                "gtk-cancel", GtkResponseType["cancel"],
                                                show = FALSE)

                 gSignalConnect(dialog, "response",
                                f = function(dialog, response, data) {
                                  if(response == GtkResponseType["ok"]) {
                                    filename <- dialog$getFilename()
                                    index.entry$setText(filename)

                                  }

                                  dialog$destroy()
                                })

                 fileFilter <- gtkFileFilter()
                 fileFilter$setName("Comma-separated values (*.csv)")
                 fileFilter$addPattern("*.csv")
                 dialog$addFilter(fileFilter)

                 dialog$run()

               })

vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

the.buttons <- gtkHButtonBoxNew()
the.buttons$setBorderWidth(5)
vbox$add(the.buttons)
the.buttons$setLayout("spread")
the.buttons$setSpacing(20)

buttonOK <- gtkButtonNewWithMnemonic("_Calculate CN I", show = TRUE)
buttonOK$setTooltipText("This process takes a while. Please, wait until the program finished")
gSignalConnect(buttonOK, "clicked",
               f = function(widget, ...) {

                 if ((landuse.entry$getText())=="") return(invisible(NULL))
                 if ((hsg.entry$getText())=="") return(invisible(NULL))
                 if ((index.entry$getText())=="") return(invisible(NULL))

                 landuse_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                 hsg_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                 index_file <-  normalizePath(path.expand(index.entry$getText()),  winslash = "/", mustWork = NA)

                 land <- raster(landuse_file)
                 soil <- raster(hsg_file)

                 landsoil <- (land + soil)
                 dev.new()
                 plot(landsoil, main= "Landsoil classes", xlab="Longitude", ylab="Latitude")
                 writeRaster(landsoil, filename = "./landsoil", format="GTiff", overwrite=TRUE)

                 index <- read.csv(index_file, header = FALSE, sep = ",")

                 cn <- reclassify(landsoil, index, include.lowest=TRUE, right=FALSE)
                 writeRaster(cn, filename = "./cn_amc_ii", format="GTiff", overwrite=TRUE)

                 cni <- round((4.2*cn/(10-(0.058*cn))))
                 writeRaster(cni, filename = "./cn_chow_i", format="GTiff", overwrite=TRUE)

                 cn_rat <- ratify(cn)
                 cn_rat
                 cn_rat@data@attributes
                 cn_types <- cn_rat@data@attributes[[1]]
                 cols <- rainbow(nrow(cn_types))
                 cols[1] <- "darkgreen"
                 dev.new()
                 image(cn_rat, col=cols, main="Curve numbers AMC II",  xlab="Longitude", ylab="Latitude")
                 legend("bottomleft", legend = cn_types$ID, fill = cols, bty="n", cex = 0.8)

                 cni_rat <- ratify(cni)
                 cni_rat
                 cni_rat@data@attributes
                 cni_types <- cni_rat@data@attributes[[1]]
                 cols <- rainbow(nrow(cni_types))
                 cols[1] <- "darkgreen"
                 dev.new()
                 image(cni_rat, col=cols, main="Curve numbers AMC I",  xlab="Longitude", ylab="Latitude")
                 legend("bottomleft", legend = cni_types$ID, fill = cols, bty="n", cex = 0.8)

                 path <- getwd()
                 dialog <- gtkMessageDialogNew(cni_win, c("modal", "destroy-with-parent"), "info", "close",
                                               "Landsoil.tif, CN_amc_ii.tif and CN_Chow_i.tif were stored in\n",
                                               "the working folder ", path, " ")

                 dialog$formatSecondaryText()
                 gSignalConnect(dialog, "response", gtkWidgetDestroy)

               })
the.buttons$packStart(buttonOK,fill=F)

buttonClear <- gtkButtonNewWithMnemonic("Clea_r", show = TRUE)
buttonClear$setTooltipText("Clic here to clean all data entries")

gSignalConnect(buttonClear, "clicked",
               f = function(widget, ...) {

                 landuse.entry$setText("")
                 hsg.entry$setText("")
                 index.entry$setText("")

               })

the.buttons$packStart(buttonClear,fill=F)

buttonCancel <- gtkButtonNewWithMnemonic("Ca_ncel", show = TRUE)
gSignalConnect(buttonCancel, "clicked", cni_win$destroy)
the.buttons$packStart(buttonCancel,fill=F)

}



mishra_i <- function(action, window)
{
  cni_win <- gtkWindow()
  cni_win$setTitle ("sara4r v0.0.8")

  frame <- gtkFrameNew("  Get Curve Numbers for the Antecedent Moisture Condition I  ")
  cni_win$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  hbox <- gtkHBoxNew(FALSE,8)
  label <- gtkLabelNew()
  label$setMarkup("<b>Mishra et al. (2008)</b>")
  label$setTooltipText("Mishra et al. (2008). Comparison of AMC-dependent CN-conversion formulae. Water Resources Management, 22(10), 1409-1420. doi:10.1007/s11269-007-9233-5")
  vbox$packStart(label,FALSE,FALSE,0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>   CNII</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>CNI =   ---------------------------          </b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>  2.2754 - 0.012754*CNII</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  vbox$packStart(hbox, FALSE, FALSE, 0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  hbox <- gtkHBoxNew(FALSE,8)
  label <- gtkLabelNew()
  label$setMarkup("<b>\n Parameters:</b>")
  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Landuse:   </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  landuse.entry <- gtkEntryNew()
  landuse.entry$setWidthChars(60)
  landuse.entry$setSensitive(FALSE)
  hbox$packStart(landuse.entry,FALSE,FALSE,0)

  land.but <- gtkButton("Browse")
  land.but$setTooltipText("Please, browse the Land use and land cover map")
  hbox$packStart(land.but,FALSE,FALSE,0)

  gSignalConnect(land.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Land use and land cover map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      landuse.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Land use and land covers", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>HSG:          </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  hsg.entry <- gtkEntryNew()
  hsg.entry$setWidthChars(60)
  hsg.entry$setSensitive(FALSE)
  hbox$packStart(hsg.entry,FALSE,FALSE,0)

  hsg.but <- gtkButton("Browse")
  hsg.but$setTooltipText("Please, browse the Hydrologic Soil Groups map")
  hbox$packStart(hsg.but,FALSE,FALSE,0)

  gSignalConnect(hsg.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Hydrologic Soil Group map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      hsg.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Hydrologic Soil Groups", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Index File:</b>")
  hbox$packStart(label,FALSE,FALSE,0)

  index.entry <- gtkEntryNew()
  index.entry$setWidthChars(60)
  index.entry$setSensitive(FALSE)

  hbox$packStart(index.entry,FALSE,FALSE,0)

  index.but <- gtkButton("Browse")
  index.but$setTooltipText("Please, browse the Curve Numbers index file")
  hbox$packStart(index.but,FALSE,FALSE,0)

  gSignalConnect(index.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the CN index file",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      index.entry$setText(filename)

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Comma-separated values (*.csv)")
                   fileFilter$addPattern("*.csv")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  the.buttons <- gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(20)

  buttonOK <- gtkButtonNewWithMnemonic("_Calculate CN I", show = TRUE)
  buttonOK$setTooltipText("This process takes a while. Please, wait until the program finished")
  gSignalConnect(buttonOK, "clicked",
                 f = function(widget, ...) {

                   if ((landuse.entry$getText())=="") return(invisible(NULL))
                   if ((hsg.entry$getText())=="") return(invisible(NULL))
                   if ((index.entry$getText())=="") return(invisible(NULL))

                   landuse_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                   hsg_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                   index_file <-  normalizePath(path.expand(index.entry$getText()),  winslash = "/", mustWork = NA)

                   land <- raster(landuse_file)
                   soil <- raster(hsg_file)

                   landsoil <- (land + soil)
                   dev.new()
                   plot(landsoil, main= "Landsoil classes", xlab="Longitude", ylab="Latitude")
                   writeRaster(landsoil, filename = "./landsoil", format="GTiff", overwrite=TRUE)

                   index <- read.csv(index_file, header = FALSE, sep = ",")

                   cn <- reclassify(landsoil, index, include.lowest=TRUE, right=FALSE)
                   writeRaster(cn, filename = "./cn_amc_ii", format="GTiff", overwrite=TRUE)

                   cni <- round((cn/(2.2754-(0.012754*cn))))
                   writeRaster(cni, filename = "./cn_mishra_i", format="GTiff", overwrite=TRUE)

                   cn_rat <- ratify(cn)
                   cn_rat
                   cn_rat@data@attributes
                   cn_types <- cn_rat@data@attributes[[1]]
                   cols <- rainbow(nrow(cn_types))
                   cols[1] <- "darkgreen"
                   dev.new()
                   image(cn_rat, col=cols, main="Curve numbers AMC II",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = cn_types$ID, fill = cols, bty="n", cex = 0.8)

                   cni_rat <- ratify(cni)
                   cni_rat
                   cni_rat@data@attributes
                   cni_types <- cni_rat@data@attributes[[1]]
                   cols <- rainbow(nrow(cni_types))
                   cols[1] <- "darkgreen"
                   dev.new()
                   image(cni_rat, col=cols, main="Curve numbers AMC I",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = cni_types$ID, fill = cols, bty="n", cex = 0.8)

                   path <- getwd()
                   dialog <- gtkMessageDialogNew(cni_win, c("modal", "destroy-with-parent"), "info", "close",
                                                 "Landsoil.tif, CN_amc_ii.tif and CN_mishra_i.tif were stored in\n",
                                                 "the working folder ", path, " ")

                   dialog$formatSecondaryText()
                   gSignalConnect(dialog, "response", gtkWidgetDestroy)

                 })
  the.buttons$packStart(buttonOK,fill=F)

  buttonClear <- gtkButtonNewWithMnemonic("Clea_r", show = TRUE)
  buttonClear$setTooltipText("Clic here to clean all data entries")

  gSignalConnect(buttonClear, "clicked",
                 f = function(widget, ...) {

                   landuse.entry$setText("")
                   hsg.entry$setText("")
                   index.entry$setText("")

                 })

  the.buttons$packStart(buttonClear,fill=F)

  buttonCancel <- gtkButtonNewWithMnemonic("Ca_ncel", show = TRUE)
  gSignalConnect(buttonCancel, "clicked", cni_win$destroy)
  the.buttons$packStart(buttonCancel,fill=F)

}



sobhani_iii <- function(action, window)
{
  cniii_win <- gtkWindow()
  cniii_win$setTitle ("sara4r v0.0.8")

  frame <- gtkFrameNew("  Get Curve Numbers for the Antecedent Moisture Condition III  ")
  cniii_win$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  hbox <- gtkHBoxNew(FALSE,8)
  label <- gtkLabelNew()
  label$setMarkup("<b>Sobhani (1975)</b>")
  label$setTooltipText("Sobhani (1975). A review of selected small watershed design methods for possible adoption to Iranian conditions. M.S. Thesis, Utah State University, Logan, UT")
  vbox$packStart(label,FALSE,FALSE,0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>   CNII</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>CNIII =   ---------------------------          </b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>  0.4036 + 0.005964*CNII</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  vbox$packStart(hbox, FALSE, FALSE, 0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  hbox <- gtkHBoxNew(FALSE,8)
  label <- gtkLabelNew()
  label$setMarkup("<b>\n Parameters:</b>")
  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Landuse:   </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  landuse.entry <- gtkEntryNew()
  landuse.entry$setWidthChars(60)
  landuse.entry$setSensitive(FALSE)
  hbox$packStart(landuse.entry,FALSE,FALSE,0)

  land.but <- gtkButton("Browse")
  land.but$setTooltipText("Please, browse the Land use and land cover map")
  hbox$packStart(land.but,FALSE,FALSE,0)

  gSignalConnect(land.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Land use and land cover map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      landuse.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Land use and land covers", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>HSG:          </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  hsg.entry <- gtkEntryNew()
  hsg.entry$setWidthChars(60)
  hsg.entry$setSensitive(FALSE)
  hbox$packStart(hsg.entry,FALSE,FALSE,0)

  hsg.but <- gtkButton("Browse")
  hsg.but$setTooltipText("Please, browse the Hydrologic Soil Groups map")
  hbox$packStart(hsg.but,FALSE,FALSE,0)

  gSignalConnect(hsg.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Hydrologic Soil Group map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      hsg.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Hydrologic Soil Groups", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Index File:</b>")
  hbox$packStart(label,FALSE,FALSE,0)

  index.entry <- gtkEntryNew()
  index.entry$setWidthChars(60)
  index.entry$setSensitive(FALSE)

  hbox$packStart(index.entry,FALSE,FALSE,0)

  index.but <- gtkButton("Browse")
  index.but$setTooltipText("Please, browse the Curve Numbers index file")
  hbox$packStart(index.but,FALSE,FALSE,0)

  gSignalConnect(index.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the CN index file",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      index.entry$setText(filename)

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Comma-separated values (*.csv)")
                   fileFilter$addPattern("*.csv")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  the.buttons <- gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(20)

  buttonOK <- gtkButtonNewWithMnemonic("_Calculate CN III", show = TRUE)
  buttonOK$setTooltipText("This process takes a while. Please, wait until the program finished")
  gSignalConnect(buttonOK, "clicked",
                 f = function(widget, ...) {

                   if ((landuse.entry$getText())=="") return(invisible(NULL))
                   if ((hsg.entry$getText())=="") return(invisible(NULL))
                   if ((index.entry$getText())=="") return(invisible(NULL))

                   landuse_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                   hsg_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                   index_file <-  normalizePath(path.expand(index.entry$getText()),  winslash = "/", mustWork = NA)

                   land <- raster(landuse_file)
                   soil <- raster(hsg_file)

                   landsoil <- (land + soil)
                   dev.new()
                   plot(landsoil, main= "Landsoil classes", xlab="Longitude", ylab="Latitude")
                   writeRaster(landsoil, filename = "./landsoil", format="GTiff", overwrite=TRUE)

                   index <- read.csv(index_file, header = FALSE, sep = ",")

                   cn <- reclassify(landsoil, index, include.lowest=TRUE, right=FALSE)
                   writeRaster(cn, filename = "./cn_amc_ii", format="GTiff", overwrite=TRUE)

                   cniii <- round((cn/(0.4036+(0.005964*cn))))
                   writeRaster(cniii, filename = "./cn_sobhani_iii", format="GTiff", overwrite=TRUE)

                   cn_rat <- ratify(cn)
                   cn_rat
                   cn_rat@data@attributes
                   cn_types <- cn_rat@data@attributes[[1]]
                   cols <- rainbow(nrow(cn_types))
                   cols[1] <- "darkgreen"
                   dev.new()
                   image(cn_rat, col=cols, main="Curve numbers AMC II",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = cn_types$ID, fill = cols, bty="n", cex = 0.8)

                   cniii_rat <- ratify(cniii)
                   cniii_rat
                   cniii_rat@data@attributes
                   cniii_types <- cniii_rat@data@attributes[[1]]
                   cols <- rainbow(nrow(cniii_types))
                   cols[1] <- "darkgreen"
                   dev.new()
                   image(cniii_rat, col=cols, main="Curve numbers AMC III",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = cniii_types$ID, fill = cols, bty="n", cex = 0.8)

                   path <- getwd()
                   dialog <- gtkMessageDialogNew(cniii_win, c("modal", "destroy-with-parent"), "info", "close",
                                                 "Landsoil.tif, CN_amc_ii.tif and CN_sobhani_iii.tif were stored in\n",
                                                 "the working folder ", path, " ")

                   dialog$formatSecondaryText()
                   gSignalConnect(dialog, "response", gtkWidgetDestroy)

                 })
  the.buttons$packStart(buttonOK,fill=F)

  buttonClear <- gtkButtonNewWithMnemonic("Clea_r", show = TRUE)
  buttonClear$setTooltipText("Clic here to clean all data entries")

  gSignalConnect(buttonClear, "clicked",
                 f = function(widget, ...) {

                   landuse.entry$setText("")
                   hsg.entry$setText("")
                   index.entry$setText("")

                 })

  the.buttons$packStart(buttonClear,fill=F)

  buttonCancel <- gtkButtonNewWithMnemonic("Ca_ncel", show = TRUE)
  gSignalConnect(buttonCancel, "clicked", cniii_win$destroy)
  the.buttons$packStart(buttonCancel,fill=F)

}



hawkins_iii <- function(action, window)
{
  cniii_win <- gtkWindow()
  cniii_win$setTitle ("sara4r v0.0.8")

  frame <- gtkFrameNew("  Get Curve Numbers for the Antecedent Moisture Condition III  ")
  cniii_win$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  hbox <- gtkHBoxNew(FALSE,8)
  label <- gtkLabelNew()
  label$setMarkup("<b>Hawkins et al. (1985)</b>")
  label$setTooltipText("Hawkins et al. (1985). Runoff probability, storm depth and curve numbers. Journal of Irrigation and Drainage Engineering, 111(4): 330-340")
  vbox$packStart(label,FALSE,FALSE,0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>   CNII</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>CNIII =   ---------------------------          </b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>  0.427 + 0.00573*CNII</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  vbox$packStart(hbox, FALSE, FALSE, 0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  hbox <- gtkHBoxNew(FALSE,8)
  label <- gtkLabelNew()
  label$setMarkup("<b>\n Parameters:</b>")
  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Landuse:   </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  landuse.entry <- gtkEntryNew()
  landuse.entry$setWidthChars(60)
  landuse.entry$setSensitive(FALSE)
  hbox$packStart(landuse.entry,FALSE,FALSE,0)

  land.but <- gtkButton("Browse")
  land.but$setTooltipText("Please, browse the Land use and land cover map")
  hbox$packStart(land.but,FALSE,FALSE,0)

  gSignalConnect(land.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Land use and land cover map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      landuse.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Land use and land covers", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>HSG:          </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  hsg.entry <- gtkEntryNew()
  hsg.entry$setWidthChars(60)
  hsg.entry$setSensitive(FALSE)
  hbox$packStart(hsg.entry,FALSE,FALSE,0)

  hsg.but <- gtkButton("Browse")
  hsg.but$setTooltipText("Please, browse the Hydrologic Soil Groups map")
  hbox$packStart(hsg.but,FALSE,FALSE,0)

  gSignalConnect(hsg.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Hydrologic Soil Group map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      hsg.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Hydrologic Soil Groups", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Index File:</b>")
  hbox$packStart(label,FALSE,FALSE,0)

  index.entry <- gtkEntryNew()
  index.entry$setWidthChars(60)
  index.entry$setSensitive(FALSE)

  hbox$packStart(index.entry,FALSE,FALSE,0)

  index.but <- gtkButton("Browse")
  index.but$setTooltipText("Please, browse the Curve Numbers index file")
  hbox$packStart(index.but,FALSE,FALSE,0)

  gSignalConnect(index.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the CN index file",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      index.entry$setText(filename)

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Comma-separated values (*.csv)")
                   fileFilter$addPattern("*.csv")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  the.buttons <- gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(20)

  buttonOK <- gtkButtonNewWithMnemonic("_Calculate CN III", show = TRUE)
  buttonOK$setTooltipText("This process takes a while. Please, wait until the program finished")
  gSignalConnect(buttonOK, "clicked",
                 f = function(widget, ...) {

                   if ((landuse.entry$getText())=="") return(invisible(NULL))
                   if ((hsg.entry$getText())=="") return(invisible(NULL))
                   if ((index.entry$getText())=="") return(invisible(NULL))

                   landuse_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                   hsg_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                   index_file <-  normalizePath(path.expand(index.entry$getText()),  winslash = "/", mustWork = NA)

                   land <- raster(landuse_file)
                   soil <- raster(hsg_file)

                   landsoil <- (land + soil)
                   dev.new()
                   plot(landsoil, main= "Landsoil classes", xlab="Longitude", ylab="Latitude")
                   writeRaster(landsoil, filename = "./landsoil", format="GTiff", overwrite=TRUE)

                   index <- read.csv(index_file, header = FALSE, sep = ",")

                   cn <- reclassify(landsoil, index, include.lowest=TRUE, right=FALSE)
                   writeRaster(cn, filename = "./cn_amc_ii", format="GTiff", overwrite=TRUE)

                   cniii <- round((cn/(0.427+(0.00573*cn))))
                   writeRaster(cniii, filename = "./cn_hawkins_iii", format="GTiff", overwrite=TRUE)

                   cn_rat <- ratify(cn)
                   cn_rat
                   cn_rat@data@attributes
                   cn_types <- cn_rat@data@attributes[[1]]
                   cols <- rainbow(nrow(cn_types))
                   cols[1] <- "darkgreen"
                   dev.new()
                   image(cn_rat, col=cols, main="Curve numbers AMC II",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = cn_types$ID, fill = cols, bty="n", cex = 0.8)

                   cniii_rat <- ratify(cniii)
                   cniii_rat
                   cniii_rat@data@attributes
                   cniii_types <- cniii_rat@data@attributes[[1]]
                   cols <- rainbow(nrow(cniii_types))
                   cols[1] <- "darkgreen"
                   dev.new()
                   image(cniii_rat, col=cols, main="Curve numbers AMC III",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = cniii_types$ID, fill = cols, bty="n", cex = 0.8)

                   path <- getwd()
                   dialog <- gtkMessageDialogNew(cniii_win, c("modal", "destroy-with-parent"), "info", "close",
                                                 "Landsoil.tif, CN_amc_ii.tif and CN_hawkins_iii.tif were stored in\n",
                                                 "the working folder ", path, " ")

                   dialog$formatSecondaryText()
                   gSignalConnect(dialog, "response", gtkWidgetDestroy)

                 })
  the.buttons$packStart(buttonOK,fill=F)

  buttonClear <- gtkButtonNewWithMnemonic("Clea_r", show = TRUE)
  buttonClear$setTooltipText("Clic here to clean all data entries")

  gSignalConnect(buttonClear, "clicked",
                 f = function(widget, ...) {

                   landuse.entry$setText("")
                   hsg.entry$setText("")
                   index.entry$setText("")

                 })

  the.buttons$packStart(buttonClear,fill=F)

  buttonCancel <- gtkButtonNewWithMnemonic("Ca_ncel", show = TRUE)
  gSignalConnect(buttonCancel, "clicked", cniii_win$destroy)
  the.buttons$packStart(buttonCancel,fill=F)

}



chow_iii <- function(action, window)
{
  cniii_win <- gtkWindow()
  cniii_win$setTitle ("sara4r v0.0.8")

  frame <- gtkFrameNew("  Get Curve Numbers for the Antecedent Moisture Condition III  ")
  cniii_win$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  hbox <- gtkHBoxNew(FALSE,8)
  label <- gtkLabelNew()
  label$setMarkup("<b>Chow et al. (1988)</b>")
  label$setTooltipText("Chow et al. (1988). Applied hydrology. McGraw-Hill, New York")
  vbox$packStart(label,FALSE,FALSE,0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>   23*CNII</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>CNIII =   ---------------------------          </b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>  10 + 0.13*CNII</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  vbox$packStart(hbox, FALSE, FALSE, 0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  hbox <- gtkHBoxNew(FALSE,8)
  label <- gtkLabelNew()
  label$setMarkup("<b>\n Parameters:</b>")
  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Landuse:   </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  landuse.entry <- gtkEntryNew()
  landuse.entry$setWidthChars(60)
  landuse.entry$setSensitive(FALSE)
  hbox$packStart(landuse.entry,FALSE,FALSE,0)

  land.but <- gtkButton("Browse")
  land.but$setTooltipText("Please, browse the Land use and land cover map")
  hbox$packStart(land.but,FALSE,FALSE,0)

  gSignalConnect(land.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Land use and land cover map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      landuse.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Land use and land covers", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>HSG:          </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  hsg.entry <- gtkEntryNew()
  hsg.entry$setWidthChars(60)
  hsg.entry$setSensitive(FALSE)
  hbox$packStart(hsg.entry,FALSE,FALSE,0)

  hsg.but <- gtkButton("Browse")
  hsg.but$setTooltipText("Please, browse the Hydrologic Soil Groups map")
  hbox$packStart(hsg.but,FALSE,FALSE,0)

  gSignalConnect(hsg.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Hydrologic Soil Group map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      hsg.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Hydrologic Soil Groups", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Index File:</b>")
  hbox$packStart(label,FALSE,FALSE,0)

  index.entry <- gtkEntryNew()
  index.entry$setWidthChars(60)
  index.entry$setSensitive(FALSE)

  hbox$packStart(index.entry,FALSE,FALSE,0)

  index.but <- gtkButton("Browse")
  index.but$setTooltipText("Please, browse the Curve Numbers index file")
  hbox$packStart(index.but,FALSE,FALSE,0)

  gSignalConnect(index.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the CN index file",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      index.entry$setText(filename)

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Comma-separated values (*.csv)")
                   fileFilter$addPattern("*.csv")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  the.buttons <- gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(20)

  buttonOK <- gtkButtonNewWithMnemonic("_Calculate CN III", show = TRUE)
  buttonOK$setTooltipText("This process takes a while. Please, wait until the program finished")
  gSignalConnect(buttonOK, "clicked",
                 f = function(widget, ...) {

                   if ((landuse.entry$getText())=="") return(invisible(NULL))
                   if ((hsg.entry$getText())=="") return(invisible(NULL))
                   if ((index.entry$getText())=="") return(invisible(NULL))

                   landuse_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                   hsg_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                   index_file <-  normalizePath(path.expand(index.entry$getText()),  winslash = "/", mustWork = NA)

                   land <- raster(landuse_file)
                   soil <- raster(hsg_file)

                   landsoil <- (land + soil)
                   dev.new()
                   plot(landsoil, main= "Landsoil classes", xlab="Longitude", ylab="Latitude")
                   writeRaster(landsoil, filename = "./landsoil", format="GTiff", overwrite=TRUE)

                   index <- read.csv(index_file, header = FALSE, sep = ",")

                   cn <- reclassify(landsoil, index, include.lowest=TRUE, right=FALSE)
                   writeRaster(cn, filename = "./cn_amc_ii", format="GTiff", overwrite=TRUE)

                   cniii <- round((23*cn/(10+(0.13*cn))))
                   writeRaster(cniii, filename = "./cn_chow_iii", format="GTiff", overwrite=TRUE)

                   cn_rat <- ratify(cn)
                   cn_rat
                   cn_rat@data@attributes
                   cn_types <- cn_rat@data@attributes[[1]]
                   cols <- rainbow(nrow(cn_types))
                   cols[1] <- "darkgreen"
                   dev.new()
                   image(cn_rat, col=cols, main="Curve numbers AMC II",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = cn_types$ID, fill = cols, bty="n", cex = 0.8)

                   cniii_rat <- ratify(cniii)
                   cniii_rat
                   cniii_rat@data@attributes
                   cniii_types <- cniii_rat@data@attributes[[1]]
                   cols <- rainbow(nrow(cniii_types))
                   cols[1] <- "darkgreen"
                   dev.new()
                   image(cniii_rat, col=cols, main="Curve numbers AMC III",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = cniii_types$ID, fill = cols, bty="n", cex = 0.8)

                   path <- getwd()
                   dialog <- gtkMessageDialogNew(cniii_win, c("modal", "destroy-with-parent"), "info", "close",
                                                 "Landsoil.tif, CN_amc_ii.tif and CN_chow_iii.tif were stored in\n",
                                                 "the working folder ", path, " ")

                   dialog$formatSecondaryText()
                   gSignalConnect(dialog, "response", gtkWidgetDestroy)

                 })
  the.buttons$packStart(buttonOK,fill=F)

  buttonClear <- gtkButtonNewWithMnemonic("Clea_r", show = TRUE)
  buttonClear$setTooltipText("Clic here to clean all data entries")

  gSignalConnect(buttonClear, "clicked",
                 f = function(widget, ...) {

                   landuse.entry$setText("")
                   hsg.entry$setText("")
                   index.entry$setText("")

                 })

  the.buttons$packStart(buttonClear,fill=F)

  buttonCancel <- gtkButtonNewWithMnemonic("Ca_ncel", show = TRUE)
  gSignalConnect(buttonCancel, "clicked", cniii_win$destroy)
  the.buttons$packStart(buttonCancel,fill=F)

}


mishra_iii <- function(action, window)
{
  cniii_win <- gtkWindow()
  cniii_win$setTitle ("sara4r v0.0.8")

  frame <- gtkFrameNew("  Get Curve Numbers for the Antecedent Moisture Condition III  ")
  cniii_win$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  hbox <- gtkHBoxNew(FALSE,8)
  label <- gtkLabelNew()
  label$setMarkup("<b>Mishra et al. (2008)</b>")
  label$setTooltipText("Mishra et al. (2008). Comparison of AMC-dependent CN-conversion formulae. Water Resources Management, 22(10), 1409-1420. doi:10.1007/s11269-007-9233-5")
  vbox$packStart(label,FALSE,FALSE,0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>   CNII</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>CNIII =   -----------------------        </b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>       0.430 + 0.0057CNII</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  vbox$packStart(hbox, FALSE, FALSE, 0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  hbox <- gtkHBoxNew(FALSE,8)
  label <- gtkLabelNew()
  label$setMarkup("<b>\n Parameters:</b>")
  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Landuse:   </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  landuse.entry <- gtkEntryNew()
  landuse.entry$setWidthChars(60)
  landuse.entry$setSensitive(FALSE)

  hbox$packStart(landuse.entry,FALSE,FALSE,0)

  land.but <- gtkButton("Browse")
  land.but$setTooltipText("Please, browse the Land use and land cover map")
  hbox$packStart(land.but,FALSE,FALSE,0)

  gSignalConnect(land.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Land use and land cover map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      landuse.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Land use and land covers", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>HSG:          </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  hsg.entry <- gtkEntryNew()
  hsg.entry$setWidthChars(60)
  hsg.entry$setSensitive(FALSE)
  hbox$packStart(hsg.entry,FALSE,FALSE,0)

  hsg.but <- gtkButton("Browse")
  hsg.but$setTooltipText("Please, browse the Hydrologic Soil Groups map")
  hbox$packStart(hsg.but,FALSE,FALSE,0)

  gSignalConnect(hsg.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Hydrologic Soil Group map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      hsg.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Hydrologic Soil Groups", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Index File:</b>")
  hbox$packStart(label,FALSE,FALSE,0)

  index.entry <- gtkEntryNew()
  index.entry$setWidthChars(60)
  index.entry$setSensitive(FALSE)
  hbox$packStart(index.entry,FALSE,FALSE,0)

  index.but <- gtkButton("Browse")
  index.but$setTooltipText("Please, browse the Curve Numbers index file")
  hbox$packStart(index.but,FALSE,FALSE,0)

  gSignalConnect(index.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the CN index file",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      index.entry$setText(filename)

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Comma-separated values (*.csv)")
                   fileFilter$addPattern("*.csv")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  the.buttons <- gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(20)

  buttonOK <- gtkButtonNewWithMnemonic("_Calculate CN III", show = TRUE)
  buttonOK$setTooltipText("This process takes a while. Please, wait until the program finished")
  gSignalConnect(buttonOK, "clicked",
                 f = function(widget, ...) {

                   if ((landuse.entry$getText())=="") return(invisible(NULL))
                   if ((hsg.entry$getText())=="") return(invisible(NULL))
                   if ((index.entry$getText())=="") return(invisible(NULL))

                   landuse_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                   hsg_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                   index_file <-  normalizePath(path.expand(index.entry$getText()),  winslash = "/", mustWork = NA)

                   land <- raster(landuse_file)
                   soil <- raster(hsg_file)

                   landsoil <- (land + soil)
                   dev.new()
                   plot(landsoil, main= "Landsoil classes", xlab="Longitude", ylab="Latitude")
                   writeRaster(landsoil, filename = "./landsoil", format="GTiff", overwrite=TRUE)

                   index <- read.csv(index_file, header = FALSE, sep = ",")

                   cn <- reclassify(landsoil, index, include.lowest=TRUE, right=FALSE)
                   writeRaster(cn, filename = "./cn_amc_ii", format="GTiff", overwrite=TRUE)

                   cniii <- round((cn/(0.43+(0.0057*cn))))
                   writeRaster(cniii, filename = "./cn_mishra_iii", format="GTiff", overwrite=TRUE)

                   cn_rat <- ratify(cn)
                   cn_rat
                   cn_rat@data@attributes
                   cn_types <- cn_rat@data@attributes[[1]]
                   cols <- rainbow(nrow(cn_types))
                   cols[1] <- "darkgreen"
                   dev.new()
                   image(cn_rat, col=cols, main="Curve numbers AMC II",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = cn_types$ID, fill = cols, bty="n", cex = 0.8)

                   cniii_rat <- ratify(cniii)
                   cniii_rat
                   cniii_rat@data@attributes
                   cniii_types <- cniii_rat@data@attributes[[1]]
                   cols <- rainbow(nrow(cniii_types))
                   cols[1] <- "darkgreen"
                   dev.new()
                   image(cniii_rat, col=cols, main="Curve numbers AMC III",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = cniii_types$ID, fill = cols, bty="n", cex = 0.8)


                   path <- getwd()
                   dialog <- gtkMessageDialogNew(cniii_win, c("modal", "destroy-with-parent"), "info", "close",
                                                 "Landsoil.tif, CN_amc_ii.tif and CN_mishra_iii.tif were stored in\n",
                                                 "the working folder ", path, " ")

                   dialog$formatSecondaryText()
                   gSignalConnect(dialog, "response", gtkWidgetDestroy)


                 })
  the.buttons$packStart(buttonOK,fill=F)

  buttonClear <- gtkButtonNewWithMnemonic("Clea_r", show = TRUE)
  buttonClear$setTooltipText("Clic here to clean all data entries")
  gSignalConnect(buttonClear, "clicked",
                 f = function(widget, ...) {

                   landuse.entry$setText("")
                   hsg.entry$setText("")
                   index.entry$setText("")

                 })

  the.buttons$packStart(buttonClear,fill=F)

  buttonCancel <- gtkButtonNewWithMnemonic("Ca_ncel", show = TRUE)
  gSignalConnect(buttonCancel, "clicked", cniii_win$destroy)
  the.buttons$packStart(buttonCancel,fill=F)

}


qvol_ii <- function(action, window)
{
  qvolii_win <- gtkWindow()
  qvolii_win$setTitle ("sara4r v0.0.8")


  frame <- gtkFrameNew("  Get Q-depth and Runoff Volume for the AMC II  ")
  qvolii_win$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  hbox <- gtkHBoxNew(FALSE,8)

  label <- gtkLabelNew()
  label$setMarkup("<b>Parameters:</b>")

  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Curve Numbers AMC II:</b>")
  hbox$packStart(label,FALSE,FALSE,0)

  cn.entry <- gtkEntryNew()
  cn.entry$setWidthChars(60)
  cn.entry$setSensitive(FALSE)
  hbox$packStart(cn.entry,FALSE,FALSE,0)

  cn.but <- gtkButton("Browse")
  cn.but$setTooltipText("Please, browse the Curve Number map according the AMC selected")
  hbox$packStart(cn.but,FALSE,FALSE,0)

  gSignalConnect(cn.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Curve Number map for AMC II",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      cn.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(cn.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Curve Numbers AMC II", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Landsoil map:                   </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  landsoil.entry <- gtkEntryNew()
  landsoil.entry$setWidthChars(60)
  landsoil.entry$setSensitive(FALSE)

  hbox$packStart(landsoil.entry,FALSE,FALSE,0)

  landsoil.but <- gtkButton("Browse")
  landsoil.but$setTooltipText("Please, browse the Landsoil map created in the previous step")
  hbox$packStart(landsoil.but,FALSE,FALSE,0)

  gSignalConnect(landsoil.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Landsoil map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      landsoil.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(landsoil.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Landsoil map", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Precipitation (in):           </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  rainfall.entry <- gtkEntryNew()
  rainfall.entry$setWidthChars(10)
  rainfall.entry$setText("2.5")
  rainfall.entry$setSensitive(TRUE)

  hbox$packStart(rainfall.entry,FALSE,FALSE,0)

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Pixel size (meters):        </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  area.entry <- gtkEntryNew()
  area.entry$setWidthChars(10)
  area.entry$setText("30")
  area.entry$setSensitive(TRUE)
  hbox$packStart(area.entry,FALSE,FALSE,0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  the.buttons <- gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(20)

  buttonOK <- gtkButtonNewWithMnemonic("_Calculate Q depth and Vol", show = TRUE)
  buttonOK$setTooltipText("This process takes a while. Please, wait until the program finished")
  gSignalConnect(buttonOK, "clicked",
                 f = function(widget, ...) {

                   if ((cn.entry$getText())=="") return(invisible(NULL))
                   if ((landsoil.entry$getText())=="") return(invisible(NULL))
                   if ((rainfall.entry$getText())=="") return(invisible(NULL))
                   if ((area.entry$getText())=="") return(invisible(NULL))

                   cn_file <- normalizePath(path.expand(cn.entry$getText()),  winslash = "/", mustWork = NA)
                   landsoil_file <- normalizePath(path.expand(landsoil.entry$getText()),  winslash = "/", mustWork = NA)

                   cn <- raster(cn_file)
                   landsoil <- raster(landsoil_file)
                   P <- as.numeric(rainfall.entry$getText())

                   S <- ((1000/cn) - 10)
                   Ia <- (0.2*S)

                   msc <- reclassify(Ia, c(-Inf, P, 1, P, Inf, 0), include.lowest=FALSE, right=TRUE)

                   Q <- ((P-(0.2*S))^2)/(P+(0.8*S))

                   q_depth <- (Q * msc)

                   dev.new()
                   plot(q_depth, main="Runoff depth (inch)",  xlab="Longitude", ylab="Latitude")

                   resolution <- as.numeric(area.entry$getText())
                   area <- (resolution * resolution)

                   runoff <- round((q_depth * 0.0254 * area), 2)

                   runoff_rat <- ratify(runoff)
                   runoff_rat
                   runoff_rat@data@attributes
                   q_types <- runoff_rat@data@attributes [[1]]
                   cols <- rainbow(nrow(q_types))
                   cols[1] <- "yellow"
                   dev.new()
                   image(runoff_rat, col=cols, main="Runoff volumen in cubic meters",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = q_types$ID, fill = cols, bty="n", cex = 0.8)

                   writeRaster(q_depth, filename = "./Q_depth_inch_amcii", format="GTiff", overwrite=TRUE)
                   writeRaster(runoff, filename = "./Runoff_m3_amcii", format="GTiff", overwrite=TRUE)

                   path <- getwd()
                   datetime <- format(Sys.time(),'_%Y%m%d_%H%M%S')
                   vol <- cellStats(runoff, 'sum', na.rm=TRUE, asSample=TRUE)

                   write.table(paste("Total runoff volume for the AMC II = ", vol, "m^3 \n\n The parameters used were::\n Precipitation = ", P, "inches \n Curve number map: ", cn_file, "\n Landsoil file: ", landsoil_file, "\n Pixel size = ", resolution, "meters\n\n sara4r"),
                               file = paste(path, "/Runoff_Vol_AMCII", datetime, '.txt', sep = ""),
                               row.names = FALSE, col.names = FALSE, quote = FALSE)

                   dialog <- gtkMessageDialogNew(qvolii_win, c("modal", "destroy-with-parent"), "info", "close",
                                                 "Q_depth_inch and Runoff_m3 were stored in\n",
                                                 "the working folder ", path, " \n\n",
                                                 "Total runoff volume for the AMC II is\n",
                                                 " = ", vol, "m^3")


                   dialog$formatSecondaryText()
                   gSignalConnect(dialog, "response", gtkWidgetDestroy)


                 })

  the.buttons$packStart(buttonOK,fill=F)

  buttonClear <- gtkButtonNewWithMnemonic("Clea_r", show = TRUE)
  buttonClear$setTooltipText("Clic here to clean all data entries")

  gSignalConnect(buttonClear, "clicked",
                 f = function(widget, ...) {

                   cn.entry$setText("")
                   landsoil.entry$setText("")

                 })

  the.buttons$packStart(buttonClear,fill=F)

  buttonCancel <- gtkButtonNewWithMnemonic("Ca_ncel", show = TRUE)
  gSignalConnect(buttonCancel, "clicked", qvolii_win$destroy)

  the.buttons$packStart(buttonCancel,fill=F)

}


qvol_i <- function(action, window)
{
  qvoli_win <- gtkWindow()
  qvoli_win$setTitle ("sara4r v0.0.8")

  frame <- gtkFrameNew("  Get Q-depth and Runoff Volume for the AMC I  ")
  qvoli_win$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  hbox <- gtkHBoxNew(FALSE,8)

  label <- gtkLabelNew()
  label$setMarkup("<b>Parameters:</b>")
  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Curve Numbers AMC I: </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  cn.entry <- gtkEntryNew()
  cn.entry$setWidthChars(60)
  cn.entry$setSensitive(FALSE)
  hbox$packStart(cn.entry,FALSE,FALSE,0)

  cn.but <- gtkButton("Browse")
  cn.but$setTooltipText("Please, browse the Curve Number map according the AMC selected")
  hbox$packStart(cn.but,FALSE,FALSE,0)

  gSignalConnect(cn.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Curve Number map for AMC I",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      cn.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(cn.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Curve Numbers AMC I", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Landsoil map:                  </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  landsoil.entry <- gtkEntryNew()
  landsoil.entry$setWidthChars(60)
  landsoil.entry$setSensitive(FALSE)

  hbox$packStart(landsoil.entry,FALSE,FALSE,0)

  landsoil.but <- gtkButton("Browse")
  landsoil.but$setTooltipText("Please, browse the Landsoil map created in the previous step")
  hbox$packStart(landsoil.but,FALSE,FALSE,0)

  gSignalConnect(landsoil.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Landsoil map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      landsoil.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(landsoil.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Landsoil map", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Precipitation (in):           </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  rainfall.entry <- gtkEntryNew()
  rainfall.entry$setWidthChars(10)
  rainfall.entry$setText("2.5")
  rainfall.entry$setSensitive(TRUE)

  hbox$packStart(rainfall.entry,FALSE,FALSE,0)

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Pixel size (meters):        </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  area.entry <- gtkEntryNew()
  area.entry$setWidthChars(10)
  area.entry$setText("30")
  area.entry$setSensitive(TRUE)
  hbox$packStart(area.entry,FALSE,FALSE,0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  the.buttons <- gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(20)

  buttonOK <- gtkButtonNewWithMnemonic("_Calculate Q depth and Vol", show = TRUE)
  buttonOK$setTooltipText("This process takes a while. Please, wait until the program finished")
  gSignalConnect(buttonOK, "clicked",
                 f = function(widget, ...) {

                   if ((cn.entry$getText())=="") return(invisible(NULL))
                   if ((landsoil.entry$getText())=="") return(invisible(NULL))
                   if ((rainfall.entry$getText())=="") return(invisible(NULL))
                   if ((area.entry$getText())=="") return(invisible(NULL))

                   cn_file <- normalizePath(path.expand(cn.entry$getText()),  winslash = "/", mustWork = NA)
                   landsoil_file <- normalizePath(path.expand(landsoil.entry$getText()),  winslash = "/", mustWork = NA)

                   cn <- raster(cn_file)
                   landsoil <- raster(landsoil_file)
                   P <- as.numeric(rainfall.entry$getText())

                   S <- ((1000/cn) - 10)
                   Ia <- (0.2*S)

                   msc <- reclassify(Ia, c(-Inf, P, 1, P, Inf, 0), include.lowest=FALSE, right=TRUE)

                   Q <- ((P-(0.2*S))^2)/(P+(0.8*S))

                   q_depth <- (Q * msc)

                   dev.new()
                   plot(q_depth, main="Runoff depth (inch)",  xlab="Longitude", ylab="Latitude")

                   resolution <- as.numeric(area.entry$getText())
                   area <- (resolution * resolution)

                   runoff <- round((q_depth * 0.0254 * area), 2)

                   runoff_rat <- ratify(runoff)
                   runoff_rat
                   runoff_rat@data@attributes
                   q_types <- runoff_rat@data@attributes [[1]]
                   cols <- rainbow(nrow(q_types))
                   cols[1] <- "yellow"
                   dev.new()
                   image(runoff_rat, col=cols, main="Runoff volumen in cubic meters",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = q_types$ID, fill = cols, bty="n", cex = 0.8)

                   writeRaster(q_depth, filename = "./Q_depth_inch_amci", format="GTiff", overwrite=TRUE)
                   writeRaster(runoff, filename = "./Runoff_m3_amci", format="GTiff", overwrite=TRUE)

                   path <- getwd()
                   datetime <- format(Sys.time(),'_%Y%m%d_%H%M%S')
                   vol <- cellStats(runoff, 'sum', na.rm=TRUE, asSample=TRUE)

                   write.table(paste("Total runoff volume for the AMC I = ", vol, "m^3 \n\n The parameters used were:\n Precipitation = ", P, "inches \n Curve number map: ", cn_file, "\n Landsoil file: ", landsoil_file, "\n Pixel size: ", resolution, "meters\n\n sara4r"),
                               file = paste(path, "/Runoff_Vol_AMCI", datetime, '.txt', sep = ""),
                               row.names = FALSE, col.names = FALSE, quote = FALSE)

                   dialog <- gtkMessageDialogNew(qvoli_win, c("modal", "destroy-with-parent"), "info", "close",
                                                 "Q_depth_inch and Runoff_m3 were stored in\n",
                                                 "the working folder ", path, " \n\n",
                                                 "Total runoff volume for the AMC I is\n",
                                                 " = ", vol, "m^3")

                   dialog$formatSecondaryText()
                   gSignalConnect(dialog, "response", gtkWidgetDestroy)


                 })

  the.buttons$packStart(buttonOK,fill=F)

  buttonClear <- gtkButtonNewWithMnemonic("Clea_r", show = TRUE)
  buttonClear$setTooltipText("Clic here to clean all data entries")

  gSignalConnect(buttonClear, "clicked",
                 f = function(widget, ...) {

                   cn.entry$setText("")
                   landsoil.entry$setText("")

                 })

  the.buttons$packStart(buttonClear,fill=F)

  buttonCancel <- gtkButtonNewWithMnemonic("Ca_ncel", show = TRUE)
  gSignalConnect(buttonCancel, "clicked", qvoli_win$destroy)

  the.buttons$packStart(buttonCancel,fill=F)

}


qvol_iii <- function(action, window)
{
  qvoliii_win <- gtkWindow()
  qvoliii_win$setTitle ("sara4r v0.0.8")

  frame <- gtkFrameNew("  Get Q-depth and Runoff Volume for the AMC III  ")
  qvoliii_win$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  hbox <- gtkHBoxNew(FALSE,8)

  label <- gtkLabelNew()
  label$setMarkup("<b>Parameters:</b>")

  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Curve Numbers AMC III:</b>")
  hbox$packStart(label,FALSE,FALSE,0)

  cn.entry <- gtkEntryNew()
  cn.entry$setWidthChars(60)
  cn.entry$setSensitive(FALSE)

  hbox$packStart(cn.entry,FALSE,FALSE,0)

  cn.but <- gtkButton("Browse")
  cn.but$setTooltipText("Please, browse the Curve Number map according the AMC selected")
  hbox$packStart(cn.but,FALSE,FALSE,0)

  gSignalConnect(cn.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Curve Number map for AMC III",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      cn.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(cn.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Curve Numbers AMC III", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Landsoil map:                    </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  landsoil.entry <- gtkEntryNew()
  landsoil.entry$setWidthChars(60)
  landsoil.entry$setSensitive(FALSE)

  hbox$packStart(landsoil.entry,FALSE,FALSE,0)

  landsoil.but <- gtkButton("Browse")
  landsoil.but$setTooltipText("Please, browse the Landsoil map created in the previous step")
  hbox$packStart(landsoil.but,FALSE,FALSE,0)

  gSignalConnect(landsoil.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Landsoil map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      landsoil.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(landsoil.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Landsoil map", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Precipitation (in):             </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  rainfall.entry <- gtkEntryNew()
  rainfall.entry$setWidthChars(10)
  rainfall.entry$setText("2.5")
  rainfall.entry$setSensitive(TRUE)

  hbox$packStart(rainfall.entry,FALSE,FALSE,0)

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Pixel size (meters):         </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  area.entry <- gtkEntryNew()
  area.entry$setWidthChars(10)
  area.entry$setText("30")
  area.entry$setSensitive(TRUE)

  hbox$packStart(area.entry,FALSE,FALSE,0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  the.buttons <- gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(20)

  buttonOK <- gtkButtonNewWithMnemonic("_Calculate Q depth and Vol", show = TRUE)
  buttonOK$setTooltipText("This process takes a while. Please, wait until the program finished")

  gSignalConnect(buttonOK, "clicked",
                 f = function(widget, ...) {

                   if ((cn.entry$getText())=="") return(invisible(NULL))
                   if ((landsoil.entry$getText())=="") return(invisible(NULL))
                   if ((rainfall.entry$getText())=="") return(invisible(NULL))
                   if ((area.entry$getText())=="") return(invisible(NULL))

                   cn_file <- normalizePath(path.expand(cn.entry$getText()),  winslash = "/", mustWork = NA)
                   landsoil_file <- normalizePath(path.expand(landsoil.entry$getText()),  winslash = "/", mustWork = NA)

                   cn <- raster(cn_file)
                   landsoil <- raster(landsoil_file)
                   P <- as.numeric(rainfall.entry$getText())

                   S <- ((1000/cn) - 10)
                   Ia <- (0.2*S)

                   msc <- reclassify(Ia, c(-Inf, P, 1, P, Inf, 0), include.lowest=FALSE, right=TRUE)

                   Q <- ((P-(0.2*S))^2)/(P+(0.8*S))

                   q_depth <- (Q * msc)

                   dev.new()
                   plot(q_depth, main="Runoff depth (inch)",  xlab="Longitude", ylab="Latitude")

                   resolution <- as.numeric(area.entry$getText())
                   area <- (resolution * resolution)

                   runoff <- round((q_depth * 0.0254 * area), 2)

                   runoff_rat <- ratify(runoff)
                   runoff_rat
                   runoff_rat@data@attributes
                   q_types <- runoff_rat@data@attributes [[1]]
                   cols <- rainbow(nrow(q_types))
                   cols[1] <- "yellow"
                   dev.new()
                   image(runoff_rat, col=cols, main="Runoff volumen in cubic meters",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = q_types$ID, fill = cols, bty="n", cex = 0.8)

                   writeRaster(q_depth, filename = "./Q_depth_inch_amciii", format="GTiff", overwrite=TRUE)
                   writeRaster(runoff, filename = "./Runoff_m3_amciii", format="GTiff", overwrite=TRUE)

                   path <- getwd()
                   datetime <- format(Sys.time(),'_%Y%m%d_%H%M%S')
                   vol <- cellStats(runoff, 'sum', na.rm=TRUE, asSample=TRUE)

                   write.table(paste("Total runoff volume for the AMC III = ", vol, "m^3 \n\n The parameters used were:\n Precipitation = ", P, "inches \n Curve number map: ", cn_file, "\n Landsoil file: ", landsoil_file, "\n Pixel size: ", resolution, "meters\n\n sara4r"),
                               file = paste(path, "/Runoff_Vol_AMCIII", datetime, '.txt', sep = ""),
                               row.names = FALSE, col.names = FALSE, quote = FALSE)

                   dialog <- gtkMessageDialogNew(qvoliii_win, c("modal", "destroy-with-parent"), "info", "close",
                                                 "Q_depth_inch and Runoff_m3 were stored in\n",
                                                 "the working folder ", path, " \n\n",
                                                 "Total runoff volume for the AMC III is\n",
                                                 " = ", vol, "m^3")


                   dialog$formatSecondaryText()
                   gSignalConnect(dialog, "response", gtkWidgetDestroy)


                 })

  the.buttons$packStart(buttonOK,fill=F)

  buttonClear <- gtkButtonNewWithMnemonic("Clea_r", show = TRUE)
  buttonClear$setTooltipText("Clic here to clean all data entries")

  gSignalConnect(buttonClear, "clicked",
                 f = function(widget, ...) {

                   cn.entry$setText("")
                   landsoil.entry$setText("")

                 })

  the.buttons$packStart(buttonClear,fill=F)

  buttonCancel <- gtkButtonNewWithMnemonic("Ca_ncel", show = TRUE)
  gSignalConnect(buttonCancel, "clicked", qvoliii_win$destroy)

  the.buttons$packStart(buttonCancel,fill=F)

}

  cnq_ii <- function(action, window)
  {
    cnqii_win <- gtkWindow()
    cnqii_win$setTitle ("sara4r v0.0.8")

    frame <- gtkFrameNew("  Get CN and Runoff Volume for the Antecedent Moisture Condition II  ")
    cnqii_win$add(frame)

    vbox <- gtkVBoxNew(FALSE, 8)
    vbox$setBorderWidth(24)
    frame$add(vbox)

    hbox <- gtkHBoxNew(FALSE,8)
    label <- gtkLabelNew()
    label$setMarkup("<b>USDA (1986)</b>")
    label$setTooltipText("USDA Soil Conservation Service 1986. Urban Hydrology for Small Watersheds. United States Department of Agriculture. Natural Resources Conservation Service. Conservation Engineering Division. Technical Release 55. 2nd edn. Washington, DC, pp. 164.")
    vbox$packStart(label,FALSE,FALSE,0)

    vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

    label <- gtkLabelNew()
    label$setMarkup("<b>(P - 0.2*S)^2</b>")
    vbox$packStart(label,FALSE,FALSE,0)

    label <- gtkLabelNew()
    label$setMarkup("<b>Q =             --------------------,   P > 0.2*S</b>")
    vbox$packStart(label,FALSE,FALSE,0)

    label <- gtkLabelNew()
    label$setMarkup("<b>(P + 0.8*S)</b>")
    vbox$packStart(label,FALSE,FALSE,0)

    label <- gtkLabelNew()
    label$setMarkup("<b>\n 1000</b>")
    vbox$packStart(label,FALSE,FALSE,0)

    label <- gtkLabelNew()
    label$setMarkup("<b>S =         ----------  -  10 (in)</b>")
    vbox$packStart(label,FALSE,FALSE,0)

    label <- gtkLabelNew()
    label$setMarkup("<b> CN</b>")
    vbox$packStart(label,FALSE,FALSE,0)

    vbox$packStart(hbox, FALSE, FALSE, 0)

    vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

    hbox <- gtkHBoxNew(FALSE,8)
    label <- gtkLabelNew()
    label$setMarkup("<b>\n Parameters:</b>")

    vbox$packStart(label,FALSE,FALSE,0)
    vbox$packStart(hbox, FALSE, FALSE, 0)

    label <- gtkLabelNew()
    label$setMarkup("<b>Landuse:   </b>")
    hbox$packStart(label,FALSE,FALSE,0)

    landuse.entry <- gtkEntryNew()
    landuse.entry$setWidthChars(60)
    landuse.entry$setSensitive(FALSE)

    hbox$packStart(landuse.entry,FALSE,FALSE,0)

    land.but <- gtkButton("Browse")
    land.but$setTooltipText("Please, browse the Land use and land cover map")
    hbox$packStart(land.but,FALSE,FALSE,0)

    gSignalConnect(land.but, "clicked",
                   f = function(widget, ...) {
                     dialog <- gtkFileChooserDialog(title = "Select the Land use and land cover map",
                                                    parent = NULL, action = "open",
                                                    "gtk-ok", GtkResponseType["ok"],
                                                    "gtk-cancel", GtkResponseType["cancel"],
                                                    show = FALSE)

                     gSignalConnect(dialog, "response",
                                    f = function(dialog, response, data) {
                                      if(response == GtkResponseType["ok"]) {
                                        filename <- dialog$getFilename()
                                        landuse.entry$setText(filename)

                                        dev.new()
                                        veirus_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                                        veirus_plot <- raster(veirus_file)
                                        plot(veirus_plot, main="Land use and land covers", xlab="Longitude", ylab="Latitude")

                                      }

                                      dialog$destroy()
                                    })

                     fileFilter <- gtkFileFilter()
                     fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                     fileFilter$addPattern("*.tif")
                     fileFilter$addPattern("*.TIFF")
                     fileFilter$addPattern("*.asc")
                     dialog$addFilter(fileFilter)

                     dialog$run()

                   })

    hbox <- gtkHBoxNew(FALSE, 8)
    vbox$packStart(hbox, FALSE, FALSE, 0)

    label <- gtkLabelNew()
    label$setMarkup("<b>HSG:          </b>")
    hbox$packStart(label,FALSE,FALSE,0)

    hsg.entry <- gtkEntryNew()
    hsg.entry$setWidthChars(60)
    hsg.entry$setSensitive(FALSE)

    hbox$packStart(hsg.entry,FALSE,FALSE,0)

    hsg.but <- gtkButton("Browse")
    hsg.but$setTooltipText("Please, browse the Hydrologic Soil Groups map")
    hbox$packStart(hsg.but,FALSE,FALSE,0)

    gSignalConnect(hsg.but, "clicked",
                   f = function(widget, ...) {
                     dialog <- gtkFileChooserDialog(title = "Select the Hydrologic Soil Group map",
                                                    parent = NULL, action = "open",
                                                    "gtk-ok", GtkResponseType["ok"],
                                                    "gtk-cancel", GtkResponseType["cancel"],
                                                    show = FALSE)

                     gSignalConnect(dialog, "response",
                                    f = function(dialog, response, data) {
                                      if(response == GtkResponseType["ok"]) {
                                        filename <- dialog$getFilename()
                                        hsg.entry$setText(filename)

                                        dev.new()
                                        veirus_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                                        veirus_plot <- raster(veirus_file)
                                        plot(veirus_plot, main="Hydrologic Soil Groups", xlab="Longitude", ylab="Latitude")

                                      }

                                      dialog$destroy()
                                    })

                     fileFilter <- gtkFileFilter()
                     fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                     fileFilter$addPattern("*.tif")
                     fileFilter$addPattern("*.TIFF")
                     fileFilter$addPattern("*.asc")
                     dialog$addFilter(fileFilter)

                     dialog$run()

                   })

    hbox <- gtkHBoxNew(FALSE, 8)
    vbox$packStart(hbox, FALSE, FALSE, 0)

    label <- gtkLabelNew()
    label$setMarkup("<b>Index File:</b>")
    hbox$packStart(label,FALSE,FALSE,0)

    index.entry <- gtkEntryNew()
    index.entry$setWidthChars(60)
    index.entry$setSensitive(FALSE)

    hbox$packStart(index.entry,FALSE,FALSE,0)

    index.but <- gtkButton("Browse")
    index.but$setTooltipText("Please, browse the Curve Numbers index file")
    hbox$packStart(index.but,FALSE,FALSE,0)

    gSignalConnect(index.but, "clicked",
                   f = function(widget, ...) {
                     dialog <- gtkFileChooserDialog(title = "Select the CN index file",
                                                    parent = NULL, action = "open",
                                                    "gtk-ok", GtkResponseType["ok"],
                                                    "gtk-cancel", GtkResponseType["cancel"],
                                                    show = FALSE)

                     gSignalConnect(dialog, "response",
                                    f = function(dialog, response, data) {
                                      if(response == GtkResponseType["ok"]) {
                                        filename <- dialog$getFilename()
                                        index.entry$setText(filename)

                                      }

                                      dialog$destroy()
                                    })

                     fileFilter <- gtkFileFilter()
                     fileFilter$setName("Comma-separated values (*.csv)")
                     fileFilter$addPattern("*.csv")
                     dialog$addFilter(fileFilter)

                     dialog$run()

                   })

    hbox <- gtkHBoxNew(FALSE, 8)
    vbox$packStart(hbox, FALSE, FALSE, 0)

    label <- gtkLabelNew()
    label$setMarkup("<b>Precipitation (in):           </b>")
    hbox$packStart(label,FALSE,FALSE,0)

    rainfall.entry <- gtkEntryNew()
    rainfall.entry$setWidthChars(10)
    rainfall.entry$setText("2.5")
    rainfall.entry$setSensitive(TRUE)

    hbox$packStart(rainfall.entry,FALSE,FALSE,0)

    hbox <- gtkHBoxNew(FALSE, 8)
    vbox$packStart(hbox, FALSE, FALSE, 0)

    label <- gtkLabelNew()
    label$setMarkup("<b>Pixel size (meters):        </b>")
    hbox$packStart(label,FALSE,FALSE,0)

    area.entry <- gtkEntryNew()
    area.entry$setWidthChars(10)
    area.entry$setText("30")
    area.entry$setSensitive(TRUE)

    hbox$packStart(area.entry,FALSE,FALSE,0)

    vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

    the.buttons <- gtkHButtonBoxNew()
    the.buttons$setBorderWidth(5)
    vbox$add(the.buttons)
    the.buttons$setLayout("spread")
    the.buttons$setSpacing(40)

    buttonOK <- gtkButtonNewWithMnemonic("_Calculate AMC II", show = TRUE)
    buttonOK$setTooltipText("This process takes a while. Please, wait until the program finished")

    gSignalConnect(buttonOK, "clicked",
                   f = function(widget, ...) {

                     if ((landuse.entry$getText())=="") return(invisible(NULL))
                     if ((hsg.entry$getText())=="") return(invisible(NULL))
                     if ((index.entry$getText())=="") return(invisible(NULL))
                     if ((rainfall.entry$getText())=="") return(invisible(NULL))
                     if ((area.entry$getText())=="") return(invisible(NULL))

                     landuse_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                     hsg_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                     index_file <-  normalizePath(path.expand(index.entry$getText()),  winslash = "/", mustWork = NA)

                     land <- raster(landuse_file)
                     soil <- raster(hsg_file)

                     landsoil <- (land + soil)
                     dev.new()
                     plot(landsoil, main= "Landsoil classes", xlab="Longitude", ylab="Latitude")
                     writeRaster(landsoil, filename = "./landsoil", format="GTiff", overwrite=TRUE)

                     index <- read.csv(index_file, header = FALSE, sep = ",")

                     cn <- reclassify(landsoil, index, include.lowest=TRUE, right=FALSE)
                     writeRaster(cn, filename = "./cn_amc_ii", format="GTiff", overwrite=TRUE)

                     cn_rat <- ratify(cn)
                     cn_rat
                     cn_rat@data@attributes
                     cn_types <- cn_rat@data@attributes[[1]]
                     cols <- rainbow(nrow(cn_types))
                     cols[1] <- "darkgreen"
                     dev.new()
                     image(cn_rat, col=cols, main="Curve numbers AMC II",  xlab="Longitude", ylab="Latitude")
                     legend("bottomleft", legend = cn_types$ID, fill = cols, bty="n", cex = 0.8)

                     P <- as.numeric(rainfall.entry$getText())

                     S <- ((1000/cn) - 10)
                     Ia <- (0.2*S)

                     msc <- reclassify(Ia, c(-Inf, P, 1, P, Inf, 0), include.lowest=FALSE, right=TRUE)

                     Q <- ((P-(0.2*S))^2)/(P+(0.8*S))

                     q_depth <- (Q * msc)

                     dev.new()
                     plot(q_depth, main="Runoff depth (inch)",  xlab="Longitude", ylab="Latitude")

                     resolution <- as.numeric(area.entry$getText())
                     area <- (resolution * resolution)

                     runoff <- round((q_depth * 0.0254 * area), 2)

                     runoff_rat <- ratify(runoff)
                     runoff_rat
                     runoff_rat@data@attributes
                     q_types <- runoff_rat@data@attributes [[1]]
                     cols <- rainbow(nrow(q_types))
                     cols[1] <- "yellow"
                     dev.new()
                     image(runoff_rat, col=cols, main="Runoff volumen in cubic meters",  xlab="Longitude", ylab="Latitude")
                     legend("bottomleft", legend = q_types$ID, fill = cols, bty="n", cex = 0.8)

                     writeRaster(q_depth, filename = "./Q_depth_inch_amcii", format="GTiff", overwrite=TRUE)
                     writeRaster(runoff, filename = "./Runoff_m3_amcii", format="GTiff", overwrite=TRUE)

                     path <- getwd()
                     datetime <- format(Sys.time(),'_%Y%m%d_%H%M%S')
                     vol <- cellStats(runoff, 'sum', na.rm=TRUE, asSample=TRUE)

                     write.table(paste("Total runoff volume for the AMC II = ", vol, "m^3 \n\n The parameters used were:\n Precipitation = ", P, "inches \nc Landuse File: ", landuse_file, "\n HSG File: ", hsg_file, "\n CN index database: ", index_file, "\n Pixel size = ", resolution, "meters\n\n sara4r" ),
                                 file = paste(path, "/Runoff_Vol_AMCII", datetime, '.txt', sep = ""),
                                 row.names = FALSE, col.names = FALSE, quote = FALSE)

                     dialog <- gtkMessageDialogNew(cnqii_win, c("modal", "destroy-with-parent"), "info", "close",
                                                   "All outputs were stored in\n",
                                                   "the working folder ", path, " \n\n",
                                                   "Total runoff volume for the AMC II is\n",
                                                   " = ", vol, "m^3")


                     dialog$formatSecondaryText()
                     gSignalConnect(dialog, "response", gtkWidgetDestroy)


                   })

    the.buttons$packStart(buttonOK,fill=F)

    buttonClear <- gtkButtonNewWithMnemonic("Clea_r", show = TRUE)
    buttonClear$setTooltipText("Clic here to clean all data entries")

    gSignalConnect(buttonClear, "clicked",
                   f = function(widget, ...) {

                     landuse.entry$setText("")
                     hsg.entry$setText("")
                     index.entry$setText("")

                   })

    the.buttons$packStart(buttonClear,fill=F)

    buttonCancel <- gtkButtonNewWithMnemonic("Ca_ncel", show = TRUE)
    gSignalConnect(buttonCancel, "clicked", cnqii_win$destroy)

    the.buttons$packStart(buttonCancel,fill=F)

 }




hawkins <- function(action, window)
{
  hawkins_win <- gtkWindow()
  hawkins_win$setTitle ("sara4r v0.0.8")

  frame <- gtkFrameNew("  Get CN and Runoff volume with the Modified NRCS-CN method  ")
  hawkins_win$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  hbox <- gtkHBoxNew(FALSE,8)
  label <- gtkLabelNew()
  label$setMarkup("<b>Hawkins et al. (2002)</b>")
  label$setTooltipText("Hawkins et al., 2002. Runoff curve number method: Examination of the initial abstraction ratio. In: Proceedings of the Second Federal Interagency Hydrologic Modeling Conference, Las Vegas, Nevada. U.S. Geological Survey, Lakewood, Colorado.")
  vbox$packStart(label,FALSE,FALSE,0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>(P - 0.05*S05)^2</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Q =             --------------------,   P > 0.05*S</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>(P + 0.95*S05)</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>\n S05 = 1.33 * S^1.15  (in)</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  vbox$packStart(hbox, FALSE, FALSE, 0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  hbox <- gtkHBoxNew(FALSE,8)

  label <- gtkLabelNew()
  label$setMarkup("<b>\n Parameters:</b>")

  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Landuse:   </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  landuse.entry <- gtkEntryNew()
  landuse.entry$setWidthChars(60)
  landuse.entry$setSensitive(FALSE)

  hbox$packStart(landuse.entry,FALSE,FALSE,0)

  land.but <- gtkButton("Browse")
  land.but$setTooltipText("Please, browse the Land use and land cover map")
  hbox$packStart(land.but,FALSE,FALSE,0)

  gSignalConnect(land.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Land use and land cover map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      landuse.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Land use and land covers", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>HSG:          </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  hsg.entry <- gtkEntryNew()
  hsg.entry$setWidthChars(60)
  hsg.entry$setSensitive(FALSE)

  hbox$packStart(hsg.entry,FALSE,FALSE,0)

  hsg.but <- gtkButton("Browse")
  hsg.but$setTooltipText("Please, browse the Hydrologic Soil Groups map")
  hbox$packStart(hsg.but,FALSE,FALSE,0)

  gSignalConnect(hsg.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Hydrologic Soil Group map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      hsg.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Hydrologic Soil Groups", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Index File:</b>")
  hbox$packStart(label,FALSE,FALSE,0)

  index.entry <- gtkEntryNew()
  index.entry$setWidthChars(60)
  index.entry$setSensitive(FALSE)

  hbox$packStart(index.entry,FALSE,FALSE,0)

  index.but <- gtkButton("Browse")
  index.but$setTooltipText("Please, browse the Curve Numbers index file")
  hbox$packStart(index.but,FALSE,FALSE,0)

  gSignalConnect(index.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the CN index file",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      index.entry$setText(filename)

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Comma-separated values (*.csv)")
                   fileFilter$addPattern("*.csv")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Precipitation (in):           </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  rainfall.entry <- gtkEntryNew()
  rainfall.entry$setWidthChars(10)
  rainfall.entry$setText("2.5")
  rainfall.entry$setSensitive(TRUE)

  hbox$packStart(rainfall.entry,FALSE,FALSE,0)

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Pixel size (meters):        </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  area.entry <- gtkEntryNew()
  area.entry$setWidthChars(10)
  area.entry$setText("30")
  area.entry$setSensitive(TRUE)

  hbox$packStart(area.entry,FALSE,FALSE,0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  the.buttons <- gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)

  buttonOK <- gtkButtonNewWithMnemonic("_Calculate", show = TRUE)
  buttonOK$setTooltipText("This process takes a while. Please, wait until the program finished")

  gSignalConnect(buttonOK, "clicked",
                 f = function(widget, ...) {
                   if ((landuse.entry$getText())=="") return(invisible(NULL))
                   if ((hsg.entry$getText())=="") return(invisible(NULL))
                   if ((index.entry$getText())=="") return(invisible(NULL))

                   landuse_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                   hsg_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                   index_file <-  normalizePath(path.expand(index.entry$getText()),  winslash = "/", mustWork = NA)

                   land <- raster(landuse_file)
                   soil <- raster(hsg_file)

                   landsoil <- (land + soil)
                   dev.new()
                   plot(landsoil, main= "Landsoil classes", xlab="Longitude", ylab="Latitude")

                   writeRaster(landsoil, filename = "./landsoil", format="GTiff", overwrite=TRUE)

                   index <- read.csv(index_file, header = FALSE, sep = ",")

                   cn <- reclassify(landsoil, index, include.lowest=TRUE, right=FALSE)

                   writeRaster(cn, filename = "./cn_amc_ii", format="GTiff", overwrite=TRUE)

                   cn_rat <- ratify(cn)
                   cn_rat
                   cn_rat@data@attributes
                   cn_types <- cn_rat@data@attributes[[1]]
                   cols <- rainbow(nrow(cn_types))
                   cols[1] <- "darkgreen"
                   dev.new()
                   image(cn_rat, col=cols, main="Curve numbers AMC II",  xlab="Longitude", ylab="Latitude")

                   legend("bottomleft", legend = cn_types$ID, fill = cols, bty="n", cex = 0.8)

                   P <- as.numeric(rainfall.entry$getText())

                   S <- ((1000/cn) - 10)
                   S05 <- (1.33*(S^1.15))
                   Ia <- (0.05*S05)

                   msc <- reclassify(Ia, c(-Inf, P, 1, P, Inf, 0), include.lowest=FALSE, right=TRUE)

                   Q <- ((P-(0.05*S05))^2)/(P+(0.95*S05))

                   q_depth <- (Q * msc)

                   dev.new()
                   plot(q_depth, main="Runoff depth (inch)",  xlab="Longitude", ylab="Latitude")

                   resolution <- as.numeric(area.entry$getText())
                   area <- (resolution * resolution)

                   runoff <- round((q_depth * 0.0254 * area), 2)

                   runoff_rat <- ratify(runoff)
                   runoff_rat
                   runoff_rat@data@attributes
                   q_types <- runoff_rat@data@attributes [[1]]
                   cols <- rainbow(nrow(q_types))
                   cols[1] <- "yellow"
                   dev.new()
                   image(runoff_rat, col=cols, main="Runoff volumen in cubic meters",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = q_types$ID, fill = cols, bty="n", cex = 0.8)

                   writeRaster(q_depth, filename = "./Q_depth_inch_hawkins", format="GTiff", overwrite=TRUE)
                   writeRaster(runoff, filename = "./Runoff_m3_hawkins", format="GTiff", overwrite=TRUE)

                   path <- getwd()
                   datetime <- format(Sys.time(),'_%Y%m%d_%H%M%S')
                   vol <- cellStats(runoff, 'sum', na.rm=TRUE, asSample=TRUE)

                   write.table(paste("Total runoff volume with the modified NRCS-CN method = ", vol, "m^3 \n\n The parameters used were:\n Precipitation: ", P, "inches \n Landuse File: ", landuse_file, "\n HSG File: ", hsg_file, "\n CN index database: ", index_file, "\n Pixel size: ", resolution, "meters\n\n sara4r"),
                               file = paste(path, "/Runoff_Vol_hawkins", datetime, '.txt', sep = ""),
                               row.names = FALSE, col.names = FALSE, quote = FALSE)

                   dialog <- gtkMessageDialogNew(hawkins_win, c("modal", "destroy-with-parent"), "info", "close",
                                                 "All outputs were stored in\n",
                                                 "the working folder ", path, " \n\n",
                                                 "Total runoff volume with the modified NRCS-CN method is\n",
                                                 " = ", vol, "m^3")

                   dialog$formatSecondaryText()
                   gSignalConnect(dialog, "response", gtkWidgetDestroy)


                 })

  the.buttons$packStart(buttonOK,fill=F)

  buttonClear <- gtkButtonNewWithMnemonic("Clea_r", show = TRUE)
  buttonClear$setTooltipText("Clic here to clean all data entries")

  gSignalConnect(buttonClear, "clicked",
                 f = function(widget, ...) {

                   landuse.entry$setText("")
                   hsg.entry$setText("")
                   index.entry$setText("")

                 })

  the.buttons$packStart(buttonClear,fill=F)

  buttonCancel <- gtkButtonNewWithMnemonic("Ca_ncel", show = TRUE)
  gSignalConnect(buttonCancel, "clicked", hawkins_win$destroy)

  the.buttons$packStart(buttonCancel,fill=F)

}


pspatial <- function(action, window)
{
  pspatial_win <- gtkWindow()
  pspatial_win$setTitle ("sara4r v0.0.8")


  frame <- gtkFrameNew("  Get Q-depth and Runoff Volume using a Precipitation image  ")
  pspatial_win$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  hbox <- gtkHBoxNew(FALSE,8)
  label <- gtkLabelNew()
  label$setMarkup("<b>USDA (1986)</b>")
  label$setTooltipText("USDA Soil Conservation Service 1986. Urban Hydrology for Small Watersheds. United States Department of Agriculture. Natural Resources Conservation Service. Conservation Engineering Division. Technical Release 55. 2nd edn. Washington, DC, pp. 164.")
  vbox$packStart(label,FALSE,FALSE,0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>(P - 0.2*S)^2</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Q =             --------------------,   P > 0.2*S</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>(P + 0.8*S)</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>\n 1000</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b>S =         ----------  -  10 (in)</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  label <- gtkLabelNew()
  label$setMarkup("<b> CN</b>")
  vbox$packStart(label,FALSE,FALSE,0)

  vbox$packStart(hbox, FALSE, FALSE, 0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  hbox <- gtkHBoxNew(FALSE,8)

  label <- gtkLabelNew()
  label$setMarkup("<b>Parameters:</b>")

  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Curve Number Map:      </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  cn.entry <- gtkEntryNew()
  cn.entry$setWidthChars(60)
  cn.entry$setSensitive(FALSE)
  hbox$packStart(cn.entry,FALSE,FALSE,0)

  cn.but <- gtkButton("Browse")
  cn.but$setTooltipText("Please, browse the Curve Number map")
  hbox$packStart(cn.but,FALSE,FALSE,0)

  gSignalConnect(cn.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Curve Number map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      cn.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(cn.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Curve Numbers", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Landsoil map:                   </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  landsoil.entry <- gtkEntryNew()
  landsoil.entry$setWidthChars(60)
  landsoil.entry$setSensitive(FALSE)

  hbox$packStart(landsoil.entry,FALSE,FALSE,0)

  landsoil.but <- gtkButton("Browse")
  landsoil.but$setTooltipText("Please, browse the Landsoil map created in the previous step")
  hbox$packStart(landsoil.but,FALSE,FALSE,0)

  gSignalConnect(landsoil.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Landsoil map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      landsoil.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(landsoil.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Landsoil map", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Precipitation map (in):   </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  rainfall.entry <- gtkEntryNew()
  rainfall.entry$setWidthChars(60)
  rainfall.entry$setSensitive(FALSE)

  hbox$packStart(rainfall.entry,FALSE,FALSE,0)

  rainfall.but <- gtkButton("Browse")
  rainfall.but$setTooltipText("Please, browse the Precipitation map")
  hbox$packStart(rainfall.but,FALSE,FALSE,0)

  gSignalConnect(rainfall.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select the Precipitation map",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      filename <- dialog$getFilename()
                                      rainfall.entry$setText(filename)

                                      dev.new()
                                      veirus_file <- normalizePath(path.expand(rainfall.entry$getText()),  winslash = "/", mustWork = NA)
                                      veirus_plot <- raster(veirus_file)
                                      plot(veirus_plot, main="Precipitation map", xlab="Longitude", ylab="Latitude")

                                    }

                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)


  label <- gtkLabelNew()
  label$setMarkup("<b>Pixel size (meters):        </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  area.entry <- gtkEntryNew()
  area.entry$setWidthChars(10)
  area.entry$setText("30")
  area.entry$setSensitive(TRUE)
  hbox$packStart(area.entry,FALSE,FALSE,0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  the.buttons <- gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(20)

  buttonOK <- gtkButtonNewWithMnemonic("_Calculate Q depth and Vol", show = TRUE)
  buttonOK$setTooltipText("This process takes a while. Please, wait until the program finished")
  gSignalConnect(buttonOK, "clicked",
                 f = function(widget, ...) {

                   if ((cn.entry$getText())=="") return(invisible(NULL))
                   if ((landsoil.entry$getText())=="") return(invisible(NULL))
                   if ((rainfall.entry$getText())=="") return(invisible(NULL))
                   if ((area.entry$getText())=="") return(invisible(NULL))

                   cn_file <- normalizePath(path.expand(cn.entry$getText()),  winslash = "/", mustWork = NA)
                   landsoil_file <- normalizePath(path.expand(landsoil.entry$getText()),  winslash = "/", mustWork = NA)
                   rainfall_file <- normalizePath(path.expand(rainfall.entry$getText()),  winslash = "/", mustWork = NA)
                   cn <- raster(cn_file)
                   landsoil <- raster(landsoil_file)
                   P<- raster(rainfall_file)

                   S <- ((1000/cn) - 10)
                   Ia <- (0.2*S)
                   PIa <- (P-Ia)

                   msc <- reclassify(PIa, c(-Inf, 0, 0, 0, Inf, 1), include.lowest=FALSE, right=TRUE)

                   Q <- ((P-(0.2*S))^2)/(P+(0.8*S))

                   q_depth <- (Q * msc)

                   dev.new()
                   plot(q_depth, main="Runoff depth (inch)",  xlab="Longitude", ylab="Latitude")

                   resolution <- as.numeric(area.entry$getText())
                   area <- (resolution * resolution)

                   runoff <- round((q_depth * 0.0254 * area), 2)

                   runoff_rat <- ratify(runoff)
                   runoff_rat
                   runoff_rat@data@attributes
                   q_types <- runoff_rat@data@attributes [[1]]
                   cols <- rainbow(nrow(q_types))
                   cols[1] <- "yellow"
                   dev.new()
                   image(runoff_rat, col=cols, main="Runoff volumen in cubic meters",  xlab="Longitude", ylab="Latitude")
                   legend("bottomleft", legend = q_types$ID, fill = cols, bty="n", cex = 0.8)

                   writeRaster(q_depth, filename = "./Q_depth_inch", format="GTiff", overwrite=TRUE)
                   writeRaster(runoff, filename = "./Runoff_m3_Pspatial", format="GTiff", overwrite=TRUE)

                   path <- getwd()
                   datetime <- format(Sys.time(),'_%Y%m%d_%H%M%S')
                   vol <- cellStats(runoff, 'sum', na.rm=TRUE, asSample=TRUE)

                   write.table(paste("Total runoff volume = ", vol, "m^3 \n\n The parameters used were:\n Curve Number File: ", cn_file, " \n Landsoil File: ", landsoil_file, "\n Precipitation File (in): ", rainfall_file, "\n Pixel size: ", resolution, "meters\n\n sara4r"),
                               file = paste(path, "/Runoff_Vol_Pspatial", datetime, '.txt', sep = ""),
                               row.names = FALSE, col.names = FALSE, quote = FALSE)

                   dialog <- gtkMessageDialogNew(pspatial_win, c("modal", "destroy-with-parent"), "info", "close",
                                                 "Q_depth_inch and Runoff_m3 were stored in\n",
                                                 "the working folder ", path, " \n\n",
                                                 "Total runoff volume using a precipitation image is\n",
                                                 " = ", vol, "m^3")

                   dialog$formatSecondaryText()
                   gSignalConnect(dialog, "response", gtkWidgetDestroy)


                 })

  the.buttons$packStart(buttonOK,fill=F)

  buttonClear <- gtkButtonNewWithMnemonic("Clea_r", show = TRUE)
  buttonClear$setTooltipText("Clic here to clean all data entries")

  gSignalConnect(buttonClear, "clicked",
                 f = function(widget, ...) {

                   cn.entry$setText("")
                   landsoil.entry$setText("")
                   rainfall.entry$setText("")

                 })

  the.buttons$packStart(buttonClear,fill=F)

  buttonCancel <- gtkButtonNewWithMnemonic("Ca_ncel", show = TRUE)
  gSignalConnect(buttonCancel, "clicked", pspatial_win$destroy)

  the.buttons$packStart(buttonCancel,fill=F)

}


hawkinsP <- function(action, window)
{
  hawkinsP_win <- gtkWindow()
hawkinsP_win$setTitle ("sara4r v0.0.8")

frame <- gtkFrameNew("  Get CN and Runoff volume with the Modified NRCS-CN method  ")
hawkinsP_win$add(frame)

vbox <- gtkVBoxNew(FALSE, 8)
vbox$setBorderWidth(24)
frame$add(vbox)

hbox <- gtkHBoxNew(FALSE,8)
label <- gtkLabelNew()
label$setMarkup("<b>Hawkins et al. (2002)</b>")
label$setTooltipText("Hawkins et al., 2002. Runoff curve number method: Examination of the initial abstraction ratio. In: Proceedings of the Second Federal Interagency Hydrologic Modeling Conference, Las Vegas, Nevada. U.S. Geological Survey, Lakewood, Colorado.")
vbox$packStart(label,FALSE,FALSE,0)

vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

label <- gtkLabelNew()
label$setMarkup("<b>(P - 0.05*S05)^2</b>")
vbox$packStart(label,FALSE,FALSE,0)

label <- gtkLabelNew()
label$setMarkup("<b>Q =             --------------------,   P > 0.05*S</b>")
vbox$packStart(label,FALSE,FALSE,0)

label <- gtkLabelNew()
label$setMarkup("<b>(P + 0.95*S05)</b>")
vbox$packStart(label,FALSE,FALSE,0)

label <- gtkLabelNew()
label$setMarkup("<b>\n S05 = 1.33 * S^1.15  (in)</b>")
vbox$packStart(label,FALSE,FALSE,0)

vbox$packStart(hbox, FALSE, FALSE, 0)

vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

hbox <- gtkHBoxNew(FALSE,8)

label <- gtkLabelNew()
label$setMarkup("<b>\n Parameters:</b>")

vbox$packStart(label,FALSE,FALSE,0)
vbox$packStart(hbox, FALSE, FALSE, 0)

label <- gtkLabelNew()
label$setMarkup("<b>Landuse:                          </b>")
hbox$packStart(label,FALSE,FALSE,0)

landuse.entry <- gtkEntryNew()
landuse.entry$setWidthChars(60)
landuse.entry$setSensitive(FALSE)

hbox$packStart(landuse.entry,FALSE,FALSE,0)

land.but <- gtkButton("Browse")
land.but$setTooltipText("Please, browse the Land use and land cover map")
hbox$packStart(land.but,FALSE,FALSE,0)

gSignalConnect(land.but, "clicked",
               f = function(widget, ...) {
                 dialog <- gtkFileChooserDialog(title = "Select the Land use and land cover map",
                                                parent = NULL, action = "open",
                                                "gtk-ok", GtkResponseType["ok"],
                                                "gtk-cancel", GtkResponseType["cancel"],
                                                show = FALSE)

                 gSignalConnect(dialog, "response",
                                f = function(dialog, response, data) {
                                  if(response == GtkResponseType["ok"]) {
                                    filename <- dialog$getFilename()
                                    landuse.entry$setText(filename)

                                    dev.new()
                                    veirus_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                                    veirus_plot <- raster(veirus_file)
                                    plot(veirus_plot, main="Land use and land covers", xlab="Longitude", ylab="Latitude")

                                  }

                                  dialog$destroy()
                                })

                 fileFilter <- gtkFileFilter()
                 fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                 fileFilter$addPattern("*.tif")
                 fileFilter$addPattern("*.TIFF")
                 fileFilter$addPattern("*.asc")
                 dialog$addFilter(fileFilter)

                 dialog$run()

               })

hbox <- gtkHBoxNew(FALSE, 8)
vbox$packStart(hbox, FALSE, FALSE, 0)

label <- gtkLabelNew()
label$setMarkup("<b>HSG:                                 </b>")
hbox$packStart(label,FALSE,FALSE,0)

hsg.entry <- gtkEntryNew()
hsg.entry$setWidthChars(60)
hsg.entry$setSensitive(FALSE)

hbox$packStart(hsg.entry,FALSE,FALSE,0)

hsg.but <- gtkButton("Browse")
hsg.but$setTooltipText("Please, browse the Hydrologic Soil Groups map")
hbox$packStart(hsg.but,FALSE,FALSE,0)

gSignalConnect(hsg.but, "clicked",
               f = function(widget, ...) {
                 dialog <- gtkFileChooserDialog(title = "Select the Hydrologic Soil Group map",
                                                parent = NULL, action = "open",
                                                "gtk-ok", GtkResponseType["ok"],
                                                "gtk-cancel", GtkResponseType["cancel"],
                                                show = FALSE)

                 gSignalConnect(dialog, "response",
                                f = function(dialog, response, data) {
                                  if(response == GtkResponseType["ok"]) {
                                    filename <- dialog$getFilename()
                                    hsg.entry$setText(filename)

                                    dev.new()
                                    veirus_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                                    veirus_plot <- raster(veirus_file)
                                    plot(veirus_plot, main="Hydrologic Soil Groups", xlab="Longitude", ylab="Latitude")

                                  }

                                  dialog$destroy()
                                })

                 fileFilter <- gtkFileFilter()
                 fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                 fileFilter$addPattern("*.tif")
                 fileFilter$addPattern("*.TIFF")
                 fileFilter$addPattern("*.asc")
                 dialog$addFilter(fileFilter)

                 dialog$run()

               })

hbox <- gtkHBoxNew(FALSE, 8)
vbox$packStart(hbox, FALSE, FALSE, 0)

label <- gtkLabelNew()
label$setMarkup("<b>Index File:                       </b>")
hbox$packStart(label,FALSE,FALSE,0)

index.entry <- gtkEntryNew()
index.entry$setWidthChars(60)
index.entry$setSensitive(FALSE)

hbox$packStart(index.entry,FALSE,FALSE,0)

index.but <- gtkButton("Browse")
index.but$setTooltipText("Please, browse the Curve Numbers index file")
hbox$packStart(index.but,FALSE,FALSE,0)

gSignalConnect(index.but, "clicked",
               f = function(widget, ...) {
                 dialog <- gtkFileChooserDialog(title = "Select the CN index file",
                                                parent = NULL, action = "open",
                                                "gtk-ok", GtkResponseType["ok"],
                                                "gtk-cancel", GtkResponseType["cancel"],
                                                show = FALSE)

                 gSignalConnect(dialog, "response",
                                f = function(dialog, response, data) {
                                  if(response == GtkResponseType["ok"]) {
                                    filename <- dialog$getFilename()
                                    index.entry$setText(filename)

                                  }

                                  dialog$destroy()
                                })

                 fileFilter <- gtkFileFilter()
                 fileFilter$setName("Comma-separated values (*.csv)")
                 fileFilter$addPattern("*.csv")
                 dialog$addFilter(fileFilter)

                 dialog$run()

               })

hbox <- gtkHBoxNew(FALSE, 8)
vbox$packStart(hbox, FALSE, FALSE, 0)

label <- gtkLabelNew()
label$setMarkup("<b>Precipitation map (in): </b>")
hbox$packStart(label,FALSE,FALSE,0)

rainfall.entry <- gtkEntryNew()
rainfall.entry$setWidthChars(60)
rainfall.entry$setSensitive(FALSE)

hbox$packStart(rainfall.entry,FALSE,FALSE,0)

rainfall.but <- gtkButton("Browse")
rainfall.but$setTooltipText("Please, browse the Precipitation map")
hbox$packStart(rainfall.but,FALSE,FALSE,0)

gSignalConnect(rainfall.but, "clicked",
               f = function(widget, ...) {
                 dialog <- gtkFileChooserDialog(title = "Select the Precipitation map",
                                                parent = NULL, action = "open",
                                                "gtk-ok", GtkResponseType["ok"],
                                                "gtk-cancel", GtkResponseType["cancel"],
                                                show = FALSE)

                 gSignalConnect(dialog, "response",
                                f = function(dialog, response, data) {
                                  if(response == GtkResponseType["ok"]) {
                                    filename <- dialog$getFilename()
                                    rainfall.entry$setText(filename)

                                    dev.new()
                                    veirus_file <- normalizePath(path.expand(rainfall.entry$getText()),  winslash = "/", mustWork = NA)
                                    veirus_plot <- raster(veirus_file)
                                    plot(veirus_plot, main="Precipitation map", xlab="Longitude", ylab="Latitude")

                                  }

                                  dialog$destroy()
                                })

                 fileFilter <- gtkFileFilter()
                 fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                 fileFilter$addPattern("*.tif")
                 fileFilter$addPattern("*.TIFF")
                 fileFilter$addPattern("*.asc")
                 dialog$addFilter(fileFilter)

                 dialog$run()

               })

hbox <- gtkHBoxNew(FALSE, 8)
vbox$packStart(hbox, FALSE, FALSE, 0)


label <- gtkLabelNew()
label$setMarkup("<b>Pixel size (meters):       </b>")
hbox$packStart(label,FALSE,FALSE,0)

area.entry <- gtkEntryNew()
area.entry$setWidthChars(10)
area.entry$setText("30")
area.entry$setSensitive(TRUE)

hbox$packStart(area.entry,FALSE,FALSE,0)

vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

the.buttons <- gtkHButtonBoxNew()
the.buttons$setBorderWidth(5)
vbox$add(the.buttons)
the.buttons$setLayout("spread")
the.buttons$setSpacing(40)

buttonOK <- gtkButtonNewWithMnemonic("_Calculate", show = TRUE)
buttonOK$setTooltipText("This process takes a while. Please, wait until the program finished")

gSignalConnect(buttonOK, "clicked",
               f = function(widget, ...) {
                 if ((landuse.entry$getText())=="") return(invisible(NULL))
                 if ((hsg.entry$getText())=="") return(invisible(NULL))
                 if ((index.entry$getText())=="") return(invisible(NULL))

                 landuse_file <- normalizePath(path.expand(landuse.entry$getText()),  winslash = "/", mustWork = NA)
                 hsg_file <- normalizePath(path.expand(hsg.entry$getText()),  winslash = "/", mustWork = NA)
                 index_file <-  normalizePath(path.expand(index.entry$getText()),  winslash = "/", mustWork = NA)
                 rainfall_file <- normalizePath(path.expand(rainfall.entry$getText()),  winslash = "/", mustWork = NA)

                 land <- raster(landuse_file)
                 soil <- raster(hsg_file)
                 P<- raster(rainfall_file)

                 landsoil <- (land + soil)
                 dev.new()
                 plot(landsoil, main= "Landsoil classes", xlab="Longitude", ylab="Latitude")

                 writeRaster(landsoil, filename = "./landsoil", format="GTiff", overwrite=TRUE)

                 index <- read.csv(index_file, header = FALSE, sep = ",")

                 cn <- reclassify(landsoil, index, include.lowest=TRUE, right=FALSE)

                 writeRaster(cn, filename = "./cn_amc_ii", format="GTiff", overwrite=TRUE)

                 cn_rat <- ratify(cn)
                 cn_rat
                 cn_rat@data@attributes
                 cn_types <- cn_rat@data@attributes[[1]]
                 cols <- rainbow(nrow(cn_types))
                 cols[1] <- "darkgreen"
                 dev.new()
                 image(cn_rat, col=cols, main="Curve numbers AMC II",  xlab="Longitude", ylab="Latitude")

                 legend("bottomleft", legend = cn_types$ID, fill = cols, bty="n", cex = 0.8)


                 S <- ((1000/cn) - 10)
                 S05 <- (1.33*(S^1.15))

                 Ia <- (0.05*S05)

                 PIa <- (P-Ia)


                 msc <- reclassify(PIa, c(-Inf, 0, 0, 0, Inf, 1), include.lowest=FALSE, right=TRUE)

                 Q <- ((P-(0.05*S05))^2)/(P+(0.95*S05))

                 q_depth <- (Q * msc)

                 dev.new()
                 plot(q_depth, main="Runoff depth (inch)",  xlab="Longitude", ylab="Latitude")

                 resolution <- as.numeric(area.entry$getText())
                 area <- (resolution * resolution)

                 runoff <- round((q_depth * 0.0254 * area), 2)

                 runoff_rat <- ratify(runoff)
                 runoff_rat
                 runoff_rat@data@attributes
                 q_types <- runoff_rat@data@attributes [[1]]
                 cols <- rainbow(nrow(q_types))
                 cols[1] <- "yellow"
                 dev.new()
                 image(runoff_rat, col=cols, main="Runoff volumen in cubic meters",  xlab="Longitude", ylab="Latitude")
                 legend("bottomleft", legend = q_types$ID, fill = cols, bty="n", cex = 0.8)

                 writeRaster(q_depth, filename = "./Q_depth_inch_hawkinsP", format="GTiff", overwrite=TRUE)
                 writeRaster(runoff, filename = "./Runoff_m3_hawkinsP", format="GTiff", overwrite=TRUE)

                 path <- getwd()
                 datetime <- format(Sys.time(),'_%Y%m%d_%H%M%S')
                 vol <- cellStats(runoff, 'sum', na.rm=TRUE, asSample=TRUE)

                 write.table(paste("Total runoff volume with the modified NRCS-CN method = ", vol, "m^3 \n\n The parameters used were:\n Landuse File: ", landuse_file, "\n HSG File: ", hsg_file, "\n CN index database: ", index_file, "\n Precipitation File (in): ", rainfall_file, "\n Pixel size: ", resolution, "meters\n\n sara4r"),
                             file = paste(path, "/Runoff_Vol_hawkinsP", datetime, '.txt', sep = ""),
                             row.names = FALSE, col.names = FALSE, quote = FALSE)

                 dialog <- gtkMessageDialogNew(hawkinsP_win, c("modal", "destroy-with-parent"), "info", "close",
                                               "All outputs were stored in\n",
                                               "the working folder ", path, " \n\n",
                                               "Total runoff volume with the modified NRCS-CN method is\n",
                                               " = ", vol, "m^3")

                 dialog$formatSecondaryText()
                 gSignalConnect(dialog, "response", gtkWidgetDestroy)


               })

the.buttons$packStart(buttonOK,fill=F)

buttonClear <- gtkButtonNewWithMnemonic("Clea_r", show = TRUE)
buttonClear$setTooltipText("Clic here to clean all data entries")

gSignalConnect(buttonClear, "clicked",
               f = function(widget, ...) {

                 landuse.entry$setText("")
                 hsg.entry$setText("")
                 index.entry$setText("")
                 rainfall.entry$setText("")

               })

the.buttons$packStart(buttonClear,fill=F)

buttonCancel <- gtkButtonNewWithMnemonic("Ca_ncel", show = TRUE)
gSignalConnect(buttonCancel, "clicked", hawkinsP_win$destroy)

the.buttons$packStart(buttonCancel,fill=F)

}



set <- function(action, INIRENA)
{
  setwin <- gtkWindow()
  setwin$setTitle ( "Set Working Dir" )

  frame <- gtkFrameNew("Specify working folder location...")
  setwin$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Input Folder Name   </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  folder.entry <- gtkEntryNew()
  folder.entry$setWidthChars(50)
  folder.entry$setSensitive(FALSE)
  hbox$packStart(folder.entry,FALSE,FALSE,0)

  folder.but <- gtkButton("Browse")
  hbox$packStart(folder.but, FALSE,FALSE,0)
  gSignalConnect(folder.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select Folder",
                                                  parent = NULL, action = "select-folder",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      foldername <- dialog$getFilename()
                                      folder.entry$setText(foldername)

                                    }
                                    dialog$destroy()
                                  })

                   dialog$run()

                 })

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  the.buttons <- gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)

  buttonOK <- gtkButtonNewFromStock("gtk-ok")
  gSignalConnect(buttonOK, "clicked",
                 f = function(widget, ...) {
                   if ((folder.entry$getText())=="") return(invisible(NULL))

                   folder <- normalizePath(path.expand(folder.entry$getText()),  winslash = "/", mustWork = NA)
                   setwd(folder)

                   dialog <- gtkMessageDialogNew(setwin, c("modal", "destroy-with-parent"), "info", "close",
                                                 "The folder selected was\n",
                                                 " ", folder, " ")

                   dialog$formatSecondaryText()
                   gSignalConnect(dialog, "response", gtkWidgetDestroy)

                 })

  the.buttons$packStart(buttonOK,fill=F)

  buttonCancel <- gtkButtonNewFromStock("gtk-close")
  gSignalConnect(buttonCancel, "clicked", setwin$destroy)
  the.buttons$packStart(buttonCancel,fill=F)
}

display <- function(action, INIRENA)
{
  display_win <- gtkWindow()
  display_win$setTitle ( "Display Image Window" )

  frame <- gtkFrameNew("Specify file location...")
  display_win$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Filename    </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  filename <- gtkEntryNew()
  filename$setWidthChars(60)
  filename$setSensitive(FALSE)
  hbox$packStart(filename,FALSE,FALSE,0)

  display.but <- gtkButton("Browse")
  hbox$packStart(display.but, FALSE,FALSE,0)
  gSignalConnect(display.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select Image File to Display",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      displayname <- dialog$getFilename()
                                      filename$setText(displayname)

                                    }
                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  hbox <- gtkHBoxNew(FALSE,8)

  label <- gtkLabelNew()
  label$setMarkup("<b>Parameters:</b>")

  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Main title:  </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  title.entry <- gtkEntryNew()
  title.entry$setWidthChars(60)
  title.entry$setText("Image File")
  hbox$packStart(title.entry,FALSE,FALSE,0)

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>X axis title:</b>")
  hbox$packStart(label,FALSE,FALSE,0)

  x.entry <- gtkEntryNew()
  x.entry$setWidthChars(60)
  x.entry$setText("X Coordinates")
  hbox$packStart(x.entry,FALSE,FALSE,0)

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Y axis title:</b>")
  hbox$packStart(label,FALSE,FALSE,0)

  y.entry <- gtkEntryNew()
  y.entry$setWidthChars(60)
  y.entry$setText("Y Coordinates")
  hbox$packStart(y.entry,FALSE,FALSE,0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  the.buttons <- gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)

  buttonOK <- gtkButtonNewFromStock("gtk-ok")
  gSignalConnect(buttonOK, "clicked",
                 f = function(widget, ...) {
                   if ((filename$getText())=="") return(invisible(NULL))
                   display <- normalizePath(path.expand(filename$getText()),  winslash = "/", mustWork = NA)
                   image_title <-  title.entry$getText()
                   x <- x.entry$getText()
                   y <- y.entry$getText()

                   displayfile <- raster(display)
                   dev.new()
                   plot(displayfile, main=image_title, xlab=x, ylab=y)

                 })

  the.buttons$packStart(buttonOK,fill=F)

  buttonCancel <- gtkButtonNewWithMnemonic("Ca_ncel", show = TRUE)
  gSignalConnect(buttonCancel, "clicked", display_win$destroy)
  the.buttons$packStart(buttonCancel,fill=F)
}

histo <- function(action, INIRENA)
{
  histo_win <- gtkWindow()

  histo_win$setTitle ( "HISTO - Image Histogram" )

  frame <- gtkFrameNew("Specify data location...")
  histo_win$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Filename    </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  filename <- gtkEntryNew()
  filename$setWidthChars(60)
  filename$setSensitive(FALSE)
  hbox$packStart(filename,FALSE,FALSE,0)

  display.but <- gtkButton("Browse")
  hbox$packStart(display.but, FALSE,FALSE,0)
  gSignalConnect(display.but, "clicked",
                 f = function(widget, ...) {
                   dialog <- gtkFileChooserDialog(title = "Select Image File to Display",
                                                  parent = NULL, action = "open",
                                                  "gtk-ok", GtkResponseType["ok"],
                                                  "gtk-cancel", GtkResponseType["cancel"],
                                                  show = FALSE)

                   gSignalConnect(dialog, "response",
                                  f = function(dialog, response, data) {
                                    if(response == GtkResponseType["ok"]) {
                                      displayname <- dialog$getFilename()
                                      filename$setText(displayname)

                                    }
                                    dialog$destroy()
                                  })

                   fileFilter <- gtkFileFilter()
                   fileFilter$setName("Raster files (*.tif, *.TIFF, *.asc)")
                   fileFilter$addPattern("*.tif")
                   fileFilter$addPattern("*.TIFF")
                   fileFilter$addPattern("*.asc")
                   dialog$addFilter(fileFilter)

                   dialog$run()

                 })

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  hbox <- gtkHBoxNew(FALSE,8)

  label <- gtkLabelNew()
  label$setMarkup("<b>Parameters:</b>")

  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Number of classes:  </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  class.entry <- gtkEntryNew()
  class.entry$setWidthChars(5)
  class.entry$setText("10")
  hbox$packStart(class.entry,FALSE,FALSE,0)

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Main title:  </b>")
  hbox$packStart(label,FALSE,FALSE,0)

  title.entry <- gtkEntryNew()
  title.entry$setWidthChars(60)
  title.entry$setText("Histogram Image File")
  hbox$packStart(title.entry,FALSE,FALSE,0)

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>X axis title:</b>")
  hbox$packStart(label,FALSE,FALSE,0)

  x.entry <- gtkEntryNew()
  x.entry$setWidthChars(60)
  x.entry$setText("X")
  hbox$packStart(x.entry,FALSE,FALSE,0)

  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNew()
  label$setMarkup("<b>Y axis title:</b>")
  hbox$packStart(label,FALSE,FALSE,0)

  y.entry <- gtkEntryNew()
  y.entry$setWidthChars(60)
  y.entry$setText("Frequency")
  hbox$packStart(y.entry,FALSE,FALSE,0)

  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

  the.buttons <- gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)

  buttonOK <- gtkButtonNewFromStock("gtk-ok")
  gSignalConnect(buttonOK, "clicked",
                 f = function(widget, ...) {
                   if ((filename$getText())=="") return(invisible(NULL))
                   histo <- normalizePath(path.expand(filename$getText()),  winslash = "/", mustWork = NA)
                   classes <- as.numeric(class.entry$getText())
                   image_title <-  title.entry$getText()
                   x <- x.entry$getText()
                   y <- y.entry$getText()
                   histoimage <- raster(histo)

                   dev.new()
                   plot<-hist(histoimage,
                              breaks=classes,
                              main=image_title,
                              col="wheat3",
                              xlab= x,
                              ylab= y)
                 })

  the.buttons$packStart(buttonOK,fill=F)

  buttonCancel <- gtkButtonNewFromStock("gtk-close")
  gSignalConnect(buttonCancel, "clicked", histo_win$destroy)
  the.buttons$packStart(buttonCancel,fill=F)
}

landuse <- function(action, INIRENA) {
  landuse_win <- gtkWindow()
  landuse_win$setTitle ( "sara4r v0.0.8" )

  frame <- gtkFrameNew(" Preparing the Landuse file ")
  landuse_win$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  label1 <- gtkLabelNewWithMnemonic("Land use and land cover map should be reclassified as follow:")
  label2 <- gtkLabelNewWithMnemonic("LULC in the first place should be reclassified as 10")
  label3 <- gtkLabelNewWithMnemonic("LULC in second place should be reclassified as 20")
  label4 <- gtkLabelNewWithMnemonic("... and so on.")
  label5 <- gtkLabelNewWithMnemonic("\n As an example, Landuse file would be:")
  label6 <- gtkLabelNewWithMnemonic("10 Tropical dry forest \n20 Agriculture \n30 Mangrove \n40 Grassland \n50 Evergreen forest \n ...")

  vbox$packStart(label1,FALSE,FALSE,0)
  vbox$packStart(label2,FALSE,FALSE,0)
  vbox$packStart(label3,FALSE,FALSE,0)
  vbox$packStart(label4,FALSE,FALSE,0)
  vbox$packStart(label5,FALSE,FALSE,0)
  vbox$packStart(label6,FALSE,FALSE,0)

}

hsg <- function(action, INIRENA) {
  soils_win <- gtkWindow()
  soils_win$setTitle ( "sara4r v0.0.8" )

  frame <- gtkFrameNew(" Preparing the HSG file ")
  soils_win$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  label1 <- gtkLabelNewWithMnemonic("Hydrologic Soil Group map should be reclassified as follow:")
  label2 <- gtkLabelNewWithMnemonic("HSG A should be reclassified as 1")
  label3 <- gtkLabelNewWithMnemonic("HSG B should be reclassified as 2")
  label4 <- gtkLabelNewWithMnemonic("HSG C should be reclassified as 3")
  label5 <- gtkLabelNewWithMnemonic("HSG D should be reclassified as 4")
  label6 <- gtkLabelNewWithMnemonic("\n Thus, GSH map would be:")
  label7 <- gtkLabelNewWithMnemonic("1 A \n2 B \n3 C \n4 D \n\n")

  vbox$packStart(label1,FALSE,FALSE,0)
  vbox$packStart(label2,FALSE,FALSE,0)
  vbox$packStart(label3,FALSE,FALSE,0)
  vbox$packStart(label4,FALSE,FALSE,0)
  vbox$packStart(label5,FALSE,FALSE,0)
  vbox$packStart(label6,FALSE,FALSE,0)
  vbox$packStart(label7,FALSE,FALSE,0)

}

cnindex <- function(action, INIRENA) {
  index_win <- gtkWindow()
  index_win$setTitle ( "sara4r v0.0.8" )

  frame <- gtkFrameNew(" Preparing the CN index file ")
  index_win$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  label1 <- gtkLabelNewWithMnemonic("The logic behind the method is:")
  label2 <- gtkLabelNewWithMnemonic("Landsoil is produced as the sum of LANDUSE and HSG maps. Thus, the possible values it can take are:")
  label3 <- gtkLabelNewWithMnemonic("11 = Landuse 1 (reclassified as 10) with HSG A (reclassified as 1) \n12 = Landuse 1 (reclassified as 10) with HSG B (reclassified as 2) \n13 = Landuse 1 (reclassified as 10) with HSG C (reclassified as 3) \n14 = Landuse 1 (reclassified as 10) with HSG D (reclassified as 4)")
  label4 <- gtkLabelNewWithMnemonic("21 = Landuse 2 (reclassified as 20) with HSG A (reclassified as 1) \n22 = Landuse 2 (reclassified as 20) with HSG B (reclassified as 2) \n23 = Landuse 2 (reclassified as 20) with HSG C (reclassified as 3) \n24 = Landuse 2 (reclassified as 20) with HSG D (reclassified as 4)")
  label5 <- gtkLabelNewWithMnemonic("31 = Landuse 3 (reclassified as 30) with HSG A (reclassified as 1) \n...")
  label6 <- gtkLabelNewWithMnemonic("\n Thus, the CN index file would be (csv file):")
  label7 <- gtkLabelNewWithMnemonic("11,12,CNvalue1 \n12,13,CNvalue2 \n13,14,CNvalue3 \n14,15,CNvalue4 \n21,22,CNvalue5 \n22,23,CNvalue6 \n23,24,CNvalue7 \n24,25,CNvalue8 \n31,32,CNvalue9 \n...\n...\n...")

  vbox$packStart(label1,FALSE,FALSE,0)
  vbox$packStart(label2,FALSE,FALSE,0)
  vbox$packStart(label3,FALSE,FALSE,0)
  vbox$packStart(label4,FALSE,FALSE,0)
  vbox$packStart(label5,FALSE,FALSE,0)
  vbox$packStart(label6,FALSE,FALSE,0)
  vbox$packStart(label7,FALSE,FALSE,0)

}

Quit <- function(...) INIRENA$destroy()

entries <- list(
  list("FileMenu", NULL, "_File"),
  list("Dir", "gtk-open", "_Set Working Dir", "<control>N", "Set Working Dir", set),
  list("Display", "gtk-new", "_Display image", "<control>O", "Open image file", display),
  list("Histogram", "gtk-justify-fill", "Image _Histogram", "<control>H", "Image Histogram", histo),
  list("Quit", "gtk-quit", "_Quit", "<control>Q", "Quit", Quit),

  list("AMCI", NULL, "   _AMC I   "),
  list("CNAMCI", NULL, "Get CN for AMC I"),
  list("Sobhani_I", NULL, "Sobhani 1975", "", "", sobhani_i),
  list("Hawkins_I", NULL, "Hawkins et al., 1985", "", "", hawkins_i),
  list("Chow_I", NULL, "Chow et al., 1988", "", "", chow_i),
  list("Mishra_I", NULL, "Mishra et al., 2008", "", "", mishra_i),
  list("QAMCI", NULL, "Get Q-depth and Runoff Volume", "", "SCS", qvol_i),

  list("AMCII", NULL, "   A_MC II   "),
  list("CNAMCII", NULL, "Get CN for AMC II", "", "", cn_ii),
  list("QAMCII", NULL, "Get Q-depth and Runoff Volume", "", "",  qvol_ii),
  list("BothAMCII", NULL, "Get both (CN and Runoff volume)", "", "", cnq_ii),

  list("AMCIII", NULL, "   AM_C III   "),
  list("CNAMCIII", NULL, "Get CN for AMC III"),
  list("Sobhani_III", NULL, "Sobhani 1975", "", "", sobhani_iii),
  list("Hawkins_III", NULL, "Hawkins et al., 1985", "", "", hawkins_iii),
  list("Chow_III", NULL, "Chow et al., 1988", "", "", chow_iii),
  list("Mishra_III", NULL, "Mishra et al., 2008", "", "", mishra_iii),
  list("QAMCIII", NULL, "Get Q-depth and Runoff Volume", "", "", qvol_iii),

  list("MISC", NULL, "   Mi_sc   "),
  list("IaP", NULL, "Initial Abstraction = 0.2 (P image)", "", "", pspatial),
  list("Hawkins", NULL, "Initial Abstraction = 0.05 (P value)", "", "", hawkins),
  list("HawkinsP", NULL, "Initial Abstraction = 0.05 (P image)", "", "", hawkinsP),

  list("HelpMenu", NULL, "_Help"),
  list("Landuse", NULL, "_Preparing the landuse file", "", "", landuse),
  list("HSG", NULL, "Preparing the HSG _file", "", "", hsg),
  list("CNindex", NULL, "Preparing the CN inde_x file", "", "", cnindex),
  list("About", NULL, "_About", "<control>A", "About", veirusGUI)

)

licence <- function(action, INIRENA) {
  licencewin <- gtkWindow()
  licencewin$setTitle ( "sara4r v0.0.8" )

  frame <- gtkFrameNew("Licence Information Window")
  licencewin$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  label1 <- gtkLabelNewWithMnemonic("SARA Software is released under the GPL license.")
  label2 <- gtkLabelNewWithMnemonic("SARA software is provided AS IS and WITH ALL FAULTS, WITHOUT WARRANTY of any kind.")
  label3 <- gtkLabelNewWithMnemonic("The Authors make NO WARRANTY that SARA Software is free of defects or")
  label4 <- gtkLabelNewWithMnemonic("is suitable for any particular purpose.")
  label5 <- gtkLabelNewWithMnemonic("In NO EVENT shall the Authors be responsible for loss or damages arising")
  label6 <- gtkLabelNewWithMnemonic("from the installation or use of the SARA Software.")

  vbox$packStart(label1,FALSE,FALSE,0)
  vbox$packStart(label2,FALSE,FALSE,0)
  vbox$packStart(label3,FALSE,FALSE,0)
  vbox$packStart(label4,FALSE,FALSE,0)
  vbox$packStart(label5,FALSE,FALSE,0)
  vbox$packStart(label6,FALSE,FALSE,0)

}

contact <- function(action, INIRENA) {
  contactwin <- gtkWindow()
  contactwin$setTitle ( "sara4r v0.0.8" )

  frame <- gtkFrameNew("Contact Information Window")
  contactwin$add(frame)

  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)

  label1 <- gtkLabelNewWithMnemonic("Please, contact rhernandez.g@mail.com")
  label2 <- gtkLabelNewWithMnemonic("for any questions, feedback or to report bugs about SARA Software.")
  label3 <- gtkLabelNewWithMnemonic("sara4r v0.0.8 has been founded by Catedras CONACYT Project No. 148 Granted to INIRENA-UMSNH")
  label4 <- gtkLabelNewWithMnemonic("sara4r v0.0.8 has been developed by Rafael Hernandez Guzman.")
  label5 <- gtkLabelNew()
  label5$setMarkup("<b>\n http://hydro-geomatic-lab.com/</b>")
  vbox$packStart(label1,FALSE,FALSE,0)
  vbox$packStart(label2,FALSE,FALSE,0)
  vbox$packStart(label3,FALSE,FALSE,0)
  vbox$packStart(label4,FALSE,FALSE,0)
  vbox$packStart(label5,FALSE,FALSE,0)
}

INIRENA <- gtkWindowNew("toplevel", show = FALSE)
INIRENA$setTitle("sara4r v0.0.8")
INIRENA$setDefaultSize(400,40)
INIRENA$showAll()

gSignalConnect(INIRENA, "delete-event", function(event, ...) {
  dialog <- gtkMessageDialog(parent = INIRENA, flags = 0,
                             type = "question",
                             buttons = "yes-no",
                             "Are you sure you want to quit?")
  out <- dialog$run()
  dialog$destroy()

  out != GtkResponseType["yes"]
})

table <- gtkTableNew(1, 5, FALSE)
INIRENA$add(table)

agroup <- gtkActionGroupNew("AppWindowActions")

agroup$addActions(entries, INIRENA)

manager <- gtkUIManagerNew()

INIRENA$setData("ui-manager", manager)
manager$insertActionGroup(agroup, 0)

INIRENA$addAccelGroup(manager$getAccelGroup())

uistr <- paste(
  "<ui>",
  "  <menubar name='MenuBar'>",
  "    <menu action='FileMenu'>",
  "      <menuitem action='Dir'/>",
  "      <separator/>",
  "      <menuitem action='Display'/>",
  "      <menuitem action='Histogram'/>",
  "      <separator/>",
  "      <menuitem action='Quit'/>",
  "    </menu>",

  "    <menu action='AMCI'>",
  "	        <menu action='CNAMCI'>",
  "          <menuitem action='Sobhani_I'/>",
  "          <menuitem action='Hawkins_I'/>",
  "          <menuitem action='Chow_I'/>",
  "          <menuitem action='Mishra_I'/>",
  "         </menu>",
  "	        <menuitem action='QAMCI'/>",
  "    </menu>",

  "    <menu action='AMCII'>",
  "        <menuitem action='CNAMCII'/>",
  "        <menuitem action='QAMCII'/>",
  "        <menuitem action='BothAMCII'/>",
  "    </menu>",

  "    <menu action='AMCIII'>",
  "        <menu action='CNAMCIII'>",
  "          <menuitem action='Sobhani_III'/>",
  "          <menuitem action='Hawkins_III'/>",
  "          <menuitem action='Chow_III'/>",
  "          <menuitem action='Mishra_III'/>",
  "         </menu>",
  "        <menuitem action='QAMCIII'/>",
  "    </menu>",

  "    <menu action='MISC'>",
  "      <menuitem action='IaP'/>",
  "      <menuitem action='Hawkins'/>",
  "      <menuitem action='HawkinsP'/>",
  "    </menu>",

  "    <menu action='HelpMenu'>",
  "      <menuitem action='Landuse'/>",
  "      <menuitem action='HSG'/>",
  "      <menuitem action='CNindex'/>",
  "      <menuitem action='About'/>",
  "    </menu>",
  "  </menubar>",

  "  <toolbar  name='ToolBar'>",
  "    <toolitem action='Dir'/>",
  "    <separator action='Sep1'/>",
  "    <toolitem action='Display'/>",
  "    <separator action='Sep2'/>",
  "    <toolitem action='Histogram'/>",
  "    <separator action='Sep3'/>",
  "    <toolitem action='Quit'/>",
  "    <separator action='Sep4'/>",
  "  </toolbar>",
  "</ui>", sep="\n")

manager$addUiFromString(uistr)
menubar <- manager$getWidget("/MenuBar")
menubar$show()
table$attach(menubar, 0, 1, 0, 1, yoptions = 0)

bar <- manager$getWidget("/ToolBar")
bar$show()
table$attach(bar, 0, 1, 1, 2, yoptions = 0)

veirusGUI()

}
