sara4r <- function() {

  land_var <- tclVar("")
  hsg_var <- tclVar("")
  index_var <- tclVar("")
  cn_var <- tclVar("")
  cn1_var <- tclVar("")
  cn3_var <- tclVar("")
  landsoil_var <- tclVar("")
  pimage_var <- tclVar("")
  rainfall_var <- tclVar("2.5")
  area_var <- tclVar("30")

  veirus_clear <- function(){
    tclvalue(land_var) <- ""
    tclvalue(hsg_var) <- ""
    tclvalue(index_var) <- ""
    tclvalue(cn_var) <- ""
    tclvalue(landsoil_var) <- ""
    tclvalue(pimage_var) <- ""

  }

  landuse <- function()
  {
    lulc <- tclvalue(tkgetOpenFile(
      filetypes = "{ {TIFF files (*.tif)} {.tif} } { {ASCII files (*.asc)} {.asc} }  { {All files} * }"))
    if (lulc == "")
      return(data.frame())
    tclvalue(land_var) <- lulc

    land_fact <- as.factor(rast(lulc))
    cols <- topo.colors(nrow(land_fact))
    dev.new()
    plot(land_fact, col=cols, main="Land use and land cover", xlab="Easting", ylab="Northing")
  }

  hsg <- function()
  {
    soils <- tclvalue(tkgetOpenFile(
      filetypes = "{ {TIFF files (*.tif)} {.tif} } { {ASCII files (*.asc)} {.asc} }  { {All files} * }"))
    if (soils == "")
      return(data.frame())
    tclvalue(hsg_var) <- soils

    hsg_fact <- as.factor(rast(soils))
    cols <-topo.colors(nrow(hsg_fact))
    dev.new()
    plot(hsg_fact, col=cols, main="Hydrologic Soil Groups", xlab="Easting", ylab="Northing")
  }

  indexfile <- function()
  {
    index <- tclvalue(tkgetOpenFile(
      filetypes = "{ {Comma delimited files (*.csv)} {.csv} } "))
    if (index == "")
      return(data.frame())
    tclvalue(index_var) <- index
  }

  curvenumbers <- function()
  {
    curvenumber_file <- tclvalue(tkgetOpenFile(
      filetypes = "{ {TIFF files (*.tif)} {.tif} } { {ASCII files (*.asc)} {.asc} }  { {All files} * }"))
    if (curvenumber_file == "")
      return(data.frame())
    tclvalue(cn_var)<- curvenumber_file

    cn_fact <- as.factor(rast(curvenumber_file))
    cols <- rainbow(nrow(cn_fact))
    dev.new()
    plot(cn_fact, col=cols, main="Curve number classes", xlab="Easting", ylab="Northing")

  }

  landsoil <- function()
  {
    ls_file <- tclvalue(tkgetOpenFile(
      filetypes = "{ {TIFF files (*.tif)} {.tif} } { {ASCII files (*.asc)} {.asc} }  { {All files} * }"))
    if (ls_file == "")
      return(data.frame())
    tclvalue(landsoil_var) <- ls_file

    landsoil_fact <- as.factor(rast(ls_file))
    cols <- topo.colors(nrow(landsoil_fact))
    dev.new()
    plot(landsoil_fact, col=cols, main= "Landsoil classes", xlab="Easting", ylab="Northing")

  }

  rainfile <- function()
  {
    name <- tclvalue(tkgetOpenFile(
      filetypes = "{ {TIFF files (*.tif)} {.tif} } { {ASCII files (*.asc)} {.asc} }  { {All files} * }"))
    if (name == "")
      return(data.frame())
    tclvalue(pimage_var) <- name

    rainfall_plot <- rast(name)
    dev.new()
    plot(rainfall_plot, main= "Precipitation", xlab="Easting", ylab="Northing")

  }

  cnii_calc <- function() {
    landuse_file <- tclvalue(land_var)
    hsg_file <- tclvalue(hsg_var)
    index_file <- tclvalue(index_var)

    landuse <- rast(landuse_file)
    soil <- rast(hsg_file)

    landsoil <- (landuse + soil)

    landsoil_fact <- as.factor(landsoil)
    cols <- topo.colors(nrow(landsoil_fact))
    dev.new()
    plot(landsoil_fact, col=cols, main= "Landsoil classes", xlab="Easting", ylab="Northing")

    writeRaster(landsoil, filename = "./landsoil.tif", overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

    index <- read.csv(tclvalue(index_var), header = FALSE, sep = ",")
    cn <- classify(landsoil, index, include.lowest=TRUE, right=FALSE)
    writeRaster(cn, filename = "./cn_amc_ii.tif", overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

    cn_fact <- as.factor(cn)

    cols <- rainbow(nrow(cn_fact))
    dev.new()
    plot(cn_fact, col=cols, main="Curve numbers AMC II",  xlab="Easting", ylab="Northing")

    path <- getwd()
    tkmessageBox(message=paste("landsoil.tif and cn_amc_ii.tif were stored in ", path, ""))

  }

  qvol_calc <- function()
  {
    cn_file <- tclvalue(cn_var)
    landsoil_file <- tclvalue(landsoil_var)

    cn <- rast(cn_file)
    landsoil <- rast(landsoil_file)
    P <- as.numeric(tclvalue(rainfall_var))

    S <- ((1000/cn) - 10)
    Ia <- (0.2*S)

    msc <- classify(Ia, c(-Inf, P, 1, P, Inf, 0), include.lowest=FALSE, right=TRUE)

    Q <- ((P-(0.2*S))^2)/(P+(0.8*S))

    q_depth <- (Q * msc)

    dev.new()
    plot(q_depth, col = rainbow(100), main="Runoff depth (inch)",  xlab="Easting", ylab="Northing")

    resolution <- as.numeric(tclvalue(area_var))
    area <- (resolution * resolution)

    runoff <- round((q_depth * 0.0254 * area), 2)

    dev.new()
    plot(runoff, col = topo.colors(100), main="Runoff volume in cubic meters",  xlab="Easting", ylab="Northing")

    add_time <- format(Sys.time(),'_%Y%m%d_%H%M%S')
    writeRaster(q_depth, filename = paste("./Q_depth_inch",add_time,".tif"), overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')
    writeRaster(runoff, filename = paste("./Runoff_m3",add_time,".tif"), overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

    path <- getwd()
    datetime <- format(Sys.time(),'_%Y%m%d_%H%M%S')
    vol <- round((global(runoff, 'sum', na.rm=TRUE)), 2)

    write.table(paste("Total runoff volume of the study area = ", vol, "m^3 \n\n The parameters used were: \n Precipitation = ", P, "inches \n Curve number map: ", cn_file, "\n Landsoil file: ", landsoil_file, "\n Pixel size = ", resolution, "meters\n\n sara4r"),
                file = paste(path, "/Runoff_Vol", datetime, '.txt', sep = ""),
                row.names = FALSE, col.names = FALSE, quote = FALSE)
    tkmessageBox(message=paste("Q_depth_inch and Runoff_m3 were stored in\n",
                               "the working folder ", path, " \n\n",
                               "Total runoff volume of the study area = ", vol, "m^3"))

  }

  usda_calc <- function() {
    landuse_file <- tclvalue(land_var)
    hsg_file <- tclvalue(hsg_var)
    index_file <- tclvalue(index_var)

    landuse <- rast(landuse_file)
    soil <- rast(hsg_file)

    landsoil <- (landuse + soil)
    landsoil_fact <- as.factor(landsoil)
    cols <- topo.colors(nrow(landsoil_fact))
    dev.new()
    plot(landsoil_fact, col=cols, main= "Landsoil classes", xlab="Easting", ylab="Northing")

    writeRaster(landsoil, filename = "./landsoil.tif", overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

    index <- read.csv(tclvalue(index_var), header = FALSE, sep = ",")
    cn <- classify(landsoil, index, include.lowest=TRUE, right=FALSE)
    writeRaster(cn, filename = "./cn_amc_ii.tif", overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

    cn_fact <- as.factor(cn)
    cols <- rainbow(nrow(cn_fact))
    dev.new()
    plot(cn_fact, col=cols, main="Curve numbers AMC II",  xlab="Easting", ylab="Northing")

    P <- as.numeric(tclvalue(rainfall_var))

    S <- ((1000/cn) - 10)
    Ia <- (0.2*S)

    msc <- classify(Ia, c(-Inf, P, 1, P, Inf, 0), include.lowest=FALSE, right=TRUE)

    Q <- ((P-(0.2*S))^2)/(P+(0.8*S))

    q_depth <- (Q * msc)

    dev.new()
    plot(q_depth, col = rainbow(100), main="Runoff depth (inch)",  xlab="Easting", ylab="Northing")

    resolution <- as.numeric(tclvalue(area_var))
    area <- (resolution * resolution)

    runoff <- round((q_depth * 0.0254 * area), 2)

    dev.new()
    plot(runoff, col = topo.colors(100), main="Runoff volume in cubic meters",  xlab="Easting", ylab="Northing")

    add_time <- format(Sys.time(),'_%Y%m%d_%H%M%S')
    writeRaster(q_depth, filename = paste("./Q_depth_inch",add_time,".tif"), overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')
    writeRaster(runoff, filename = paste("./USDA_Runoff_m3",add_time,".tif"), overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

    path <- getwd()
    datetime <- format(Sys.time(),'_%Y%m%d_%H%M%S')
    vol <- round((global(runoff, 'sum', na.rm=TRUE)), 2)

    write.table(paste("Total runoff volume of the study area = ", vol, "m^3  \n\n The parameters used were: \n Precipitation = ", P, "inches \n Landuse File: ", landuse_file, "\n HSG File: ", hsg_file, "\n CN index database: ", index_file, "\n Pixel size = ", resolution, "meters\n\n sara4r"),
                file = paste(path, "/Runoff_Vol", datetime, '.txt', sep = ""),
                row.names = FALSE, col.names = FALSE, quote = FALSE)
    tkmessageBox(message=paste("Outputs were stored in\n",
                               "the working folder ", path, " \n\n",
                               "Total runoff volume of the study area = ", vol, "m^3"))

  }

  usdaP_calc <- function() {
    cn_file <- tclvalue(cn_var)
    landsoil_file <- tclvalue(landsoil_var)
    rainfall_file <- tclvalue(pimage_var)

    cn <- rast(cn_file)
    landsoil <- rast(landsoil_file)
    P <- rast(rainfall_file)

    S <- ((1000/cn) - 10)
    Ia <- (0.2*S)
    PIa <- (P-Ia)

    msc <- classify(PIa, c(-Inf, 0, 0, 0, Inf, 1), include.lowest=FALSE, right=TRUE)

    Q <- ((P-(0.2*S))^2)/(P+(0.8*S))

    q_depth <- (Q * msc)

    dev.new()
    plot(q_depth, col = rainbow(100), main="Runoff depth (inch)",  xlab="Easting", ylab="Northing") # Here

    resolution <- as.numeric(tclvalue(area_var))
    area <- (resolution * resolution)

    runoff <- round((q_depth * 0.0254 * area), 2)

    dev.new()
    plot(runoff, col = topo.colors(100), main="Runoff volume in cubic meters",  xlab="Easting", ylab="Northing")

    add_time <- format(Sys.time(),'_%Y%m%d_%H%M%S')
    writeRaster(q_depth, filename = paste("./Q_depth_inch",add_time,".tif"), overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')
    writeRaster(runoff, filename = paste("./Runoff_m3_Pmap",add_time,".tif"), overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

    path <- getwd()
    datetime <- format(Sys.time(),'_%Y%m%d_%H%M%S')
    vol <- round((global(runoff, 'sum', na.rm=TRUE)), 2)

    write.table(paste("Total runoff volume of the study area = ", vol, "m^3 \n\n The parameters used were: \n Curve number map: ", cn_file, "\n Landsoil file: ", landsoil_file, "\n Precipitation = ", rainfall_file, "\n Pixel size = ", resolution, "meters\n\n sara4r"),
                file = paste(path, "/Runoff_Vol_Pmap", datetime, '.txt', sep = ""),
                row.names = FALSE, col.names = FALSE, quote = FALSE)
    tkmessageBox(message=paste("Q_depth_inch and Runoff_m3_Pmap were stored in\n",
                               "the working folder ", path, " \n\n",
                               "Total runoff volume of the study area = ", vol, "m^3"))

  }

  hawkins_calc <- function() {
    landuse_file <- tclvalue(land_var)
    hsg_file <- tclvalue(hsg_var)
    index_file <- tclvalue(index_var)

    landuse <- rast(landuse_file)
    soil <- rast(hsg_file)

    landsoil <- (landuse + soil)
    landsoil_fact <- as.factor(landsoil)
    cols <- topo.colors(nrow(landsoil_fact))
    dev.new()
    plot(landsoil_fact, col=cols, main= "Landsoil classes", xlab="Easting", ylab="Northing")

    writeRaster(landsoil, filename = "./landsoil.tif", overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

    index <- read.csv(tclvalue(index_var), header = FALSE, sep = ",")
    cn <- classify(landsoil, index, include.lowest=TRUE, right=FALSE)
    writeRaster(cn, filename = "./cn_amc_ii.tif", overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

    cn_fact <- as.factor(cn)
    cols <- rainbow(nrow(cn_fact))

    dev.new()
    plot(cn_fact, col=cols, main="Curve numbers AMC II",  xlab="Easting", ylab="Northing")

    P <- as.numeric(tclvalue(rainfall_var))

    S <- ((1000/cn) - 10)
    S05 <- (1.33*(S^1.15))
    Ia <- (0.05*S05)

    msc <- classify(Ia, c(-Inf, P, 1, P, Inf, 0), include.lowest=FALSE, right=TRUE)

    Q <- ((P-(0.05*S05))^2)/(P+(0.95*S05))

    q_depth <- (Q * msc)

    dev.new()
    plot(q_depth, col = rainbow(100), main="Runoff depth (inch)",  xlab="Easting", ylab="Northing")

    resolution <- as.numeric(tclvalue(area_var))
    area <- (resolution * resolution)

    runoff <- round((q_depth * 0.0254 * area), 2)

    dev.new()
    plot(runoff, col = topo.colors(100), main="Runoff volume in cubic meters",  xlab="Easting", ylab="Northing")

    add_time <- format(Sys.time(),'_%Y%m%d_%H%M%S')
    writeRaster(q_depth, filename = paste("./Hawkins_Q_depth_inch",add_time,".tif"), overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')
    writeRaster(runoff, filename = paste("./Hawkins_Runoff_m3",add_time,".tif"), overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

    path <- getwd()
    datetime <- format(Sys.time(),'_%Y%m%d_%H%M%S')
    vol <- round((global(runoff, 'sum', na.rm=TRUE)), 2)

    write.table(paste("Total runoff volume with the modified NRCS-CN method = ", vol, "m^3  \n\n The parameters used were: \n Precipitation = ", P, "inches \n Landuse File: ", landuse_file, "\n HSG File: ", hsg_file, "\n CN index database: ", index_file, "\n Pixel size = ", resolution, "meters\n\n sara4r"),
                file = paste(path, "/Hawkins_Runoff_Vol", datetime, '.txt', sep = ""),
                row.names = FALSE, col.names = FALSE, quote = FALSE)
    tkmessageBox(message=paste("Outputs were stored in\n",
                               "the working folder ", path, " \n\n",
                               "Total runoff volume with the modified NRCS-CN method is = ", vol, "m^3"))

  }

  hawkinsP_calc <- function() {
    landuse_file <- tclvalue(land_var)
    hsg_file <- tclvalue(hsg_var)
    index_file <- tclvalue(index_var)
    rainfall_file <- tclvalue(pimage_var)

    landuse <- rast(landuse_file)
    soil <- rast(hsg_file)

    landsoil <- (landuse + soil)
    landsoil_fact <- as.factor(landsoil)
    cols <- topo.colors(nrow(landsoil_fact))
    dev.new()
    plot(landsoil_fact, col=cols, main= "Landsoil classes", xlab="Easting", ylab="Northing")

    writeRaster(landsoil, filename = "./landsoil.tif", overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

    index <- read.csv(tclvalue(index_var), header = FALSE, sep = ",")
    cn <- classify(landsoil, index, include.lowest=TRUE, right=FALSE)
    writeRaster(cn, filename = "./cn_amc_ii.tif", overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

    cn_fact <- as.factor(cn)
    cols <- rainbow(nrow(cn_fact))
    dev.new()
    plot(cn_fact, col=cols, main="Curve numbers AMC II",  xlab="Easting", ylab="Northing")

    P <- rast(rainfall_file)

    S <- ((1000/cn) - 10)
    S05 <- (1.33*(S^1.15))

    Ia <- (0.05*S05)

    PIa <- (P-Ia)


    msc <- classify(PIa, c(-Inf, 0, 0, 0, Inf, 1), include.lowest=FALSE, right=TRUE)

    Q <- ((P-(0.05*S05))^2)/(P+(0.95*S05))

    q_depth <- (Q * msc)

    dev.new()
    plot(q_depth, col = rainbow(100), main="Runoff depth (inch)",  xlab="Easting", ylab="Northing")

    resolution <- as.numeric(tclvalue(area_var))
    area <- (resolution * resolution)

    runoff <- round((q_depth * 0.0254 * area), 2)

    dev.new()
    plot(runoff, col = topo.colors(100), main="Runoff volume in cubic meters",  xlab="Easting", ylab="Northing")

    add_time <- format(Sys.time(),'_%Y%m%d_%H%M%S')
    writeRaster(q_depth, filename = paste("./HawkinsP_Q_depth_inch",add_time,".tif"), overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')
    writeRaster(runoff, filename = paste("./HawkinsP_Runoff_m3",add_time,".tif"), overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

    path <- getwd()
    datetime <- format(Sys.time(),'_%Y%m%d_%H%M%S')
    vol <- round((global(runoff, 'sum', na.rm=TRUE)), 2)

    write.table(paste("Total runoff volume with the modified NRCS-CN method = ", vol, "m^3  \n\n The parameters used were: \n Landuse File: ", landuse_file, "\n HSG File: ", hsg_file, "\n CN index database: ", index_file, "\n Precipitation = ", P, "\n Pixel size = ", resolution, "meters\n\n sara4r"),
                file = paste(path, "/HawkinsP_Runoff_Vol", datetime, '.txt', sep = ""),
                row.names = FALSE, col.names = FALSE, quote = FALSE)
    tkmessageBox(message=paste("Outputs were stored in\n",
                               "the working folder ", path, " \n\n",
                               "Total runoff volume with the modified NRCS-CN method is = ", vol, "m^3"))

  }

  "about" <- function(show, history)
  {
    about_win <- tktoplevel()
    tkwm.title(about_win, "sara4r v0.1.0")

    TFrame <- tkframe(about_win, relief= "groove")
    tkgrid(tklabel(TFrame,text= "Intentionally written in Spanish", foreground= "blue"))
    tkgrid(TFrame)

    tkgrid(tklabel(about_win, text = "    "))

    tkgrid(tklabel(about_win, text = " Con todo mi amor para mi hija, "))
    tkgrid(tklabel(about_win, text = " la personita que me inspira"))
    tkgrid(tklabel(about_win, text = "    "))
    tkgrid(tklabel(about_win, text = " Sara", font= "Times 18", foreground= "blue"))
    tkgrid(tklabel(about_win, text = "    "))
    tkgrid(tklabel(about_win, text = " https://hydro-geomatic-lab.com/  "))
    tkgrid(tklabel(about_win, text = " Veirus Software  "), sticky = "e")
    tkgrid(tklabel(about_win, text = "    "))

  }

  "set_work" <- function(show, history)
  {
    set_win <- tktoplevel()
    tkwm.title(set_win, "sara4r v0.1.0 - Set Working Directory")

    dfvar <- tclVar("")

    TFrame <- tkframe(set_win, relief= "groove")
    tkgrid(tklabel(TFrame, text= "Set Working Directory", foreground= "blue"))
    tkgrid(TFrame)

    IOFrame <- tkframe(set_win, relief= "groove", borderwidth= 2)
    v.folder <- tk2entry(IOFrame, width= 50, font= "Times 12", foreground= "blue", textvariable=dfvar, state = c("readonly"))
    empty.label <- tklabel(IOFrame, width= 2)
    choose.but <- tk2button(IOFrame, text= " Browse ", default= "active", command = function() browse_folder())
    tkgrid(tklabel(IOFrame,text= "Input folder name: ", foreground= "blue"), v.folder, empty.label, choose.but, sticky= "w")
    tkgrid(IOFrame)

    reset <- function()
    {
      tclvalue(dfvar) <- ""
    }

    browse_folder <- function()
    {
      setwd('~')

      foldername <- tclvalue(tkchooseDirectory())
      if (!nchar(foldername)) {
        tclvalue(dfvar) <- ""
      } else {
        tclvalue(dfvar) <- foldername
      }

    }

    set_folder <- function()
    {
      folder <- tk2entry(set_win, textvariable=dfvar)
      setwd(tclvalue(dfvar))
      tkmessageBox(message = paste( "The selected folder was ", tclvalue(dfvar)))
      tkdestroy(set_win)
    }

    RFrame <- tkframe(set_win, relief= "groove")
    tkgrid(tklabel(RFrame, text = "    "))
    reset.but <- tk2button(RFrame, text= "  Clear  ", command= reset)
    submit.but <- tk2button(RFrame, text= "  Submit  ", command = function() set_folder())
    cancel.but <- tk2button(RFrame, text= "  Dismiss  ", command = function()  tkdestroy(set_win))
    tkgrid(reset.but, submit.but, cancel.but, ipadx= 20)
    tkgrid(tklabel(RFrame, text = "    "))
    tkgrid(RFrame)

  }


  "cnii" <- function(show, history)
  {

    cnii_win <- tktoplevel()
    tkwm.title(cnii_win, "sara4r v0.1.0 - Get the Curve Number map for the AMC II")

    tclvalue(land_var) <- ""
    tclvalue(hsg_var) <- ""
    tclvalue(index_var) <- ""

    ini_Frame <- tkframe(cnii_win, relief= "groove")
    tkgrid(tklabel(ini_Frame, text = "    "))
    tkgrid(tklabel(ini_Frame, text= "   Parameters:   ", foreground="blue"))
    tkgrid(ini_Frame)

    CNII_Frame <- tkframe(cnii_win, relief= "groove", borderwidth= 2)
    landuse.entry <- tk2entry(CNII_Frame,
                              width= 50,
                              font= "Times 12",
                              foreground= "blue",
                              textvariable= land_var,
                              state = c("readonly"))

    land.btn <- tk2button(CNII_Frame,
                          tip="Please, select the Land use and land cover map.",
                          text=" Browse ",
                          default= "active",
                          command=function() landuse())

    hsg.entry <- tk2entry(CNII_Frame,
                          width=50,
                          font= "Times 12",
                          foreground= "blue",
                          textvariable= hsg_var,
                          state = c("readonly"))

    hsg.btn <- tk2button(CNII_Frame,
                         tip="Please, select the Hydrologic Soil Group map.",
                         text=" Browse ",
                         command=function() hsg())

    index.entry <- tk2entry(CNII_Frame,
                            width= 50,
                            font= "Times 12",
                            foreground= "blue",
                            textvariable= index_var,
                            state = c("readonly"))

    index.btn <- tk2button(CNII_Frame,
                           tip="Please, select the Curve Number index file.",
                           text=" Browse ",
                           command=function() indexfile())

    tkgrid(tklabel(CNII_Frame, text = "    "))
    tkgrid(tklabel(CNII_Frame,text= "Landuse:    ", foreground= "blue"), landuse.entry, land.btn, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(CNII_Frame,text= "HSG:        ", foreground= "blue"), hsg.entry, hsg.btn, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(CNII_Frame,text= "index file: ", foreground= "blue"), index.entry, index.btn, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(CNII_Frame, text = "    "))
    tkgrid(CNII_Frame)

    btn.cnii_frame <- tkframe(cnii_win, relief= "groove")
    btn.cnii.calc <- tk2button(btn.cnii_frame,
                               tip="This process takes a while. Please, wait until the program finished.",
                               text=" Calculate CN II ",
                               command=function() cnii_calc())
    btn.cnii.clear <- tk2button(btn.cnii_frame,
                               tip="Click here if you want to clear the 'Landuse, HSG and index' data entries.",
                               text=" Clear ",
                               command=function() veirus_clear())
    btn.cnii.cancel <- tk2button(btn.cnii_frame,
                                text= "  Dismiss  ",
                                command = function() tkdestroy(cnii_win))

    tkgrid(tklabel(btn.cnii_frame, text = "    "))
    tkgrid(btn.cnii.calc, btn.cnii.clear, btn.cnii.cancel, ipadx= 20)
    tkgrid(tklabel(btn.cnii_frame, text = "    "))
    tkgrid(btn.cnii_frame)

  }


  "cni" <- function(show, history)
  {
    cni_win <- tktoplevel()
    tkwm.title(cni_win, "sara4r v0.1.0 - Get the Curve Number map for the AMC I")

    tclvalue(cn1_var)  <- ""

    reset <- function()
    {
      tclvalue(cn1_var) <- ""
    }

    curve_numbers_II <- function()
    {
      cnii_file <- tclvalue(tkgetOpenFile(
        filetypes = "{ {TIFF files (*.tif)} {.tif} } { {ASCII files (*.asc)} {.asc} }  { {All files} * }"))
      if (cnii_file == "")
        return(data.frame())
      tclvalue(cn1_var)<- cnii_file

      cn_plot <- rast(tclvalue(cn1_var))
       cn_fact <- as.factor(cn_plot)
       cols <- rainbow(nrow(cn_fact))
      dev.new()
      plot(cn_fact, col=cols, main="Curve number classes", xlab="Easting", ylab="Northing")

    }

    sob_cni <- function()
    {
      cn <- rast(tclvalue(cn1_var))

      cni <- round((cn/(2.334-(0.01334*cn))))
      writeRaster(cni, filename = "./cn_sobhani_amci.tif", overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

      cni_fact <- as.factor(cni)
      cols <- rainbow(nrow(cni_fact))
      dev.new()
      plot(cni_fact, col=cols, main="Curve numbers AMC I (Sobhani)",  xlab="Easting", ylab="Northing")

      path <- getwd()
      tkmessageBox(message=paste("cn_sobhani_amci.tif was stored in ", path, ""))

    }

    hwk_cni <- function()
    {
      cn <- rast(tclvalue(cn1_var))

      cni <- round((cn/(2.281-(0.01281*cn))))
      writeRaster(cni, filename = "./cn_hawkins_amci.tif", overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

      cni_fact <- as.factor(cni)
      cols <- rainbow(nrow(cni_fact))
      dev.new()
      plot(cni_fact, col=cols, main="Curve numbers AMC I (Hawkins)",  xlab="Easting", ylab="Northing")

      path <- getwd()
      tkmessageBox(message=paste("cn_hawkins_amci.tif was stored in ", path, ""))

    }

    chow_cni <- function()
    {
      cn <- rast(tclvalue(cn1_var))

      cni <- round((4.2*cn/(10-(0.058*cn))))
      writeRaster(cni, filename = "./cn_chow_amci.tif", overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

      cni_fact <- as.factor(cni)
      cols <- rainbow(nrow(cni_fact))
      dev.new()
      plot(cni_fact, col=cols, main="Curve numbers AMC I (Chow)",  xlab="Easting", ylab="Northing")

      path <- getwd()
      tkmessageBox(message=paste("cn_chow_amci.tif was stored in ", path, ""))

    }

    mish_cni <- function()
    {

      cn <- rast(tclvalue(cn1_var))

      cni <- round((cn/(2.2754-(0.012754*cn))))
      writeRaster(cni, filename = "./cn_mishra_amci.tif", overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

      cni_fact <- as.factor(cni)
      cols <- rainbow(nrow(cni_fact))
      dev.new()
      plot(cni_fact, col=cols, main="Curve numbers AMC I (Mishra)",  xlab="Easting", ylab="Northing")

      path <- getwd()
      tkmessageBox(message=paste("cn_mishra_amci.tif was stored in ", path, ""))

    }

    ini_Frame <- tkframe(cni_win, relief= "groove")
    tkgrid(tklabel(ini_Frame, text= "Please, select the CN II map", foreground= "blue"))
    tkgrid(ini_Frame)

    CNI_Frame <- tkframe(cni_win, relief= "groove", borderwidth= 2)
    cn.entry <- tk2entry(CNI_Frame,
                         width= 50,
                         font= "Times 12",
                         foreground= "blue",
                         textvariable=cn1_var,
                         state = c("readonly"))

    cn.btn <- tk2button(CNI_Frame,
                        tip="Please, select the Curve Number map for the AMC II",
                        text=" Browse ",
                        default= "active",
                        command=function() curve_numbers_II())

    tkgrid(tklabel(CNI_Frame, text= "CN II map: ", foreground= "blue"), cn.entry, cn.btn, pady= 5, padx= 10, sticky= "w")
    tkgrid(CNI_Frame)

    method_Frame <- tkframe(cni_win, relief= "groove")
    tkgrid(tklabel(method_Frame, text = "    "))
    tkgrid(tklabel(method_Frame, text= "Please, select a method to calculate CN I", foreground= "blue"))
    tkgrid(method_Frame)

    out_frame <- tkframe(cni_win, relief= "groove")
    btn.sob <- tk2button(out_frame, text = "  Sobhani 1975  ", command = sob_cni)
    btn.haw <- tk2button(out_frame, text = "  Hawkins et al 1985  ", command = hwk_cni)
    btn.cho <- tk2button(out_frame, text = "  Chow et al 1988  ", command = chow_cni)
    btn.mis <- tk2button(out_frame, text = "  Mishra et al 2008  ", command = mish_cni)

    tkgrid(btn.sob, btn.haw, btn.cho, btn.mis, ipadx= 20)
    tkgrid(tklabel(out_frame, text = "    "))
    tkgrid(out_frame)

  }


  "cniii" <- function(show, history)
  {
    cniii_win <- tktoplevel()
    tkwm.title(cniii_win, "sara4r v0.1.0 - Get the Curve Number map for the AMC III")

    tclvalue(cn3_var) <- ""

    reset <- function()
    {
      tclvalue(cn3_var) <- ""
    }


    curve_numbers_II <- function()
    {
      cnii_file <- tclvalue(tkgetOpenFile(
        filetypes = "{ {TIFF files (*.tif)} {.tif} } { {ASCII files (*.asc)} {.asc} }  { {All files} * }"))
      if (cnii_file == "")
        return(data.frame())
      tclvalue(cn3_var) <- cnii_file

      cn_plot <- rast(tclvalue(cn3_var))
      cn_fact <- as.factor(cn_plot)
      cols <- rainbow(nrow(cn_fact))
      dev.new()
      plot(cn_fact, col=cols, main="Curve number classes", xlab="Easting", ylab="Northing")

    }
##
    sob_cniii <- function()
    {
      cn <- rast(tclvalue(cn3_var))

      cni <- round((cn/(0.4036+(0.005964*cn))))
      writeRaster(cni, filename = "./cn_sobhani_amciii.tif", overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

      cni_fact <- as.factor(cni)
      cols <- rainbow(nrow(cni_fact))
      dev.new()
      plot(cni_fact, col=cols, main="Curve numbers AMC III (Sobhani)",  xlab="Easting", ylab="Northing")

      path <- getwd()
      tkmessageBox(message=paste("cn_sobhani_amciii.tif was stored in ", path, ""))

    }

    hwk_cniii <- function()
    {
      cn <- rast(tclvalue(cn3_var))

      cni <- round((cn/(0.427+(0.00573*cn))))
      writeRaster(cni, filename = "./cn_hawkins_amciii.tif", overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

      cni_fact <- as.factor(cni)
      cols <- rainbow(nrow(cni_fact))
      dev.new()
      plot(cni_fact, col=cols, main="Curve numbers AMC III (Hawkins)",  xlab="Easting", ylab="Northing")

      path <- getwd()
      tkmessageBox(message=paste("cn_hawkins_amciii.tif was stored in ", path, ""))

    }

    chow_cniii <- function()
    {
      cn <- rast(tclvalue(cn3_var))

      cni <- round((23*cn/(10+(0.13*cn))))
      writeRaster(cni, filename = "./cn_chow_amciii.tif", overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

      cni_fact <- as.factor(cni)
      cols <- rainbow(nrow(cni_fact))
      dev.new()
      plot(cni_fact, col=cols, main="Curve numbers AMC III (Chow)",  xlab="Easting", ylab="Northing")

      path <- getwd()
      tkmessageBox(message=paste("cn_chow_amciii.tif was stored in ", path, ""))

    }

    mish_cniii <- function()
    {
      cn <- rast(tclvalue(cn3_var))

      cni <- round((cn/(0.43+(0.0057*cn))))
      writeRaster(cni, filename = "./cn_mishra_amciii.tif", overwrite=T, gdal=c("COMPRESS=LZW", "TFW=YES"), datatype='FLT4S')

      cni_fact <- as.factor(cni)
      cols <- rainbow(nrow(cni_fact))
      dev.new()
      plot(cni_fact, col=cols, main="Curve numbers AMC III (Mishra)",  xlab="Easting", ylab="Northing")

      path <- getwd()
      tkmessageBox(message=paste("cn_mishra_amciii.tif was stored in ", path, ""))

    }

    ini_Frame <- tkframe(cniii_win, relief= "groove")
    tkgrid(tklabel(ini_Frame, text= "Please, select the CN II map", foreground= "blue"))
    tkgrid(ini_Frame)

    CNIII_Frame <- tkframe(cniii_win, relief= "groove", borderwidth= 2)
    cn.entry <- tk2entry(CNIII_Frame,
                         width= 50,
                         font= "Times 12",
                         foreground= "blue",
                         textvariable=cn3_var,
                         state = c("readonly"))

    cn.btn <- tk2button(CNIII_Frame,
                        tip="Please, select the Curve Number map for the AMC II",
                        text=" Browse ",
                        default= "active",
                        command=function() curve_numbers_II())

    tkgrid(tklabel(CNIII_Frame, text= "CN II map: ", foreground= "blue"), cn.entry, cn.btn, pady= 5, padx= 10, sticky= "w")
    tkgrid(CNIII_Frame)

    method_Frame <- tkframe(cniii_win, relief= "groove")
    tkgrid(tklabel(method_Frame, text = "    "))
    tkgrid(tklabel(method_Frame, text= "Please, select a method to calculate CN III", foreground= "blue"))
    tkgrid(method_Frame)

    out_frame <- tkframe(cniii_win, relief= "groove")
    btn.sob <- tk2button(out_frame, text = "  Sobhani 1975  ", command = sob_cniii)
    btn.haw <- tk2button(out_frame, text = "  Hawkins et al 1985  ", command = hwk_cniii)
    btn.cho <- tk2button(out_frame, text = "  Chow et al 1988  ", command = chow_cniii)
    btn.mis <- tk2button(out_frame, text = "  Mishra et al 2008  ", command = mish_cniii)

    tkgrid(btn.sob, btn.haw, btn.cho, btn.mis, ipadx= 20)
    tkgrid(tklabel(out_frame, text = "    "))
    tkgrid(out_frame)

  }

  "q_volume" <- function(show, history)
  {
    qvol_win <- tktoplevel()
    tkwm.title(qvol_win, "sara4r v0.1.0 - Get Q-depth and runoff volume")

    tclvalue(cn_var) <- ""
    tclvalue(landsoil_var) <- ""

    ini_Frame <- tkframe(qvol_win, relief= "groove")
    tkgrid(tklabel(ini_Frame, text = "    "))
    tkgrid(tklabel(ini_Frame, text= "   Parameters:   ", foreground="blue"))
    tkgrid(ini_Frame)

    Q_Frame <- tkframe(qvol_win, relief= "groove", borderwidth= 2)

    cn.entry <- tk2entry(Q_Frame,
                         width= 50,
                         font= "Times 12",
                         foreground= "blue",
                         textvariable=cn_var,
                         state = c("readonly"))

    cn.btn <- tk2button(Q_Frame,
                        tip="Please, select the CN map according to the Antecedent Moisture Condition you want.",
                        text=" Browse ",
                        default= "active",
                        command=function() curvenumbers())

    landsoil.entry <- tk2entry(Q_Frame,
                               width=50,
                               font= "Times 12",
                               foreground= "blue",
                               textvariable= landsoil_var,
                               state = c("readonly"))

    landsoil.btn <- tk2button(Q_Frame,
                              tip="Please, select the Landsoil map created in the previous step.",
                              text=" Browse ",
                              command=function() landsoil())

    rainfall.entry <- tk2entry(Q_Frame,
                               tip="Please, provide the precipitation value in inches. Use a dot (.) as a decimal separator instead of a comma.",
                               width= 10,
                               font= "Times 12",
                               foreground= "blue",
                               textvariable= rainfall_var,
                               justify = c("center"))

    area.entry <- tk2entry(Q_Frame,
                           tip="Please, provide the pixel size in meters.",
                           width= 10,
                           font= "Times 12",
                           foreground= "blue",
                           textvariable= area_var,
                           justify = c("center"))

    tkgrid(tklabel(Q_Frame, text = "    "))
    tkgrid(tklabel(Q_Frame, text= "Curve numbers:       ", foreground= "blue"), cn.entry, cn.btn, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(Q_Frame, text= "Landsoil:            ", foreground= "blue"), landsoil.entry, landsoil.btn, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(Q_Frame, text= "Precipitation (in):  ", foreground= "blue"), rainfall.entry, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(Q_Frame, text= "Pixel size (m):      ", foreground= "blue"), area.entry, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(Q_Frame, text = "    "))
    tkgrid(Q_Frame)

    btn.qvol_frame <- tkframe(qvol_win, relief= "groove")
    qvol_calc.btn <- tk2button(btn.qvol_frame,
                               tip="This process takes a while. Please, wait until the program finished.",
                               text=" Calculate ",
                               command=function() qvol_calc())

    qvol_clear.btn <- tk2button(btn.qvol_frame,
                                tip="Click here if you want to clear the 'Curve numbers and Landsoil' data entries.",
                                text=" Clear ",
                                command=function() veirus_clear())

    qvol_cancel.btn <- tk2button(btn.qvol_frame, text= "  Dismiss  ", command = function() tkdestroy(qvol_win))

    tkgrid(tklabel(btn.qvol_frame, text = "    "))
    tkgrid(qvol_calc.btn, qvol_clear.btn, qvol_cancel.btn, ipadx= 20)
    tkgrid(tklabel(btn.qvol_frame, text = "    "))
    tkgrid(btn.qvol_frame)

  }

  "usda" <- function(show, history)
  {
    usda_win <- tktoplevel()
    tkwm.title(usda_win, "sara4r v0.1.0 - Get CN and Runoff volume for the AMC II")

    tclvalue(land_var) <- ""
    tclvalue(hsg_var) <- ""
    tclvalue(index_var) <- ""

    usda_Frame <- tkframe(usda_win, relief= "groove")
    tkgrid(tklabel(usda_Frame, text = "    "))
    tkgrid(tklabel(usda_Frame, text= "   USDA (1986)   ", foreground="blue"))
    tkgrid(tklabel(usda_Frame, text = "    "))
    tkgrid(usda_Frame)

    usda_formula <- tkframe(usda_win, relief= "groove")
    tkgrid(tklabel(usda_formula, text = " (P - 0.2*S)^2  ", foreground="blue"))
    tkgrid(tklabel(usda_formula, text= "     Q =      ------------------,    P > 0.2*S ", foreground="blue"))
    tkgrid(tklabel(usda_formula, text = " (P + 0.8*S)  ", foreground="blue"))
    tkgrid(usda_formula)

    s_formula <- tkframe(usda_win, relief= "groove")
    tkgrid(tklabel(s_formula, text = "   1000  ", foreground="blue"))
    tkgrid(tklabel(s_formula, text= "   S =       ---------- - 10 (in)  ", foreground="blue"))
    tkgrid(tklabel(s_formula, text = "   CN  ", foreground="blue"))
    tkgrid(tklabel(s_formula, text = "    "))
    tkgrid(s_formula)


    ini_Frame <- tkframe(usda_win, relief= "groove")
    tkgrid(tklabel(ini_Frame, text= "   Parameters:   ", foreground="blue"))
    tkgrid(ini_Frame)

    USDA_Frame <- tkframe(usda_win, relief= "groove", borderwidth= 2)
    usda_landuse.entry <- tk2entry(USDA_Frame,
                              width= 50,
                              font= "Times 12",
                              foreground= "blue",
                              textvariable=land_var,
                              state = c("readonly"))

    land.btn <- tk2button(USDA_Frame,
                          tip="Please, select the Land use and land cover map.",
                          text=" Browse ",
                          default= "active",
                          command=function() landuse())

    usda_hsg.entry <- tk2entry(USDA_Frame,
                          width= 50,
                          font= "Times 12",
                          foreground= "blue",
                          textvariable= hsg_var,
                          state = c("readonly"))

    hsg.btn <- tk2button(USDA_Frame,
                         tip="Please, select the Hydrologic Soil Groups map.",
                         text=" Browse ",
                         command=function() hsg())

    usda_index.entry <- tk2entry(USDA_Frame,
                            width= 50,
                            font= "Times 12",
                            foreground= "blue",
                            textvariable= index_var,
                            state = c("readonly"))

    index.btn <- tk2button(USDA_Frame,
                           tip="Please, select the Curve Numbers index file.",
                           text=" Browse ",
                           command=function() indexfile())

    rainfall.entry <- tk2entry(USDA_Frame,
                               tip="Please, insert the precipitation value in inches.",
                               width= 10,
                               font= "Times 12",
                               foreground= "blue",
                               textvariable= rainfall_var,
                               justify = c("center"))

    area.entry <- tk2entry(USDA_Frame,
                           tip="Please, insert the pixel size in meters",
                           width= 10,
                           font= "Times 12",
                           foreground= "blue",
                           textvariable= area_var,
                           justify = c("center"))

    tkgrid(tklabel(USDA_Frame, text = "    "))
    tkgrid(tklabel(USDA_Frame ,text= "Landuse:    ", foreground= "blue"), usda_landuse.entry, land.btn, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(USDA_Frame, text= "HSG:        ", foreground= "blue"), usda_hsg.entry, hsg.btn, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(USDA_Frame, text= "index file: ", foreground= "blue"), usda_index.entry, index.btn, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(USDA_Frame, text= "Precipitation (in): ", foreground= "blue"), rainfall.entry, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(USDA_Frame, text= "Pixel size (m): ",  foreground= "blue"), area.entry, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(USDA_Frame, text = "    "))
    tkgrid(USDA_Frame)

    btn.usda_frame <- tkframe(usda_win, relief= "groove")
    usda_calc.btn <- tk2button(btn.usda_frame,
                               tip="This process takes a while. Please, wait until the program finished.",
                               text=" Calculate ",
                               command=function() usda_calc())
    usda_clear.btn <- tk2button(btn.usda_frame,
                                tip="Click here if you want to clear the 'Landuse, HSG and index' data entries.",
                                text=" Clear ",
                                command=function() veirus_clear())
    usda_cancel.btn <- tk2button(btn.usda_frame, text= "  Dismiss  ", command = function() tkdestroy(usda_win))

    tkgrid(tklabel(btn.usda_frame, text = "    "))
    tkgrid(usda_calc.btn, usda_clear.btn, usda_cancel.btn, ipadx= 20)
    tkgrid(tklabel(btn.usda_frame, text = "    "))
    tkgrid(btn.usda_frame)

  }

  "usdaP" <- function(show, history)
  {
    usdap_win <- tktoplevel()
    tkwm.title(usdap_win, "sara4r v0.1.0 - Get CN and Runoff volume using a Precipitation image")

    tclvalue(cn_var) <- ""
    tclvalue(landsoil_var) <- ""
    tclvalue(pimage_var) <- ""

    usdap_Frame <- tkframe(usdap_win, relief= "groove")
    tkgrid(tklabel(usdap_Frame, text = "    "))
    tkgrid(tklabel(usdap_Frame, text= "   USDA (1986)   ", foreground="blue"))
    tkgrid(tklabel(usdap_Frame, text = "    "))
    tkgrid(usdap_Frame)

    usdap_formula <- tkframe(usdap_win, relief= "groove")
    tkgrid(tklabel(usdap_formula, text = " (P - 0.2*S)^2  ", foreground="blue"))
    tkgrid(tklabel(usdap_formula, text= "     Q =      ------------------,    P > 0.2*S ", foreground="blue"))
    tkgrid(tklabel(usdap_formula, text = " (P + 0.8*S)  ", foreground="blue"))
    tkgrid(usdap_formula)

    s_formula <- tkframe(usdap_win, relief= "groove")
    tkgrid(tklabel(s_formula, text = "   1000  ", foreground="blue"))
    tkgrid(tklabel(s_formula, text= "   S =       ---------- - 10 (in)  ", foreground="blue"))
    tkgrid(tklabel(s_formula, text = "   CN  ", foreground="blue"))
    tkgrid(tklabel(s_formula, text = "    "))
    tkgrid(s_formula)


    ini_Frame <- tkframe(usdap_win, relief= "groove")
    tkgrid(tklabel(ini_Frame, text= "   Parameters:   ", foreground="blue"))
    tkgrid(ini_Frame)

    USDAP_Frame <- tkframe(usdap_win, relief= "groove", borderwidth= 2)
    cn.entry <- tk2entry(USDAP_Frame,
                         width= 50,
                         font= "Times 12",
                         foreground= "blue",
                         textvariable=cn_var,
                         state = c("readonly"))

    cn.but <- tk2button(USDAP_Frame,
                        tip="Please, select the Curve Number map.",
                        text=" Browse ",
                        default= "active",
                        command=function() curvenumbers())

    landsoil.entry <- tk2entry(USDAP_Frame,
                               width= 50,
                               font= "Times 12",
                               foreground= "blue",
                               textvariable= landsoil_var,
                               state = c("readonly"))

    landsoil.but <- tk2button(USDAP_Frame,
                              tip="Please, select the Landsoil map created in previous steps.",
                              text=" Browse ",
                              command=function() landsoil())

    rainfall.entry <- tk2entry(USDAP_Frame,
                               width= 50,
                               font= "Times 12",
                               foreground= "blue",
                               textvariable= pimage_var,
                               state = c("readonly"))

    rainfall.but <- tk2button(USDAP_Frame,
                              tip="Please, select the Precipitation map.",
                              text=" Browse ",
                              command=function() rainfile())

    area.entry <- tk2entry(USDAP_Frame,
                           tip="Please, insert the pixel size in meters",
                           width= 10,
                           font= "Times 12",
                           foreground= "blue",
                           textvariable= area_var,
                           justify = c("center"))

    tkgrid(tklabel(USDAP_Frame, text = "    "))
    tkgrid(tklabel(USDAP_Frame, text= "Curve Number Map:      ", foreground= "blue"), cn.entry, cn.but, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(USDAP_Frame, text= "Landsoil map:          ", foreground= "blue"), landsoil.entry, landsoil.but, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(USDAP_Frame, text= "Precipitation Map (in):", foreground= "blue"), rainfall.entry, rainfall.but, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(USDAP_Frame, text= "Pixel size (m):        ",  foreground= "blue"), area.entry, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(USDAP_Frame, text = "    "))
    tkgrid(USDAP_Frame)

    btn.usdap_frame <- tkframe(usdap_win, relief= "groove")
    usdap_calc.btn <- tk2button(btn.usdap_frame,
                                tip="This process takes a while. Please, wait until the program finished.",
                                text=" Calculate ",
                                command=function() usdaP_calc())
    usdap_clear.btn <- tk2button(btn.usdap_frame,
                                 tip="Click here if you want to clear the 'CN, Landsoil and Precipitation' data entries.",
                                 text=" Clear ",
                                 command=function() veirus_clear())
    usdap_cancel.btn <- tk2button(btn.usdap_frame, text= "  Dismiss  ", command = function() tkdestroy(usdap_win))

    tkgrid(tklabel(btn.usdap_frame, text = "    "))
    tkgrid(usdap_calc.btn, usdap_clear.btn, usdap_cancel.btn, ipadx= 20)
    tkgrid(tklabel(btn.usdap_frame, text = "    "))
    tkgrid(btn.usdap_frame)

  }


  "hawkins" <- function(show, history)
  {
    hawkins_win <- tktoplevel()
    tkwm.title(hawkins_win, "sara4r v0.1.0 - Get CN and Runoff volume with the Modified NRCS-CN method")

    tclvalue(land_var) <- ""
    tclvalue(hsg_var) <- ""
    tclvalue(index_var) <- ""

    hawk_Frame <- tkframe(hawkins_win, relief= "groove")
    tkgrid(tklabel(hawk_Frame, text = "    "))
    tkgrid(tklabel(hawk_Frame, text= "   Hawkins et al. (2002)   ", foreground="blue"))
    tkgrid(tklabel(hawk_Frame, text = "    "))
    tkgrid(hawk_Frame)

    hawkins_formula <- tkframe(hawkins_win, relief= "groove")
    tkgrid(tklabel(hawkins_formula, text = " (P - 0.05*S05)^2  ", foreground="blue"))
    tkgrid(tklabel(hawkins_formula, text= "     Q =      ------------------,    P > 0.05*S ", foreground="blue"))
    tkgrid(tklabel(hawkins_formula, text = " (P + 0.95*S05)  ", foreground="blue"))
    tkgrid(hawkins_formula)

    sh_formula <- tkframe(hawkins_win, relief= "groove")
    tkgrid(tklabel(sh_formula, text = "    "))
    tkgrid(tklabel(sh_formula, text= "   S05 = 1.33*S^1.15 (in)  ", foreground="blue"))
    tkgrid(tklabel(sh_formula, text = "    "))
    tkgrid(sh_formula)


    ini_Frame <- tkframe(hawkins_win, relief= "groove")
    tkgrid(tklabel(ini_Frame, text= "   Parameters:   ", foreground="blue"))
    tkgrid(ini_Frame)

    HAWKINS_FRAME <- tkframe(hawkins_win, relief= "groove", borderwidth= 2)

    landuse.entry <- tk2entry(HAWKINS_FRAME,
                              width= 50,
                              font= "Times 12",
                              foreground= "blue",
                              textvariable=land_var,
                              state = c("readonly"))

    land.but <- tk2button(HAWKINS_FRAME,
                          tip="Please, select the Land use and land cover map.",
                          text=" Browse ",
                          default= "active",
                          command=function() landuse())

    hsg.entry <- tk2entry(HAWKINS_FRAME,
                          width= 50,
                          font= "Times 12",
                          foreground= "blue",
                          textvariable= hsg_var,
                          state = c("readonly"))

    hsg.but <- tk2button(HAWKINS_FRAME,
                         tip="Please, select the Hydrologic Soil Groups map.",
                         text=" Browse ",
                         command=function() hsg())

    index.entry <- tk2entry(HAWKINS_FRAME,
                            width= 50,
                            font= "Times 12",
                            foreground= "blue",
                            textvariable= index_var,
                            state = c("readonly"))

    index.but <- tk2button(HAWKINS_FRAME,
                           tip="Please, select the Curve Numbers index file.",
                           text=" Browse ",
                           command=function() indexfile())

    rainfall.entry <- tk2entry(HAWKINS_FRAME,
                               tip="Please, insert the precipitation value in inches.",
                               width= 10,
                               font= "Times 12",
                               foreground= "blue",
                               textvariable= rainfall_var,
                               justify = c("center"))

    area.entry <- tk2entry(HAWKINS_FRAME,
                           tip="Please, insert the pixel size in meters",
                           width= 10,
                           font= "Times 12",
                           foreground= "blue",
                           textvariable= area_var,
                           justify = c("center"))

    tkgrid(tklabel(HAWKINS_FRAME, text = "    "))
    tkgrid(tklabel(HAWKINS_FRAME, text= "Landuse:    ", foreground= "blue"), landuse.entry, land.but, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(HAWKINS_FRAME, text= "HSG:        ", foreground= "blue"), hsg.entry, hsg.but, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(HAWKINS_FRAME, text= "index file: ", foreground= "blue"), index.entry, index.but, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(HAWKINS_FRAME, text= "Precipitation (in): ", foreground= "blue"), rainfall.entry, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(HAWKINS_FRAME, text= "Pixel size (m): ",  foreground= "blue"), area.entry, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(HAWKINS_FRAME, text = "    "))
    tkgrid(HAWKINS_FRAME)

    btn.haw_frame <- tkframe(hawkins_win, relief= "groove")
    haw_calc.btn <- tk2button(btn.haw_frame,
                              tip="This process takes a while. Please, wait until the program finished.",
                              text=" Calculate ",
                              command=function() hawkins_calc())
    haw_clear.btn <- tk2button(btn.haw_frame,
                               tip="Click here if you want to clear the 'Landuse, HSG and index' data entries.",
                               text=" Clear ",
                               command=function() veirus_clear())
    haw_cancel.btn <- tk2button(btn.haw_frame, text= "  Dismiss  ", command = function() tkdestroy(hawkins_win))

    tkgrid(tklabel(btn.haw_frame, text = "    "))
    tkgrid(haw_calc.btn, haw_clear.btn, haw_cancel.btn, ipadx= 20)
    tkgrid(tklabel(btn.haw_frame, text = "    "))
    tkgrid(btn.haw_frame)

  }

  "hawkinsP" <- function(show, history)
  {
    hawkinsp_win <- tktoplevel()
    tkwm.title(hawkinsp_win, "sara4r v0.1.0 - Get CN and Runoff volume with the Modified NRCS-CN method")

    tclvalue(land_var) <- ""
    tclvalue(hsg_var) <- ""
    tclvalue(index_var) <- ""
    tclvalue(pimage_var) <- ""

    hawkp_Frame <- tkframe(hawkinsp_win, relief= "groove")
    tkgrid(tklabel(hawkp_Frame, text = "    "))
    tkgrid(tklabel(hawkp_Frame, text= "   Hawkins et al. (2002)   ", foreground="blue"))
    tkgrid(tklabel(hawkp_Frame, text = "    "))
    tkgrid(hawkp_Frame)

    hawkins_formula <- tkframe(hawkinsp_win, relief= "groove")
    tkgrid(tklabel(hawkins_formula, text = " (P - 0.05*S05)^2  ", foreground="blue"))
    tkgrid(tklabel(hawkins_formula, text= "     Q =      ------------------,    P > 0.05*S ", foreground="blue"))
    tkgrid(tklabel(hawkins_formula, text = " (P + 0.95*S05)  ", foreground="blue"))
    tkgrid(hawkins_formula)

    sh_formula <- tkframe(hawkinsp_win, relief= "groove")
    tkgrid(tklabel(sh_formula, text = "    "))
    tkgrid(tklabel(sh_formula, text= "   S05 = 1.33*S^1.15 (in)  ", foreground="blue"))
    tkgrid(tklabel(sh_formula, text = "    "))
    tkgrid(sh_formula)


    ini_Frame <- tkframe(hawkinsp_win, relief= "groove")

    tkgrid(tklabel(ini_Frame, text= "   Parameters:   ", foreground="blue"))
    tkgrid(ini_Frame)

    HAWKINSP_FRAME <- tkframe(hawkinsp_win, relief= "groove", borderwidth= 2)

    landuse.entry <- tk2entry(HAWKINSP_FRAME,
                              width= 50,
                              font= "Times 12",
                              foreground= "blue",
                              textvariable=land_var,
                              state = c("readonly"))

    land.but <- tk2button(HAWKINSP_FRAME,
                          tip="Please, select the Land use and land cover map.",
                          text=" Browse ",
                          default= "active",
                          command=function() landuse())

    hsg.entry <- tk2entry(HAWKINSP_FRAME,
                          width= 50,
                          font= "Times 12",
                          foreground= "blue",
                          textvariable= hsg_var,
                          state = c("readonly"))

    hsg.but <- tk2button(HAWKINSP_FRAME,
                         tip="Please, select the Hydrologic Soil Groups map.",
                         text=" Browse ",
                         command=function() hsg())

    index.entry <- tk2entry(HAWKINSP_FRAME,
                            width= 50,
                            font= "Times 12",
                            foreground= "blue",
                            textvariable= index_var,
                            state = c("readonly"))

    index.but <- tk2button(HAWKINSP_FRAME,
                           tip="Please, select the Curve Numbers index file.",
                           text=" Browse ",
                           command=function() indexfile())

    rainfall.entry <- tk2entry(HAWKINSP_FRAME,
                               width= 50,
                               font= "Times 12",
                               foreground= "blue",
                               textvariable= pimage_var,
                               state = c("readonly"))

    rainfall.but <- tk2button(HAWKINSP_FRAME,
                              tip="Please, select the Precipitation map.",
                              text=" Browse ",
                              command=function() rainfile())

    area.entry <- tk2entry(HAWKINSP_FRAME,
                           tip="Please, insert the pixel size in meters",
                           width= 10,
                           font= "Times 12",
                           foreground= "blue",
                           textvariable= area_var,
                           justify = c("center"))

    tkgrid(tklabel(HAWKINSP_FRAME, text = "    "))
    tkgrid(tklabel(HAWKINSP_FRAME, text= "Landuse:    ", foreground= "blue"), landuse.entry, land.but, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(HAWKINSP_FRAME, text= "HSG:        ", foreground= "blue"), hsg.entry, hsg.but, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(HAWKINSP_FRAME, text= "index file: ", foreground= "blue"), index.entry, index.but, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(HAWKINSP_FRAME, text= "Precipitation Map (in):", foreground= "blue"), rainfall.entry, rainfall.but, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(HAWKINSP_FRAME, text= "Pixel size (m): ",  foreground= "blue"), area.entry, pady= 5, padx= 10, sticky= "w")
    tkgrid(tklabel(HAWKINSP_FRAME, text = "    "))
    tkgrid(HAWKINSP_FRAME)

    btn.hawp_frame <- tkframe(hawkinsp_win, relief= "groove")
    haw_calc.btn <- tk2button(btn.hawp_frame,
                              tip="This process takes a while. Please, wait until the program finished.",
                              text=" Calculate ",
                              command=function() hawkinsP_calc())
    haw_clear.btn <- tk2button(btn.hawp_frame,
                               tip="Click here if you want to clear the 'Landuse, HSG, index and Precipitation' data entries.",
                               text=" Clear ",
                               command=function() veirus_clear())
    haw_cancel.btn <- tk2button(btn.hawp_frame, text= "  Dismiss  ", command = function() tkdestroy(hawkinsp_win))

    tkgrid(tklabel(btn.hawp_frame, text = "    "))
    tkgrid(haw_calc.btn, haw_clear.btn, haw_cancel.btn, ipadx= 20)
    tkgrid(tklabel(btn.hawp_frame, text = "    "))
    tkgrid(btn.hawp_frame)

  }

  runoff_win <- tktoplevel()
  tktitle(runoff_win) <- "sara4r v0.1.0"

  menu_frame <- tkframe(runoff_win, relief= "groove")
  btn.dir <- tk2button(menu_frame, text = " Set working dir ",  default = "active", command = set_work)
  btn.about <- tk2button(menu_frame, text = " About ", command = about)
  tkgrid(btn.dir, btn.about)
  tkgrid(menu_frame)

  CNII_Frame <- tkframe(runoff_win, relief= "groove")
  tkgrid(tklabel(CNII_Frame, text = "    "))
  tkgrid(tklabel(CNII_Frame, text="   Get curve numbers for the AMC II   ", foreground="blue"))
  btn.cnii <- tk2button(CNII_Frame,
                          text = " CN II ",
                          command = cnii)

  tkgrid(btn.cnii)
  tkgrid(CNII_Frame)

  XFrame <- tkframe(runoff_win, relief= "groove")
  tkgrid(tklabel(XFrame, text = "    "))
  tkgrid(tklabel(XFrame, text= "   Get curve numbers (please select an AMC)   ", foreground="blue"))
  tkgrid(XFrame)

  cn_frame <- tkframe(runoff_win, relief= "groove", borderwidth= 2)

  btn.amci <- tk2button(cn_frame, text = "  Antecedent Moisture Condition I   ", command = cni)
  btn.amciii <- tk2button(cn_frame, text = "  Antecedent Moisture Condition III   ", command = cniii)

  tkgrid(tklabel(cn_frame, text = "    "))
  tkgrid(btn.amci, btn.amciii, ipadx= 20)
  tkgrid(tklabel(cn_frame, text = "    "))
  tkgrid(cn_frame)

  YFrame <- tkframe(runoff_win, relief= "groove")
  tkgrid(tklabel(YFrame, text = "    "))
  tkgrid(tklabel(YFrame, text="   Get Q-dept and Runoff volume   ", foreground="blue"))
  btn.runoff <- tk2button(YFrame,
                          tip="Please, use data generated in the previous step",
                          text = " Q-depth and Vol ",
                          command = q_volume)

  tkgrid(btn.runoff)
  tkgrid(YFrame)

  original_frame <- tkframe(runoff_win, relief= "groove")
  tkgrid(tklabel(original_frame, text = "    "))
  tkgrid(tklabel(original_frame, text="   Calculate runoff volume with the original method   ", foreground="blue"))
  btn.original <- tk2button(original_frame,
                            text = " USDA 1986 ",
                            command = usda)

  tkgrid(btn.original)
  tkgrid(original_frame)

  ZFrame <- tkframe(runoff_win, relief= "groove")
  tkgrid(tklabel(ZFrame, text = "    "))
  tkgrid(tklabel(ZFrame, text="   Special cases   ", foreground="blue"))
  tkgrid(ZFrame)

  misc_frame <- tkframe(runoff_win, relief= "groove", borderwidth= 2)

  Ia_img.btn <- tk2button(misc_frame, text = "  Ia = 0.2 (P image)   ", command = usdaP)
  Ia_P.btn <- tk2button(misc_frame, text = "  Ia = 0.05*S (P value)   ", command = hawkins)
  Ia_Pim.btn <- tk2button(misc_frame, text = "  Ia = 0.05*S (P image)   ",  command = hawkinsP)

  tkgrid(tklabel(misc_frame, text = "    "))
  tkgrid(Ia_img.btn, Ia_P.btn, Ia_Pim.btn, ipadx= 20)
  tkgrid(tklabel(misc_frame, text = "    "))
  tkgrid(misc_frame)


  ENDFrame <- tkframe(runoff_win, relief= "groove")
  tkgrid(tklabel(ENDFrame, text = " - O - "))
  tkgrid(ENDFrame)

  }

