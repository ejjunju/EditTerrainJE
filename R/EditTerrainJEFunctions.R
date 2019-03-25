#Interactively edit terrain. Perform cuts and fills in terrain using lines with start and end elevations
#library(tcltk2)
#----------------------------------------------------------------------------------------------------------------------#
#' Function to check and install pacakges
#' @param x vector of packages to check and install
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @export
fx.CheckInstallLoadPkgs = Vectorize(function(x) {
  #Ensure antivirus doesnt stop mising package installation (included in #fx.CheckInstallLoadPkgs)
  #interactive alternative #trace(utils:::unpackPkgZip, edit=TRUE)
  trace(utils:::unpackPkgZip, quote(Sys.sleep(5)),at = which(grepl("Sys.sleep", body(utils:::unpackPkgZip), fixed = TRUE)))
  #check and install
  stopifnot(is.character(x))
  if(!is.element(x, rownames(installed.packages()))) install.packages(x,dependencies = TRUE)
  #load
  library(x, character.only = T, logical.return = T, quietly = T)
})

#----------------------------------------------------------------------------------------------------------------------#
#' Function to create a SpatialPolygonsDataFrame from a matrix of xy coordinates
#' @param pts matrix of x,y points
#' @return SpatialPolygonsDataFrame from
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @export
polygonFromXY<-function(pts=null,Raster=null,n=4,coord.sys="+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "){
  if(!is.null(Raster)){
    pts<-fx.click(Raster,n=n,xy=TRUE)
    pts<-pts[,1:2]
    pts<-rbind(pts,pts[1,])
    points(pts)
    coord.sys<-proj4string(Raster)
  }
  print(pts)
  if(!is.null(pts)){
    print("Creating Mask Polygon")
    P1 = Polygon(pts)
    Ps1 = Polygons(list(P1), ID = "1")
    Ps2=SpatialPolygons(list(Ps1),proj4string=CRS(coord.sys))
    polys.df <- SpatialPolygonsDataFrame(Ps2, data = data.frame(id=1, row.names=1))
    plot(polys.df,add=T)
    return(polys.df)
  } else{print("No points to create polygon")}
}

#' Draw Polygon by cklicking in a plotted Raster
#' @param r raster
#' @param clicks number of clicks
#' @return shp
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @export
fx.PolygonFromRasterClicks<-function(r,clicks=100){
  hills(r,main="Click in this map: wait for message Box")
  tk_messageBox(type="ok",message="Click to draw Mask polygon?")
  cp<-fx.click(r,n=clicks)
  cp<-cp[,-3]
  srl<-list(Polygon(cp))
  Srl<-list(Polygons(srl, ID=1))
  Sr<-SpatialPolygons(Srl,proj4string=CRS(proj4string(r)))
  shp<-SpatialPolygonsDataFrame(Sr, data=data.frame(ID=1), match.ID = TRUE)
  plot(shp,add=T,col=rgb(0.55,0.55,0.55,alpha=0.5))
  return(shp)
}



#----------------------------------------------------------------------------------------------------------------------#
#' Function for drawing points at a given interval along a line
#' @param x A SpatialLinesDataFrame (must have a projection in metres)
#' @param sdist interval in metres
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @return \code{results} A SpatialPointsDataFrame
#' @export
sample.line <- function(x, sdist=100)
{
  ##fx.CheckInstallLoadPkgs("sp")
  #fx.CheckInstallLoadPkgs(c("rgdal","raster","sp","tcltk2","akima","compiler"))
  #if (!require(sp)) stop("sp PACKAGE MISSING")
  if (!inherits(x, "SpatialLinesDataFrame")) stop("MUST BE SP SpatialLinesDataFrame OBJECT")
  lgth <- SpatialLinesLengths(x)

  #Line1
  lsub <- x[1,]
  ns <- round( (lgth[1] / sdist), digits=0)
  lsamp <- spsample(lsub, n=ns, type="regular", offset=c(0.5,0.5))
  results <- SpatialPointsDataFrame(lsamp, data=data.frame(ID=rep(rownames(x@data[1,]),ns)))
  #start/end + vertices


  #Other Lines
  for (i in 2:dim(x)[1] )
  {
    lsub <- x[i,]
    ns <- round( (lgth[i] / sdist), digits=0)
    lsamp <- spsample(lsub, n=ns, type="regular")
    lsamp <- SpatialPointsDataFrame(lsamp, data=data.frame(ID=rep(rownames(x@data[i,]),ns)))
    results <- rbind(results, lsamp)
  }
  return( results )
}

#----------------------------------------------------------------------------------------------------------------------#
#' Function for drawing points at a given interval along a line
#' Find the elavtion of points by interpolating between the elevations of the start and endpoints if \code{interpZ==TRUE}
#' Create a raster by interpolating the elevations
#' @param x A SpatialLinesDataFrame (must have a projection in metres), Lines along which points are created. Must have data with first column (ID) second column (Zs) and third column (Ze) . Zs and Ze are start and end elevations for each line (ID).
#' @param sdist interval in metres
#' @param interp.z Should be TRUE (for now)
#' @param spdf A SpatialPolygonsData Frame (a bounding polygon for the interpolation )
#' @param zmanual TRUE/FALSE decide whether to read the elevations from the raster (feks Excavation)or manually enter them (feks filling)
#' @return \code{results} A SpatialPointsDataFrame
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @export
sample.line.inkl.start.end.make.dem <- function(x,
                                                sdist=1,
                                                interp.z=TRUE,
                                                spdf, #mask polygon
                                                zmanual=FALSE
){
  EditingLines<-x
  #fx.CheckInstallLoadPkgs(c("rgdal","raster","sp","tcltk2","akima","compiler"))
  #if (!require(sp)) stop("sp PACKAGE MISSING")
  if (!inherits(x, "SpatialLinesDataFrame")) stop("MUST BE SP SpatialLinesDataFrame OBJECT")
  lgth <- SpatialLinesLengths(x)

  #Line1
  for (i in 1:dim(x)[1] )
  {
    lsub <- x[i,]
    ns <- round( (lgth[i] / sdist), digits=0)
    lsamp <- spsample(lsub, n=ns, type="regular", offset=c(0.5,0.5))
    sev<-as(x[i,], "SpatialPoints")#start, end and vertices
    csev<-coordinates(sev)
    cslamp<-coordinates(lsamp)
    cslamp<-rbind(csev[1,],cslamp,csev[nrow(csev),])
    lsamp <- SpatialPointsDataFrame(cslamp, data=data.frame(ID=rep(i,ns+2)))
    cslamp<-data.frame(cslamp)
    lsamp@data<-cbind(lsamp@data,cslamp)
    names(cslamp)<-c("x","y")
    if(i==1){
      Out <- lsamp
    } else {
      Out <- rbind(Out, lsamp)
    }
  }

  #calculate distances along line
  names(Out@data)<-c("ID","X","Y")
  Out@data$dL<-NA
  Out@data$dist<-NA
  Out@data$Z<-NA

  for(i in unique(Out$ID)){

    idx<-which(Out$ID==i)
    subdf<-Out@data[idx,]
    diffx<-c(0,diff(subdf$X))
    diffy<-c(0,diff(subdf$Y))
    dL<-(diffx^2+diffy^2)^0.5
    dist<-cumsum(dL)
    Out@data$dist[idx]<-dist
    Out@data$dL[idx]<-dL

    #interpolate elevations long lines
    if(interp.z==TRUE){

      plot(x,axes=TRUE,add=TRUE,col="red")
      plot(x[i,],col="white",lwd=2,add=TRUE)
      #points(Out@data$X,Out@data$Y,pch=16,col="grey",cex=0.5)
      points(subdf$X[1],subdf$Y[1],pch=16,col="blue",cex=1)
      points(subdf$X[nrow(subdf)],subdf$Y[nrow(subdf)],pch=16,col="red",cex=1)

      # text(x=subdf$X[1],y=subdf$Y[1],
      #      labels=i,
      #      cex=1,
      #      adj=0,font=2,col="black",pos=2)
      #
      legend("top",col=c("blue","red"),pch=c(16,16),cex=c(1,1),legend = c("Start","End"),horiz=TRUE)
      print(Li<-range(dist))
      Zi<-c(NA,NA)

      if(zmanual==TRUE){
        print(showtxt<-paste("Enter Start and End elevation for Line[",i,"] of",length(unique(Out$ID))))
        #Zi<-scan(what = "numeric",n = 2)
        Zi<-tk.2.values(showtxt)
        print("Manual Elevations")
        print(Zi)
      }else {
        #print(Zi<-(EditingLines@data[i,2:3]))
        print(head(EditingLines@data))
        #scan(n=1)
        Zi[1]<-EditingLines@data[i,2]
        Zi[2]<-EditingLines@data[i,3]
        print("Terrain Elevations")
        print(Zi)
      }
      #points(subdf$X[1],subdf$Y[1],pch=16,col="grey",cex=1)#start
      #points(subdf$X[nrow(subdf)],subdf$Y[nrow(subdf)],pch=16,col="grey",cex=1) #end
      fx<-approxfun(Li,Zi)
      Z<-fx(dist)
      Out@data$Z[idx]<-Z
    }

  }


  if(interp.z==TRUE){
    library("raster")
    #make a dem
    x1<-Out@data$X
    y1<-Out@data$Y
    z1<-Out@data$Z
    xo<-seq(min(x1),max(x1),sdist)
    yo<-seq(min(y1),max(y1),sdist)
    library("akima")
    alt<-interp(x=x1,y=y1,z=z1,xo=xo,yo=yo,duplicate="mean")
    alt<-raster(alt)

    plot(spdf,add=T,border="grey",col=rgb(0.55,0.55,0.55,alpha=0.4),lty=3)
    spdf.ras<-rasterize(spdf,alt) #rasterize
    alt<-crop(alt,spdf.ras) #crop raster
  }

  print("check that Points along lines are created and Z calculated")
  print(head(Out@data))

  Ret<-list(Vertices=Out,dem=alt)
  return(Ret)
}

#----------------------------------------------------------------------------------------------------------------------#
#' Plot terrain with hillsahde
#' @param  r raster
#' @param xlim xlimits to plot
#' @param ylim ylimits to plot
#' @param  main Title
#' @param legend FALSE/TRUE decide whether to plot a legend. Dont plot a legend if you plan to add more objects to the plot. They wont be plotted properly
#' @param brks optionally provide breaks to decide the colorlevels
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @export
hills<-function(r,xlim=NULL,ylim=NULL,main="Terreng",legend=TRUE,brks=NA){
  #fx.CheckInstallLoadPkgs(c("rgdal","raster","sp","tcltk2","akima","compiler"))
  par(mar=c(9,2,2,1))
  slope <- terrain(r, opt='slope')
  aspect <- terrain(r, opt='aspect')
  hill <- hillShade(slope, aspect, 45, 315,normalize=TRUE)

  if(is.null(xlim)){
    plot(hill, col=grey(0:100/100), legend=FALSE,main=main,cex.axis=0.5)
  } else {
    plot(hill, col=grey(0:100/100), legend=FALSE,xlim=xlim,ylim=ylim,main=main,cex.axis=0.5)
  }

  if(is.na(brks)[1]){
    rng<-range(getValues(r),na.rm=T)
    (brks<-seq(rng[1],rng[2],diff(rng)/32))
    (brks<-round(brks,2))
  }

  (Lbrk<-length(brks))
  Lbrk<-length(brks)
  colorRampAlpha <- function(..., n, alpha) {
    colors <- colorRampPalette(...)(n)
    paste(colors, sprintf("%x", ceiling(255*alpha)), sep="")
  }
  kol<-colorRampAlpha( c("blue","cadetblue1","yellowgreen","darkgreen","gold3","firebrick","darkred","chocolate4","gray81","white"),n=Lbrk-1,alpha=0.35)
  if(is.null(xlim)){
    plot(r, col=kol,breaks=brks,legend=FALSE, add=TRUE)
    box("outer",lwd=3)
    grid()
  } else{
    plot(r, col=kol,breaks=brks,legend=FALSE, add=TRUE,xlim=xlim,ylim=ylim)
    box("outer",lwd=3)
    grid()
  }
  if(legend==TRUE){hills.legend(r,xlim=NULL,ylim=NULL,brks=brks)}
}

#----------------------------------------------------------------------------------------------------------------------#
#' get breaks from a raster based on a given number of breaks. default is 32
#' @param r raster
#' @param n Number of breaks (default is 32)
#' @return brks The breaks
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @export
fx.brks<-function(r,n=32){
  rng<-range(getValues(r),na.rm=T)
  (brks<-seq(rng[1],rng[2],diff(rng)/n))
  (brks<-round(brks,2))
  return(brks)
}

#----------------------------------------------------------------------------------------------------------------------#
#' Plot a legend
#' @param r a raster
#' @param xlim xlimits to plot
#' @param ylim ylimits to plot
#' @param  brks optionally provide breaks
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @export
hills.legend<-function(r,xlim=NULL,ylim=NULL,brks=NA){
  #fx.CheckInstallLoadPkgs(c("rgdal","raster","sp","tcltk2","akima","compiler"))
  if(is.na(brks)[1]){
    rng<-range(getValues(r),na.rm=T)
    (brks<-seq(rng[1],rng[2],diff(rng)/32))
    (brks<-round(brks,2))
  }

  (Lbrk<-length(brks))
  Lbrk<-length(brks)

  colorRampAlpha <- function(..., n, alpha) {
    colors <- colorRampPalette(...)(n)
    paste(colors, sprintf("%x", ceiling(255*alpha)), sep="")
  }
  kol<-colorRampAlpha( c("blue","cadetblue1","yellowgreen","darkgreen","gold3","firebrick","darkred","chocolate4","gray81","white"),n=Lbrk-1,alpha=0.35)
  plot(r, horizontal=T,add=T,
       #smallplot=c(.94, .95, .11, .91),
       smallplot=c(0.1, .8, 0.1,0.11),
       legend.only=TRUE,
       legend.args=list(text='Elevation (m)', side=1, font=2, line=3.5, cex=0.8),
       col=kol,
       breaks=brks,
       axis.args=list(at=brks,labels=sprintf("%.2f", brks),cex.axis=0.8,las=2,col="grey")
  )
}

#----------------------------------------------------------------------------------------------------------------------#
#get a numeric value for tk dialog box
#' Return a value entered using a tk dialog box
#' @param msg message
#' @param varname Number of Lines to draw
#' @return \code{x} a valuye entered
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @export
tk.1.value <- function(msg="Enter number of Parallel Interpolation Lines (minimum 2)",varname="Number of Lines to Draw"){
  #fx.CheckInstallLoadPkgs(c("rgdal","raster","sp","tcltk2","akima","compiler"))
  xvar <- tclVar("")

  win1 <- tktoplevel()
  tkwm.title(win1,varname)
  x.entry <- tk2entry(win1, width = "25",textvariable=xvar)

  reset <- function()
  {
    tclvalue(xvar)<-""
  }

  reset.but <- tkbutton(win1, text="Reset", command=reset)

  submit <- function() {
    x <- as.numeric(tclvalue(xvar))
    e <- parent.env(environment())
    e$x <- x
    tkdestroy(win1)
  }
  submit.but <- tkbutton(win1, text="OK", command=submit)

  tkgrid(tklabel(win1,text=msg),columnspan=2)
  tkgrid(tklabel(win1,text=varname), x.entry, pady = 10, padx =10)
  tkgrid(submit.but, reset.but)

  tkwait.window(win1)
  return(c(x))
}

#get a character string  value for tk dialog box
#' Return a value entered using a tk dialog box
#' @param msg message
#' @param varname Number of Lines to draw
#' @return \code{x} a valuye entered
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @export
tk.1.string <- function(msg="Output Prefix",varname="OutPrefix"){
  #fx.CheckInstallLoadPkgs(c("rgdal","raster","sp","tcltk2","akima","compiler"))
  xvar <- tclVar("Result")

  win1 <- tktoplevel()
  tkwm.title(win1,varname)
  x.entry <- tk2entry(win1, width = "25",textvariable=xvar)

  reset <- function()
  {
    tclvalue(xvar)<-""
  }

  reset.but <- tkbutton(win1, text="Reset", command=reset)

  submit <- function() {
    x <- as.character(tclvalue(xvar))
    e <- parent.env(environment())
    e$x <- x
    tkdestroy(win1)
  }
  submit.but <- tkbutton(win1, text="OK", command=submit)

  tkgrid(tklabel(win1,text=msg),columnspan=2)
  tkgrid(tklabel(win1,text=varname), x.entry, pady = 10, padx =10)
  tkgrid(submit.but, reset.but)

  tkwait.window(win1)
  return(c(x))
}
#----------------------------------------------------------------------------------------------------------------------#
#get a value for tk dialog box
#' Return two numeric values entered using a tk dialog box
#' @param msg message
#' @param varname Number of Lines to draw
#' @return \code{c(x,y)} values entered
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @export
tk.2.values <- function(msg="Enter Start and End Elevations for current Line",varname=c("Start","end")){
  #fx.CheckInstallLoadPkgs(c("rgdal","raster","sp","tcltk2","akima","compiler"))
  xvar <- tclVar("")
  yvar <- tclVar("")

  win1 <- tktoplevel()
  tkwm.title(win1,varname)
  x.entry <- tk2entry(win1, width = "25",textvariable=xvar)
  y.entry <- tk2entry(win1, width = "25",textvariable=yvar)
  reset <- function()
  {
    tclvalue(xvar)<-""
    tclvalue(yvar)<-""
  }


  reset.but <- tkbutton(win1, text="Reset", command=reset)

  submit <- function() {
    x <- as.numeric(tclvalue(xvar))
    y <- as.numeric(tclvalue(yvar))
    e <- parent.env(environment())
    e$x <- x
    e$y <- y
    tkdestroy(win1)
  }
  submit.but <- tkbutton(win1, text="OK", command=submit)

  tkgrid(tklabel(win1,text=msg),columnspan=2)
  tkgrid(tklabel(win1,text=varname[1]), x.entry, pady = 10, padx =10)
  tkgrid(tklabel(win1,text=varname[2]), y.entry, pady = 10, padx =10)
  tkgrid(submit.but, reset.but)

  tkwait.window(win1)
  return(c(x,y))
}

#----------------------------------------------------------------------------------------------------------------------#
#' Return n number of numeric values entered using a tk dialog box
#' @param msg message
#' @param n Number of values to enter and return
#' @return \code{out} values entered
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @export
tk.n.values <- function(msg="Enter Line Numbers from left to Right",n=2){
  require(tcltk2)
  varname<-paste("Line",1:n,sep="")
  quotes<-""
  #fx.CheckInstallLoadPkgs(c("rgdal","raster","sp","tcltk2","akima","compiler"))
  #xvar <- tclVar("")
  xvar.txt<-paste("xvar",1:n,"<-tclVar(quotes)",sep="")
  eval(parse(text=xvar.txt))

  win1 <- tktoplevel()
  tkwm.title(win1,"Line Order")
  #x.entry <- tk2entry(win1, width = "25",textvariable=xvar)
  x.entry.txt<-paste("x.entry",1:n," <- tk2entry(win1, width = as.character(25),textvariable=xvar",1:n,")",sep="")
  eval(parse(text=x.entry.txt))

  reset <- function()
  {
    #tclvalue(xvar)<-""
    txt<-paste("tclvalue(xvar,",1:n,")<-quotes",sep="")
    eval(parse(text=txt))
  }

  reset.but <- tkbutton(win1, text="Reset", command=reset)

  submit <- function() {
    #x <- as.numeric(tclvalue(xvar))
    (txt1<-paste("x",1:n,"<- as.numeric(tclvalue(xvar",1:n,"))",sep=""))
    eval(parse(text=txt1))
    e <- parent.env(environment())
    #e$x <- x
    txt2<-paste("e$x",1:n," <- x",1:n,sep="")
    eval(parse(text=txt2))
    tkdestroy(win1)
  }
  submit.but <- tkbutton(win1, text="OK", command=submit)

  tkgrid(tklabel(win1,text=msg),columnspan=2)
  #tkgrid(tklabel(win1,text=varname[1]), x.entry, pady = 10, padx =10)
  txt3<-paste("tkgrid(tklabel(win1,text=varname[",1:n,"]), x.entry",1:n,",pady = 10, padx =10)",sep="")
  eval(parse(text=txt3))
  tkgrid(submit.but, reset.but)

  tkwait.window(win1)
  txtout<-paste("out<-c(",paste(paste("x",1:n,sep=""),collapse=","),")",sep="")
  (eval(parse(text=txtout)))
  return(out)

}

#----------------------------------------------------------------------------------------------------------------------#
#' Function to edit a DEM(depends on other functions above)
#' @param dem Path to a raster withe levation data (tif).
#' @param nlines NUmber of editing lines to be drawn for cutting/filling terrain
#' @param zmanual TRUE/FALSE determines whether elevations fro start end endpoints to the editing lines are enteered manually or read from the terrain
#' @param OutPrefix a prefix for the output filenames
#' @return A list with elements: (1) Drawn interpolation lines and (2) bounding polygon polys.df and (3) Edited terrain model dem
#' @examples
#' fx.edit.terrain("dem.tif",nlines=4,zmanual=F,OutPrefix="Result")
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @export
fx.edit.terrain<-function(dem="dem.tif",
                          nlines=4,
                          zmanual=FALSE,
                          OutPrefix="Result",
                          coord.sys="+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs",
                          plot.shapes=FALSE,
                          male.pts=NULL,
                          other.shps=NULL,
                          xlim=NULL,
                          ylim=NULL){


  fx.CheckInstallLoadPkgs(c("rgdal","raster","sp","tcltk2","akima","compiler"))
  #channel i based on the elevations i the raster at the start and end points of lines
  options(warn=0)
  #graphics.off()

  library(rgdal)
  #get a raster to modify
  #raster that covers the interest period
  library(raster)
  #terr<-raster("C:/Users/noemma/Dropbox/DEV/R/HydroSearch/Floreng/NILE/srtm100nile/w001001.adf")
  #writeRaster(terr,"WhiteNileDEM.tif",format="GTiff",,NAflag=-9999,)
  # library(RQGIS) #https://jannes-m.github.io/RQGIS/index.html
  #https://rstudio-pubs-static.s3.amazonaws.com/255241_3e5566bd3f6747689af598a58932047f.html
  # args$INPUT <- terr
  # args$OUTPUT <- file.path("fdr.tif")
  r<-raster(dem)
  raster2x11(r = r,H = 20,n = 1,xlim =xlim ,ylim = ylim)
  par(mfrow=c(1,1))
  hills(r,xlim=xlim,ylim=ylim,legend = FALSE)
  Sys.sleep(2)

  #Plot points withe elevation attribute POINT_Z
  # if(!is.null(male.pts)){
  #   print("Plotting reference Points with Point_Z")
  #   points(male.pts,pch=16,cex=1)
  #   raster::text(male.pts,round(male.pts@data$POINT_Z,2),cex=0.6, halo=TRUE,srt=90,adj=-0.4,font=4,col=1)
  # }
  if(!is.null(male.pts)){
    print("Plotting reference Points with Point_Z")
    if(class(male.pts)=="SpatialPointsDataFrame"){
      points(male.pts,cex=1,pch=16)
      kmp<-coordinates(male.pts)
      if(is.null(male.pts@data$POINT_Z)){print("Warning: POINT_Z attribute missing")} else {
      shadowtext(x=kmp[,1],y=kmp[,2],
               labels=round(male.pts@data$POINT_Z,2),
               cex=0.8,
               adj=0,font=2,col="white",pos=3)
      }
      } else {
                 print("Warning: No points provided")
               }
  }
  if(!is.null(other.shps)){
    shplist<-names(other.shps)
    if(length(other.shps)>0){
      colf<-col<-colorRampPalette(c(rgb(1,0,0,alpha=0.5),rgb(0.8,0,0.2,alpha=0.5),rgb(0.6,0,0.4,alpha=0.5)))
      (col<-colf(length(other.shps)))
      (LWd<-rep(5,length(other.shps)))
      #check if name contains River or elv (hhelps to assign plotting colour)
      (isriver<-sapply(c("elv","river"),function(x) grepl(x,tolower(shplist))))
      (isriver<-apply(isriver,1,function(x) any(x==TRUE)))
      col[which(isriver==TRUE)]<-rgb(0,0,1,alpha=0.4)
      LWd[which(isriver==TRUE)]<-0.5
      for(osi in 1:length(other.shps)){
        shp2plot<-other.shps[[osi]]
        if(class(shp2plot)=="SpatialPolygonsDataFrame"){
          plot(other.shps[[osi]],border=col[osi],col="transparent",add=TRUE,lwd=1)

        } else {
          plot(other.shps[[osi]],col=col[osi],add=TRUE,lwd=1)
        }
      }
    }
  }

  #use extract to draw line xyz vertices  where the terrain editing is required
  print("Create Interpolation Guide Lines by clicking on the plot the click FINISH when done")
  print("-------------------------------------------------------------------------------------------------------------")
  #tkmessageBox(message = "Create Cut or Fill Line by clicking\n (maks 100 clicks per line)", icon = "info", type = "ok")
  fx.CheckInstallLoadPkgs("sp")
  ll<-vector("list",length = nlines)
  zse<-array(dim=c(nlines,2)) #start end elevations
  for(i in 1:nlines){
    if(i==1){print(tkmessageBox(message = paste("Create Editing Lines\nClick to create vertices (Max 100/Line)\nFirst Line ",min(i,nlines),"/",nlines),
                                    icon = "info", type = "ok"))}
    p1<-fx.click(r,n=100,xy=TRUE,PolyLine = TRUE)
    l1<-as.matrix(p1[,-3])
    lines(p1$x,p1$y,pch=16,lty=1,col=rgb(1,0,0,alpha=0.5),lwd=3) #red lines that are from clicking
    ll[[i]]<-l1
    zse[i,1]<-p1[1,3]
    zse[i,2]<-p1[nrow(p1),3]
    if(i<nlines){
      print(tkmessageBox(message = paste("Next Line ",min(i+1,nlines),"/",nlines),icon = "info", type = "ok"))
      } else{
        if(i==nlines){tkmessageBox(message = paste("Lines Created ",min(i+1,nlines),"/",nlines),icon = "info", type = "ok")}
      }
    }
  zse<-as.data.frame(zse)
  names(zse)<-c("Zs","Ze")
  #Creette a polyline shapefile from the extracted xyz point vertices ###
  ll2<-lapply(ll,Line)
  names(ll2)<-1:length(ll2)
  ll3<-vector("list",length = nlines)
  for(i in 1:length(ll2)){
    ll3[[i]] = Lines(list(ll2[[i]]), ID=i)
  }
  EditingLines = SpatialLines(ll3,proj4string=CRS(coord.sys))
  ldf <- data.frame(ID=1:nlines, zse)
  EditingLines <- SpatialLinesDataFrame(EditingLines, data = ldf)
  names(EditingLines@data)<-c("ID","Zs","Ze")

  #Create an envelope polygon
  print("Find the Start and endpoints to all the Guide -lines")
  print("-------------------------------------------------------------------------------------------------------------")
  lxy<- lapply(slot(EditingLines, "lines"),
               function(x) lapply(slot(x, "Lines"),
                                  function(y) slot(y, "coords"))) #Line vertices

  #(Midvert<-do.call("rbind",lapply(lapply(lxy, function(x) x[[1]]),function(x) x[ceiling(nrow(x)/2),])))
  fx.CheckInstallLoadPkgs("maptools")
  (MidPts<-coordinates(SpatialLinesMidPoints(EditingLines)))

  ab<-NA #startpoints
  bc<-NA #end points
  for(k in 1:length(lxy)){
    (a<-lxy[[k]][[1]][1,]);
    (b<-lxy[[k]][[1]]);
    (b<-b[nrow(b),])

    if(k==1){ab<-a;bc<-b} else {
      ab<-rbind(ab,a)
      bc<-rbind(bc,b)
    }
  }
  rownames(ab)<-c() #rownames(bc)<-1:nrow(bc)
  rownames(bc)<-c()
  starts<-data.frame(ab)
  ends<-data.frame(bc)
  #cpts<-do.call("rbind",lapply(coordinates(EditingLines),function(x) apply(x[[1]],2,mean)))

  #plot(EditingLines,add=T,col="white",asp=1,lwd=2,lty=3)
  #points(x=starts[,1],y=starts[,2],cex=2,pch=2,col="lightblue")
  shadowtext(x=MidPts[,1],y=MidPts[,2],
             labels=1:length(EditingLines),
             cex=1,
             adj=0,font=2,col="red",bg="white",pos=3)
  # shadowtext(x=ends[,1],y=ends[,2],
  #      labels=paste("E",1:length(EditingLines)),
  #      cex=0.8,
  #      adj=0,font=2,col="red",pos=3)

  Sys.sleep(2)
  print("Arrange the points so as to create a polygon")
  print("-------------------------------------------------------------------------------------------------------------")

  #To find the edge lines
  #Sort the start points by using distance nearest and westest/northest points
  endpts<-ends
  endpts$id<-1:nrow(endpts)
  startpts<-starts
  startpts$id<-1:nrow(startpts)
  startpts$d<-NA

  # sortNS<-tkmessageBox(
  #   message = "Are start endpoints Sorted
  #   \nNorth_South? (yes)
  #   \nor \nEast-West? (no)
  #   \nor \nManual Soting (cancel)",
  #   icon = "question", type = "yesno", default = "yes")
  #
  # if(as.character(sortNS)=="yes"){
  #   Sort<-startpts[order(startpts$y),] #sort south to north
  #   pi<-Sort[1,1:2]
  # }
  # if(as.character(sortNS)=="no") {
  #   Sort<-startpts[order(startpts$x),]#sort west to eat
  #   pi<-Sort[1,1:2]
  # }

  #if(as.character(sortNS)=="cancel") {
  #tkmessageBox(message = "Enter Line Numbers at Start end From Left->Right", icon = "info", type = "ok")
  if(nrow(startpts)>2){ #Sort order if more than 2 lines are drawn
    orderi<-tk.n.values(msg="Enter Line Numbers at Start endpoints \nFrom \nLeft->Right \nOR \nRight->Left " ,n=nrow(startpts))
    } else {orderi<-1:2}
  Sort<-startpts[orderi,]#sort west to eat
  pi<-Sort[1,1:2]
  #}


  # Sort<-startpts[order(startpts$x),]#sort west to eat
  # pi<-Sort[1,1:2]
  # Sort$d<-((Sort[,1]-pi$x)^2+(Sort[,2]-pi$y)^2)^0.5
  # if(any(diff(Sort$d)<0)){
  #   Sort<-startpts[order(startpts$y),] #sort south to north
  #   pi<-Sort[1,1:2]
  #   Sort$d<-((Sort[,1]-pi$x)^2+(Sort[,2]-pi$y)^2)^0.5
  # }

  #Points for polygon
  startpts<-Sort[,1:2]
  edge1<-as.data.frame(lxy[[Sort$id[length(Sort$d)]]])
  endpts<-endpts[rev(Sort$id),1:2]
  edge2<-as.data.frame(lxy[[Sort$id[1]]])
  edge2<-edge2[nrow(edge2):1,]
  pts<-rbind(startpts,edge1,endpts,edge2,startpts[1,])
  rownames(pts)<-c()
  #create the polygon
  print("Creating Mask Polygon")
  print("-------------------------------------------------------------------------------------------------------------")
  P1 = Polygon(pts)
  Ps1 = Polygons(list(P1), ID = "1")
  Ps2=SpatialPolygons(list(Ps1),proj4string=CRS(coord.sys))
  polys.df <- SpatialPolygonsDataFrame(Ps2, data = data.frame(id=1, row.names=1))
  plot(polys.df,add=T,col=rgb(0.55,0.55,0.55,alpha = 0.3))


  #Points along lines with elevbations interpreted between start and end
  #Run sample function and display results
  # Out<- sample.line(EditingLines, sdist=0.5)
  # plot(EditingLines, col = c("red", "blue"),axes=T)
  # plot(Out, pch=20, add=TRUE)
  sdist<-res(r)[1]
  print("Manual Entry of Elevations?")
  print(zmanual)
  print("Interpolating points along lines and creating a dem")
  print("-------------------------------------------------------------------------------------------------------------")
  Out<- sample.line.inkl.start.end.make.dem (x = EditingLines,
                                             sdist=sdist,
                                             spdf=polys.df,
                                             interp.z = TRUE,
                                             zmanual = zmanual)
  print("Combining Edited Area and original Raster")
  print("-------------------------------------------------------------------------------------------------------------")

  print("Crop edited raster region to edited region boundaries")
  r2<-crop(Out$dem,polys.df)

  print("rasterize edited region boundaries")
  r3<-rasterize(polys.df,r2)
  r2<-crop(r2,r3)*r3/r3

  #print("Resample Raster")
  r2<-resample(r2,r)

  r2[is.na(r2[])]<-r[is.na(r2[])]
  print("Finished Combining")
  print("-------------------------------------------------------------------------------------------------------------")

  raster2x11(r = r,H = 20,n = 2,xlim =xlim ,ylim = ylim)
  par(mfrow=c(1,2))
  demf<-unlist(strsplit(dem,"/"));demf<-demf[length(demf)]#name of original raster in result folder
  brks<-fx.brks(r2)
  hills(r,xlim=xlim,ylim=ylim,main=paste( "",demf,sep=""),legend = F,brks=brks)
  hills(r2,xlim=xlim,ylim=ylim,main=paste("",OutPrefix,".tif",sep=""),legend = F,brks=brks)
  if(plot.shapes==TRUE){
    plot(polys.df,add=T,border="transparent",col=rgb(0.55,0.55,0.55,alpha = 0.3))
    #plot(EditingLines,add=T,col="white",asp=1,lwd=2,lty=3)
  }
  hills.legend(r2,xlim=xlim,ylim=ylim,brks=brks)

  # raster2x11(r2,20,2)
  par(mfrow=c(1,1))
  #return an object with the lines, mask polygon, new raster
  #also write them to file
  print("Saving files")
  print("-------------------------------------------------------------------------------------------------------------")
  print(dsnf<-paste(getwd(),OutPrefix,sep="/"))
  print("Saving Files to")
  print(dsnf)
  writeOGR(EditingLines,dsn = dsnf,layer = paste(OutPrefix,"_EditingLines"),driver = "ESRI Shapefile",overwrite_layer = TRUE)
  writeOGR(polys.df,dsn = dsnf,layer = paste(OutPrefix,"_EditedExtentPolygon"),driver = "ESRI Shapefile",overwrite_layer = TRUE)
  print(OutPrefix)
  print(rasout<-paste(dsnf,"/",OutPrefix,"_",demf,".tif",sep=""))
  writeRaster(r2,rasout,overwrite=TRUE,format="GTiff",NAflag=-9999)
  #Sys.sleep(5)
  out<-list(EditingLines,Mask=polys.df,dem=r2)
  return(out)
}

#----------------------------------------------------------------------------------------------------------------------##' Interactively edit terrain
#' Select Working Directory
#' Select raster with elevation data
#' Read help Shapefiles (a point shapefile with POINT_Z attribute)
#' Read help Shapefiles (Other points,polygons and polylines)
#' harmonize coordinate system/projection of rasters and help shapefiles
#' Decide Zoom area to process
#' Decide number of near parallel editing lines to construct for editing terrin
#' Decide whether to read elevations of start and endpoints of editing lines from terrain or enter them manually
#' Call edit Terrain function
#' Interactive_terrain_edit()
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @param OutPrefix prefix to output filename. Also used as output foldername
#' @export
Interactive_terrain_edit<-function(OutPrefix="Result"){
  #WORKING DIRECTORIES
  library(tcltk2)

  wd<-tk_choose.dir(default = getwd(), caption = "Select Working directory")
  setwd(wd)
  resultfolder<-paste(wd,"/",OutPrefix,sep="")
  print("RESULTS ARE IN FOLDER")
  print(OutPrefix)
  print(resultfolder)
  if(!exists(resultfolder)){dir.create(resultfolder)}
  #EXPLORE RASTER
  {
    print("Reading and processing terrain")
    library(raster)
    Filters <- matrix(c("Raster", ".tif"),1, 2, byrow = TRUE)
    fdem<-tk_choose.files(default = "",
                          caption = "Elevation raster (tif)\n Warning:Large rasters take too much time!",
                          multi = TRUE,
                          filters = Filters,
                          index = 1)
    print(paste("Reading:",fdem))
    r<-raster(fdem)
    print(coord.sys<-proj4string(r)) #set coordsyetem to raster coordinates
    #output name
    fdemCopy<-unlist(strsplit(fdem,"/"))
    fdemCopy<-fdemCopy[length(fdemCopy)]
    fdemOut<-paste(OutPrefix,"_",fdemCopy,sep="")
    fdemCopy<-paste(OutPrefix,"/",fdemCopy,sep="")


  }

  #SHAPEFILES TO PLOT
  library(rgdal)
  #point Shapefile with POINT_Z attribute to guide terraine editing
  {
    print("Reading and processing Features")
    f1<-NA
    Filters <- matrix(c("Shapefiles", ".shp"),1, 2, byrow = TRUE)
    yesno<-tkmessageBox(message = "Do you have a Shp with POINT_Z attribute",
                        icon = "question", type = "yesno", default = "yes")
    if(as.character(yesno)=="yes"){
      f1<-tk_choose.files(default = "",
                          caption = "Select Shapefile\n in the working directory",
                          multi = FALSE,
                          filters = Filters,
                          index = 1)
      (brkpaths1<-do.call(rbind, strsplit(f1,"/")))
      (brkpaths1<-matrix(brkpaths1[,-ncol(brkpaths1)],nrow=1))
      (adsn1<-apply(brkpaths1,1,function(x) paste(x,collapse="/")))

      f1<-do.call(rbind, strsplit(f1,"\\."))[,1]
      f1<-unlist(strsplit(f1,"/"))
      f1<-f1[length(f1)]
      male.pts<-readOGR(adsn1,f1) #POINTS WITH REF ELEVATIONS
      male.pts<-spTransform(male.pts,coord.sys)
    } else {male.pts<-NULL}

    #Other Shapefiles to plot
    yesno<-tkmessageBox(message = "Other help-shapefiles to plot?",
                        icon = "question", type = "yesno", default = "yes")
    if(as.character(yesno)=="yes"){
      print(shplist<-tk_choose.files(default = "",
                                     caption = "Select Shapefiles in the working directory",
                                     multi = TRUE,
                                     filters = Filters,
                                     index = 1))
      #Sys.sleep(5)
      if(length(shplist)==1){shplist<-rbind(shplist,shplist)}
      (brkpaths<-do.call(rbind, strsplit(shplist,"/")))
      (brkpaths<-brkpaths[,-ncol(brkpaths)])
      (adsn<-apply(brkpaths,1,function(x) paste(x,collapse="/")))
      (shplist<-do.call(rbind, strsplit(shplist,"/")))
      (shplist<-shplist[,ncol(shplist)])
      (shplist<-unlist(strsplit(shplist,".shp")))
      if(!is.na(f1)){
        iPTS<-(which(shplist!=f1))
        shplist<-shplist[iPTS]
      } #remove points shapefile
      print("here")
      if(length(shplist)>0){
        other.shps<-vector("list",length=length(shplist))
        for(osi in 1:length(other.shps)){
          print(paste("Reading:",shplist[osi]))
          print("---------------------------------------------------------------------")
          rdshp<-readOGR(adsn[osi],shplist[osi]) #Read
          if(is.na(proj4string(rdshp))){proj4string(rdshp)<-coord.sys} #project same as raster if no projection
          other.shps[[osi]]<-rdshp
          names(other.shps)[osi]<-shplist[osi]
          other.shps[[osi]]<-spTransform(other.shps[[osi]],coord.sys) #reproject
        }
      }else{
        other.shps<-NULL
      }
    } else {other.shps<-NULL}
  }

  raster2x11(r = r,H = 20,n = 1)
  par(mfrow=c(1,1))
  #COPY THE RASTER
  print("Copying Elevation data")
  r1<-r
  writeRaster(r1,fdemCopy,overwrite=TRUE,format="GTiff",NAflag=-9999)
  #PLOT THE DATA
  print("Plotting Elevation data")
  {
    r1<-raster(fdemCopy)
    hills(r1,legend = FALSE)
    if(!is.null(male.pts)){plot(male.pts,add=TRUE,cex=0.5,pch="+")}
    if(!is.null(other.shps)){
      shplist<-names(other.shps)
      if(length(other.shps)>0){
        colf<-col<-colorRampPalette(c(rgb(1,0,0,alpha=0.5),rgb(0.8,0,0.2,alpha=0.5),rgb(0.6,0,0.4,alpha=0.5)))
        (col<-colf(length(other.shps)))
        (LWd<-rep(5,length(other.shps)))
        #check if name contains River or elv (hhelps to assign plotting colour)
        (isriver<-sapply(c("elv","river"),function(x) grepl(x,tolower(shplist))))
        (isriver<-apply(isriver,1,function(x) any(x==TRUE)))
        col[which(isriver==TRUE)]<-rgb(0,0,1,alpha=0.4)
        LWd[which(isriver==TRUE)]<-0.5
        for(osi in 1:length(other.shps)){
          shp2plot<-other.shps[[osi]]
          if(class(shp2plot)=="SpatialPolygonsDataFrame"){
            plot(other.shps[[osi]],border=col[osi],col="transparent",add=TRUE,lwd=1)

          } else {
            plot(other.shps[[osi]],col=col[osi],add=TRUE,lwd=1)
          }
        }
      }
    }
  }
  #SELECT TO AREA TO EDIT
  {
    tkmessageBox(message = "Click 2 points defining Zoomed area", icon = "info", type = "ok")
    print("Click two extent points")
    lims<-click(r1,n=2,xy=TRUE)
    exg<-expand.grid(lims[,1],lims[,2])
    exg<-rbind(exg[1:2,],exg[4:3,],exg[1,])
    lines(exg[order(exg[,1]),],col=2,lwd=3)
    lines(exg,col="white",lwd=3)
    lines(exg,col=2,lwd=1)
    #Sys.sleep(5)
    lims<-apply(lims,2,range)
    xlim<-lims[,1]
    ylim<-lims[,2]

    #tkmessageBox(message = "Click to Continue?", icon = "info", type = "ok")
  }

  #Edit terrain#
  {
    dem=fdemCopy;
    nlines=max(2,tk.1.value())
    yesno<-tkmessageBox(message = paste("Lines=",nlines,"\nGet End-Point Elevations from Terrain?"),
                        icon = "question", type = "yesno", default = "yes")
    if(as.character(yesno)=="yes"){zmanual=FALSE} else {zmanual=TRUE}
    OutPrefix2=unlist(strsplit(fdemOut,".tif"));
    print(OutPrefix)#;scan(n=1)
    coord.sys=coord.sys;
    plot.shapes=FALSE;
    male.pts=male.pts;
    other.shps=other.shps;
    xlim=xlim;
    ylim=ylim;

    #Edit terrain function
    Ret<-fx.edit.terrain(dem,
                         nlines,
                         zmanual,
                         OutPrefix,
                         coord.sys,
                         plot.shapes,
                         male.pts,
                         other.shps,
                         xlim,
                         ylim)
  }

  #Plot entire terrain
  yesno<-tkmessageBox(message = "Plot entire extent",
                      icon = "question", type = "yesno", default = "yes")
  if(as.character(yesno)=="yes"){
    demf<-unlist(strsplit(dem,"/"));demf<-demf[length(demf)]#name of original raster in result folder
    brks<-fx.brks(Ret$dem)
    raster2x11(r = r,H = 20,n = 2)
    par(mfrow=c(1,2))
    hills(r,main=paste( "",demf,sep=""),legend = F,brks=brks)
    hills.legend(Ret$dem)
    hills(Ret$dem,main=paste("",OutPrefix2,".tif",sep=""),legend = F,brks=brks)
    hills.legend(Ret$dem,brks=brks)
  }
}

#----------------------------------------------------------------------------------------------------------------------#
#' Function to add text with a halo/shadow
#' @param x x coordinate(s)
#' @param y y coordinate(s)
#' @param labels labels to add (same lengths as x and y)
#' @param col label foreground colour
#' @param bg lable halo/shadow color
#' @param theta theta controls appearance of shadow
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @export
shadowtext <- function(x, y=NULL, labels, col='white', bg='black',
                       theta= seq(0, 2*pi, length.out=50), r=0.1, ... ) {

  xy <- xy.coords(x,y)
  xo <- r*strwidth('A')
  yo <- r*strheight('A')

  # draw background text with small shift in x and y in background colour
  for (i in theta) {
    text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... )
  }
  # draw actual text in exact xy position in foreground colour
  text(xy$x, xy$y, labels, col=col, ... )
}
#----------------------------------------------------------------------------------------------------------------------#

#' Function to call x11 with H and W based on a raster's dimensions
#' @param r raster
#' @param H height
#' @param n number of widths
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @export
raster2x11<-function(r,H=20,n=1,xlim=NULL,ylim=NULL){

  if(!is.null(xlim)){
    r<-raster(xmn=xlim[1],xmx=xlim[2],ymn=ylim[1],ymx=ylim[2],res=res(r)[1])
  }
  dimr<-dim(r)
  ratioHW=dimr[1]/dimr[2]
  W<-H/ratioHW*n
  x11(width = W,height = H)
}


#' USER INTERFACE
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @param OutPrefix prefix to output filename. Also used as output foldername
#' @export
ui<-function(){
  #rm(list=ls(),envir = .GlobalEnv )
  fx.CheckInstallLoadPkgs("tcltk2")
  graphics.off()
  OutPrefix<-tk.1.string("Name of Output sub-directory\n[Also used as prefix for output filename]")
  #Run Interactive edit
  Interactive_terrain_edit(OutPrefix)
}


#' Draw an elevation profile line on a raster
#' @param r raster
#' @return table of elevations
fx.click.profile<-function(r){
  require(raster)
  #create a line
    ll<-vector("list",length=1)
    p1<-click(r,n=100,xy=TRUE)

    l1<-as.matrix(p1[,1:2])
    lines(p1$x,p1$y,pch=16,lty=1,col=rgb(1,0,0,alpha=0.5),lwd=3) #red lines that are from clicking
    ll[[1]]<-l1
  #Create a polyline shapefile from the extracted xyz point vertices ###
  ll2<-lapply(ll,Line)
  names(ll2)<-1:length(ll2)
  ll3<-vector("list",length = 1)
  for(i in 1:length(ll2)){
    ll3[[i]] = Lines(list(ll2[[i]]), ID=i)
  }
  EditingLines = SpatialLines(ll3,proj4string=CRS(proj4string(r)))
  ldf <- data.frame(ID=1)
  EditingLines <- SpatialLinesDataFrame(EditingLines, data = ldf)
  #r <- raster('dem.tif')
  #lines <- readOGR(dsn='lines.shp', layer='lines')
  elevations <- extract(r, EditingLines,cellnumbers=TRUE)
  p1<-as.data.frame(xyFromCell(r,elevations[[1]][,1]))
  p1$dx<-c(0,diff(p1$x))
  p1$dy<-c(0,diff(p1$y))
  p1$dL<-((p1$dx)^2+(p1$dy)^2)^0.5
  p1$L<-c(cumsum(p1$dL))
  p1$Z<-elevations[[1]][,2]
  plot(p1$L,p1$Z,xlab="L(m)",ylab="moh",type="l")
  return(p1)
}

#----------------------------------------------------------------------------------------------------------------------#
#' Return n number of values entered using a tk dialog box
#' @param msg message
#' @param n Number of values to enter and return
#' @return \code{out} values entered
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @export
tk.n.elv <- function(msg="Enter Line Numbers from left to Right",n=2){
  require(tcltk2)
  varname<-paste("Elevation",1:n,sep="")
  quotes<-""
  #fx.CheckInstallLoadPkgs(c("rgdal","raster","sp","tcltk2","akima","compiler"))
  #xvar <- tclVar("")
  xvar.txt<-paste("xvar",1:n,"<-tclVar(quotes)",sep="")
  eval(parse(text=xvar.txt))

  win1 <- tktoplevel()
  tkwm.title(win1,"Line Order")
  #x.entry <- tk2entry(win1, width = "25",textvariable=xvar)
  x.entry.txt<-paste("x.entry",1:n," <- tk2entry(win1, width = as.character(25),textvariable=xvar",1:n,")",sep="")
  eval(parse(text=x.entry.txt))

  reset <- function()
  {
    #tclvalue(xvar)<-""
    txt<-paste("tclvalue(xvar,",1:n,")<-quotes",sep="")
    eval(parse(text=txt))
  }

  reset.but <- tkbutton(win1, text="Reset", command=reset)

  submit <- function() {
    #x <- as.numeric(tclvalue(xvar))
    (txt1<-paste("x",1:n,"<- as.numeric(tclvalue(xvar",1:n,"))",sep=""))
    eval(parse(text=txt1))
    e <- parent.env(environment())
    #e$x <- x
    txt2<-paste("e$x",1:n," <- x",1:n,sep="")
    eval(parse(text=txt2))
    tkdestroy(win1)
  }
  submit.but <- tkbutton(win1, text="OK", command=submit)

  tkgrid(tklabel(win1,text=msg),columnspan=2)
  #tkgrid(tklabel(win1,text=varname[1]), x.entry, pady = 10, padx =10)
  txt3<-paste("tkgrid(tklabel(win1,text=varname[",1:n,"]), x.entry",1:n,",pady = 10, padx =10)",sep="")
  eval(parse(text=txt3))
  tkgrid(submit.but, reset.but)

  tkwait.window(win1)
  txtout<-paste("out<-c(",paste(paste("x",1:n,sep=""),collapse=","),")",sep="")
  (eval(parse(text=txtout)))
  return(out)

}

#----------------------------------------------------------------------------------------------------------------------#
#'prompts user to Enter strings and returns them to the variable
#'
#' @param msg message
#' @param n number of values
#' @param Varnames variable names
#'
#' @return out
#' @export
#'
tk.n.strings<-function (msg = "Enter Values", n = 2,Varnames=NULL)
{
  if(is.null(Varnames)){Varnames<- LETTERS[1:n]}
  require(tcltk2)
  varname <- paste(Varnames, 1:n, sep = "")
  quotes <- ""
  xvar.txt <- paste("xvar", 1:n, "<-tclVar(quotes)", sep = "")
  eval(parse(text = xvar.txt))
  win1 <- tktoplevel()
  tkwm.title(win1, "Variables")
  x.entry.txt <- paste("x.entry", 1:n, " <- tk2entry(win1, width = as.character(25),textvariable=xvar",
                       1:n, ")", sep = "")
  eval(parse(text = x.entry.txt))
  reset <- function() {
    txt <- paste("tclvalue(xvar,", 1:n, ")<-quotes", sep = "")
    eval(parse(text = txt))
  }
  reset.but <- tkbutton(win1, text = "Reset", command = reset)
  submit <- function() {
    (txt1 <- paste("x", 1:n, "<- as.character(tclvalue(xvar",
                   1:n, "))", sep = ""))
    eval(parse(text = txt1))
    e <- parent.env(environment())
    txt2 <- paste("e$x", 1:n, " <- x", 1:n, sep = "")
    eval(parse(text = txt2))
    tkdestroy(win1)
  }
  submit.but <- tkbutton(win1, text = "OK", command = submit)
  tkgrid(tklabel(win1, text = msg), columnspan = 2)
  txt3 <- paste("tkgrid(tklabel(win1,text=varname[", 1:n, "]), x.entry",
                1:n, ",pady = 10, padx =10)", sep = "")
  eval(parse(text = txt3))
  tkgrid(submit.but, reset.but)
  tkwait.window(win1)
  txtout <- paste("out<-c(", paste(paste("x", 1:n, sep = ""),
                                   collapse = ","), ")", sep = "")
  (eval(parse(text = txtout)))
  return(out)
}

#----------------------------------------------------------------------------------------------------------------------#
#' Create Example project
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @export
example.project<-function(){
  library(tcltk2)
  wd<-tk_choose.dir(default = getwd(), caption = "Select Parent Directory")
  setwd(wd)
  fx.CheckInstallLoadPkgs(c("raster","rgdal"))
  data("POINTS_Z")
  data("River")
  data("Terrain")
  if(!dir.exists("Example")){dir.create("Example")}
  setwd("Example")
  writeRaster(x = Terrain,filename = "Terrain.tif",format="GTiff",NAflag=-9999,overwrite=TRUE)
  writeOGR(POINTS_Z,dsn = "Example",layer = "POINTS_Z",driver = "ESRI Shapefile",overwrite_layer = TRUE)
  writeOGR(River,dsn = "Example",layer = "River",driver = "ESRI Shapefile",overwrite_layer = TRUE)
  setwd("Example")
}

#----------------------------------------------------------------------------------------------------------------------#
#datasets
#' Terrain data example
#' @format A raster dataset with elevatiopms:
#' \describe{
#'   \item{Terrain}{mas}
#' }
#' @source \url{hoydedata.no}
"Terrain"
#----------------------------------------------------------------------------------------------------------------------#
#datasets
#' Terrain data example
#' @format a point shapefile withe elevations:
#' \describe{
#'   \item{POINTS_Z}{Attribute withy heigh (masl)}
#' }
"POINTS_Z"
#----------------------------------------------------------------------------------------------------------------------#
#datasets
#' Terrain data example
#' @format A shapefile showing the river banks:
#' \describe{
#'   \item{Shapefile}{Polygon or Lines}
#' }
#' @source \url{hoydedata.no}
"River"
#----------------------------------------------------------------------------------------------------------------------#

#Functions related to Elevation colume curve and hypsometric curve
#require(EditTerrainJE)
#' Round and add trailing zeros
#' @param x Number to round and add trailing
#' @param n Number of decimal points
#' @return x
#' @export
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
#' @examples
#' fx.trail(20,5)
fx.trail<-function(x,n=2){
  txt<-paste("%.",n,"f",sep="")
  x<-sprintf(txt, round(x,n))
  return(x)
}

#'Click and plot clicked points while still clicking
#' @param r raster
#' @param n number of clicks
#' @return cp
#' @export
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
fx.click<-function(r,n=100,xy=TRUE,PolyLine=FALSE){
  clicks<-n
  i<-1
  cp<-NA
  while(i<clicks){
    cpi<-click(r,n=1,xy=xy)
    if(i==1){cp<-cpi} else{cp<-rbind(cp,cpi)}
    points(cp,pch=16,col=2)
    lines(cp,lty=2)
    Sys.sleep(2)
    yn1<-tk_messageBox(type="yesno",message="Next Point?")
    if(yn1=="yes"){i<-i+1} else {
      i<-clicks
      if(PolyLine==FALSE){cp<-rbind(cp,cp[1,])}
      points(cp,pch=16,col=2);lines(cp,lty=2)
    }
  }
  return(cp)
}

#' Draw Polygon by cklicking in a plotted Raster
#' @param r raster
#' @param clicks number of clicks
#' @return shp
#' @export
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
fx.PolygonFromRasterClicks<-function(r,clicks=100){
  hills(r,main="Click in this map: wait for message Box")
  tk_messageBox(type="ok",message="Click to draw Mask polygon?")
  cp<-fx.click(r,n=clicks)
  cp<-cp[,-3]
  srl<-list(Polygon(cp))
  Srl<-list(Polygons(srl, ID=1))
  Sr<-SpatialPolygons(Srl,proj4string=CRS(proj4string(r)))
  shp<-SpatialPolygonsDataFrame(Sr, data=data.frame(ID=1), match.ID = TRUE)
  plot(shp,add=T,col=rgb(0.55,0.55,0.55,alpha=0.5))
  return(shp)
}

#' Extract by mask
#' @param r raster
#' @param clicks number of clicks to make a mask polygon (maks 100)
#' @return out
#' @export
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
fx.ExtractByMask<-function(r,clicks=100){
  yn<-tk_messageBox(type="yesno",message="Do you have a Mask polygon?")
  if(yn=="no"){
    #Draw a polygon
    shp<-fx.PolygonFromRasterClicks(r,clicks = clicks)
  } else {
    #Choose an exisiting polygon
    Filters <- matrix(c("Shapefiles", ".shp"),1, 2, byrow = TRUE)
    f1<-tk_choose.files(default = "",
                        caption = "Reservoir boundary",
                        multi = TRUE,
                        filters = Filters,
                        index = 1)
    (brkpaths1<-do.call(rbind, strsplit(f1,"/")))
    (brkpaths1<-matrix(brkpaths1[,-ncol(brkpaths1)],nrow=1))
    (adsn1<-apply(brkpaths1,1,function(x) paste(x,collapse="/")))
    f1<-do.call(rbind, strsplit(f1,"\\."))[,1]
    f1<-unlist(strsplit(f1,"/"))
    f1<-f1[length(f1)]
    shp<-readOGR(adsn1,f1)
    shp<-spTransform(shp,CRS(proj4string(r)))
    plot(shp,add=T,col=rgb(0.55,0.55,0.55,alpha=0.5))
  }

  r<-crop(r,shp)
  shpr<-rasterize(shp,r);shpr<-shpr/shpr
  r<-r*shpr
  writeOGR(shp,".","Mask",driver = "ESRI Shapefile",overwrite_layer = TRUE)
  out<-list(shp=shp,shpr=shpr,r=r)
  return(out)
}

#' Plots volume-elevation curve and hypsographic curve
#'
#' @param zlim height limits over which to compute volume curve
#' @param clip2shp TRUE/FALSE decides if raster is to be clippd to a shapefile
#' @param change.dir  Change working directory to directy containing raster
#' @param zones  Number of elevation zones for curves
#' @export
#' @author Emmanuel Jjunju, \email{ejjunju@@gmail.com}
fx.Terrain2Curves<-function(
  zlim=NULL,
  clip2shp=TRUE,
  zones=10,
  change.dir=TRUE
){
  fx.CheckInstallLoadPkgs("audio")
  #Terrain.....................................................................
  fx.CheckInstallLoadPkgs(c("raster","fields","plotrix","tcltk2","rgdal"))
  Filters <- matrix(c("Raster", ".tif"),1, 2, byrow = TRUE)
  dem<-tk_choose.files(default = "",
                       caption = "Reservoir raster",
                       multi = FALSE,
                       filters = Filters,
                       index = 1)
  r <- raster(dem) # read the raster
  dir<-unlist(strsplit(dem,"/"))
  dir<-paste(dir[-length(dir)],collapse = "/")
  if(change.dir==TRUE){setwd(dir)}

  if(is.null(zlim)){#retain onklyvakues within a given range
    zlim=range(r[],na.rm=T)
    #hills(r,main=paste("Range",paste(round(zlim,2),collapse="-")))
  } else {
    r[r<zlim[1]]<-NA
    r[r>zlim[2]]<-NA
    #hills(r,main=paste("Range",paste(round(zlim,2),collapse="-")))
  }

  graphics.off()
  par(mfrow=c(1,2))
  hills(r,main=paste("Elevation| Range = c(",paste(round(zlim,2),collapse="-"),")"))

  #Clip raster to a given shapefile or to draw one............................................
  if(clip2shp==TRUE){
    out<-fx.ExtractByMask(r,clicks=100)
    r<-out$r
  }
  #hypsographic curve based on percentiles.....................................................
  b<-na.omit(getValues(r)) #get all raster values
  b<-sort(b) #sort them from lowest to highest
  P=seq(0,100,100/zones[1]) #Create percentiles for a hypdographic curve
  H<-quantile(b,na.rm=T,probs=P/100)#Create quantiles

  ##hypsographic curve based on Real Area.....................................................
  bi<-ceiling(seq(0,length(b),length(b)/10))
  Hi<-H
  Hi[]<-c(b[1],b[bi])
  (hypso<-data.frame(P,H,Hi))
  hypso$Zmean<-NA
  for(i in 2:(length(Hi))){
    hypso$Zmean[i]<- mean(b[(bi[i-1]+1):(bi[i])])
  }
  hypso[,-1]<-apply(hypso[,-1],2,function(x) fx.trail(x,2))
  print(hypso)
  #Map of Elevation Zones Zones................................................................
  (m<-Hi)
  (m<-cbind(m[-length(m)],m[-1]))
  (m<-cbind(m,1:nrow(m)))
  (rc <- reclassify(r, m,include.lowest=T)) #slow
  par(mfrow=c(2,2))
  hills(r,main="Elevation")
  box("figure",lwd=1,col=rgb(0,0,1,alpha = 0.5))
  #hills(rc,brks=P/10,main="Elevation Zones")


  hills(r,brks=unique(Hi),main="Elevation Zones")
  box("figure",lwd=1,col=rgb(0,0,1,alpha = 0.5))

  print(RES<-table(rc[]))
  cellarea<-prod(res(r))
  Area<-cumsum(RES*cellarea)

  par(mar=c(5,5,5,5))
  plot(hypso$P,as.numeric(hypso$H),pch=16,axes=F,main="Hypsographic curve")
  lines(hypso$P,as.numeric(hypso$Hi),col=2)
  axis(1,P,P,cex=0.5)
  axis(2,round(H,0),round(H,0),las=2,cex=0.5)
  abline(h=H,lty=1,col="grey")
  abline(v=P,lty=1,col="grey")
  grid()
  box()
  #plot(1:10,xlim=c(1,0),ylim=c(1,10),pch="",axes=F,ylab="",xlab="")
  addtable2plot(30, Hi[1], hypso, bty = "o", display.rownames = F, hlines = TRUE,
                vlines = TRUE,xpad=1,ypad=0.3)
  box("figure",lwd=1,col=rgb(0,0,1,alpha = 0.5))
  write.table(hypso,"hypsocurve",col.names = F)

  #Elevation Volume curve.........................................................................
  P2=seq(0,100,100/zones[1]) #Create percentiles for a E/V curve
  b2<-quantile(b,na.rm=T,probs=P2/100)#Create quantiles
  Kurve<-as.data.frame(array(dim=c(length(b2),2)))
  names(Kurve)<-c("Vol1000m3","H")
  Kurve$H<-b2
  Kurve$Vol1000m3[1]<-0
  for(i in 2:length(b2)){
    bs<-b[which(b<=b2[i])]
    DH<-b2[i]-bs
    Kurve$Vol1000m3[i]<-sum(DH*cellarea)/1000
  }
  write.table(Kurve,"rescurve.txt",col.names = F)
  #hills(r,brks=unique(b2))#col=rainbow(length(b2)-1))#,col=tim.colors(length(b1)-1))
  plot(Kurve,type="o",col=2,pch=16,main="Elevation - Volume Curve")
  grid()
  Kurve2<-apply(Kurve,2,function(x) fx.trail(x,2))
  addtable2plot(Kurve$Vol1000m3[ceiling(0.6*zones[1])], min(Kurve$H), Kurve2, bty = "o", display.rownames = F, hlines = TRUE,
                vlines = TRUE,xpad=1,ypad=0.3)
  box("figure",lwd=1,col=rgb(0,0,1,alpha = 0.5))

  box("outer",lwd=3,col="blue")


}

