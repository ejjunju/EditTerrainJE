rm(list=ls())
graphics.off()
#x11(20,14)
library(tcltk2)
#Only run if using rstudio to start in script directory#################################################################
yesno<-tkmessageBox(message = "Are you using Rstudio GUI?",
                    icon = "question", type = "yesnocancel", default = "yes")
if(as.character(yesno)=="yes"){
  trace(utils:::unpackPkgZip, quote(Sys.sleep(5)),at = which(grepl("Sys.sleep", body(utils:::unpackPkgZip), fixed = TRUE))) 
if(!require(rstudioapi))install.packages("rstudioapi")
require(rstudioapi)
scriptdir<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(scriptdir)
print(getwd())
}
#CHECK INSTALL LOAD DEPENDNCIES#########################################################################################
CheckInstallLoadPkgs = Vectorize(function(x) {
  #Ensure antivirus doesnt stop mising package installation (included in CheckInstallLoadPkgs)
  #interactive alternative #trace(utils:::unpackPkgZip, edit=TRUE)
  trace(utils:::unpackPkgZip, quote(Sys.sleep(5)),at = which(grepl("Sys.sleep", body(utils:::unpackPkgZip), fixed = TRUE))) 
  #check and install
  stopifnot(is.character(x))
  if(!is.element(x, rownames(installed.packages()))) install.packages(x,dependencies = TRUE)
  #load
  library(x, character.only = T, logical.return = T, quietly = T)
})
CheckInstallLoadPkgs(c("rgdal","raster","sp","tcltk2","akima","compiler")) #load/install required packages.

#CHECK if "EditTerrainJE" is installed then install and LOAD "EditTerrainJE" or Use Edit_Terrain_Functions.r###########
pkg<-require("EditTerrainJE")
if(pkg==FALSE){
  yesno<-tkmessageBox(message = "Do you have the RScript file EditTerrainJEFunctions.R?",
                      icon = "question", type = "yesnocancel", default = "yes")
  if(as.character(yesno)=="yes"){
    #Run using Rscript
    fscript<-tk_choose.files(default = "",
                             caption = "Select <<EditTerrainJEFunctions.R>>",
                             multi = TRUE, 
                             filters = matrix(c("Rscript","r"),1,2), 
                             index = 1)
    source(fscript)
    #Run Interactive edit
    Interactive_terrain_edit()
    
  } else{
    yesno<-tkmessageBox(message = "Do you want to  Install package EditTerrainJE (requires file EditTerrainJE.gz)?",
                        icon = "question", type = "yesnocancel", default = "yes")
    if(as.character(yesno)=="yes"){
      yesno<-tkmessageBox(message = "Is rtools installed? https://cran.r-project.org/bin/windows/Rtools/",
                          icon = "question", type = "yesnocancel", default = "yes")
      if(as.character(yesno)=="yes"){
        path_to_file<-tk_choose.files(caption = "Choose EditTerrainJE.zip",filters = matrix(c("zip","zip"),1,2))
        install.packages(path_to_file, repos = NULL, type="source")
        
        #Run using Just installed package
        library("EditTerrainJE")
        library(compiler)
        enableJIT(3)
        #Run Interactive edit
        Interactive_terrain_edit()
      } else {
        
        tkmessageBox(message = "You need to install rtools from https://cran.r-project.org/bin/windows/Rtools/", icon = "info", type = "ok")
      }
      
    } else {
      tkmessageBox(message = "You need to install pkg <<EditTerrainJE>> or use  <<EditTerrainJEFunctions.R>>.", icon = "info", type = "ok")
    }
  }
} else {
  #Run using installed package
  library("EditTerrainJE")
  library(compiler)
  enableJIT(3)
  #Run Interactive edit
  Interactive_terrain_edit()
}

