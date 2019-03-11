# #instructions to make package
#http://kbroman.org/pkg_primer/pages/build.html
# install.packages("devtools")
# library("devtools")#......1
# devtools::install_github("klutometis/roxygen")
# library(roxygen2)#......2
# #Step 1: Create your package directory
# setwd("C:/Users/noemma/Dropbox/DEV/R/SlopingLines2Raster")
# create("EditTerrainJE")
# #Step 2: Add functions in R folder
# #Step 3: Add documentation
# #Step 4: Process your documentation
# setwd("./EditTerrainJE")
# document()
# build() #create the .tar.gz file.
# #Step 5: Install!
# setwd("..")
# install("EditTerrainJE")
#(Bonus) Step 6: Make the package a GitHub repo
# install_github('EditTerrainJE','ejjunju')
#install from github
#devtools install_github()
library(devtools)
library(roxygen2)
document()
build()
remove.packages("EditTerrainJE")
install.packages("../EditTerrainJE_0.1.1.tar.gz", repos = NULL, type="source",dependencies = TRUE)

#OR
#install from git_hub
#Ensures that antivirus doesnt stop installation of packages
#trace(utils:::unpackPkgZip, quote(Sys.sleep(5)),at = which(grepl("Sys.sleep", body(utils:::unpackPkgZip), fixed = TRUE)))
#installs EditTerrainJE from github
#Noneed to run this everytime
#install_github('EditTerrainJE','ejjunju',force=TRUE) #force=TRUE ensures yiu have the lates version

#loads package
library(EditTerrainJE)

#OR
#if you dont want to use the package
#source('./EditTerrainJE/R/EditTerrainJEFunctions.R')

#Runs user interface
example.project()
ui()

#GITHIB
#connect package to git
#connect with github
#https://cfss.uchicago.edu/git05.html
#https://github.com/ejjunju/EditTerrainJE
#ejj-Bm311!

#Rstudio Tools shell
#git remote add origin https://github.com/ejjunju/EditTerrainJE
#git pull origin master
#git push -u origin master
#update git
# $ git config --global user.name "ejjunju"
# git config --global user.name
# $ git config --global user.email "ejjunju@gmail.com"
# $ git config --global user.email
#Then push
