# EditTerrainJE
Interactively edit terrain. Perform cuts and fills in terrain using lines with start and end elevations


INSTALLATION (One time)
1)	If you already haven't ALREADY installed R and RStudio
•	If you already haven't installed R and RStudio
•	 Start by downloading R  from https://cran.uib.no/ , alternatively download from Microsoft R open https://mran.microsoft.com/download )
•	Download RStudio (https://www.rstudio.com/products/rstudio/download/#download)  
•	Install R
•	Install RStudio
•	Save the attached “EditTerrainJE_0.1.0.tar.gz” into a directory of your choice for example c “C:\Users\YOURUSERNAME\Documents”
•	Open RStudio 
 
•	Packages installation; In the console type and enter the following.
•	trace(utils:::unpackPkgZip, quote(Sys.sleep(5)),at = which(grepl("Sys.sleep", body(utils:::unpackPkgZip), fixed = TRUE)))
2)	Then install dependent packages by typing and entering in the console
Install.packages(c(‘raster','rgdal','akima','tcltk2','compiler','sp','devtools','roxygen2','maptools'),dependencies=TRUE)
3)	Then change directory to the working directory in which you saved the  EditTerrainJE_0.1.0.tar.gz”  by typing in the console
setwd(“C:\Users\ YOURUSERNAME \Documents”)
4)	Install the package by typing at the console (Only necessary to redo if there are updates)
install.packages("EditTerrainJE_0.1.0.tar.gz", repos = NULL, type="source",dependencies = TRUE)

RUNNING THE PROGRAMME
5)	Load the Package to mke the functions available (done once for each run)
library(EditTerrainJE)
6)	Before you run
a.	(Remember to have your Terrain (tif) and help -shapefiles (shp) in the same folder and make them have the same projection
b.	If you have a point shapefile with elevations, the elevations should be in an attribute table field named POINT_Z
7)	Run the program by typing in the console  and entering
ui()
•	Follow the prompts
•	Remember to have your Terrain (tif) and help -shapefiles (shp) in the same folder and make them have the same projection
•	If you have a point shapefile with elevations, the elevations should be in an attribute table field named POINT_Z
•	Your Results are written to a folder named Results 
•	All new files have the prefix “Result” in the file name


STEPS 5-7 are the only ones needed after the initial install

