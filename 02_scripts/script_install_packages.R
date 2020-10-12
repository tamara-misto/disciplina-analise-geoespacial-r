#' ---
#' title: instalar pacotes
#' author: mauricio vancine
#' date: 2020-04-25
#' ---

# data manipulation and visualization -------------------------------------
# manipulation and visualization
if(!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE)
if(!require(lubridate)) install.packages("lubridate", dependencies = TRUE)

# directory
if(!require(here)) install.packages("here", dependencies = TRUE)

# xlsx
if(!require(openxlsx)) install.packages("openxlsx", dependencies = TRUE)
if(!require(readxl)) install.packages("readxl", dependencies = TRUE)
if(!require(writexl)) install.packages("writexl", dependencies = TRUE)

# geospatial data ---------------------------------------------------------
# vectors
if(!require(geobr)) install.packages("geobr")
if(!require(rnaturalearth)) install.packages("rnaturalearth", dependencies = TRUE)
if(!require(sf)) install.packages("sf", dependencies = TRUE)

# manipulation and visualization
if(!require(raster)) install.packages("raster", dependencies = TRUE)
if(!require(rgdal)) install.packages("rgdal", dependencies = TRUE)
if(!require(wesanderson)) install.packages("wesanderson")

# maps
if(!require(ggspatial)) install.packages("ggspatial", dependencies = TRUE)
if(!require(tmap)) install.packages("tmap", dependencies = TRUE)

# end ---------------------------------------------------------------------