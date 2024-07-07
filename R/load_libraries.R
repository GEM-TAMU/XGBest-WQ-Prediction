library(here)
library(lubridate)
library(hydroGOF)
library(hydroTSM)
library(caret)
library(dataRetrieval)
library(stringr)
library(tools)
library(survival)
library(EGRET)
library(tidyverse)
library(foreach)
library(doParallel)
library(xgboost)
library(tidytext)
library(SHAPforxgboost)
library(gg.layers)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(scales)
library(purrr)
library(future)
library(furrr)

#Don't load library for rloadest, 
#smwrQW package that is required in the rloadest package makes everything go haywire.
#Use loadest functions directly

#install.packages("remotes")
#remotes::install_github("USGS-R/smwrData")
#remotes::install_github("USGS-R/smwrBase")
#remotes::install_github("USGS-R/smwrGraphs")
#remotes::install_gitlab("water/analysis-tools/smwrStats",
#                        host = "code.usgs.gov")
#remotes::install_gitlab("water/analysis-tools/smwrQW",
#                        host = "code.usgs.gov")
#remotes::install_github("USGS-R/rloadest")

#remotes::install_github('rpkgs/gg.layers')
