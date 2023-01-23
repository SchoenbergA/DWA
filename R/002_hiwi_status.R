### Get HiWi Progress and Stats

# set environment paths
wd <- "C:/Envimaster/DWA" # local path to repository
dat <- file.path(wd,"Data")
r <- file.path(wd,"R")

# source function script
source(file.path(r,"DWA_functions.R"))

# load data
al <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"DWA_full_AL.xlsx"))
jh <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"DWA_full_JH.xlsx"))
fs <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"DWA_full_fs.xlsx"))
jr <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"DWA_full_jr.xlsx"))

# check stats
DWA_stats()
WS_stats()
DWA_unsure()
