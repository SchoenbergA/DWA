### Get HiWi Progress and Stats

# load current data, check progress and get full data
# check for unsure and WS

# set environment paths
wd <- "C:/Envimaster/DWA" # local path to repository
dat <- file.path(wd,"Data")
r <- file.path(wd,"R")

# source function script
source(file.path(r,"DWA_status_functions.R"))

# check stats
dwa_full <-DWA_stats(path=dat,dup_check = T)
WS_stats(dat)
DWA_unsure(dat)

write.xlsx(dwa_full,file.path(wd,"table_for_control.xlsx"))

# check NA status
which(is.na(dwa_full$`49..Gurke`)) # 2395
which(is.na(dwa_full$`19..Deichsel`))# 1933
which(is.na(dwa_full$`60..Hebamme.(Die.Frau,.die.die.Entbindung.der.Wöchnerinnen.vornimmt)`))# 2432
which(is.na(dwa_full$`80..Katze.(männlich)`))# 882
which(is.na(dwa_full$`109..Mutterschwein`)) # 811
which(is.na(dwa_full$`170a..Obertasse`)) # 757, 1110, 1196

