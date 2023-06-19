### Check merged Table Level 3 cleaning Unsure and missing values

# set environment paths
wd <- "C:/Envimaster/DWA/Data" # local path to repository
dat <- file.path(wd,"Data")
r <- file.path(wd,"R")

require(openxlsx)
# source function script
source(file.path(r,"DWA_status_functions.R"))

# load tables
old <- read.xlsx(file.path(wd,"DWA_Level_2.xlsx")) # equal to act stand aus allen 4 tabs
new <- read.xlsx(file.path(wd,"DWA_Level_3.xlsx")) # weniger NA als old, aber auch mismatches
cle <- read.xlsx(file.path(wd,"DWA_Level_1_unsure_cleaned.xlsx")) # unbekannt welcher stagte das ist

# all data sorted by hand in order by DigiIndex

# check if order is equal
head(cle)[1:5]
head(old)[1:5]
head(new)[1:5]

# check NA
cols<- 1:ncol(df)
cols <-c(25:40,42:50,52,54:56)
check_NA(cle,cols)
check_NA(old,cols)
check_NA(new,cols)

# check unsure
check_unsure(cle)
check_unsure(old)
check_unsure(new)

# all,equal test
all.equal(old,new)
all.equal(cle,old)
all.equal(new,cle)


# compare mismatches old vs new
all.equal(old,new)

which(old$`51..Hagebutte`!=new$`51..Hagebutte`)
old$`51..Hagebutte`[1914]
new$`51..Hagebutte`[1914]

which(old$`52..Hahn,.Henne`!=new$`52..Hahn,.Henne`)
old$`52..Hahn,.Henne`[1914]
new$`52..Hahn,.Henne`[1914]

# maybe a generall issue with this row 1914 ???
old$`Bearbeiten/in`[1914]


# check differences
which(old$Koord_unten!=cle$Koord_unten)

old$Koord_unten[c(1,357)]
cle$Koord_unten[c(1,357)]
