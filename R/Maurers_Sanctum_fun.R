hndl_UCnew2 <- function(data,cmin,cmax){
  
  # set container
  count_ncol <-0
  count_non  <-0
  count_ncolu<-0
  count_nonu <-0
  #loop
  
  
  for(i in cmin:cmax){
    # print for each row the name, Non NFC strings and by this caused more uniques
    cat(paste0("Column_",i," - ",length(which(stri_trans_isnfc(data[,i])==F))," Non NFC strings detected, leading to ",
               length(unique(data[,i]))-length(unique(stri_trans_nfc(data[,i]))),
               " more uniques"),sep = "\n")
    
    
    # sum up
    if(length(which(stri_trans_isnfc(data[,i])==F))>0){
      count_ncol <- count_ncol+1}
    count_non <- count_non+length(which(stri_trans_isnfc(data[,i])==F))
    if(length(unique(data[,i]))-length(unique(stri_trans_nfc(data[,i])))>0){
      count_ncolu <- count_ncolu+1}
    count_nonu <- count_nonu+length(unique(data[,i]))-length(unique(stri_trans_nfc(data[,i])))
    
    # transform all columns
    data[,i] <-stri_trans_nfc(data[,i])
    
  }# end loop
  cat(paste0(count_ncol," columns contain a total of ",count_non, " non UFC strings, leading to a total of ",
             count_nonu, " lesser uniques in ",count_ncolu, " columns."))
  res_uc <-paste0(count_ncol," columns contain a total of ",count_non, " non UFC strings. Transformation to NFC leads to a total of ",
                  count_nonu, " lesser uniques in ",count_ncolu, " columns.")
  
  # return
  return(list(data=data,res_uc=res_uc))
}# end function

anyWS <- function(df){
  # check for tailling
  tail <- lapply(1:ncol(df), function(x){
    length(which((grepl(" $", df[,x])==T)))
  })
  # print column
  for(i in 1:ncol(df)){
    cat(paste0("Column_",i, " - ", tail[i]," tailing whitespaces detected"),sep="\n")
  }
  
  # sum up
  cat(paste0(length(which(tail>0))," columns contain a total of ",sum(unlist(tail)), " tailling whitespaces"),sep = "\n")
  res_tail <-paste0(length(which(tail>0))," columns contain a total of ",sum(unlist(tail)), " tailling whitespaces")
  
  # check for leading
  lead <- lapply(1:ncol(df), function(x){
    length(which((grepl("^ ", df[,x])==T)))
  })
  # print column
  for(i in 1:ncol(df)){
    cat(paste0("Column_",i, " - ", lead[i]," leading whitespaces detected"),sep="\n")
  }
  
  # sum up
  cat(paste0(length(which(lead>0))," columns contain a total of ",sum(unlist(lead)), " leading whitespaces"),sep = "\n")
  res_lead <-paste0(length(which(lead>0))," columns contain a total of ",sum(unlist(lead)), " leading whitespaces")
  
  # trim both leading amnd tailing whitespaces
  for (i in 1:ncol(df)) {
    df[,i] <-str_trim(df[,i], "both") 
  }
  
  # return
  return(list(df=df,res_lead=res_lead,res_tail=res_tail))
} # end of function

testCol <- function(df,n){
  # get containers to count results
  count <-0
  count_na<-0
  count_e <-0
  count_ka <-0
  
  for(i in 1:ncol(df)){
    nempty <-length(which(df[,i]=="" ))
    nkempty <-length(which(df[,i]=="k.A." ))
    naempty <-length(which(is.na(org[,i])))
    if(nempty/nrow(df)>n){
      per=round(nempty/nrow(df),digits=5)
      cat(paste0("Column_",i, " - ", per*100,"% of entries with ' ' detected"),sep="\n")
      count<- count+1
      count_e <-count_e +1
    }
    if(naempty/nrow(df)>n){
      per=round(naempty/nrow(df),digits=5)
      cat(paste0("Column_",i, " - ", per*100,"% of entries with 'NA' detected"),sep="\n")
      count<- count+1
      count_na <-count_na +1
    }
    if(nkempty/nrow(df)>n){
      per=round(nkempty/nrow(df),digits=5)
      cat(paste0("Column_",i, " - ", per*100,"% of entries with 'k.A.' detected"),sep="\n") 
      count<- count+1
      count_ka <-count_ka +1
    }
  }
  res_col <- paste0(count," Columns have more than ",n*100,"% missing values")
  res_col_na <- paste0(count_na," Columns have more than ",n*100,"% 'NA' values")
  res_col_e <- paste0(count_e," Columns have more than ",n*100,"% ' ' (empty) values")
  res_col_ka <- paste0(count_ka," Columns have more than ",n*100,"% 'k.A.' values")
  
  return (list(res_col=res_col,res_col_na=res_col_na,res_col_e=res_col_e,res_col_ka=res_col_ka))
  
} # end of function

checkMissing <- function(df,ka,ka_mm,replace_ka=F){
  # print amount of matches
  
  # print amount of correct "keine Angabe"
  cat(paste0(length(which(df==ka,arr.ind = T)), " entires '",ka, "' detected"   ),sep="\n")
  for(i in 1: length(ka_mm)){
    # print amount of correct "for every item in ka_mm
    cat(paste0(length(which(df==ka_mm[i],arr.ind = T)), " entires '",ka_mm[i], "' detected"   ),sep="\n")
  }
  
  # replace
  if(replace_ka==T){
    cat(paste0("Replacing '",ka_mm,"' with '",ka,"'"),sep = "\n")
    for(i in 1: length(ka_mm)){
      
      # replace
      df[which(df==ka_mm[i],arr.ind = T)] <- ka
    }
    return(df)
  }
  
  
} # end of function

multiPlaces <- function(df){
  # get table
  occ <- data.frame(table(df$LONG))
  # merge with df to get places names
  merg <- base::merge(df, occ, by.x = "LONG",by.y= "Var1")
  # subset rows with multiple occurrences
  merg1 <- merg[merg$Freq>1,]
  merg_uc <-merg[merg$Freq==1,]
  # set container for count
  count <-0
  # get name of place which occures multiple times
  for(i in 1:length(unique(merg1$LONG))){
    count <- count+1
    cat(paste0("Place '", merg1$Ort[which(merg1$LONG==unique(merg1$LONG)[i])[1]],"' has ",
               merg1$Freq[which(merg1$LONG==unique(merg1$LONG)[i])[1]]," rows"),sep="\n")
    
  }
  # print total amount
  cat(paste0(count," Places have muliple rows"),sep="\n")
  res_mp <- paste0(count," Places have muliple rows")
  
  # calculate total amount of unique places
  n_places <- nrow(merg_uc)+count
  # return
  return(list(res_mp=res_mp,n_places=n_places))
}# end of function

convert2utm <- function(df,long_col,lat_col){
  # convert long and lat columns to numeric
  df[,long_col] <- as.numeric(df[,long_col])
  df[,lat_col] <- as.numeric(df[,lat_col])
  
  # convert to spatial object
  wgs <- SpatialPointsDataFrame(df[,c(long_col,lat_col)],df)
  
  # set crs wgs84
  proj4string(wgs) <- "+proj=longlat +datum=WGS84 +no_defs"
  
  #plot(wgs)
  
  # project to utm32
  utm <- spTransform(wgs,CRS("+init=epsg:25832"))
  
  #plot(utm)
  # get geometry
  geo <- geom(utm)
  # write UTM geometry
  df$utm_e <- geo[,2]
  df$utm_n <- geo[,3]
  
  # reorder columns
  df3 <- df[,c(1:lat_col,ncol(df)-1,ncol(df),
               (lat_col+1):(ncol(df)-2))]
  
  # return
  return(list(df=df3,mapview_utm=utm,mapview_wgs=wgs))
}# end of function