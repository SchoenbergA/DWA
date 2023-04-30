### DWA Brackets claen up function



clean_dwa <- function(df,cols){
  
  # split, clean WS and rearrange strings for DF
  for(c in cols){
    cat(paste0("cleaning column ",c," ",c-min(cols)+1," / ",length(cols)),sep="\n")
    for(r in 1:nrow(df)){
      # clean all comments <> {} for df
      df[r,c] <-gsub("<[^>]*>", "", df[r,c])
      df[r,c] <-gsub("\\{[^}]*}", "", df[r,c])
      
      # delete unsure [] and keep content
      df[r,c] <-gsub("\\[|\\]", "", df[r,c])
      ### split by "," and trim WS
      # split by ',' 
      spl <-str_split(df[r,c],        ", ")
      # trim ws for each string in list
      for(i in 1:length(spl[[1]])){
        spl[[1]][i] <- str_trim(spl[[1]][i]) 
      }
      # rearrange with ", "
      str <-paste(spl[[1]],collapse = " ") 
      
      ### split by " " and identify which strings strat with "("
      str <-str_split(str,        " ")
      # delete strings beginning with ( an ending with )
      str <- str[[1]][grepl("^\\(.*$",str[[1]])==F]
      str <- str[grepl(".\\)$",str)==F]
      # trim ws for each string in list
      for(i in 1:length(str)){
        str[i] <- str_trim(str[i]) 
      }
      # rearrange with ", "
      df[r,c] <-paste(str,collapse = ", ") # klappt
      
      # delete unsure t(e)st and keep content
      df[r,c] <-gsub("\\(|\\)", "", df[r,c])
    }# rows
    
  }# column loop
  
  
  return(df)
}# end of function

