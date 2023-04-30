### Brackets madness


a<- "a,b,(c),[d], {e}, <f>"

# delete () and leave inhalt
gsub("\\(|\\)", "", a)

# delet all in brackets ()
gsub(r"{\s*\([^\)]+\)}","",a)

# delet all in brackets ()
gsub(r"{\s*\{[^\}]+\)}","",a)

# delet all in brackets ()
gsub(r"{\s*\([^\)]+\)}","",a)

# delete alle in brackets (all bracket typs)
gsub("\\([^)]*)", "", a)
gsub("\\{[^}]*}", "", a)
gsub("\\[[^]]*]", "", a)
gsub("<[^>]*>", "", a)

# mulitple items
b <-"erstes Wort, zweites Wort (w)"
gsub("\\([^)]*)", "", b) # leaves whitespace
b <-"erstes Wort (m), zweites Wort (w)"
gsub("\\([^)]*)", "", b) # leaves whitespace

# () in word
c <-"amo(i)se"
gsub("\\([^)]*)", "", c) # klappt

c <-"amo(i)se, am(e)ise (w), ameise (w)"

# handle whitespaces
str_trim(gsub("\\([^)]*)", "", c) )# klappt nicht
spl <-str_split(gsub("\\([^)]*)", "", c) ,        ",")
spl[[1]]
paste(spl[[1]],collapse = ", ")# klappt paste aber nicht whitespace
paste(str_trim(spl[[1]]),collapse = ", ") # klappt 

# test for df

a <-c("amoise (w)","ameise (m)","amo(i)se (m), Ameise")
b <-c("baum (m)","Bäume <blah blah>","  Beme  , Bäume")

df <-cbind(a,b)
df
gs <-gsub("\\([^)]*)", "", df)
spl <-str_split(gs,        ",")
spl
spl <-str_split(df[,1],        ",")
spl
df[1,1]
spl <-str_split(df[1,1],        ",")
spl
paste(spl[[1]],collapse = ", ") # klappt

### dwa examples

d <- "H. Wint[e][?][e] <Rektor>"
gsub("\\([^)]*)", "", d)
gsub("\\{[^}]*}", "", d)
gsub("\\[[^]]*]", "", df)
gsub("<[^>]*>", "", d)

# delete () and leave inhalt
gsub("\\[|\\]", "", d)

da <-gsub("\\[|\\]", "", d)
da <-gsub("<[^>]*>", "", da)
da

# am(o)ise issue solved
a <- "amo(i)se (w), ameise (w)"
a <- str_split(a," ")[[1]]
grepl("^\\(.*$",a)

a <- a[grepl("^\\(.*$",a)==F]
a

paste(gsub("^\\(.*$","",a),collapse = ", ")

# dev fun
a <-c("amoise (w)","ameise (m)","amo(i)se (m), Ameise")
b <-c("baum (m)","Bä[?]ume <blah blah>","  Beme  , Bäume {doof}")
c <-c("katze (m)","Ka(t)ze (die)"," Rolli  , Rol[l]i")
df <-cbind(a,b,c)
df
df <-gsub("\\([^)]*)", "", df)
df <-gsub("\\([^)]*)", "", df)
df <-gsub("\\{[^}]*}", "", df)
df <-gsub("\\[[^]]*]", "", df)
df <-gsub("<[^>]*>", "", df)

df
c=1
r=1

### issue word (w, m)
df[r,c] <- "Ame(i)se (w, die)"


grepl("^\\(.*$",str_split(a," "))

# split, clean WS and rearrange strings for DF
for(c in 1:ncol(df)){
  
  for(r in 1:nrow(df)){
    # split by ',' 
    spl <-str_split(df[r,c],        ",")
    print(spl)
    # trim ws for each string in list
    for(i in 1:length(spl[[1]])){
      spl[[1]][i] <- str_trim(spl[[1]][i]) 
    }
    # rearrange with ", "
    df[r,c] <-paste(spl[[1]],collapse = ", ") # klappt
    

  }# rows
  
}# column loop

df
a <- "amo(i)se (w), ameise (w)"
a <- str_split(a," ")[[1]]
grepl("^\\(.*$",a)

a <- a[grepl("^\\(.*$",a)==F]
a

paste(gsub("^\\(.*$","",a),collapse = ", ")
c=3
r=3
### func dev

fun <- function(df){
  
  
  # split, clean WS and rearrange strings for DF
  for(c in 1:ncol(df)){
    
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
      # delete strings beginning with ( 
      str <- str[[1]][grepl("^\\(.*$",str[[1]])==F]
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

# generate test df
a <-c(1,2,3)
b <-c("amoise (w)","ameise (m)","amo(i)se (m), Ameise")
c <-c("baum (m)","Bä[?]ume <blah blah>","  Beme (w, m)  , Bäume {doof}")
d <-c("katze (m)","Ka(t)ze (die)"," Rolli  , Rol[l]i")
e <-c(" Hund (w) , Hun([d])e","(de[r]) H([?]u)nd"," {der} H(u)nd (w)")
df <-cbind(a,b,c,d,e)
df
df <-as.data.frame(df)
df
df2 <-clean_dwa(df,1:5)
df2
class(df)
require(stringdist)
stringdist(df[,2],df2[,2])
stringdist("a?c","abc")
stringdist("a?c","ac")
stringdist("ac","abc")
