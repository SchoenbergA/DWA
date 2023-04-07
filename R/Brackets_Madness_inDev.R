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
gsub("\\[[^]]*]", "", d)
gsub("<[^>]*>", "", d)

# delete () and leave inhalt
gsub("\\[|\\]", "", d)

da <-gsub("\\[|\\]", "", d)
da <-gsub("<[^>]*>", "", da)
da
