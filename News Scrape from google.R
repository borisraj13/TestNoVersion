library(DT)


library(feedeR)  



myquery <- feed.extract("https://news.google.com/rss/search?q=hotels market sector india")
hsn <- data.frame(myquery$items)

hsn <- subset(hsn, select = c("date","title","link"))

names(hsn)[1] <- "Date"
names(hsn)[2] <- "Title"
names(hsn)[3] <- "Link"
hsn$Date <- as.Date(hsn$Date, format="%Y-%m-%d")
hsn$Link <- paste0("<a href='",hsn$Link,"'>",hsn$Link,"</a>")
hsn <- datatable(hsn,escape = 3)


myquery <- feed.extract("https://news.google.com/rss/search?q=expert advice hotels market sector india")
exn <- data.frame(myquery$items)

exn <- subset(exn, select = c("date","title","link"))

names(exn)[1] <- "Date"
names(exn)[2] <- "Title"
names(exn)[3] <- "Link"
exn$Date <- as.Date(exn$Date, format="%Y-%m-%d")
exn$Link <- paste0("<a href='",exn$Link,"'>",exn$Link,"</a>")
exn <- datatable(exn,escape = 3)

#Mahindra Holidays

myquery <- feed.extract("https://news.google.com/rss/search?q=mahindra holidays and resort")
mhrn <- data.frame(myquery$items)

mhrn <- subset(mhrn, select = c("date","title","link"))

head(mhrn)


#Indian Hotels

myquery <- feed.extract("https://news.google.com/rss/search?q=indian hotels company limited")
ihn <- data.frame(myquery$items)

ihn <- subset(ihn, select = c("date","title","link"))
class(ihn$date)
ihn$link <- paste0("<a href='",ihn$link,"'>",ihn$link,"</a>")
ihn <- datatable(ihn, escape = 3)
#Oriental Hotels

myquery <- feed.extract("https://news.google.com/rss/search?q=oriental hotels ltd")
ohn <- data.frame(myquery$items)

ohn <- subset(ohn, select = c("date","title","link"))

head(ohn)

#Taj gvk hotels

myquery <- feed.extract("https://news.google.com/rss/search?q=taj gvk hotels")
tajn <- data.frame(myquery$items)

tajn <- subset(tajn, select = c("date","title","link"))

head(tajn)

class(tajn$link)
