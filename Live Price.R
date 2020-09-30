library(quantmod)
c <- getQuote("MHRIL.BO",what = standardQuote())

getQuote("MHRIL.BO",what=yahooQF())
         
c
class(c)

d <- getQuote("MHRIL.BO;MHRIL.NS;INDHOTEL.BO",what = standardQuote())
d <- subset(d,select = c(2,3,4,5,6,7))
d <- datatable(d)
