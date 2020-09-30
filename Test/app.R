library(ggplot2)
library(plotly)
library(quantmod)
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

names(mhrn)[1] <- "Date"
names(mhrn)[2] <- "Title"
names(mhrn)[3] <- "Link"
mhrn$Date <- as.Date(mhrn$Date, format="%Y-%m-%d")
mhrn$Link <- paste0("<a href='",mhrn$Link,"'>",mhrn$Link,"</a>")
mhrn <- datatable(mhrn,escape = 3)


#Indian Hotels

myquery <- feed.extract("https://news.google.com/rss/search?q=indian hotels company limited")
ihn <- data.frame(myquery$items)

ihn <- subset(ihn, select = c("date","title","link"))
names(ihn)[1] <- "Date"
names(ihn)[2] <- "Title"
names(ihn)[3] <- "Link"
ihn$Date <- as.Date(ihn$Date, format="%Y-%m-%d")
ihn$Link <- paste0("<a href='",ihn$Link,"'>",ihn$Link,"</a>")
ihn <- datatable(ihn,escape = 3)
head(ihn)

#Oriental Hotels

myquery <- feed.extract("https://news.google.com/rss/search?q=oriental hotels ltd")
ohn <- data.frame(myquery$items)
ohn <- subset(ohn, select = c("date","title","link"))
names(ohn)[1] <- "Date"
names(ohn)[2] <- "Title"
names(ohn)[3] <- "Link"
ohn$Date <- as.Date(ohn$Date, format="%Y-%m-%d")
ohn$Link <- paste0("<a href='",ohn$Link,"'>",ohn$Link,"</a>")
ohn <- datatable(ohn,escape = 3)

head(ohn)

#Taj gvk hotels

myquery <- feed.extract("https://news.google.com/rss/search?q=taj gvk hotels")
tajn <- data.frame(myquery$items)

tajn <- subset(tajn, select = c("date","title","link"))
names(tajn)[1] <- "Date"
names(tajn)[2] <- "Title"
names(tajn)[3] <- "Link"
tajn$Date <- as.Date(tajn$Date, format="%Y-%m-%d")
tajn$Link <- paste0("<a href='",tajn$Link,"'>",tajn$Link,"</a>")
tajn <- datatable(tajn,escape = 3)
head(tajn)



# get data
# get data
getSymbols("MHRIL.NS",src='yahoo')
MHRIL.NS <- na.omit(MHRIL.NS)
df <- data.frame(Date=index(MHRIL.NS),coredata(MHRIL.NS))

# create Bollinger Bands
bbands <- BBands(MHRIL.NS[,c("MHRIL.NS.High","MHRIL.NS.Low","MHRIL.NS.Close")])

# join and subset data
df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2015-01-01")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
    if (df$MHRIL.NS.Close[i] >= df$MHRIL.NS.Open[i]) {
        df$directionn[i] = 'Increasing'
    } else {
        df$direction[i] = 'Decreasing'
    }
}

i <- list(line = list(color = '#34B828'))
d <- list(line = list(color = '#C71616'))

# plot candlestick chart

fig <- df %>% plot_ly(x = ~Date, type="candlestick",
                      open = ~MHRIL.NS.Open, close = ~MHRIL.NS.Close,
                      high = ~MHRIL.NS.High, low = ~MHRIL.NS.Low, name = "MHRIL.NS",
                      increasing = i, decreasing = d) 
fig <- fig %>% add_lines(x = ~Date, y = ~up , name = "B Bands",
                         line = list(color = '#ccc', width = 0.5),
                         legendgroup = "Bollinger Bands",
                         hoverinfo = "none", inherit = F) 
fig <- fig %>% add_lines(x = ~Date, y = ~dn, name = "B Bands",
                         line = list(color = '#ccc', width = 0.5),
                         legendgroup = "Bollinger Bands", inherit = F,
                         showlegend = FALSE, hoverinfo = "none") 
fig <- fig %>% add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
                         line = list(color = '#E377C2', width = 0.5),
                         hoverinfo = "none", inherit = F) 
fig <- fig %>% layout(yaxis = list(title = "Price"))

# plot volume bar chart
fig2 <- df 
fig2 <- fig2 %>% plot_ly(x=~Date, y=~MHRIL.NS.Volume, type='bar', name = "MHRIL.NS Volume",
                         color = ~direction, colors = c('#34B828','#C71616')) 
fig2 <- fig2 %>% layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
               list(count=1,
                    label='RESET',
                    step='all'),
               list(count=1,
                    label='1 YR',
                    step='year',
                    stepmode='backward'),
               list(count=3,
                    label='3 MO',
                    step='month',
                    stepmode='backward'),
               list(count=1,
                    label='1 MO',
                    step='month',
                    stepmode='backward'),
               list(count=5,
                    label='5 DY',
                    step='day',
                    stepmode='backward'),
               list(count=1,
                    label='1 DY',
                    step='day',
                    stepmode='backward')
           ))

# subplot with shared x axis
fig <- fig %>% layout(title = paste("MHRIL.NS: 2015-01-01 -",Sys.Date()),
                      xaxis = list(rangeselector = rs),
                      legend = list(orientation = 'h', x = 0.5, y = 1,
                                    xanchor = 'center', yref = 'paper',
                                    font = list(size = 10),
                                    bgcolor = 'transparent'))



fig2 <- fig2 %>% layout(title = paste("Volume MHRIL.NS: 2015-01-01 -",Sys.Date()),
                        xaxis = list(rangeselector = rs),
                        legend = list(orientation = 'h', x = 0.5, y = 1,
                                      xanchor = 'center', yref = 'paper',
                                      font = list(size = 10),
                                      bgcolor = 'transparent'))

fig
fig2




#BS

# get data
getSymbols("MHRIL.BO",src='yahoo')
MHRIL.BO <- na.omit(MHRIL.BO)
df1 <- data.frame(Date=index(MHRIL.BO),coredata(MHRIL.BO))

# create Bollinger Bands
bbands <- BBands(MHRIL.BO[,c("MHRIL.BO.High","MHRIL.BO.Low","MHRIL.BO.Close")])

# join and subset data
df1 <- subset(cbind(df1, data.frame(bbands[,1:3])), Date >= "2015-01-01")

# colors column for increasing and decreasing
for (i in 1:length(df1[,1])) {
    if (df1$MHRIL.BO.Close[i] >= df1$MHRIL.BO.Open[i]) {
        df1$directionn[i] = 'Increasing'
    } else {
        df1$direction[i] = 'Decreasing'
    }
}

i1 <- list(line = list(color = '#34B828'))
d1 <- list(line = list(color = '#C71616'))

# plot candlestick chart

figbo <- df1 %>% plot_ly(x = ~Date, type="candlestick",
                         open = ~MHRIL.BO.Open, close = ~MHRIL.BO.Close,
                         high = ~MHRIL.BO.High, low = ~MHRIL.BO.Low, name = "MHRIL.BO",
                         increasing = i1, decreasing = d1) 
figbo <- figbo %>% add_lines(x = ~Date, y = ~up , name = "B Bands",
                             line = list(color = '#ccc', width = 0.5),
                             legendgroup = "Bollinger Bands",
                             hoverinfo = "none", inherit = F) 
figbo <- figbo %>% add_lines(x = ~Date, y = ~dn, name = "B Bands",
                             line = list(color = '#ccc', width = 0.5),
                             legendgroup = "Bollinger Bands", inherit = F,
                             showlegend = FALSE, hoverinfo = "none") 
figbo <- figbo %>% add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
                             line = list(color = '#E377C2', width = 0.5),
                             hoverinfo = "none", inherit = F) 
figbo <- figbo %>% layout(yaxis = list(title = "Price"))

# plot volume bar chart
fig2bo <- df1 
fig2bo <- fig2bo %>% plot_ly(x=~Date, y=~MHRIL.BO.Volume, type='bar', name = "MHRIL.BO Volume",
                             color = ~direction, colors = c('#34B828','#C71616')) 
fig2bo <- fig2bo %>% layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
               list(count=1,
                    label='RESET',
                    step='all'),
               list(count=1,
                    label='1 YR',
                    step='year',
                    stepmode='backward'),
               list(count=3,
                    label='3 MO',
                    step='month',
                    stepmode='backward'),
               list(count=1,
                    label='1 MO',
                    step='month',
                    stepmode='backward'),
               list(count=5,
                    label='5 DY',
                    step='day',
                    stepmode='backward'),
               list(count=1,
                    label='1 DY',
                    step='day',
                    stepmode='backward')
           ))

# subplot with shared x axis
figbo <- figbo %>% layout(title = paste("MHRIL.BO: 2015-01-01 -",Sys.Date()),
                          xaxis = list(rangeselector = rs),
                          legend = list(orientation = 'h', x = 0.5, y = 1,
                                        xanchor = 'center', yref = 'paper',
                                        font = list(size = 10),
                                        bgcolor = 'transparent'))



fig2bo <- fig2bo %>% layout(title = paste("Volume MHRIL.BO: 2015-01-01 -",Sys.Date()),
                            xaxis = list(rangeselector = rs),
                            legend = list(orientation = 'h', x = 0.5, y = 1,
                                          xanchor = 'center', yref = 'paper',
                                          font = list(size = 10),
                                          bgcolor = 'transparent'))

figbo
fig2bo






# get data
getSymbols("INDHOTEL.NS",src='yahoo')
INDHOTEL.NS <- na.omit(INDHOTEL.NS)
df3 <- data.frame(Date=index(INDHOTEL.NS),coredata(INDHOTEL.NS))

# create Bollinger Bands
bbands <- BBands(INDHOTEL.NS[,c("INDHOTEL.NS.High","INDHOTEL.NS.Low","INDHOTEL.NS.Close")])

# join and subset data
df3 <- subset(cbind(df3, data.frame(bbands[,1:3])), Date >= "2015-01-01")

# colors column for increasing and decreasing
for (i in 1:length(df3[,1])) {
    if (df3$INDHOTEL.NS.Close[i] >= df3$INDHOTEL.NS.Open[i]) {
        df3$directionn[i] = 'Increasing'
    } else {
        df3$direction[i] = 'Decreasing'
    }
}

i2 <- list(line = list(color = '#34B828'))
d2 <- list(line = list(color = '#C71616'))

# plot candlestick chart

fig3 <- df3 %>% plot_ly(x = ~Date, type="candlestick",
                        open = ~INDHOTEL.NS.Open, close = ~INDHOTEL.NS.Close,
                        high = ~INDHOTEL.NS.High, low = ~INDHOTEL.NS.Low, name = "INDHOTEL.NS",
                        increasing = i2, decreasing = d2) 
fig3 <- fig3 %>% add_lines(x = ~Date, y = ~up , name = "B Bands",
                           line = list(color = '#ccc', width = 0.5),
                           legendgroup = "Bollinger Bands",
                           hoverinfo = "none", inherit = F) 
fig3 <- fig3 %>% add_lines(x = ~Date, y = ~dn, name = "B Bands",
                           line = list(color = '#ccc', width = 0.5),
                           legendgroup = "Bollinger Bands", inherit = F,
                           showlegend = FALSE, hoverinfo = "none") 
fig3 <- fig3 %>% add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
                           line = list(color = '#E377C2', width = 0.5),
                           hoverinfo = "none", inherit = F) 
fig3 <- fig3 %>% layout(yaxis = list(title = "Price"))

# plot volume bar chart
fig4 <- df3 
fig4 <- fig4 %>% plot_ly(x=~Date, y=~INDHOTEL.NS.Volume, type='bar', name = "INDHOTEL.NS Volume",
                         color = ~direction, colors = c('#34B828','#C71616')) 
fig4 <- fig4 %>% layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
               list(count=1,
                    label='RESET',
                    step='all'),
               list(count=1,
                    label='1 YR',
                    step='year',
                    stepmode='backward'),
               list(count=3,
                    label='3 MO',
                    step='month',
                    stepmode='backward'),
               list(count=1,
                    label='1 MO',
                    step='month',
                    stepmode='backward'),
               list(count=5,
                    label='5 DY',
                    step='day',
                    stepmode='backward'),
               list(count=1,
                    label='1 DY',
                    step='day',
                    stepmode='backward')
           ))

# subplot with shared x axis
fig3 <- fig3 %>% layout(title = paste("INDHOTEL.NS: 2015-01-01 -",Sys.Date()),
                        xaxis = list(rangeselector = rs),
                        legend = list(orientation = 'h', x = 0.5, y = 1,
                                      xanchor = 'center', yref = 'paper',
                                      font = list(size = 10),
                                      bgcolor = 'transparent'))



fig4 <- fig4 %>% layout(title = paste("Volume INDHOTEL.NS: 2015-01-01 -",Sys.Date()),
                        xaxis = list(rangeselector = rs),
                        legend = list(orientation = 'h', x = 0.5, y = 1,
                                      xanchor = 'center', yref = 'paper',
                                      font = list(size = 10),
                                      bgcolor = 'transparent'))

fig3
fig4




#BS

# get data
getSymbols("INDHOTEL.BO",src='yahoo')
INDHOTEL.BO <- na.omit(INDHOTEL.BO)
df4 <- data.frame(Date=index(INDHOTEL.BO),coredata(INDHOTEL.BO))

# create Bollinger Bands
bbands <- BBands(INDHOTEL.BO[,c("INDHOTEL.BO.High","INDHOTEL.BO.Low","INDHOTEL.BO.Close")])

# join and subset data
df4 <- subset(cbind(df4, data.frame(bbands[,1:3])), Date >= "2015-01-01")

# colors column for increasing and decreasing
for (i in 1:length(df4[,1])) {
    if (df4$INDHOTEL.BO.Close[i] >= df4$INDHOTEL.BO.Open[i]) {
        df4$directionn[i] = 'Increasing'
    } else {
        df4$direction[i] = 'Decreasing'
    }
}

i3 <- list(line = list(color = '#34B828'))
d3 <- list(line = list(color = '#C71616'))

# plot candlestick chart

figbo1 <- df4 %>% plot_ly(x = ~Date, type="candlestick",
                          open = ~INDHOTEL.BO.Open, close = ~INDHOTEL.BO.Close,
                          high = ~INDHOTEL.BO.High, low = ~INDHOTEL.BO.Low, name = "INDHOTEL.BO",
                          increasing = i3, decreasing = d3) 
figbo1 <- figbo1 %>% add_lines(x = ~Date, y = ~up , name = "B Bands",
                               line = list(color = '#ccc', width = 0.5),
                               legendgroup = "Bollinger Bands",
                               hoverinfo = "none", inherit = F) 
figbo1 <- figbo1 %>% add_lines(x = ~Date, y = ~dn, name = "B Bands",
                               line = list(color = '#ccc', width = 0.5),
                               legendgroup = "Bollinger Bands", inherit = F,
                               showlegend = FALSE, hoverinfo = "none") 
figbo1 <- figbo1 %>% add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
                               line = list(color = '#E377C2', width = 0.5),
                               hoverinfo = "none", inherit = F) 
figbo1 <- figbo1 %>% layout(yaxis = list(title = "Price"))

# plot volume bar chart
fig2bo1 <- df4 
fig2bo1 <- fig2bo1 %>% plot_ly(x=~Date, y=~INDHOTEL.BO.Volume, type='bar', name = "INDHOTEL.BO Volume",
                               color = ~direction, colors = c('#34B828','#C71616')) 
fig2bo1 <- fig2bo1 %>% layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
               list(count=1,
                    label='RESET',
                    step='all'),
               list(count=1,
                    label='1 YR',
                    step='year',
                    stepmode='backward'),
               list(count=3,
                    label='3 MO',
                    step='month',
                    stepmode='backward'),
               list(count=1,
                    label='1 MO',
                    step='month',
                    stepmode='backward'),
               list(count=5,
                    label='5 DY',
                    step='day',
                    stepmode='backward'),
               list(count=1,
                    label='1 DY',
                    step='day',
                    stepmode='backward')
           ))

# subplot with shared x axis
figbo1 <- figbo1 %>% layout(title = paste("INDHOTEL.BO: 2015-01-01 -",Sys.Date()),
                            xaxis = list(rangeselector = rs),
                            legend = list(orientation = 'h', x = 0.5, y = 1,
                                          xanchor = 'center', yref = 'paper',
                                          font = list(size = 10),
                                          bgcolor = 'transparent'))



fig2bo1 <- fig2bo1 %>% layout(title = paste("Volume INDHOTEL.BO: 2015-01-01 -",Sys.Date()),
                              xaxis = list(rangeselector = rs),
                              legend = list(orientation = 'h', x = 0.5, y = 1,
                                            xanchor = 'center', yref = 'paper',
                                            font = list(size = 10),
                                            bgcolor = 'transparent'))

figbo1
fig2bo1







library(plotly)
library(quantmod)

# get data
getSymbols("ORIENTHOT.NS",src='yahoo')
ORIENTHOT.NS <- na.omit(ORIENTHOT.NS)
df <- data.frame(Date=index(ORIENTHOT.NS),coredata(ORIENTHOT.NS))

# create Bollinger Bands
bbands <- BBands(ORIENTHOT.NS[,c("ORIENTHOT.NS.High","ORIENTHOT.NS.Low","ORIENTHOT.NS.Close")])

# join and subset data
df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2015-01-01")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
    if (df$ORIENTHOT.NS.Close[i] >= df$ORIENTHOT.NS.Open[i]) {
        df$directionn[i] = 'Increasing'
    } else {
        df$direction[i] = 'Decreasing'
    }
}

i <- list(line = list(color = '#34B828'))
d <- list(line = list(color = '#C71616'))

# plot candlestick chart

figo <- df %>% plot_ly(x = ~Date, type="candlestick",
                       open = ~ORIENTHOT.NS.Open, close = ~ORIENTHOT.NS.Close,
                       high = ~ORIENTHOT.NS.High, low = ~ORIENTHOT.NS.Low, name = "ORIENTHOT.NS",
                       increasing = i, decreasing = d) 
figo <- figo %>% add_lines(x = ~Date, y = ~up , name = "B Bands",
                           line = list(color = '#ccc', width = 0.5),
                           legendgroup = "Bollinger Bands",
                           hoverinfo = "none", inherit = F) 
figo <- figo %>% add_lines(x = ~Date, y = ~dn, name = "B Bands",
                           line = list(color = '#ccc', width = 0.5),
                           legendgroup = "Bollinger Bands", inherit = F,
                           showlegend = FALSE, hoverinfo = "none") 
figo <- figo %>% add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
                           line = list(color = '#E377C2', width = 0.5),
                           hoverinfo = "none", inherit = F) 
figo <- figo %>% layout(yaxis = list(title = "Price"))

# plot volume bar chart
fig2o <- df 
fig2o <- fig2o %>% plot_ly(x=~Date, y=~ORIENTHOT.NS.Volume, type='bar', name = "ORIENTHOT.NS Volume",
                           color = ~direction, colors = c('#34B828','#C71616')) 
fig2o <- fig2o %>% layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
               list(count=1,
                    label='RESET',
                    step='all'),
               list(count=1,
                    label='1 YR',
                    step='year',
                    stepmode='backward'),
               list(count=3,
                    label='3 MO',
                    step='month',
                    stepmode='backward'),
               list(count=1,
                    label='1 MO',
                    step='month',
                    stepmode='backward'),
               list(count=5,
                    label='5 DY',
                    step='day',
                    stepmode='backward'),
               list(count=1,
                    label='1 DY',
                    step='day',
                    stepmode='backward')
           ))

# subplot with shared x axis
figo <- figo %>% layout(title = paste("ORIENTHOT.NS: 2015-01-01 -",Sys.Date()),
                        xaxis = list(rangeselector = rs),
                        legend = list(orientation = 'h', x = 0.5, y = 1,
                                      xanchor = 'center', yref = 'paper',
                                      font = list(size = 10),
                                      bgcolor = 'transparent'))



fig2o <- fig2o %>% layout(title = paste("Volume ORIENTHOT.NS: 2015-01-01 -",Sys.Date()),
                          xaxis = list(rangeselector = rs),
                          legend = list(orientation = 'h', x = 0.5, y = 1,
                                        xanchor = 'center', yref = 'paper',
                                        font = list(size = 10),
                                        bgcolor = 'transparent'))

figo
fig2o




#BS

# get data
getSymbols("ORIENTHOT.BO",src='yahoo')
ORIENTHOT.BO <- na.omit(ORIENTHOT.BO)
df1 <- data.frame(Date=index(ORIENTHOT.BO),coredata(ORIENTHOT.BO))

# create Bollinger Bands
bbands <- BBands(ORIENTHOT.BO[,c("ORIENTHOT.BO.High","ORIENTHOT.BO.Low","ORIENTHOT.BO.Close")])

# join and subset data
df1 <- subset(cbind(df1, data.frame(bbands[,1:3])), Date >= "2015-01-01")

# colors column for increasing and decreasing
for (i in 1:length(df1[,1])) {
    if (df1$ORIENTHOT.BO.Close[i] >= df1$ORIENTHOT.BO.Open[i]) {
        df1$directionn[i] = 'Increasing'
    } else {
        df1$direction[i] = 'Decreasing'
    }
}

i1 <- list(line = list(color = '#34B828'))
d1 <- list(line = list(color = '#C71616'))

# plot candlestick chart

figboo <- df1 %>% plot_ly(x = ~Date, type="candlestick",
                          open = ~ORIENTHOT.BO.Open, close = ~ORIENTHOT.BO.Close,
                          high = ~ORIENTHOT.BO.High, low = ~ORIENTHOT.BO.Low, name = "ORIENTHOT.BO",
                          increasing = i1, decreasing = d1) 
figboo <- figboo %>% add_lines(x = ~Date, y = ~up , name = "B Bands",
                               line = list(color = '#ccc', width = 0.5),
                               legendgroup = "Bollinger Bands",
                               hoverinfo = "none", inherit = F) 
figboo <- figboo %>% add_lines(x = ~Date, y = ~dn, name = "B Bands",
                               line = list(color = '#ccc', width = 0.5),
                               legendgroup = "Bollinger Bands", inherit = F,
                               showlegend = FALSE, hoverinfo = "none") 
figboo <- figboo %>% add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
                               line = list(color = '#E377C2', width = 0.5),
                               hoverinfo = "none", inherit = F) 
figboo <- figboo %>% layout(yaxis = list(title = "Price"))

# plot volume bar chart
fig2boo <- df1 
fig2boo <- fig2boo %>% plot_ly(x=~Date, y=~ORIENTHOT.BO.Volume, type='bar', name = "ORIENTHOT.BO Volume",
                               color = ~direction, colors = c('#34B828','#C71616')) 
fig2boo <- fig2boo %>% layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
               list(count=1,
                    label='RESET',
                    step='all'),
               list(count=1,
                    label='1 YR',
                    step='year',
                    stepmode='backward'),
               list(count=3,
                    label='3 MO',
                    step='month',
                    stepmode='backward'),
               list(count=1,
                    label='1 MO',
                    step='month',
                    stepmode='backward'),
               list(count=5,
                    label='5 DY',
                    step='day',
                    stepmode='backward'),
               list(count=1,
                    label='1 DY',
                    step='day',
                    stepmode='backward')
           ))

# subplot with shared x axis
figboo <- figboo %>% layout(title = paste("ORIENTHOT.BO: 2015-01-01 -",Sys.Date()),
                            xaxis = list(rangeselector = rs),
                            legend = list(orientation = 'h', x = 0.5, y = 1,
                                          xanchor = 'center', yref = 'paper',
                                          font = list(size = 10),
                                          bgcolor = 'transparent'))



fig2boo <- fig2boo %>% layout(title = paste("Volume ORIENTHOT.BO: 2015-01-01 -",Sys.Date()),
                              xaxis = list(rangeselector = rs),
                              legend = list(orientation = 'h', x = 0.5, y = 1,
                                            xanchor = 'center', yref = 'paper',
                                            font = list(size = 10),
                                            bgcolor = 'transparent'))

figboo
fig2boo




# get data
getSymbols("TAJGVK.NS",src='yahoo')
TAJGVK.NS <- na.omit(TAJGVK.NS)
df <- data.frame(Date=index(TAJGVK.NS),coredata(TAJGVK.NS))

# create Bollinger Bands
bbands <- BBands(TAJGVK.NS[,c("TAJGVK.NS.High","TAJGVK.NS.Low","TAJGVK.NS.Close")])

# join and subset data
df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2015-01-01")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
    if (df$TAJGVK.NS.Close[i] >= df$TAJGVK.NS.Open[i]) {
        df$directionn[i] = 'Increasing'
    } else {
        df$direction[i] = 'Decreasing'
    }
}

i <- list(line = list(color = '#34B828'))
d <- list(line = list(color = '#C71616'))

# plot candlestick chart

figt <- df %>% plot_ly(x = ~Date, type="candlestick",
                       open = ~TAJGVK.NS.Open, close = ~TAJGVK.NS.Close,
                       high = ~TAJGVK.NS.High, low = ~TAJGVK.NS.Low, name = "TAJGVK.NS",
                       increasing = i, decreasing = d) 
figt <- figt %>% add_lines(x = ~Date, y = ~up , name = "B Bands",
                           line = list(color = '#ccc', width = 0.5),
                           legendgroup = "Bollinger Bands",
                           hoverinfo = "none", inherit = F) 
figt <- figt %>% add_lines(x = ~Date, y = ~dn, name = "B Bands",
                           line = list(color = '#ccc', width = 0.5),
                           legendgroup = "Bollinger Bands", inherit = F,
                           showlegend = FALSE, hoverinfo = "none") 
figt <- figt %>% add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
                           line = list(color = '#E377C2', width = 0.5),
                           hoverinfo = "none", inherit = F) 
figt <- figt %>% layout(yaxis = list(title = "Price"))

# plot volume bar chart
fig2t <- df 
fig2t <- fig2t %>% plot_ly(x=~Date, y=~TAJGVK.NS.Volume, type='bar', name = "TAJGVK.NS Volume",
                           color = ~direction, colors = c('#34B828','#C71616')) 
fig2t <- fig2t %>% layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
               list(count=1,
                    label='RESET',
                    step='all'),
               list(count=1,
                    label='1 YR',
                    step='year',
                    stepmode='backward'),
               list(count=3,
                    label='3 MO',
                    step='month',
                    stepmode='backward'),
               list(count=1,
                    label='1 MO',
                    step='month',
                    stepmode='backward'),
               list(count=5,
                    label='5 DY',
                    step='day',
                    stepmode='backward'),
               list(count=1,
                    label='1 DY',
                    step='day',
                    stepmode='backward')
           ))

# subplot with shared x axis
figt <- figt %>% layout(title = paste("TAJGVK.NS: 2015-01-01 -",Sys.Date()),
                        xaxis = list(rangeselector = rs),
                        legend = list(orientation = 'h', x = 0.5, y = 1,
                                      xanchor = 'center', yref = 'paper',
                                      font = list(size = 10),
                                      bgcolor = 'transparent'))



fig2t <- fig2t %>% layout(title = paste("Volume TAJGVK.NS: 2015-01-01 -",Sys.Date()),
                          xaxis = list(rangeselector = rs),
                          legend = list(orientation = 'h', x = 0.5, y = 1,
                                        xanchor = 'center', yref = 'paper',
                                        font = list(size = 10),
                                        bgcolor = 'transparent'))

figt
fig2t




#BS

# get data
getSymbols("TAJGVK.BO",src='yahoo')
TAJGVK.BO <- na.omit(TAJGVK.BO)
df1 <- data.frame(Date=index(TAJGVK.BO),coredata(TAJGVK.BO))

# create Bollinger Bands
bbands <- BBands(TAJGVK.BO[,c("TAJGVK.BO.High","TAJGVK.BO.Low","TAJGVK.BO.Close")])

# join and subset data
df1 <- subset(cbind(df1, data.frame(bbands[,1:3])), Date >= "2015-01-01")

# colors column for increasing and decreasing
for (i in 1:length(df1[,1])) {
    if (df1$TAJGVK.BO.Close[i] >= df1$TAJGVK.BO.Open[i]) {
        df1$directionn[i] = 'Increasing'
    } else {
        df1$direction[i] = 'Decreasing'
    }
}

i1 <- list(line = list(color = '#34B828'))
d1 <- list(line = list(color = '#C71616'))

# plot candlestick chart

figbot <- df1 %>% plot_ly(x = ~Date, type="candlestick",
                          open = ~TAJGVK.BO.Open, close = ~TAJGVK.BO.Close,
                          high = ~TAJGVK.BO.High, low = ~TAJGVK.BO.Low, name = "TAJGVK.BO",
                          increasing = i1, decreasing = d1) 
figbot <- figbot %>% add_lines(x = ~Date, y = ~up , name = "B Bands",
                               line = list(color = '#ccc', width = 0.5),
                               legendgroup = "Bollinger Bands",
                               hoverinfo = "none", inherit = F) 
figbot <- figbot %>% add_lines(x = ~Date, y = ~dn, name = "B Bands",
                               line = list(color = '#ccc', width = 0.5),
                               legendgroup = "Bollinger Bands", inherit = F,
                               showlegend = FALSE, hoverinfo = "none") 
figbot <- figbot %>% add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
                               line = list(color = '#E377C2', width = 0.5),
                               hoverinfo = "none", inherit = F) 
figbot <- figbot %>% layout(yaxis = list(title = "Price"))

# plot volume bar chart
fig2bot <- df1 
fig2bot <- fig2bot %>% plot_ly(x=~Date, y=~TAJGVK.BO.Volume, type='bar', name = "TAJGVK.BO Volume",
                               color = ~direction, colors = c('#34B828','#C71616')) 
fig2bot <- fig2bot %>% layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
               list(count=1,
                    label='RESET',
                    step='all'),
               list(count=1,
                    label='1 YR',
                    step='year',
                    stepmode='backward'),
               list(count=3,
                    label='3 MO',
                    step='month',
                    stepmode='backward'),
               list(count=1,
                    label='1 MO',
                    step='month',
                    stepmode='backward'),
               list(count=5,
                    label='5 DY',
                    step='day',
                    stepmode='backward'),
               list(count=1,
                    label='1 DY',
                    step='day',
                    stepmode='backward')
           ))

# subplot with shared x axis
figbot <- figbot %>% layout(title = paste("TAJGVK.BO: 2015-01-01 -",Sys.Date()),
                            xaxis = list(rangeselector = rs),
                            legend = list(orientation = 'h', x = 0.5, y = 1,
                                          xanchor = 'center', yref = 'paper',
                                          font = list(size = 10),
                                          bgcolor = 'transparent'))



fig2bot <- fig2bot %>% layout(title = paste("Volume TAJGVK.BO: 2015-01-01 -",Sys.Date()),
                              xaxis = list(rangeselector = rs),
                              legend = list(orientation = 'h', x = 0.5, y = 1,
                                            xanchor = 'center', yref = 'paper',
                                            font = list(size = 10),
                                            bgcolor = 'transparent'))

figbot
fig2bot






library(shiny)
library(shinythemes)

# Define UI ----
ui <- fluidPage(
    theme = shinytheme("flatly"),
    titlePanel(h1("Group 17 - Hospitality")),
    
    navbarPage(
        title = "",
        tabPanel(h3("Sector Scan"),
                 tabsetPanel(
                     tabPanel( h4("Sector News"),
                               h3("Sector Headlines", align = "center"),
                               DT::dataTableOutput('table5'),
                               p("Data Source: Google News.",align = "right")),
                     tabPanel(h4("Expert Advice"),
                              h3("Expert Pieces", align = "center"),
                              DT::dataTableOutput('table6'),
                              p("Data Source: Google News.",align = "right"))
                 )
                ),
        tabPanel(h3("Mahindra Holidays & Resorts India Ltd."),
                 tabsetPanel(
                     tabPanel(h4("Company Info")),
                     tabPanel(h4("MHRIL.NS - NSE"),
                              h3("Stock Prices", align = "center"),
                              plotlyOutput("plot"),
                              h3("Trade Volumes", align = "center"),
                              plotlyOutput("plot1"),
                              p("Data Source: Yahoo Finance.",align = "right")),
                     tabPanel(h4("MHRIL.BO - BSE"),
                              h3("Stock Prices", align = "center"),
                              plotlyOutput("plot2"),
                              h3("Trade Volumes", align="center"),
                              plotlyOutput("plot3"),
                              p("Data Source: Yahoo Finance."),align="right"),
                     tabPanel(h4("Latest News"),
                              h3("Recent Headlines",align="center"),
                              DT::dataTableOutput('table1'),
                              p("News Source: Google News.",align="right")),
                     tabPanel(h4(" Price Forecast"))
                 )
                 
                 
                 
                 
        ),
        tabPanel(h3("The Indian Hotels Co. Ltd."),
                 tabsetPanel(
                     tabPanel(h4("Company Info")),
                     tabPanel(h4("INDHOTEL.NS - NSE"),
                              h3("Stock Prices",align="center"),
                              plotlyOutput("plot4"),
                              h3("Trade Volumes", align="center"),
                              plotlyOutput("plot5"),
                              p("Data Source: Yahoo Finance.",align = "right")),
                     tabPanel(h4("INDHOTEL.BO - BSE"),
                              h3("Stock Prices",align="center"),
                              plotlyOutput("plot6"),
                              h4("Trade Volumes", align="center"),
                              plotlyOutput("plot7"),
                              p("Data Source: Yahoo Finance."),align="right"),
                     tabPanel(h4("Latest News"),
                              h3("Recent Headlines",align="center"),
                              DT::dataTableOutput('table2'),
                              p("News Source: Google News.",align="right")),
                     tabPanel(h4(" Price Forecast"))
                 )
        ),
        tabPanel(h3("Oriental Hotels Ltd."),
                 tabsetPanel(
                     tabPanel(h4("Company Info")),
                     tabPanel(h4("ORIENTHOT.NS - NSE"),
                              h3("Stock Prices",align="center"),
                              plotlyOutput("plot8"),
                              h3("Trade Volumes", align="center"),
                              plotlyOutput("plot9"),
                              p("Data Source: Yahoo Finance.",align = "right")),
                     tabPanel(h4("ORIENTHOT.BO - BSE"),
                              h3("Stock Prices",align="center"),
                              plotlyOutput("plot10"),
                              h3("Trade Volumes", align="center"),
                              plotlyOutput("plot11"),
                              p("Data Source: Yahoo Finance.",align = "right")),
                     tabPanel(h4("Latest News"),
                              h3("Recent Headlines",align="center"),
                              DT::dataTableOutput('table3'),
                              p("News Source: Google News.",align="right")),
                     tabPanel(h4(" Price Forecast"))
                 )),
        tabPanel(h3("TAJGVK Hotels & Resorts Ltd."),
                 tabsetPanel(
                     tabPanel(h4("Company Info")),
                     tabPanel(h4("TAJGVK.NS - NSE"),
                              h3("Stock Prices",align="center"),
                              plotlyOutput("plot12"),
                              h3("Trade Volumes", align="center"),
                              plotlyOutput("plot13"),
                              p("Data Source: Yahoo Finance.",align = "right")),
                     tabPanel(h4("TAJGVK.BO - BSE"),
                              h3("Stock Prices",align="center"),
                              plotlyOutput("plot14"),
                              h3("Trade Volumes", align="center"),
                              plotlyOutput("plot15"),
                              p("Data Source: Yahoo Finance.",align = "right")),
                     tabPanel(h4("Latest News"),
                              h3("Recent Headlines",align="center"),
                              DT::dataTableOutput('table4'),
                              p("News Source: Google News.",align="right")),
                     tabPanel(h4(" Price Forecast"))
                 )),
        tabPanel(h3("Team Details"),
                 h2("Members List:"),
                 h3("20030242075     Aruvansh Kumar"),
                 h3("20030242050     Priyal Khanna"),
                 h3("20030242046     Prachi Lad"),
                 h3("20030242021     Boris Raj Borgohain"))
        
    )
    
)

# Define server logic ----
server <- function(input, output, session) {
    
    output$plot <- renderPlotly(fig)
    output$plot1 <- renderPlotly(fig2)
    output$plot2 <- renderPlotly(figbo)
    output$plot3 <- renderPlotly(fig2bo)
    output$plot4 <- renderPlotly(fig3)
    output$plot5 <- renderPlotly(fig4)
    output$plot6 <- renderPlotly(figbo1)
    output$plot7 <- renderPlotly(fig2bo1)
    output$plot8 <- renderPlotly(figo)
    output$plot9 <- renderPlotly(fig2o)
    output$plot10 <- renderPlotly(figboo)
    output$plot11 <- renderPlotly(fig2boo)
    output$plot12 <- renderPlotly(figt)
    output$plot13 <- renderPlotly(fig2t)
    output$plot14 <- renderPlotly(figbot)
    output$plot15 <- renderPlotly(fig2bot)
    output$table5 <- DT::renderDataTable({hsn})
    output$table6 <- DT::renderDataTable({exn})
    output$table1 <- DT::renderDataTable({mhrn})
    output$table2 <- DT::renderDataTable({ihn})
    output$table3 <- DT::renderDataTable({ohn})
    output$table4 <- DT::renderDataTable({tajn})
    
    
    
}

# Run the app ----
shinyApp(ui = ui, server = server)

