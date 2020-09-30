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

