library(plotly)
library(quantmod)

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

