library(plotly)
library(quantmod)

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

i <- list(line = list(color = '#34B828'))
d <- list(line = list(color = '#C71616'))

# plot candlestick chart

figbo <- df1 %>% plot_ly(x = ~Date, type="candlestick",
                      open = ~MHRIL.BO.Open, close = ~MHRIL.BO.Close,
                      high = ~MHRIL.BO.High, low = ~MHRIL.BO.Low, name = "MHRIL.BO",
                      increasing = i, decreasing = d) 
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

