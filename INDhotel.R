library(plotly)
library(quantmod)

# get data
getSymbols("INDHOTEL.NS",src='yahoo')
INDHOTEL.NS <- na.omit(INDHOTEL.NS)
df3 <- data.frame(Date=index(INDHOTEL.NS),coredata(INDHOTEL.NS))

# create Bollinger Bands
bbands <- BBands(INDHOTEL.NS[,c("INDHOTEL.NS.High","INDHOTEL.NS.Low","INDHOTEL.NS.Close")])

# join and subset data
df3 <- subset(cbind(df3, data.frame(bbands[,1:3])), Date >= "2015-01-01")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
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

