library(plotly)
library(quantmod)

# get data
getSymbols("AAPL",src='yahoo')

df <- data.frame(Date=index(AAPL),coredata(AAPL))

# create Bollinger Bands
bbands <- BBands(AAPL[,c("AAPL.High","AAPL.Low","AAPL.Close")])

# join and subset data
df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2015-02-14")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
  if (df$AAPL.Close[i] >= df$AAPL.Open[i]) {
    df$direction[i] = 'Increasing'
  } else {
    df$direction[i] = 'Decreasing'
  }
}

i <- list(line = list(color = '#34B828'))
d <- list(line = list(color = '#C71616'))

# plot candlestick chart

fig <- df %>% plot_ly(x = ~Date, type="candlestick",
                      open = ~AAPL.Open, close = ~AAPL.Close,
                      high = ~AAPL.High, low = ~AAPL.Low, name = "AAPL",
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
fig2 <- fig2 %>% plot_ly(x=~Date, y=~AAPL.Volume, type='bar', name = "AAPL Volume",
                         color = ~direction, colors = c('#17BECF','#7F7F7F')) 
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
fig <- subplot(fig, fig2, heights = c(0.7,0.2), nrows=2,
               shareX = TRUE, titleY = TRUE)
fig <- fig %>% layout(title = paste("Apple: 2015-02-14 -",Sys.Date()),
                      xaxis = list(rangeselector = rs),
                      legend = list(orientation = 'h', x = 0.5, y = 1,
                                    xanchor = 'center', yref = 'paper',
                                    font = list(size = 10),
                                    bgcolor = 'transparent'))

fig
