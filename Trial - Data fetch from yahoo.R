# Get quantmod
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

start <- as.Date("2015-01-01")
end <- as.Date("2020-09-21")

# Let's get Apple stock data; Apple's ticker symbol is AAPL. We use the
# quantmod function getSymbols, and pass a string as a first argument to
# identify the desired ticker symbol, pass 'yahoo' to src for Yahoo!
# Finance, and from and to specify date ranges

# The default behavior for getSymbols is to load data directly into the
# global environment, with the object being named after the loaded ticker
# symbol. This feature may become deprecated in the future, but we exploit
# it now.

getSymbols("MHRIL.BO", src = "yahoo", from = start, to = end)
na.omit(MHRIL.BO)
head(MHRIL.BO)
tail(MHRIL.BO)

candleChart(MHRIL.BO, up.col = "black", dn.col = "red", theme = "white")

ch <- candleChart(MHRIL.BO, up.col = "black", dn.col = "red", theme = "white", subset = "2020-01-01/")
addSMA(n = 20)

