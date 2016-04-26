install.packages("Quandl", dependencies = TRUE)
library(Quandl)
install.packages("quantmod")
library(quantmod)

?Quandl

# Assign your first dataset to the variable:
mydata <- Quandl("NSE/OIL")

# Assign the Prague Stock Exchange to:
PragueStockExchange <- Quandl("PRAGUESE/PX")

# Load the Facebook data with the help of Quandl
Facebook <- Quandl("GOOG/NASDAQ_FB", type = "xts")

# Plot the chart with the help of candleChart()
candleChart(Facebook)

# It is possible to search Quandl datasets using R as follows:
# Quandl.search(query = "Search Term", page = int, source = "Specific source to search", silent = TRUE|FALSE)

# Look up the first 3 results for 'Bitcoin' within the Quandl database:
results <- Quandl.search(query = "Bitcoin", silent = FALSE)

# Print out the results
str(results)

# Assign the data set with code BCHAIN/TOTBC
BitCoin <- Quandl("BCHAIN/TOTBC", type = 'xts')


# The Quandl() function has two arguments, start_date and end_date, that can be used to specify the time range of the data to load. 
# The format to specify the date is: yyyy-mm-dd.

# Assign to the variable Exchange
Exchange <- Quandl("BNP/USDEUR", type = 'xts', start_date = "2013-01-01" ,  end_date = "2013-12-01")
candleChart(Exchange)

# Quandl can transform your data before serving it. You can set the transformation argument to:
#"diff"
#"rdiff"
#"cumul", and
#"normalize".
# You want to know the Canadian GDP annual percent change. Use the rdiff transformation and assign the result to GDP_Change. 
#Use the code "FRED/CANRGDPR"; more information can be found on Quandl's website.
# The result:
GDP_Change <- Quandl("FRED/CANRGDPR", transform = "rdiff")


# By altering the collapse parameter you can easily indicate the desired frequency. The available options are:
# none|daily|weekly|monthly| quarterly|annual
# Quandl returns the last observation for the given period. So, if you collapse a daily dataset to "monthly", you will get a sample of the original dataset 
# where the observation for each month is the last data point available for that month.
# Get crude oil prices on a quarterly basis from the EIA's daily spot price (use the code DOE/RWTC) 
# and assign these to the variable eiaQuarterly.
# The result:
eiaQuarterly <- Quandl("DOE/RWTC", collapse = "monthly", type = 'xts')
candleChart(eiaQuarterly)

# Two other arguments of the Quandl() function are rows and sort.
# The meaning of sort is straightforward. By default it is set on descending (desc), but you can change it to ascending 
# by setting the sort argument to asc.
# The rows argument on the other hand allows you to get only the first n rows of your query. 
# For example, you can use rows=1 to get the latest observation of any dataset. 
# You only want the first 5 observations of the crude oil prices from the EIA (use the code DOE/RWTC) in an ascending order. 
# The result should be assigned to TruSo.
# Assign to TruSo the first 5 observations of the crude oil prices
TruSo <- Quandl("DOE/RWTC", order = 'asc', rows = 5) 
  
# Print the result
TruSo  

# Putting all together
# You want to have the daily percent change in oil prices from January 2005 to March 2010, in ascending order.
Final <- Quandl("DOE/RWTC", start_date = "2005-01-01" ,  end_date = "2010-03-01",  collapse = 'daily', transform = 'rdiff', order = 'asc')


  
  
