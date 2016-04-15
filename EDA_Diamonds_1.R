library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
data("diamonds")
?diamonds
str(diamonds)

diamonds <- tbl_df(diamonds)

unique(diamonds$color)

# Create a histogram of the price of
# all the diamonds in the diamond data set.

# TYPE YOUR CODE BELOW THE LINE
# =======================================

ggplot(aes(x = price), data = diamonds) + 
  geom_histogram(binwidth = 1000, color = 'black', fill = '#099DD9')

summary(diamonds$price)

#How many diamonds cost less than $500
nrow(subset(diamonds, price < 500))

#How many diamonds cost less than $250
nrow(subset(diamonds, price < 250))

#How many diamonds cost $15000 or more
nrow(subset(diamonds, price >= 15000))


# Explore the largest peak in the
# price histogram you created earlier.

# Try limiting the x-axis, altering the bin width,
# and setting different breaks on the x-axis.

# There won't be a solution video for this
# question so go to the discussions to
# share your thoughts and discover
# what other people find.

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Submit your final code when you are ready.

# TYPE YOUR CODE BELOW THE LINE
# ======================================================================

ggplot(aes(x = price), data = diamonds) + 
  geom_histogram(binwidth = 500, color = 'black', fill = '#099DD9')


ggplot(aes(x = price), data = subset(diamonds, price > 500 & price < 2000)) + 
  geom_histogram(binwidth = 100, color = 'black', fill = '#099DD9') + 
  scale_y_continuous(breaks = seq(0, 20000, 1000)) + 
  xlab('Price of Diamond') + 
  ylab('Number of Diamonds in sample') +
  ggsave('priceHistogram.png')


##There are no diamonds that cost $1500.. 

# Break out the histogram of diamond prices by cut.

# You should have five histograms in separate
# panels on your resulting plot.

# TYPE YOUR CODE BELOW THE LINE
# ======================================================

ggplot(aes(x = price), data = diamonds) + 
  geom_histogram(binwidth = 1000, color = 'black', fill = '#099DD9') + 
  scale_y_continuous(breaks = seq(0, 10000, 1000)) + 
  facet_wrap(~cut) +
  xlab('Price of Diamond') + 
  ylab('Number of Diamonds in sample')

#Which cut has the highest priced diamond?
diamonds %>%
  group_by(cut) %>%
  summarise(maxprice = max(price)) %>%
  arrange(maxprice)

which.max(diamonds$price)
diamonds$cut[27750]
diamonds$price[27750]

#Which cut has the lowest priced diamond?
diamonds %>%
  group_by(cut) %>%
  summarise(minprice = min(price)) %>% 
  arrange(minprice)

which.min(diamonds$price)
diamonds$cut[1]
diamonds$price[1]

#Which cut has the lowest median price?
diamonds %>%
  group_by(cut) %>%
  summarise(medianpricebycut = median(price)) %>%
  arrange(medianpricebycut)

# In the two last exercises, we looked at
# the distribution for diamonds by cut.

# Run the code below in R Studio to generate
# the histogram as a reminder.

# ===============================================================

qplot(x = price, data = diamonds) + facet_wrap(~cut)

# ===============================================================

# In the last exercise, we looked at the summary statistics
# for diamond price by cut. If we look at the output table, the
# the median and quartiles are reasonably close to each other.

# diamonds$cut: Fair
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     337    2050    3282    4359    5206   18570 
# ------------------------------------------------------------------------ 
# diamonds$cut: Good
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     327    1145    3050    3929    5028   18790 
# ------------------------------------------------------------------------ 
# diamonds$cut: Very Good
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     336     912    2648    3982    5373   18820 
# ------------------------------------------------------------------------ 
# diamonds$cut: Premium
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     326    1046    3185    4584    6296   18820 
# ------------------------------------------------------------------------ 
# diamonds$cut: Ideal
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     326     878    1810    3458    4678   18810 

# This means the distributions should be somewhat similar,
# but the histograms we created don't show that.

# The 'Fair' and 'Good' diamonds appear to have 
# different distributions compared to the better
# cut diamonds. They seem somewhat uniform
# on the left with long tails on the right.

# Let's look in to this more.

# Look up the documentation for facet_wrap in R Studio.
# Then, scroll back up and add a parameter to facet_wrap so that
# the y-axis in the histograms is not fixed. You want the y-axis to
# be different for each histogram.

# If you want a hint, check out the Instructor Notes.

ggplot(aes(x = price), data = diamonds) + 
  geom_histogram(binwidth = 1000, color = 'black', fill = '#099DD9') + 
  facet_wrap(~cut, scales = 'free')
  xlab('Price of Diamond') + 
  ylab('Number of Diamonds in sample')

# Create a histogram of price per carat
# and facet it by cut. You can make adjustments
# to the code from the previous exercise to get
# started.

# Adjust the bin width and transform the scale
# of the x-axis using log10.

# Submit your final code when you are ready.

# ENTER YOUR CODE BELOW THIS LINE

ggplot(aes(x = price), data = diamonds) + 
  geom_histogram(color = 'black', fill = '#099DD9') +
  scale_x_log10(waiver()) +
  facet_wrap(~cut, scales = 'free') +
  xlab('Price of Diamond (Log)') + 
  ylab('Number of Diamonds in sample')

# Investigate the price of diamonds using box plots,
# numerical summaries, and one of the following categorical
# variables: cut, clarity, or color.

# There won't be a solution video for this
# exercise so go to the discussion thread for either
# BOXPLOTS BY CLARITY, BOXPLOT BY COLOR, or BOXPLOTS BY CUT
# to share you thoughts and to
# see what other people found.

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Copy and paste all of the code that you used for
# your investigation, and submit it when you are ready.
# =================================================================

p1 <- qplot(data = diamonds, x = cut, y = price, geom = 'boxplot', color=cut) +
  coord_cartesian(ylim=c(0,7000))
p2 <- qplot(data = diamonds, x = clarity, y=price, geom = 'boxplot', color = clarity) +
  coord_cartesian(ylim=c(0,7000))
p3 <- qplot(data=diamonds, x=color, y=price, geom = 'boxplot', color=color) +
  coord_cartesian(ylim=c(0,8000))

grid.arrange(p1, p2, p3) +
ggsave('Diamonds_BoxPlots.png')

#What is the price range for Color D
ColorD <- subset(diamonds, color == 'D')
quantile(x = ColorD$price, probs = seq(0,1,.25))

#What is the price range for Color J
ColorJ <- subset(diamonds, color == 'J')
quantile(x = ColorJ$price, probs = seq(0,1,.25))


IQR(subset(diamonds, color == 'D')$price)
IQR(subset(diamonds, color == 'J')$price)

# Investigate the price per carat of diamonds across
# the different colors of diamonds using boxplots.

# Go to the discussions to
# share your thoughts and to discover
# what other people found.

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Copy and paste all of the code that you used for
# your investigation, and submit it when you are ready.

# SUBMIT YOUR CODE BELOW THIS LINE
# ===================================================================
summary(diamonds)

qplot(data = diamonds, x = color, y = price/carat, geom = 'boxplot', color=color) +
  coord_cartesian(ylim=c(0,7000)) + 
  ggsave('PriceCaratperColor.png')

#Investigate the weight of the diamonds (carat) using a frequency polygon. Use different bin widths to see how
#the frequency polygon changes. What carat size has a count greater than 2000?

ggplot(diamonds, aes(carat)) +
  geom_freqpoly(binwidth = .6) + 
  scale_x_continuous(breaks = seq(-1, 8.0, .5)) + 
  scale_y_continuous(breaks = seq(0, 20000, 1000)) +
  xlab('Carat Size') + 
  ylab('Count')

table(diamonds$carat)

# The Gapminder website contains over 500 data sets with information about
# the world's population. Your task is to download a data set of your choice
# and create 2-5 plots that make use of the techniques from Lesson 3.

# You might use a simple histogram, a boxplot split over a categorical variable,
# or a frequency polygon. The choice is yours!

# You can find a link to the Gapminder website in the Instructor Notes.

# Once you've completed your investigation, create a post in the discussions that includes:
#       1. any questions you answered, your observations, and summary statistics
#       2. snippets of code that created the plots
#       3. links to the images of your plots

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Copy and paste all of the code that you used for
# your investigation, and submit it when you are ready.
# ====================================================================================

oil <- read.csv("Oil Consumption.csv")
hightech <- read.csv("High tech export.csv")
summary(oil)
summary(hightech)
names(oil)[names(oil)== "Oil.Consumption.total..tonnes.per.year."] <- "Country"
names(hightech)[names(hightech)== "High.technology.exports....of.manufactured.exports."] <- "Country"

oil <- tbl_df(oil)
hightech <- tbl_df(hightech)
glimpse(oil)
glimpse(hightech)

g.oil <- gather(oil, "Year", "Oil", 2:length(colnames(oil)))
g.hightech <- gather(hightech, "Year", "HighTech", 2:length(colnames(hightech)))
View(g.oil)
View(g.hightech)

#Combine dataframes
oil.hightech <- inner_join(g.hightech, g.oil, by=c('Country', 'Year')) 
View(oil.hightech)
#Remove X
oil.hightech$Year <- gsub("X", '', oil.hightech$Year)

oil.hightech %>%
  group_by(Country) %>%
  summarise(meanoil = mean(Oil), meantech = mean(`HighTech%`))

View(oil.hightech)

#Picked four countries that are interesting in both regards. 
#US, Germany, Japan and Switzerland. 

US <- oil.hightech %>%
  filter(Country == "United States")
p1 <- qplot(x = US$Year, y = US$Oil, data=US)
p2 <- qplot(x = US$Year, y = US$HighTech, data=US)
grid.arrange(p1,p2)

Japan <- oil.hightech %>%
  filter(Country == "Japan")
p1 <- qplot(x = Japan$Year, y = Japan$Oil, data=Japan)
p2 <- qplot(x = Japan$Year, y = Japan$HighTech, data=Japan)
grid.arrange(p1,p2)

Germany <- oil.hightech %>%
  filter(Country == "Germany")
p1 <- qplot(x = Germany$Year, y = Germany$Oil, data=Germany)
p2 <- qplot(x = Germany$Year, y = Germany$HighTech, data=Germany)
grid.arrange(p1,p2)

Switzerland. <- oil.hightech %>%
  filter(Country == "Switzerland")
p1 <- qplot(x = Switzerland.$Year, y = Switzerland.$Oil, data=Switzerland)
p2 <- qplot(x = Switzerland.$Year, y = Switzerland.$HighTech, data=Switzerland)
grid.arrange(p1,p2)

  
##Any correlation?
