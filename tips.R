library(reshape2)
# Look at first few rows. Use Tip Data
head(tips)

library(ggplot2)
sp <- ggplot(tips, aes(x=total_bill, y=tip/total_bill)) + geom_point(shape=1)
sp

# Divide by levels of "sex", in the vertical direction
sp + facet_grid(sex ~ .)

# Divide by levels of "sex", in the horizontal direction
sp + facet_grid(. ~ sex)

# Divide with "sex" vertical, "day" horizontal
sp + facet_grid(sex ~ day)

# Divide by day, going horizontally and wrapping with 2 columns
sp + facet_wrap( ~ day, ncol=2)

sp + facet_grid(sex ~ day) +
  theme(strip.text.x = element_text(size=8, angle=75),
        strip.text.y = element_text(size=12, face="bold"),
        strip.background = element_rect(colour="red", fill="#CCCCFF"))

labels <- c(Female = "Women", Male = "Men")
sp + facet_grid(. ~ sex, labeller=labeller(sex = labels))

tips2 <- tips
levels(tips2$sex)[levels(tips2$sex)=="Female"] <- "Women"
levels(tips2$sex)[levels(tips2$sex)=="Male"]   <- "Men"
head(tips2, 3)
#>   total_bill  tip   sex smoker day   time size
#> 1      16.99 1.01 Women     No Sun Dinner    2
#> 2      10.34 1.66   Men     No Sun Dinner    3
#> 3      21.01 3.50   Men     No Sun Dinner    3

# Both of these will give the same output:
sp2 <- ggplot(tips2, aes(x=total_bill, y=tip/total_bill)) + geom_point(shape=1)
sp2 + facet_grid(. ~ sex)


# Reverse each strings in a character vector
reverse <- function(strings) {
  strings <- strsplit(strings, "")
  vapply(strings, function(x) {
    paste(rev(x), collapse = "")
  }, FUN.VALUE = character(1))
}

sp + facet_grid(. ~ sex, labeller=labeller(sex = reverse))

# A histogram of bill sizes
hp <- ggplot(tips, aes(x=total_bill)) + geom_histogram(binwidth=2,colour="white")
hp

# Histogram of total_bill, divided by sex and smoker
hp + facet_grid(sex ~ smoker)


# Same as above, with scales="free_y"
hp + facet_grid(sex ~ smoker, scales="free_y")

# With panels that have the same scaling, but different range (and therefore different physical sizes)
hp + facet_grid(sex ~ smoker, scales="free", space="free")