
#############
library(ggplot2)
ggplot(data.frame(date = test_dates), aes(x = date)) +
    geom_histogram(binwidth = 30)  # binwidth depends on the data density

ggplot(data.frame(date = test_dates), aes(x = factor(1), y = date)) +
    geom_boxplot()


ggplot(data.frame(date = test_dates, value = seq_along(test_dates)), aes(x = date, y = value)) +
    geom_line() + geom_point()
#############

