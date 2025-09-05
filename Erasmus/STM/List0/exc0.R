data <- read.csv("HearingThresholds.csv", header = TRUE)
data$avg1k = (data$l1k+data$r1k)/2
data$avg = (
    data$l500 +
    data$l1k +
    data$l2k +
    data$l3k +
    data$l4k +
    data$l6k +
    data$l8k +
    data$r500 +
    data$r1k +
    data$r2k +
    data$r3k +
    data$r4k +
    data$r6k +
    data$r8k
) / 14
data$left = (
    data$l500 +
    data$l1k +
    data$l2k +
    data$l3k +
    data$l4k +
    data$l6k +
    data$l8k
) / 7
data$right = (
    data$r500 +
    data$r1k +
    data$r2k +
    data$r3k +
    data$r4k +
    data$r6k +
    data$r8k
)
data_plot = subset(data, select=c(avg, left, right, age))

# plot(data$age, data$avg, type ='p', pch = 19,col ='dark red', frame.plot = FALSE)
# range(data$avg)
# hist(data$avg, breaks = seq(from = 0, to = 90, by = 5))

x <- data$age # explanatory
y <- data$avg # dependent
n <- length(x)

xbar <- mean(x)
ybar <- mean(y)

SSx <- sum((x - xbar)^2)
SSy <- sum((y - ybar)^2)
SSxy <- sum((x - xbar)*(y - ybar))

b1hat <- SSxy / SSx # slope parameter
b0hat <- ybar - b1hat * xbar # intercept parameter

mu <- function(x){
    b0hat + b1hat * x
}

# plot(x, y)
# lines(range(x), mu(range(x)), col=2, lwd=3)

yhat <- mu(x)
eps <- y - yhat

SSe <- sum(eps^2)

Rsq <- 1 - SSe / SSy