data <- read.csv("opt/tabu.csv")

print(summary(data))

model <- lm(
  cost ~ tabuLength + iter + neighbourhood + 
            I(tabuLength^2) + I(iter^2) + I(neighbourhood^2) +
            tabuLength:iter + tabuLength:neighbourhood + 
            iter:neighbourhood,
  data = data
)

objective <- function(x) {

  newdata <- data.frame(
    tabuLength = x[1],
    iter = x[2],
    neighbourhood = x[3]
  )

  predict(model, newdata = newdata)
}

result <- optim(
  par = c(1000, 50, 0),   # starting point

  fn = objective,

  method = "L-BFGS-B",

  lower = c(
    min(data$tabuLength),
    min(data$iter),
    min(data$neighbourhood)
  ),

  upper = c(
    max(data$tabuLength),
    max(data$iter),
    max(data$neighbourhood)
  )
)

print(result$par)