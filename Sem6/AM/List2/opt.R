data <- read.csv("opt/annealing.csv")

model <- lm(
  cost ~ temp + cooling + epochs + steps +
            I(temp^2) + I(cooling^2) + I(steps^2) + I(epochs^2) +
            temp:cooling + temp:epochs + temp:steps +
            cooling:epochs + cooling:steps +
            epochs:steps,
  data = data
)

objective <- function(x) {

  newdata <- data.frame(
    temp = x[1],
    cooling = x[2],
    epochs = x[3],
    steps = x[4]
  )

  predict(model, newdata = newdata)
}

result <- optim(
  par = c(100, 0.95, 100, 20),   # starting point

  fn = objective,

  method = "L-BFGS-B",

  lower = c(
    min(data$temp),
    min(data$cooling),
    min(data$epochs),
    min(data$steps)
  ),

  upper = c(
    max(data$temp),
    max(data$cooling),
    max(data$epochs),
    max(data$steps)
  )
)

print(result$par)