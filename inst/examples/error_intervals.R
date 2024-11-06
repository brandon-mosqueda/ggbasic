set.seed(1)

data <- data.frame(
  Letter = LETTERS[seq(5)],
  Value = rnorm(5, 20, 2),
  Value_SE = rnorm(5)
)

bar_plot(data, "Letter", "Value", fill_by = "Letter") +
  error_intervals("Value", "Value_SE")
