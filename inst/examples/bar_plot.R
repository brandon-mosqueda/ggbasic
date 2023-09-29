# For factor variables, it generates a bar plot with the frequency of each class
bar_plot(iris, x = Species)

# For numeric variables, it generates a histogram
bar_plot(iris, x = Sepal.Length)

# It can be used with categorical variables in the x-axis and numeric variables
# in the y-axis to generate a bar plot
data <- data.frame(Letter = LETTERS[seq(5)], Value = rnorm(5))
bar_plot(data, x = Letter, y = Value, fill_by = Letter)

# Easily divide the plot into multiple plots using facet_row and facet_col
titanic <- as.data.frame(Titanic)
bar_plot(
  titanic,
  x = Class,
  y = Freq,
  fill_by = Survived,
  facet_row = Sex,
  title = "Titanic survivors by sex and class"
)
