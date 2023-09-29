# For a single variable
box_plot(iris, x = Sepal.Length)

# Desagregated by a categorical variable
box_plot(iris, x = Sepal.Length, y = Species)

# Easily divide the plot into multiple plots using facet_row and facet_col
titanic <- as.data.frame(Titanic)
box_plot(
  titanic,
  x = Class,
  y = Freq,
  fill_by = Survived,
  facet_row = Sex,
  title = "Titanic survivors by sex and class"
)
