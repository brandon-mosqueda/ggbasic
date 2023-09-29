# Normal scatter plot
scatter_plot(iris, x = Sepal.Length, y = Petal.Length)

# With a facet for each species
scatter_plot(iris, x = Sepal.Length, y = Petal.Length, facet_row = Species)

# With bigger red points
scatter_plot(
  iris,
  x = Sepal.Length,
  y = Petal.Length,
  point_size = 10,
  color = "red"
)
