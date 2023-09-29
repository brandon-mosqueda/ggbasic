# Like a scatter plot, but with hexagons instead of points to represent the
# density of points in a given area.
hex_plot(iris, x = Sepal.Length, y = Petal.Length)

# With a facet for each species
hex_plot(iris, x = Sepal.Length, y = Petal.Length, facet_row = Species)

# With custom gradient colors
hex_plot(
  iris,
  x = Sepal.Length,
  y = Petal.Length,
  low_color = "white",
  high_color = "red"
)
