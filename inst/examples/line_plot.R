# Normal line plot
line_plot(iris, x = "Petal.Width", y = "Sepal.Length")

# With different colors for each species
line_plot(iris, x = "Petal.Width", y = "Sepal.Length", fill_by = "Species")

# With different row facets for each species
line_plot(iris, x = "Petal.Width", y = "Sepal.Length", facet_row = "Species")
