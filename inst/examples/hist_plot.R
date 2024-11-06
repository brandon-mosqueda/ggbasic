# For a single variable
hist_plot(iris, x = "Sepal.Length")

# Desagregated by a categorical variable
hist_plot(iris, x = "Sepal.Width", fill_by = "Species")

# With a facet for each species
hist_plot(iris, x = "Petal.Length", fill_by = "Species", facet_row = "Species")
