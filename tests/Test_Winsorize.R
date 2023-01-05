# Set the seed for reproducibility
set.seed(123)
# Sys.setenv("PKG_CXXFLAGS"="-std=c++11")

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)
library(RFunctions)

# Simulate 1000 observations from a normal distribution
x = rnorm(1000)

# Compile the functions
# Rcpp::sourceCpp("src/winsorize.cpp")

# Winsorize the vector x using the winsorize function
x_winsorized = winsorize(x, lower_cutoff = -1, upper_cutoff = 1)

# Winsorize the vector x using the winsorize_quantiles function
x_winsorized_quantiles = winsorize_quantiles(x, lower_quantile = 10, upper_quantile = 90, print_quantiles = T)

# Winsorize the vector x using the winsorize_one_sided function (lower-sided)
x_winsorized_one_sided1 = winsorize_one_sided(x, cutoff = -1, lower = TRUE)

# Winsorize the vector x using the winsorize_one_sided function (upper-sided)
x_winsorized_one_sided2 = winsorize_one_sided(x, cutoff = 1, lower = FALSE)

# Winsorize the vector x using the winsorize_one_sided_quantile function (lower-sided)
x_winsorized_one_sided_quantile1 = winsorize_one_sided_quantile(x, quantile = 5, lower = TRUE)

# Winsorize the vector x using the winsorize_one_sided_quantile function (upper-sided)
x_winsorized_one_sided_quantile2 = winsorize_one_sided_quantile(x, quantile = 95, lower = FALSE)

# Create a data frame with the original and winsorized vectors
df <- data.frame(x = x, x_winsorized = x_winsorized,
                 x_winsorized_quantiles = x_winsorized_quantiles ,
                 x_winsorized_one_sided_quantile1 = x_winsorized_one_sided_quantile1,
                 x_winsorized_one_sided_quantile2 = x_winsorized_one_sided_quantile2
                 )

# Plot x versus x_winsorized using the ggplot2 package
ggplot(df, aes(x = x, y = x_winsorized)) + geom_point()

ggplot(df, aes(x = x, y = x_winsorized_quantiles)) + geom_point()

ggplot(df, aes(x = x, y = x_winsorized_one_sided_quantile1)) + geom_point()

ggplot(df, aes(x = x, y = x_winsorized_one_sided_quantile2)) + geom_point()

