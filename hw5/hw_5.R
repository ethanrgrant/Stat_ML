#name: Ethan Grant
#uni: erg2145
#hw#: hw5
#class: STAT W4400

theta <- 1 
alpha <- 2
beta <- .2
n <- c(4, 8, 16, 256)
plot_points <- seq(0, 4, 0.001)
data <- rexp(256, 1)

for (i in 1:4) {
  plot <- dgamma(plot_points, rate = beta + sum(data[1:n[i]]), shape = alpha + n[i])
  if (i == 1) 
    plot(plot_points, plot, ylim = range(0, 7))
  else points(plot_points, plot, col = i*10)
}