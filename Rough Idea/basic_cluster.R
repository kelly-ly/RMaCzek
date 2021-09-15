library("RColorBrewer")

# Preliminary idea for finding clustering breakpoints
basic_cluster = function(plot_values, num_cluster = 3, num_obs){
  S = rep(0, num_obs)
  for (i in 0:(num_obs-1)) {
    diff = abs(diff(plot_values[(i*num_obs+1):((i+1)*num_obs)]))
    ordered = order(diff, decreasing = TRUE)

    score = 4
    foo = diff[ordered[1]]
    for (j in ordered) {
      if(score == 0 || diff[j] == 0){
        break()
      }
      if(diff[j] == foo){
        S[j] = S[j] + score
      }else{
        score = score - 1
        foo = diff[j]
        S[j] = S[j] + score
      }
    }
  }

  orders = order(S, decreasing = TRUE)
  breakpoints = c(0, sort(orders[1:(num_cluster-1)]), num_obs)

  pal = brewer.pal(num_cluster, "Dark2")

  my_matrix = matrix(NA, nrow = num_obs, ncol = num_obs)
  for (i in 1:num_obs) {
    for (c in 1:num_cluster) {
      if(i > breakpoints[c] && i <= breakpoints[c+1]){
        my_matrix[i,] = rep(pal[c], num_obs)
      }
    }
  }
  my_col = as.vector(my_matrix)

  n <- num_obs
  p <- n
  plot_y <- rep(n:1, p)
  plot_x <- rep(1:p, rep(n, p))

  plot(plot_x, plot_y, cex = plot_values, col = my_col,
       pch = 20, axes = FALSE, xlab = "", ylab = "",
       xlim = c(0.5, p + 0.5), ylim = c(0.5, n + 0.5), main = "Czekanowski's diagram")

  return(breakpoints)
}

# load plot_value
# plot_value for mtcars symmetrically
load("symmetric")
# plot_value for mtcars asymmetrically
# load("asymmetric")

num_obs = nrow(mtcars)
basic_cluster(plot_values, num_cluster = 3, num_obs)

