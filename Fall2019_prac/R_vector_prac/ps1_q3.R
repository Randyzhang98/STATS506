# INPUT: n×3 matrix representing the trajectory (x, y, t)
# OUTPUT: nx3 matrix begining with time zero at the origin
set_origin_zero = function(x) {
  n <- dim(x)[1]
  origin <- x[1,]
  y <- x
  for (i in 1:n) {
    y[i,] <- y[i,] - origin
  }
  return(y)
}

# INPUT: xops and yops value of a point
# OUTPUT: the angle θ formed by the line connecting the origin and the point
compute_angle = function(x, y) {
  if (x == 0) {
    if (y == 0) {theta <- 0}
    else {
      theta <- sign(y) * (1 / 2) * pi
    }
  }
  else if (x > 0) { 
    theta <- atan(y / x)
  }
  else {
    tmp <- atan(y / x)
    theta <- tmp - sign(tmp) * pi
  }
  names(theta) <- NULL
  return(theta)
}

# INPUT: n×3 matrix representing the trajectory (x, y, t) beginning at zero
# OUTPUT: the angle θ formed by the secant line connecting the origin and the final position in the trajectory
compute_secant_angle = function(x) {
  n <- dim(x)[1]
  final <- x[n,]
  final_x <- final[1]
  final_y <- final[2]
  theta <- compute_angle(final_x, final_y)
  return(theta)
}

# INPUT: n×3 matrix representing the trajectory (x, y, t) beginning at zero
# OUTPUT: rotate the (x, y) coordinates of a trajectory so that the final point lies along the positive x-axis.
rotate_end_to_xaxis = function(x) {
  n <- dim(x)[1]
  y <- x
  theta <- compute_secant_angle(x)
  for (i in 1:n) {
    theta_i <- compute_angle(x[i, 1], x[i, 2])
    theta_i <- theta_i - theta
    secant_distance <- sqrt(x[i, 1]^2 + x[i, 2]^2)
    y[i,1] <- secant_distance * cos(theta_i)
    y[i,2] <- secant_distance * sin(theta_i)
  }
  return(y)
}

# INPUT: n×3 matrix representing the trajectory (x, y, t)
# OUTPUT: rotate the (x, y) coordinates of a trajectory so that the final point lies along the positive x-axis.
normalize = function(x) {
  y <- set_origin_zero(x)
  y <- rotate_end_to_xaxis(y)
  return(y)
}


# INPUT: a normalized trajectory
# OUTPUT:  the metrics describing its curvature 
measure_curvature = function(x) {
  # Calc the total (Euclidean) distance traveled
  n <- dim(x)[1]
  tot_dist <- 0
  for (i in 1:(n - 1)) {
    dist_i <- sqrt((x[i, 1] - x[i + 1, 1])^2 + (x[i, 2] - x[i + 1, 2])^2)
    tot_dist <- dist_i + tot_dist
  }
  
  # Calc the maximum absolute deviation from the secant connecting the starting and final positions
  max_abs_dev <- max(abs(x[, 2])) 
  
  # Calc the average absolute deviation of the observed trajectory from the direct path
  avg_abs_dev <- mean(abs(x[, 2]))
  
  # Calc the (absolute) area under the curve for the trajectory relative to the secant line using the trapezoidal rule to integrate
  AUC <- 0
  for (i in 1:(n - 1)) {
    x_dist <- x[i + 1, 1] - x[i, 1]
    AUC_i <- (1 / 2) * x_dist * (abs(x[i + 1, 2]) + abs(x[i, 2]))
    AUC <- AUC + AUC_i
  }
  re <- c(tot_dist, max_abs_dev, avg_abs_dev, AUC)
  return(re)
}

1+2
