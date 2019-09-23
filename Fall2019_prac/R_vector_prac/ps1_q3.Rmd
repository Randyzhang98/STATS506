---
title: "ps1_q3"
author: "Sijun Zhang"
date: "2019/9/23"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
# Problem 3

## Set Origin Zero 
[5pts] Write a function that accepts a n n×3 matrix representing the trajectory (x, y, t) and translates it to begin with time zero at the origin.

```{r}
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
```
In this part, I sequentially minused each point with the origin coordination, so that the curve was translated to the one beginning at origin and time zero.

## Compute Angle
[5pts] Write a function that computes the angle θ formed by the secant line connecting the origin and the final position in the trajectory. Your answer should be an angle between [−π,π]. Be sure your your solution works for a trajectory ending in any of the four quadrants.

```{r}
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
    if (y == 0) {
      theta <- -pi
    }
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
```
In this part, I wrote a universal function to compute the angle between the origin and the selected pint. I found that the period of tan function is only π, there were two special cases need to aware, one is the points at 3rd and 2nd quadrants and the other one is the points lying on the negative axis. I set the the points lying on the negative axis have -π angle to avoid runtime error.

## Rotate End to X-axis
[5pts] Write a function to rotate the (x, y) coordinates of a trajectory so that the final point lies along the positive x-axis.

```{r}
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
```
In this part, I computed each point's angle to the origin and minus the angle of the last points, then maintain the secant distance unchanged to rotate each point to the new angle, so that the curve was rotated to have the ends on x-axis.

## Normalize
[2pts] Combine the three parts above into a single function that normalizes an n×3 trajectory matrix to begin at the origin and end on the positive x-axis.

```{r}
# INPUT: n×3 matrix representing the trajectory (x, y, t)
# OUTPUT: rotate the (x, y) coordinates of a trajectory so that the final point lies along the positive x-axis.
normalize = function(x) {
  y <- set_origin_zero(x)
  y <- rotate_end_to_xaxis(y)
  return(y)
}
```
This part combine the previous functions into a pipeline.

## Measure the Curvature
[8pts] Write a function that accepts a normalized trajectory and computes the following metrics describing its curvature:

```{r}
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
```
In this part, the total (Euclidean) distance traveled,


