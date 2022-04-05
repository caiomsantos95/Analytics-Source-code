optimal_capacity <- function(a, b, input_rate, service_rate) {
  seq <- seq(a, b, 1)
  out <- numeric()
  for (i in seq)
{ utilization <- input_rate/(service_rate*i)
  ave_inventory <- ((utilization^sqrt(2*(i+1)))/(1-utilization))
  wait_time <- (ave_inventory/input_rate)*60
  out <- c(out, wait_time)
  if (wait_time <= 1) {
    break}
  }
  print(i)
  print(wait_time)
  }
