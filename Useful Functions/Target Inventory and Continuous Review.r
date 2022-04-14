install.packages("glue")
library(glue)

total_stock <- function(review_period, lead_time, demand, sd, service_level ) {
  safety_stock <- service_level * sqrt(lead_time + review_period) * sd
  cycle_stock <- (review_period * demand)/2
  pipeline_stock <- lead_time * demand
  target_level <- (lead_time + review_period) * demand + safety_stock
  total_stock <- safety_stock + cycle_stock + pipeline_stock
  
  ### print answers
  print(glue('safety stock is {safety_stock}'))
  print(glue('cycle stock is {cycle_stock}'))
  print(glue('pipeline stock is {pipeline_stock}'))
  print(glue('target level is {target_level}'))
  print(glue('total stock is {total_stock}'))
}

#### EXERCISE 1
ex1<- total_stock(1, 2, 40, 20, 1.64)
ex1[1]

ex1_order <- 177 + 100 - 65
ex1_order

#### EXERCISE 1
continuos_review <- function (demand, sd, lead_time, fixed_cost, holding_cost, unit_cost, service_level) {
  order_quantity <- sqrt((2 * demand * fixed_cost) / (holding_cost * unit_cost)  )
  r <- lead_time * demand + service_level * sqrt(lead_time) * sd ### E[DDLT] + σ[DDLT]
  print(glue('Q is {order_quantity}'))
  print(glue('R is {r}'))
}
continuos_review(100, 35, 2, 50, 0.12, 10, 1.29)
