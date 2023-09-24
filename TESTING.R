

library(dplyr)
library(plotly)
library(data.table)

n <- 100
stars <- c("0", "*", "**", "***")
dt <- data.table(Type = factor(sample(stars, 
                                         n, 
                                         replace = T), 
                                  levels = stars),
                 Rand_var_1 = rnorm(n), 
                 Rand_var_2 = rnorm(n))

dt[, .N, by = Type] %>%
plot_ly( 
  x = ~Type,
  y = ~N,
  type = "bar"
  )

NSummary <- function(dt, var){
  dt[, .N, by = var]
}

n_summary_df <- NSummary(dt, deparse(substitute(Type)))



SummaryPlot <- function(dt, var){
  var_prime <- deparse(substitute(var))
  dt[, .N, by = var_prime] %>%
  plot_ly(
    x = enquo(var),
    y = ~N,
    type = "bar"
  )
}

SummaryPlot(n_summary_df, Type)



fun1 <- function(dt, y, by_col) {
  expr <- quote(dt[, 
                   .(lm_results = lapply(.SD, function(x) summary(lm(Y ~ x)))),
                   .SDcols = sdcols,
                   by = byexpr])
  eval(do.call(substitute, list(expr, 
                                list(sdcols=substitute(!y), Y=as.name(y), byexpr=substitute(by_col)))))
}

fun1(data1, "colA", colD)
            
SummaryPlot <- function(dt, var){
  var_prime <- deparse(substitute(var))
  dt[, .N, by = var_prime] %>%
  SummaryPlot(enquo(var))
}

MakeSummaryPlot(dt, Type)


























