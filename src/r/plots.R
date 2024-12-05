library(tidyverse)
plot_hist_means <- function(x, lb, ub, binwidth, xlab, plot_lb = TRUE, plot_ub = TRUE) {
  stopifnot(plot_lb | plot_ub)
  if(plot_lb & plot_ub){
    cond <- x < lb | x > ub
    vline <- geom_vline(xintercept = c(lb, ub))
  }else if(plot_lb){
    cond <- x < lb
    vline <- geom_vline(xintercept = c(lb))
  }else{
    cond <- x > ub
    vline <- geom_vline(xintercept = c(ub))
  }
  tibble(x = x) %>%
    mutate(color = if_else(cond, "green", "red")) %>%
    ggplot(aes(x = x, fill = color)) +
    geom_histogram(binwidth = binwidth) +
    vline +
    labs(y = 'Häufigkeit', x = xlab) +
    theme(legend.position = "none")
}

plot_hist_denstiy_expected_value <- function(means, mu, sigma, n, xlab, binwidth){
  sigma_g <- sigma / sqrt(n)
  x_range <- seq(min(means), max(means), length.out = 1000)
  bell_curve = tibble(
    x = x_range,
    y = dnorm(x, mu, sigma_g)
  )
  t_curve = tibble(
    x = x_range,
    y = dt((x - mu) / sigma_g, n - 1) / sigma_g
  )
  tibble(x = means) %>%
    ggplot() +
    geom_histogram(aes(x = x, 
                       y = after_stat(density)), 
                   binwidth = binwidth) +
    geom_line(
      data = bell_curve,
      aes(x = x, y = y),
      colour = "#f12489",
      linewidth = 2
    ) +
    geom_line(
      data = t_curve,
      aes(x = x, y = y),
      colour = "#038992",
      linetype = "dashed",
      linewidth = 2
    ) +
    labs(y = 'Wahrscheinlichkeitsdichte', x = xlab) +
    theme(legend.position = "none")
}