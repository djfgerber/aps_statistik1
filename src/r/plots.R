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

plot_hist_denstiy_expected_value1 <- function(means, xlab, binwidth,
                                              curves = list()){
  x_range <- seq(min(means), max(means), length.out = 1000)
  curve_data <- tibble(
    x = x_range,
    y = curves[[1]](x),
    Verteilung = names(curves)[1]
  )
  if(length(curves)==2){
    curve_data <- curve_data %>% 
      bind_rows(tibble(
      x = x_range,
      y = curves[[2]](x),
      Verteilung = names(curves)[2]
    ))
  }
  tibble(x = means) %>%
    ggplot() +
    geom_histogram(aes(x = x, 
                       y = after_stat(density)), 
                   binwidth = binwidth) +
    geom_line(
      data = curve_data,
      aes(x = x, y = y, color = Verteilung),
      linewidth = 2
    ) +
    labs(y = 'Wahrscheinlichkeitsdichte', x = xlab)+
    theme(legend.position = "bottom")
}

# plot_hist_denstiy_expected_value(rnorm(10000, 100, 15 / sqrt(100)),
#                                  100,
#                                  15,
#                                  100, 
#                                  'IQ',
#                                  1)
# plot_hist_denstiy_expected_value1(rnorm(10000, 100, 15 / sqrt(100)),
#                                   'IQ',
#                                   1,
#                                   list(
#                                     "t-Verteilung" = function(x)
#                                       dt((x-100)/(15 / sqrt(100)), 99) / (15 / sqrt(100)),
#                                     "other t" = function(x)
#                                       dt((x-100)/(15 / sqrt(100)), 3) / (15 / sqrt(100))
#                                   ))

# 
# plot_hist_denstiy_expected_value1 <- function(means, mu, sigma_g, df, xlab, binwidth){
#   x_range <- seq(min(means), max(means), length.out = 1000)
#   bell_curve = tibble(
#     x = x_range,
#     y = dnorm(x, mu, sigma_g)
#   )
#   t_curve = tibble(
#     x = x_range,
#     y = dt((x - mu) / sigma_g, df) / sigma_g
#   )
#   tibble(x = means) %>%
#     ggplot() +
#     geom_histogram(aes(x = x, 
#                        y = after_stat(density)), 
#                    binwidth = binwidth) +
#     geom_line(
#       data = bell_curve,
#       aes(x = x, y = y),
#       colour = "#f12489",
#       linewidth = 2
#     ) +
#     geom_line(
#       data = t_curve,
#       aes(x = x, y = y),
#       colour = "#038992",
#       linetype = "dashed",
#       linewidth = 2
#     ) +
#     labs(y = 'Wahrscheinlichkeitsdichte', x = xlab) +
#     theme(legend.position = "none")
# }

plot_attr_reja_pval <- list(
  add_colors = function(x, side, upper, lower, test_stat, mu0) {
    if (side == "both") {
      x <- x %>%
        mutate(color = case_when(
          plot_type %>% str_ends(" p-Wert") &
            (abs(test_stat - mu0) < abs(x - mu0)) ~ "red",
          plot_type %>% str_ends(" Ablehnungsbereich") &
            (x < lower | x > upper) ~ "red",
          TRUE ~ "green",
        ))
    } else if(side == "right"){
      x <- x %>%
        mutate(color = case_when(
          plot_type %>% str_ends(" p-Wert") &
            x > test_stat ~ "red",
          plot_type %>% str_ends(" Ablehnungsbereich") &
            x > upper ~ "red",
          TRUE ~ "green",
        ))
    } else if(side == "left"){
      x <- x %>%
        mutate(color = case_when(
          plot_type %>% str_ends(" p-Wert") &
            x < test_stat ~ "red",
          plot_type %>% str_ends(" Ablehnungsbereich") &
            x < lower ~ "red",
          TRUE ~ "green",
        ))
    }
    x %>% 
      mutate(color  = color %>%
               factor(levels = c("red", "green")))
  },
  custom_labeller =  as_labeller(c(
    "Histogram p-Wert" = "p-Wert",
    "Histogram Ablehnungsbereich" = "Ablehnungsbereich",
    "Verteilung p-Wert" = "p-Wert",
    "Verteilung Ablehnungsbereich" = "Ablehnungsbereich"
  )),
  create_text_data = function(x_positions, labels, y_value, plot_type) {
    tibble(
      label = labels,
      x = x_positions,
      y = rep(y_value, 2),
      color = c("red", "green"),
      plot_type = plot_type
    )
  },
  theme = theme(
      axis.text.y = element_text(),
      axis.ticks.y = element_line(),
      axis.title.y = element_text(),
      strip.placement = "outside",
      legend.position = "none"
      )
)

# 
# df <- 3L
# mu0 <- 100000
# scale_factor <- mu0/df
# 
# n <- 20L
# set.seed(61)
# x <- scale_factor * rchisq(n, df)
# scores_hover <- x / 1000 # hovering with K CHF in scale
# x_mean <- x %>% mean()
# x_sd <- x %>% sd()
# t_emp <- (x_mean - mu0)/(x_sd/sqrt(n))
# p_value_the <- 1-pt((x_mean - 100000)/ (x_sd / sqrt(n)), n-1)
# effect_size <- (x_mean - mu0) / x_sd
# 
# n_samples <- 3000L
# x_means <- 1:n_samples %>% 
#   map_dbl(~ mean(scale_factor*rchisq(n, df)))
# q95_x_means <- x_means %>% quantile(c(0.95)) %>% unlist() %>% unname()
# q05_x_means <- x_means %>% quantile(c(0.05)) %>% unlist() %>% unname()
# p_value_emp <- mean(x_means > x_mean)
# 
# # distribution if H_0 were true
# histogram_data <- tibble(
#   x = rep(x_means, 2),
#   plot_type = rep(c("Histogram p-Wert", 
#                     "Histogram Ablehnungsbereich"), 
#                   each = length(x_means))
# ) %>%
#   add_colors(side = "both", upper = q95_x_means, lower = q05_x_means, test_stat = x_mean, mu0 = mu0)
#   # add_colors(side = "right", upper = q95_x_means, test_stat = x_mean)
#   # add_colors(side = "left", lower = q05_x_means, test_stat = x_mean)
# 
# # Create data for density plots
# line_xlim <- xlim(c(40000, 180000))
# sigma_g <- x_sd / sqrt(n)
# x_range <- seq(mu0-3*sigma_g, mu0+3*sigma_g, length.out = 1000)
# density_data <- tibble(
#   x = rep(x_range, 2),
#   y = dt((x - mu0) / sigma_g, n - 1) / sigma_g,
#   plot_type = rep(c("Verteilung p-Wert", 
#                     "Verteilung Ablehnungsbereich"), 
#                   each = length(x_range))
# ) %>%
#   add_colors(side = "both", upper = q95_x_means, lower = q05_x_means, test_stat = x_mean, mu0 = mu0)
#   # add_colors(side = "right", upper = q95_x_means, test_stat = x_mean)
#   # add_colors(side = "left", lower = q05_x_means, test_stat = x_mean)
# 
# # Create text data for all plots
# text_pval <- str_c(round(100 * c(1 - p_value_emp, p_value_emp), 1), '%')
# text_sign <- c('95%', '5%')
# create_text_data <- function(labels, y_value, plot_type) {
#   tibble(
#     label = labels,
#     x = c(60000, 160000),
#     y = rep(y_value, 2),
#     color = c( "green", "red") %>%
#       factor(levels = c("red","green")),
#     plot_type = plot_type
#   )
# }
# text_data_all <- bind_rows(
#   create_text_data(
#     text_pval,
#     200,
#     "Histogram p-Wert"
#   ),
#   create_text_data(
#     text_sign, 
#     200,
#     "Histogram Ablehnungsbereich"
#   ),
#   create_text_data(
#     round(100 *c(1-p_value_the,p_value_the),1) %>% 
#       str_c('%'),
#     0.000015, 
#     "Verteilung p-Wert"
#   ),
#   create_text_data(
#     text_sign, 
#     0.000015,
#     "Verteilung Ablehnungsbereich"
#   )
# )
# 
# custom_labeller <- as_labeller(c(
#   "Histogram p-Wert" = "p-Wert",
#   "Histogram Ablehnungsbereich" = "Ablehnungsbereich",
#   "Verteilung p-Wert" = "p-Wert",
#   "Verteilung Ablehnungsbereich" = "Ablehnungsbereich"
# ))
# 
# 
# ggplot() +
#   geom_histogram(data = histogram_data,
#                  aes(x = x, fill = color),
#                  binwidth = 4300) +
#   geom_area(data = density_data,
#             aes(x = x, y = y, fill = color),
#             stat = "identity") +
#   geom_vline(xintercept = x_mean) +
#   geom_text(data = text_data_all,
#             mapping = aes(x = x, y = y, label = label, colour = color)) +
#   facet_wrap(~ plot_type, nrow = 2, scales = "free_y", 
#              strip.position = "top",
#              labeller = custom_labeller) +
#   line_xlim +
#   theme(
#     axis.text.y = element_text(),
#     axis.ticks.y = element_line(),
#     axis.title.y = element_text(),
#     strip.placement = "outside",
#     legend.position = "none"
#   ) +
#   labs(x = "Vermögen", y = "W.-dichte                              Häufigkeit")
# 
