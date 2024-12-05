library(tidyverse)
library(RColorBrewer)
print("Load bag_sampling...")
plot_ball_bag <- function(scores_bag,
                          scores_hover,
                          score_name,
                          bag_color = "lightgrey",
                          seed = NA_integer_,
                          color_bag_balls = TRUE,
                          no_legend = FALSE,
                          limit_scores = NULL) {
  if (!is.null(seed))
    set.seed(seed)

  n <- length(scores_bag)
  k <- length(scores_hover)
  scores <- c(scores_bag, scores_hover)
  
  if (is.null(limit_scores)){
    limit_scores <- c(min(scores), max(scores))
  }
  
  random_position <- function(count, xmin, xmax, ymin, ymax) {
    tibble(x = runif(count, xmin, xmax),
           y = runif(count, ymin, ymax))
  }
  df_bag <- random_position(n, 2.5, 7.5, 1.5, 5.5) %>%
    mutate(type = "bag", score = scores_bag)
  df_hover <- tibble(x = seq(3, 7, length.out = k), y = rep(7.5, k)) %>%
    mutate(type = "hover", score = scores_hover)
  
  
  df_all <- bind_rows(df_bag, df_hover)
  
  if (!color_bag_balls) {
    df_all <- df_all %>%
      mutate(score = if_else(type == "bag", NA_real_, score))
  }
  
  if(no_legend){
    legend_pos = "none"
  }else{
    legend_pos = "right"
  }
  
  p <- ggplot() +
    geom_rect(
      aes(
        xmin = 2,
        xmax = 8,
        ymin = 1,
        ymax = 6
      ),
      fill = bag_color,
      color = "black",
      linewidth = 1
    ) +
    geom_point(data = df_all, aes(x = x, y = y, color = score), size = 5) +
    scale_color_gradientn(
      colors = rev(brewer.pal(9, "RdYlGn")),
      name = score_name,
      limits = limit_scores
    ) +
    annotate(
      "text",
      x = 1.5,
      y = 3.5,
      label = "Population",
      angle = 90,
      hjust = 0.5
    ) +
    annotate(
      "text",
      x = 1.5,
      y = 7.5,
      label = "Stichprobe",
      angle = 90,
      hjust = 0.5
    ) +
    # Add arrow
    annotate(
      "segment",
      x = 5,
      xend = 5,
      y = 6.2,
      yend = 7,
      arrow = arrow(length = unit(0.3, "cm")),
      linewidth = 1
    ) +
    # Adjust the plot
    coord_fixed(ratio = 1,
                xlim = c(0, 10),
                ylim = c(0, 10)) +
    theme_void() +
    theme(
      legend.position = legend_pos,
      legend.title = element_text(angle = 0, vjust = 0.5),
      legend.box.margin = margin(0, 0, -50, -50)
    )
  
  return(p)
}
# plot_ball_bag(rnorm(100), rnorm(10), "score_name", "grey90", 123, TRUE)
# plot_ball_bag(rnorm(100), rnorm(10), "score_name", "grey90", 123, FALSE)
# plot_ball_bag(sample(c("a", "b", "c"),size = 100, replace = TRUE), 
#               sample(c("a", "b", "c"),size = 10, replace = TRUE), 
#               "score_name", "grey90", FALSE)
# plot_ball_bag(rnorm(100), rnorm(10), "score_name", "grey90", FALSE)
