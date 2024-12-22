library(tidyverse)
library(RColorBrewer)
print("Load bag_sampling...")

random_position <- function(count, xmin, xmax, ymin, ymax) {
  tibble(x = runif(count, xmin, xmax),
         y = runif(count, ymin, ymax))
}

annotate_side_text <- function(y, label){
  return(
    annotate(
      "text",
      x = 1.5,
      y = y,
      label = label,
      angle = 90,
      hjust = 0.5
  ))
}

annotate_arrow <- function(y, yend){
  return(
    annotate(
      "segment",
      x = 5,
      xend = 5,
      y = y,
      yend = yend,
      arrow = arrow(length = unit(0.3, "cm")),
      linewidth = 1
  ))
}

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
  
  df_bag <- random_position(n, 2.5, 7.5, 1.5, 5.5) %>%
    mutate(type = "bag", score = scores_bag)
  df_hover <- tibble(x = seq(3, 7, length.out = k), y = rep(7.5, k)) %>%
    mutate(type = "hover", score = scores_hover)
  
  
  df_all <- bind_rows(df_bag, df_hover)
  
  if (!color_bag_balls) {
    df_all <- df_all %>%
      mutate(score = if_else(type == "bag", NA_real_, score))
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
    annotate_side_text(3.5, "Population")+
    annotate_side_text(7.5, "Stichprobe")+
    annotate_arrow(7.2, 8)+ # up
    coord_fixed(ratio = 1,
                xlim = c(0, 10),
                ylim = c(0, 10)) +
    theme_void() +
    theme(
      legend.position = if_else(no_legend, "none", "right"),
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
# plot_ball_bag(rnorm(100), rnorm(10), "score_name", "grey90",seed = 123L, no_legend = TRUE)


plot_ball_bag_two_samples <- function(scores_bag1,
                                      scores_bag2,
                                      scores_hover1,
                                      scores_hover2,
                                      score_name,
                                      group_names,
                                      bag_color = "lightgrey",
                                      seed = NA_integer_,
                                      color_bag_balls = TRUE,
                                      no_separation_line = FALSE,
                                      no_legend = FALSE,
                                      limit_scores = NULL) {
  if (!is.na(seed))
    set.seed(seed)
  
  n1 <- length(scores_bag1)
  k1 <- length(scores_hover1)
  n2 <- length(scores_bag2)
  k2 <- length(scores_hover2)
  
  scores <- c(scores_bag1, scores_hover1, scores_bag2, scores_hover2)
  if (is.null(limit_scores)) {
    limit_scores <- c(min(scores), max(scores))
  }
  
  df_bag1 <- random_position(n1, 2.5, 7.5, 2.5, 4.2) %>%
    mutate(type = "bag", score = scores_bag1)
  df_bag2 <- random_position(n2, 2.5, 7.5, 4.8, 6.5) %>%
    mutate(type = "bag", score = scores_bag2)
  df_hover1 <- tibble(x = seq(3, 7, length.out = k1), y = rep(0.5, k1)) %>%
    mutate(type = "hover", score = scores_hover1)
  df_hover2 <- tibble(x = seq(3, 7, length.out = k2), y = rep(8.5, k2)) %>%
    mutate(type = "hover", score = scores_hover2)
  df_all <- bind_rows(df_bag1, df_hover1, df_bag2, df_hover2)
  
  if (!color_bag_balls) {
    df_all <- df_all %>%
      mutate(score = if_else(type == "bag", NA_real_, score))
  }
  
  p <- ggplot() +
    geom_rect(
      aes(
        xmin = 2,
        xmax = 8,
        ymin = 2,
        ymax = 7
      ),
      fill = bag_color,
      color = "black",
      linewidth = 1
    )
    
  if (!no_separation_line) {
    p <- p +
      geom_segment(
        data = tibble(
          x = 2,
          y = 4.5,
          x_end = 8,
          y_end = 4.5
        ),
        mapping = aes(
          x = x,
          y = y,
          xend = x_end,
          yend = y_end
        )
      ) +
    annotate_side_text(5.7, "Pop. 1") +
    annotate_side_text(3.3, "Pop. 2") 
  }else{
    p <- p +
      annotate_side_text(4.5, "Population")
    
  }
  
  p <- p +
    geom_point(data = df_all, aes(x = x, y = y, color = score), size = 5) +
    scale_color_gradientn(colors = rev(brewer.pal(9, "RdYlGn")),
                          name = score_name,
                          limits = limit_scores) +
    annotate_side_text(8.5, "Stichp. 1") +
    annotate_side_text(.8, "Stichp. 2") +
    annotate_arrow(7.2, 8) + # up
    annotate_arrow(1.8, 1) + # down
    coord_fixed(ratio = 1,
                xlim = c(0, 10),
                ylim = c(0, 10)) +
    theme_void() +
    theme(
      legend.position = if_else(no_legend, "none", "right"),
      legend.title = element_text(angle = 0, vjust = 0.5),
      legend.box.margin = margin(0, 0, -10, -50)
    )
  
  return(p)
}

# plot_ball_bag_two_samples(rnorm(100), rnorm(100), rnorm(10),
#                           rnorm(10), 'blood', c('a','b'), seed = 1297L)
# plot_ball_bag_two_samples(rnorm(100, 5), rnorm(100), rnorm(10, 5),
#                           rnorm(10), 'blood', c('a','b'), seed = 1297L, 
#                           no_separation_line = TRUE)



