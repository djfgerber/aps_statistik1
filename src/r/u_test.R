library(tidyverse)
library(RColorBrewer)

#' Plot U-Test Illustration
#'
#' @param x Tibble with x = Variable and Gruppe = Group
#' @param var_name What the Variable is called
#'
#' @returns A dot plot illustrating the U-Test
#' @export
#'
#' @examples
#' set.seed(1239)
#' n <- 30
#' x1 <- tibble(x = c(rnorm(n/2), rnorm(n/2, 3)), 
#'              Gruppe = c(rep("A", n/2), rep("B", n/2)))
#' x2 <- tibble(x = c(rnorm(n/2), rnorm(n/2, 0)),
#'              Gruppe = c(rep("hei", n/2), rep("ho", n/2)))
#' x3 <- tibble(x = c(rnorm(n/2), rnorm(n/2, 3)),
#'               Gruppe = c(rep("very-long", n/3), rep("very-very-long", 2*n/3)))
#' x4 <- tibble(x = c(rnorm(n/2), rnorm(n/2, 0)),
#'               Gruppe = c(rep("A", n/3), rep("B", 2*n/3)))
#'               
#' plot_utest_order(x1, "generic")
#' plot_utest_order(x2, "srt")
#' plot_utest_order(x3, "very-very-long")
#' plot_utest_order(x4, "no_clue")
plot_utest_order <- function(x, var_name){
  x <- x %>% 
    mutate(i = 1:nrow(.)) %>% 
    arrange(x) %>% 
    mutate(r_i = 1:nrow(.),
           y1 = 4,
           y2 = 3,
           y3 = 2,
           y4 = 1) 
  p <- x %>% 
    ggplot() +
    geom_point(aes(x = r_i, y = y2, color = x, shape = Gruppe), size = 5)+
    geom_text(aes(x = r_i, y = y2, label = r_i), size = 3)+
    geom_point(data = x %>% filter(Gruppe == unique(Gruppe)[1]),
               aes(x = r_i, y = y3, color = x, shape = Gruppe), size =5)+
    geom_text(data = x %>% filter(Gruppe == unique(Gruppe)[1]),
              aes(x = r_i, y = y3, label = r_i), size = 3)+
    geom_point(data = x %>% filter(Gruppe ==unique(Gruppe)[2]),
               aes(x = r_i, y = y1, color = x, shape = Gruppe), size =5)+
    geom_text(data = x %>% filter(Gruppe == unique(Gruppe)[2]),
              aes(x = r_i, y = y1, label = r_i), size = 3)+
    scale_x_continuous(limits = c(1, nrow(x) + 5))+
    scale_color_gradientn(name = var_name,
      colors = rev(brewer.pal(9, "RdYlGn"))
    )+
    theme_void() +
    theme(
      legend.title = element_text(angle = 0, vjust = 0.5),
      legend.box.margin = margin(0, 0, -50, -50)
    )+
    annotate(
      "segment",
      x = nrow(x)/2,
      xend = nrow(x)/2,
      y = 3.2,
      yend = 3.7,
      arrow = arrow(length = unit(0.3, "cm")),
      linewidth = 1
    )+
    annotate(
      "segment",
      x = nrow(x)/2,
      xend = nrow(x)/2,
      y = 2.8,
      yend = 2.3,
      arrow = arrow(length = unit(0.3, "cm")),
      linewidth = 1
    )
  return(p)
}





