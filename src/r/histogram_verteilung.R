rm(list = ls())

library(tidyverse)

set.seed(1982)

plot_density_histogram <- function(x, density = TRUE, binwidth = 0.05) {
  if(density){
    aes_fn <- aes(x=x, after_stat(density))
    ylab_fn <- ylab("Wahrscheinlichkeitsdichte")
  }else{
    aes_fn <- aes(x=x)
    ylab_fn <- ylab("Häufigkeit")
  }
  tibble(x = x) %>% 
    ggplot(aes_fn)+
    geom_histogram(binwidth = binwidth)+
    ylab_fn+
    xlab("Beobachtete Werte")
}

# Erweiterung des Histograms ist die Wahrscheinlichkeitsdichte
# Idee: Histogramm mit unendlich vielen Beobachtungen gibt Linie
# Idee: Linie ist die Verteilung der 'Beobachtungen' in der Grundgesamtheit
xlim_fn <- xlim(c(-4,4))
plot_density_histogram(x = rnorm(2000), FALSE, binwidth = 0.5)+
  xlim_fn
plot_density_histogram(x = rnorm(2000), binwidth = 0.5)+
  xlim_fn
plot_density_histogram(x = rnorm(2000), binwidth = 0.5)+
  xlim_fn+
  geom_line(data = tibble(x = seq(-4,4, 0.01),
                          y = dnorm(x)),
            aes(x=x,y=y),
            linewidth=1.5,
            color = 'blue',
            alpha =0.5)

plot_density_histogram(x = rnorm(20000), FALSE)+
  xlim_fn
plot_density_histogram(x = rnorm(20000))+
  xlim_fn
plot_density_histogram(x = rnorm(20000))+
  xlim_fn+
  geom_line(data = tibble(x = seq(-4,4, 0.01),
                          y = dnorm(x)),
            aes(x=x,y=y),
            linewidth=1.5,
            color = 'blue',
            alpha =0.5)

plot_density_histogram(x = rnorm(200000), FALSE)+
  xlim_fn
plot_density_histogram(x = rnorm(200000))+
  xlim_fn
plot_density_histogram(x = rnorm(200000))+
  xlim_fn+
  geom_line(data = tibble(x = seq(-4,4, 0.01),
                          y = dnorm(x)),
            aes(x=x,y=y),
            linewidth=1.5,
            color = 'blue',
            alpha =0.5)

# Wieso Wahrscheinlichkeitsidchte und nicht Wahrscheinlichkeit?
# Wahrscheinlichkeit ist ein Wert zwischen 0 und 1
# Wenn die beobachteten Werte in einem sehr engen Balken sind, 
# muss die Höhe enstprechend höher als 1 gewählt werden, damit die Gesamtzahl der Balken 1 gibt. 
# 1 Enstpricht der Wahrscheinlichkeit, dass etwas passiert.
xlim_fn <- xlim(c(-4,4))
plot_density_histogram(x = rnorm(2000, sd = 0.25), FALSE, binwidth = 0.25)+
  xlim_fn
plot_density_histogram(x = rnorm(2000, sd = 0.25), binwidth = 0.25)+
  xlim_fn
plot_density_histogram(x = rnorm(2000, sd = 0.25), binwidth = 0.25)+
  xlim_fn+
  geom_line(data = tibble(x = seq(-4,4, 0.01),
                          y = dnorm(x, sd =0.25)),
            aes(x=x,y=y),
            linewidth=1.5,
            color = 'blue',
            alpha =0.5)

plot_density_histogram(x = rnorm(20000,sd = 0.25), FALSE, binwidth = 0.05)+
  xlim_fn
plot_density_histogram(x = rnorm(20000, sd = 0.25), binwidth = 0.05)+
  xlim_fn
plot_density_histogram(x = rnorm(20000, sd = 0.25), binwidth = 0.05)+
  xlim_fn+
  geom_line(data = tibble(x = seq(-4,4, 0.01),
                          y = dnorm(x, sd =0.25)),
            aes(x=x,y=y),
            linewidth=1.5,
            color = 'blue',
            alpha =0.5)

plot_density_histogram(x = rnorm(200000,sd = 0.25), FALSE, binwidth = 0.01)+
  xlim_fn
plot_density_histogram(x = rnorm(200000, sd = 0.25), binwidth = 0.01)+
  xlim_fn
plot_density_histogram(x = rnorm(200000, sd = 0.25), binwidth = 0.01)+
  xlim_fn+
  geom_line(data = tibble(x = seq(-4,4, 0.01),
                          y = dnorm(x, sd =0.25)),
            aes(x=x,y=y),
            linewidth=1.5,
            color = 'blue',
            alpha =0.5)


# Was für Verteilungen gibt es?

## Uniform: Alle Werte sind gleich wahrscheinlich. 
# Beispiel: Geburtstagswahrscheinlichkeit in Sekunden ab 1. Januar
xlim_fn <- xlim(c(-0,1))
plot_density_histogram(x = runif(2000), FALSE, binwidth = 0.05)+
  xlim_fn
plot_density_histogram(x = runif(2000), binwidth = 0.05)+
  xlim_fn
plot_density_histogram(x = runif(2000), binwidth = 0.05)+
  xlim_fn+
  geom_line(data = tibble(x = seq(-0,1, 0.01),
                          y = dunif(x)),
            aes(x=x,y=y),
            linewidth=1.5,
            color = 'blue',
            alpha =0.5)

plot_density_histogram(x = runif(20000), FALSE, binwidth = 0.01)+
  xlim_fn
plot_density_histogram(x = runif(20000), binwidth = 0.01)+
  xlim_fn
plot_density_histogram(x = runif(20000), binwidth = 0.01)+
  xlim_fn+
  geom_line(data = tibble(x = seq(-0,1, 0.01),
                          y = dunif(x)),
            aes(x=x,y=y),
            linewidth=1.5,
            color = 'blue',
            alpha =0.5)

plot_density_histogram(x = runif(200000), FALSE, binwidth = 0.005)+
  xlim_fn
plot_density_histogram(x = runif(200000), binwidth = 0.005)+
  xlim_fn
plot_density_histogram(x = runif(200000), binwidth = 0.005)+
  xlim_fn+
  geom_line(data = tibble(x = seq(-0,1, 0.01),
                          y = dunif(x)),
            aes(x=x,y=y),
            linewidth=1.5,
            color = 'blue',
            alpha =0.5)

## Exponentiell: Alle Werte sind gleich wahrscheinlich. 
# Beispiel: Zeit zwischen zwei Geysierausbrüchen, zwischen zwei Ladenkund:innen, zwischen Erdbeben
xlim_fn <- xlim(c(-0,10))
plot_density_histogram(x = rexp(2000), FALSE, binwidth = 0.5)+
  xlim_fn
plot_density_histogram(x = rexp(2000), binwidth = 0.5)+
  xlim_fn+
  geom_line(data = tibble(x = seq(-0,10, 0.01),
                          y = dexp(x)),
            aes(x=x,y=y),
            linewidth=1.5,
            color = 'blue',
            alpha =0.5)

plot_density_histogram(x = rexp(20000), FALSE, binwidth = 0.1)+
  xlim_fn
plot_density_histogram(x = rexp(20000), binwidth = 0.1)+
  xlim_fn
plot_density_histogram(x = rexp(20000), binwidth = 0.1)+
  xlim_fn+
  geom_line(data = tibble(x = seq(-0,10, 0.01),
                          y = dexp(x)),
            aes(x=x,y=y),
            linewidth=1.5,
            color = 'blue',
            alpha =0.5)

plot_density_histogram(x = rexp(200000), FALSE, binwidth = 0.05)+
  xlim_fn
plot_density_histogram(x = rexp(200000), binwidth = 0.05)+
  xlim_fn
plot_density_histogram(x = rexp(200000), binwidth = 0.05)+
  xlim_fn+
  geom_line(data = tibble(x = seq(-0,10, 0.01),
                          y = dexp(x)),
            aes(x=x,y=y),
            linewidth=1.5,
            color = 'blue',
            alpha =0.5)

# Chi-Quadrat Verteilung
# Spezifisch konstruiert um ein Problem zu lösen.
xlim_fn <- xlim(c(-0,20))
df <- 4
plot_density_histogram(x = rchisq(2000, df), FALSE, binwidth = 0.5)+
  xlim_fn
plot_density_histogram(x = rchisq(2000, df), binwidth = 0.5)+
  xlim_fn+
  geom_line(data = tibble(x = seq(-0,20, 0.01),
                          y = dchisq(x,df)),
            aes(x=x,y=y),
            linewidth=1.5,
            color = 'blue',
            alpha =0.5)

plot_density_histogram(x = rchisq(20000, df), FALSE, binwidth = 0.2)+
  xlim_fn
plot_density_histogram(x = rchisq(20000, df), binwidth = 0.2)+
  xlim_fn
plot_density_histogram(x = rchisq(20000, df), binwidth = 0.2)+
  xlim_fn+
  geom_line(data = tibble(x = seq(-0,20, 0.01),
                          y = dchisq(x,df)),
            aes(x=x,y=y),
            linewidth=1.5,
            color = 'blue',
            alpha =0.5)

plot_density_histogram(x = rchisq(200000, df), FALSE)+
  xlim_fn
plot_density_histogram(x = rchisq(200000, df))+
  xlim_fn
plot_density_histogram(x = rchisq(200000, df))+
  xlim_fn+
  geom_line(data = tibble(x = seq(-0,20, 0.01),
                          y = dchisq(x,df)),
            aes(x=x,y=y),
            linewidth=1.5,
            color = 'blue',
            alpha =0.5)

