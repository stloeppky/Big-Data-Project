#fullllist <- read_csv("Data - Clean/fulllist.csv")
#all.countries <- read_csv("Data - Clean/all.countries.csv")
#Data.K <-  read_csv("Data - Clean/Data-K.csv")

# Josh's Data visualization

pdf(file = "Figures/Dance and Acous Scatterplot.pdf")
ggplot(data = fulllist, aes(x = Danceability, y = Acousticness, colour = Country, fill = Country)) +
  geom_point(shape = 19, size = 2, alpha = 0.8) +
  xlim(0,1) +
  ylim(0,1) +
  labs(title = "Danceability & Acousticness of Songs by Country") +
  theme_minimal()
dev.off()

dance <- ggplot(data = fulllist, aes(Danceability, Country, fill = Country, colour = Country)) +
  stat_density_ridges(quantiles = 2, quantile_lines = TRUE, alpha = 0.8, scale = 1.5, rel_min_height = 0.01) +
  labs(title = "Danceability of the Top 50 songs") +
  xlim(0,1)

acous <- ggplot(data = fulllist, aes(Acousticness, Country, fill = Country, colour = Country)) +
  stat_density_ridges(quantiles = 2, quantile_lines = TRUE, alpha = 0.8, scale = 1.5, rel_min_height = 0.01) +
  labs(title = "Acousticness of the Top 50 songs") +
  xlim(0,1) 

pdf(file = "Figures/Dance and Acousticness Ridges.pdf")
ggarrange(dance, acous)
dev.off()


# Sage's Data visualization

pdf(file = "Figures/Mean Energy Barplot.pdf")
barplot(means.all, xlab = "Countries", ylab = "Mean Energy of top 50", ylim = c(0, 1), names.arg = names, col = blues9)
dev.off()

pdf(file = "Figures/Energy Density.pdf")
ggplot(all.countries, aes(x = energy, fill = Country, colour = Country)) +
  geom_density(alpha = 0.3)
dev.off()

pdf(file = "Figures/Enery Ridgeplot.pdf")
ggplot(data = all.countries, aes(energy, Country, fill = Country, colour = Country)) +
  stat_density_ridges(quantiles = 2, quantile_lines = TRUE, alpha = 0.8, scale = 1.5, rel_min_height = 0.01) +
  xlim(0, 1)
dev.off()


# Khay's Data visualization

#Distribution Color based on tempo values


t.r <- ggplot(Tempo.C, aes(x = `tempo`, y = `Country`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Tempo (BPM)", option = "C") +
  labs(title = 'Tempo (BPM) Trends In Top 50 For Different Countries') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
ggsave("Figures/Tempo Ridge plot.png", plot = t.r)


#as a violin Graph

t <- Tempo.C %>%
  mutate(text = fct_reorder(Country, tempo)) %>% # Reorder data
  ggplot( aes(x=Country, y=tempo, fill=Country, color=Country)) +
  geom_violin(width=1.1, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none"
  ) +
  coord_flip() + #switch X and Y axis and turns it to the horizontal version
  xlab("Top 50 Songs In Different Countries") +
  ylab("Tempo (BPM)")
ggsave("Figures/Tempo Violin plot.png", plot = t)

#Valence: —---------------------------------------------------------------------------------------
  
  #Color based on valence values


v.r <- ggplot(Valance.C, aes(x = `valence`, y = `Country`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
  scale_fill_viridis(name = "valence", option = "C") +
  labs(title = 'Valence Trends In Top 50 For Different Countries') +
  theme_ipsum() + xlim(0,1) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
ggsave("Figures/Valence Ridge plot.png", plot = v.r)

# As a violin plot


v <- Valance.C %>%
  mutate(text = fct_reorder(Country, valence)) %>% # Reorder data
  ggplot( aes(x=Country, y=valence, fill=Country, color=Country)) +
  geom_violin(width=1.3, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none"
  ) +
  coord_flip() + #switch X and Y axis and turns it to the horizontal version
  xlab("Top 50 Songs In Different Countries") +
  ylab("Valence") +
  ylim(0,1)
ggsave("Figures/Valence Violin plot.png", plot = v)


