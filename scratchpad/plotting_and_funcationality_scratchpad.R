dataset <- read_csv("BaseballSavant.csv") %>%
  rename("age" = "player_age",
         "fullName" = `last_name, first_name`) %>%
  mutate(PositionGroup =
  case_when(position %in% c('C', '1B', '2B', '3B', 'SS') ~ "Infield",
            position %in% c('LF', 'CF', 'RF') ~ "Outfield",
            position %in% c('DH') ~ "Designated Hitter"))
dataset

my_palette <- brewer.pal(name="Set1",n=9)[1:9]


# Numerical Data summaries
ggplot(dataset,
       aes(x=xslg, # INPUT var
           y=home_run,
           color = PositionGroup, # Option for coloring
           outline = "Gray"
       )) +
  scale_color_brewer(palette = "Set1") +
  geom_point() +
  theme_bw()

# Histogram
ggplot(dataset,
       aes(x=home_run) #input variable
       ) + 
  geom_histogram(bins=10, fill = "#377EB8", color = "black") + # choose density or histogram
  theme_classic()


ggplot(dataset,
       aes(x=home_run) #input variable
       ) + 
  geom_density() + # choose density or histogram
  theme_classic()

# Categorical Datas
ggplot(dataset,
       aes(
         x=factor(PositionGroup, levels = c("Infield", "Outfield",
                                            "Designated Hitter"))
       )) + 
  geom_bar(fill ="#377EB8", color = "black") +
  ylab("Total Count") +
  xlab("Position Group") +
  theme_classic()

ggplot(dataset,
       aes(
         x=factor(position, levels = c("C", "1B", "2B", "3B", "SS",
                                               "LF", "CF", "RF", "DH"))
       )) + 
  geom_bar(fill ="#377EB8", color = "black") +
  ylab("Total Count") +
  xlab("Position") +
  theme_classic()


table(factor(dataset$position))
summary(factor(dataset$position, levels = c("C", "1B", "2B", "3B", "SS",
                                            "LF", "CF", "RF", "DH")))

# Numerical Summaries

### Getting numbers of interest
paste0("dataset$","home_run")
Mean <- mean(dataset$home_run)
Median <- median(dataset$home_run)
Variance <- var(dataset$home_run)
`Std. Dev` <- sd(dataset$home_run)
IQR <- IQR(dataset$home_run)
cbind(Mean, Median, Variance, `Std. Dev`, IQR)
