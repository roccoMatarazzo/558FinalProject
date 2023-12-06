dataset <- read_csv("SavantData.csv")

dataset

# Numerical Data summaries
ggplot(dataset,
       aes(x=age,
           y=home_run
             )) +
  geom_point() +
  theme_classic()

# Numerical Data summaries
ggplot(dataset,
       aes(x=age,
           y=home_run,
           color = position
       )) +
  scale_color_brewer(palette = "BuPu") +
  geom_point() +
  theme_classic()

# Categorical Data
table(savantfinal$position, savantfinal$league)
table(savantfinal$age, savantfinal$league)
table(savantfinal$age, savantfinal$position)

ggplot(dataset,
       aes(
         x=age #input for either position or league
       )) + 
  geom_bar() +
  theme_classic()

ggplot(dataset,
       aes(
         x=league
       )) + 
  geom_bar() +
  theme_classic()

ggplot(dataset,
       aes(
         x=position
       )) + 
  geom_bar(fill ="blue") +
  theme_classic()
