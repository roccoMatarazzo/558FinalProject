# 558FinalProject
This Shiny App is a culmination of the Master's Course ST558, Data Science with R, taught at North Carolina State University.

This app explores baseball data from the website Baseball Savant. The general goal of this app is to predict a player's Home Run total for a season using a variety of statistics. There are 3 Main Tabs, About, Data Exploration, and Modeling. The About tab gives a general rundown on the app, while the Data Exploration app allows the user to play with and familiarize themselves with the data, culminating in fitting models and seeing the results in the Modeling tab.

The following packages are used in this app:
- shiny
- shinythemes
- shinycssloaders
- DT
- tidyverse
- caret
- RColorBrewer

To install each of these at once, then run the app itself, run the following:

```{r}
# Install Packages
install.packages(c("shiny", "shinythemes", "shinycssloaders", "DT", "tidyverse","caret", "RColorBrewer")

# Run App
shiny::runGitHub(repo = "558FinalProject", username = "RoccoMatarazzo", ref="main")
```
