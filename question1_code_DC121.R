# Options ----

# By default, R uses polynomial contrasts for ordered factors in linear models:
# > options("contrasts") 
# So we make ordered factors use treatment contrasts instead
options(contrasts = rep("contr.treatment", 2),
        dplyr.summarise.inform = FALSE)


# Load packages ----

if (!require("pacman", character.only = TRUE)) {
  install.packages("pacman", dependencies = TRUE)
}

pacman::p_load(
  "tidyverse",
  "lubridate",
  "WeightIt",
  "broom",
  "broom.mixed",
  "lme4",
  "marginaleffects",
  "ggokabeito",
  "ggmosaic",
  "scales",
  "here",
  install = TRUE
)

source(here("R", "graphics.R"))

