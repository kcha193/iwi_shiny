
library(readxl)
library(tidyverse)


dat <- read_excel("data/All iwi 2013 Te Kupenga_fixed.xlsx")


dat_final <-
  dat %>%
  pivot_longer(starts_with("Q"), values_to = "Percent") %>%
  separate(name, c("Question", "Categories"), ": ") %>%
   mutate(Categories = factor(
    Categories, levels = c(
      "Yes",
      "Don't need  help",
      "Very easy",
      "Easy",
      "Sometimes easy/ sometimes hard",
      "Hard/ very hard",
      "Very  important",
      "Quite  important",
      "Somewhat  important",
      "A little  important",
      "Not at all  important"
    )
  )) %>%
  arrange(Categories) %>%
  select(-Safety) %>%
  mutate(
    Question = case_when(
      Question == "Q1" ~ "Visited ancestral marae in previous 12 months",
      Question == "Q2" ~ "Ease of getting support with Māori cultural practices",
      Question == "Q3" ~ "Importance of being engaged in Māori culture"
    ),
    Percent = Percent / 100
  )





