

# Packages and helper functions -------------------------------------------

extrafont::loadfonts("win")
library(imager)
library(tidyverse)
library(supernova)

add_border <- function(file) {
  load.image(file) %>%
    autocrop() %>%
    pad(50, "xy", val = "white") %>%
    pad(5, "xy", val = "black") %>%
    save.image(file)
}


# Output settings ---------------------------------------------------------

output_dir <- "figures"
if (!dir.exists(output_dir)) dir.create(output_dir)


# Print model output to images --------------------------------------------

candy_rankings <- fivethirtyeight::candy_rankings %>%
  mutate(sugarpercent = sugarpercent * 100)

models <- lst(
  fit_chocolate = lm(winpercent ~ chocolate, candy_rankings),
  fit_sugar = lm(winpercent ~ sugarpercent, candy_rankings),
  fit_chocolate_sugar = lm(winpercent ~ chocolate + sugarpercent, candy_rankings),
  fit_sugar_chocolate = lm(winpercent ~ sugarpercent + chocolate, candy_rankings),
  anova_chocolate = supernova(lm(winpercent ~ chocolate, candy_rankings), 1),
  anova_sugar = supernova(lm(winpercent ~ sugarpercent, candy_rankings), 1),
  anova_chocolate_sugar = supernova(lm(winpercent ~ chocolate + sugarpercent, candy_rankings), 1),
  anova_sugar_chocolate = supernova(lm(winpercent ~ sugarpercent + chocolate, candy_rankings), 1)
)

outputs <- map(models, function(x) {
  capture.output(x) %>%
    paste0(collapse = "\n") %>%
    enframe(name = NULL)
})

plots <- map(outputs, function(x) {
  ggplot(x, aes(0, 0, label = value)) +
    geom_text(hjust = "left", vjust = "top", family = "Consolas") +
    coord_cartesian(xlim = c(0, 3)) +
    theme_void()
})

iwalk(plots, function(x, name) {
  path <- file.path(output_dir, sprintf("%s.png", paste("output", name, sep = "_")))
  height = if (str_starts(name, "fit_")) 3 else 7
  ggsave(path, x, width = 6.5, height = height)
  add_border(path)
})
