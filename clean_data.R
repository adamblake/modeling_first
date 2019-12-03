
# Description -------------------------------------------------------------

# This is the script I used to take the raw data, de-identify it, and clean it.
# A number of Rds files are created using this file, and they are of two types:
# the cleaned up data files for sharing, and personally identifiable data files
# prefixed with "PII_" that should not be shared.


# Packages ----------------------------------------------------------------

library(readxl)
library(tidyverse)


# Demographics ------------------------------------------------------------

partial_demographics_F18 <- read_rds("data/raw/2018 Fall/pre_survey/presurvey.Rds") %>%
  select(uid = UID, sex, race_ethnic) %>%
  filter(!is.na(uid)) %>%
  mutate(uid = str_pad(uid, width = 9, pad = 0), term = "F18")

value_maps <- lst(
  race = c("White", "African-American", "Asian", "Latinx", "Other"),
  sex = c("Male", "Female", "Non-binary/Other/Prefer not to answer")
)

partial_demographics_W19 <- read_rds("data/PII_response_data_W19.Rds") %>%
  select(student_id, prompt, response) %>%
  filter(prompt %in% c(
    "Student ID (omitting spaces and dashes)",
    "What is your sex?",
    "What is your racial/ethnic background?"
  ), response != "test") %>%
  spread(prompt, response) %>%
  select(-student_id) %>%
  set_names(c("uid", "sex", "race_ethnic")) %>%
  mutate_at(vars(-uid), ~ as.integer(parse_number(.)) + 1) %>%
  mutate(
    uid = str_pad(uid, width = 9, pad = 0),
    term = "W19",
    sex = value_maps$sex[sex],
    race_ethnic = value_maps$race[race_ethnic]
  )

partial_demographics <- bind_rows(
  partial_demographics_F18,
  partial_demographics_W19
) %>%
  write_rds("data/PII_partial_demographics.Rds")


# Grades sent to Registrar ------------------------------------------------

letter_grades <- map(c("A", "B", "C", "D", "F"), ~paste0(.x, c("+", "", "-"))) %>%
  flatten_chr() %>%
  rev()

final_grades <- bind_rows(
  read_excel("data/raw/PSYCH 100A Stigler 18F.xls", skip = 7, col_names = c(
    "status", "name", "uid", "section", paste0("hw_", 1:10),
    "final_exam", paste0("quiz_", 1:5), "final_grade"), na = c("", "Excused")
  ) %>%
    mutate(term = "F18") %>%
    mutate_at(vars(starts_with("hw_"), starts_with("quiz_")), as.numeric),

  read_excel("data/raw/PSYCH 100A Reise 19W.xls", skip = 7, col_names = c(
    "status", "name", "uid", "section", "hw_1", "hw_10", paste0("hw_", 2:9),
    "final_exam", paste0("quiz_", 1:5), "final_grade"), na = c("", "Excused")
  ) %>%
    mutate(term = "W19") %>%
    mutate_at(vars(starts_with("hw_"), starts_with("quiz_")), as.numeric)
) %>%
  mutate(
    uid = str_remove_all(uid, "[^0-9]"),
    final_grade = factor(final_grade, levels = letter_grades, ordered = TRUE)
  ) %>%
  filter(!is.na(uid)) %>%
  select(term, uid, final_grade, starts_with("hw_"), starts_with("quiz_"), final_exam) %>%
  write_rds("data/PII_final_grades.Rds")


# Transfer items from Final -----------------------------------------------

transfer_items <- suppressMessages(bind_rows(
  read_csv("data/raw/PSYCH 100A Stigler 18F - Final Exam.csv", skip = 3,
    col_names = names(read_csv("data/raw/PSYCH 100A Stigler 18F - Final Exam.csv", n_max = 1))
  ) %>%
    mutate(term = factor("F18", levels = c("W19", "F18"))) %>%
    select(term, matches("^Q4$"), matches("^Q5[0-6]$")) %>%
    set_names(c("term", "uid", paste0("Q", 1:7))),

  read_csv("data/raw/PSYCH 100A Reise 19W - Final Exam.csv", skip = 3,
    col_names = names(read_csv("data/raw/PSYCH 100A Reise 19W - Final Exam.csv", n_max = 1))
  ) %>%
    mutate(term = factor("W19", levels = c("W19", "F18"))) %>%
    select(term, matches("^Q1\\.4$"), matches("Q5\\.[5-9]|Q5.10|Q5.11")) %>%
    set_names(c("term", "uid", paste0("Q", 1:7)))
)) %>%
  mutate_all(as.character) %>%
  write_rds("data/PII_transfer_items.Rds")


# Other items from Final  ----------------------------------------------

other_items <- suppressMessages(bind_rows(
  read_csv("data/raw/PSYCH 100A Stigler 18F - Final Exam.csv", skip = 3,
           col_names = names(read_csv("data/raw/PSYCH 100A Stigler 18F - Final Exam.csv", n_max = 1))
  ) %>%
    mutate(term = factor("F18", levels = c("W19", "F18"))) %>%
    select(term, matches("^Q4$"), Q19, Q21, Q40) %>%
    set_names(c("term", "uid", paste0("Q", c(19, 21, 40)))),

  read_csv("data/raw/PSYCH 100A Reise 19W - Final Exam.csv", skip = 3,
           col_names = names(read_csv("data/raw/PSYCH 100A Reise 19W - Final Exam.csv", n_max = 1))
  ) %>%
    mutate(term = factor("W19", levels = c("W19", "F18"))) %>%
    select(term, Q1.4, Q4.3, Q4.5, Q4.23) %>%
    set_names(c("term", "uid", paste0("Q", c(19, 21, 40))))
)) %>%
  mutate_all(as.character) %>%
  write_rds("data/PII_other_items.Rds")


# De-identify and combine -------------------------------------------------

PII_key <- final_grades %>%
  distinct(uid, .keep_all = TRUE) %>%
  transmute(uid = uid, term = term, id = as_factor(seq_along(uid))) %>%
  write_rds("data/PII_key.Rds")

transfer_data <- lst(PII_key, final_grades, partial_demographics, transfer_items) %>%
  map(function(x) {
    x[x$uid %in% PII_key$uid, ] %>% distinct(uid, .keep_all = TRUE)
  }) %>%
  reduce(left_join, by = c("term", "uid")) %>%
  select(-uid) %>%
  write_rds("data/transfer_data.Rds")

other_data <- lst(PII_key, other_items) %>%
  map(function(x) {
    x[x$uid %in% PII_key$uid, ] %>% distinct(uid, .keep_all = TRUE)
  }) %>%
  reduce(left_join, by = c("term", "uid")) %>%
  select(-uid) %>%
  write_rds("data/other_data.Rds")


# Clean up ----------------------------------------------------------------

rm(list = ls())
