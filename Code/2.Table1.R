# Table 1

rm(list=ls())
library(tidyverse)
library(finalfit)
library(kableExtra)
library(officer)
library(flextable)
library(table1)
library(here)

# Load data
base_dir <- here('../')
print(base_dir)
in_dir <- paste0(base_dir, 'Data/')
out_dir <- paste0(base_dir, 'Results/')
df <- read_csv(paste0(in_dir, "final_dataset_clean.csv")) %>%
  mutate(Period = case_when(Year < 2012 ~ '1992-2011',
                            Year > 2011 ~ '2012-2023'),
         Year = as.numeric(Year),
         Scale_Period = interaction(
           Scale,
           Period,
           sep = " | ",
           drop = TRUE
         )) %>%
  ungroup()

# Median [IQR] age
median(df$MeanAge, na.rm=T)
quantile(df$MeanAge, c(.25, .5, .75), na.rm=T)

# Table 1
my_render <- function(x, name, ...) {
  if (name %in% c('Year','YearPub')){
    render.default(
      x,
      name = name,
      ...,
      digits = 4,
      render.continuous = c(. = 'Median [Q1, Q3]')
    )
  } else {
    render.default(
      x,
      name = name,
      ...,
      digits = 3,
      render.continuous = c(. = 'Mean (SD)')
    )
  }
  # digits <- if (name %in% c("Year", "YearPub")) 4 else 2
  # render.default(x, name = name, ..., digits = digits)
}
t1 <- table1(~ Year + YearPub + N + MeanAge + PropFem | Period + Scale,
               data = df, render = my_render, overall ="Combined sample")
t1flex(t1) %>% save_as_docx(path = paste0(out_dir, 'Table1.docx'))
t1total <- table1(~ Year + YearPub + N + MeanAge + PropFem,
                  data = df, render = my_render)
t1flex(t1total) %>% save_as_docx(path = paste0(out_dir, 'Table1_overall.docx'))

# Total Ns
df %>%
  group_by(Scale) %>%
  summarize(total_N = sum(N)) %>%
  mutate(pct = total_N/sum(total_N)*100)
df %>%
  summarize(total_N = sum(N)) %>%
  mutate(pct = total_N/sum(total_N)*100)
df %>%
  group_by(Scale,Period) %>%
  summarize(total_N = sum(N)) %>%
  mutate(pct = total_N/sum(total_N)*100)

# Table 1 - split by scale
explanatory <- c('Year','YearPub','N','MeanAge','PropFem','IUS_Mean','IUS_SD')
dependent <- c('Scale')
df %>%
  summary_factorlist(dependent = dependent, explanatory = explanatory,
                     add_col_totals = T, na_include = T, p = F, total_col = T) 
