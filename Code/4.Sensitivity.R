# Sensitivity analyses

# 1. using publication year rather than data collection year
# 2. without outlier removal
# 3. with scale form as covariate (incl scale-time interaction)

rm(list=ls())
library(tidyverse)
library(finalfit)
library(segmented)
library(kableExtra)
library(officer)
library(flextable)
library(broom)
library(RColorBrewer)
library(ggthemes)
library(here)

##################################################################################################
####### S1: model with pub year instead of data collection year

# Load data
base_dir <- here('../')
print(base_dir)
in_dir <- paste0(base_dir, 'Data/')
out_dir <- paste0(base_dir, 'Results/')
df <- read_csv(paste0(in_dir, "final_dataset_clean.csv")) 

# Recode IUS mean in SD units
print(df %>% group_by(Scale) %>% summarize(mean_SD = weighted.mean(IUS_SD, N, na.rm=T)))
df <- df %>%
  group_by(Scale) %>%
  mutate(IUS_Mean_SDunit = IUS_Mean / weighted.mean(IUS_SD, N, na.rm=T))

# I want 9 models: long/short/combined by 92-23/92-11/12-23
datasets <- list(df %>% filter(Scale=='long', Year < 2012),
                 df %>% filter(Scale=='long', Year > 2011),
                 df %>% filter(Scale=='long'),
                 df %>% filter(Scale=='short', Year < 2012),
                 df %>% filter(Scale=='short', Year > 2011),
                 df %>% filter(Scale=='short'),
                 df %>% filter(Year < 2012),
                 df %>% filter(Year >= 2012),
                 df)
names(datasets) <- c('long_early','long_late','long_all','short_early','short_late','short_all','combined_early','combined_late','combined_all')

# I want 2 models: unadjusted, adjusted. I want 2 outcomes: unscaled, scaled
all_out_pubyear <- data.frame()
for (dsname in names(datasets)){
  ds <- datasets[[dsname]]
  for (adj in c('Model 1','Model 2')){
    for (scaled in c('Unscaled','Scaled')){
      if (!(str_detect(dsname, 'combined') & scaled=='Unscaled')){ # Only run unscaled models when not combining long and short form
        if (adj == 'Model 1' & scaled == 'Unscaled'){model <- lm('IUS_Mean ~ PubYear_shifted', data = ds, weights = ds$N)}
        else if (adj == 'Model 1' & scaled == 'Scaled'){model <- lm('IUS_Mean_SDunit ~ PubYear_shifted', data = ds, weights = ds$N)}
        else if (adj == 'Model 2' & scaled == 'Unscaled'){model <- lm('IUS_Mean ~ PubYear_shifted + MeanAge + PercFem', data = ds, weights = ds$N)}
        else if (adj == 'Model 2' & scaled == 'Scaled'){model <- lm('IUS_Mean_SDunit ~ PubYear_shifted + MeanAge + PercFem', data = ds, weights = ds$N)}
        else if (adj == 'Model 1' & scaled == 'Item'){model <- lm('IUS_Mean_item ~ PubYear_shifted', data = ds, weights = ds$N)}
        else if (adj == 'Model 2' & scaled == 'Item'){model <- lm('IUS_Mean_item ~ PubYear_shifted + MeanAge + PercFem', data = ds, weights = ds$N)}
        else {model <- 'ERROR'}
        out <- broom::tidy(model, conf.int = T)
        out$N <- nobs(model)
        out$ds <- dsname
        out$model <- sprintf('%s, %s',adj, scaled)
        all_out_pubyear <- rbind(all_out_pubyear, out)
      }
    }
  }
}

t2_pubyear <- all_out_pubyear %>%
  mutate(effect = sprintf('%.2f (%.2f, %.2f)',estimate, conf.low, conf.high)) %>%
  dplyr::select(ds, model, term, effect) %>%
  pivot_wider(id_cols = c('ds','term'), names_from = model, values_from = effect) %>%
  dplyr::select(ds, term, `Model 1, Unscaled`, `Model 2, Unscaled`, `Model 1, Scaled`, `Model 2, Scaled`)
t2_pubyear %>% write.csv(paste0(out_dir, 'eTable2_pubyear.csv'))

# Print string with Ns per model
model_Ns <-
  all_out_pubyear %>%
  distinct(ds, model, N) %>%
  separate_wider_delim(cols = 'model', delim = ', ', names = c('Model', 'Adjustment')) %>%
  dplyr::select(-Adjustment) %>%
  distinct() %>%
  separate_wider_delim(cols = 'ds', delim = '_', names = c('Scale','Time')) %>%
  mutate(Scale = str_replace_all(Scale, "long", "Long form"),
         Scale = str_replace_all(Scale, "short", "Short form"),
         Scale = str_replace_all(Scale, "combined", "Both forms"),
         Time = str_replace_all(Time, "early", "1992-2011"),
         Time = str_replace_all(Time, "late", "2012-2022"),
         Time = str_replace_all(Time, "all", "1992-2022"))
string_for_paper <- ''
for (rowi in 1:nrow(model_Ns)){
  row <- model_Ns[rowi,]
  row_string <- sprintf('%s, %s, %s: k = %i',row$Scale, row$Time, row$Model, row$N)
  # print(row_string)
  string_for_paper <- paste(string_for_paper, row_string, sep = '; ')
}

# Save to word
ft2_pubyear <- t2_pubyear %>%
  flextable() %>%
  bold(part = "header") %>%
  autofit() %>%                              # fit columns to content
  set_table_properties(layout = "autofit") %>%  # let table expand with content
  valign(val = "center")                 # vertical align cells to center
read_docx() %>%
  body_add_flextable(ft2_pubyear) %>%
  print(target = paste0(out_dir, 'eTable2_pubyear.docx'))

##################################################################################################
####### S2: model without outlier removal
df <- read_csv(paste0(in_dir, "final_dataset.csv"))  %>%
  arrange(Source, YearPub, Study) %>%
  mutate(PercFem = PropFem*100,
         Year_shifted = Year - 1990,
         PubYear_shifted = YearPub - 1990)
dim(df)
# Recode IUS mean in SD units
print(df %>% group_by(Scale) %>% summarize(mean_SD = weighted.mean(IUS_SD, N, na.rm=T)))
df <- df %>%
  group_by(Scale) %>%
  mutate(IUS_Mean_SDunit = IUS_Mean / weighted.mean(IUS_SD, N, na.rm=T))


# I want 9 models: long/short/combined by 92-23/92-11/12-23
datasets <- list(df %>% filter(Scale=='long', Year < 2012),
                 df %>% filter(Scale=='long', Year > 2011),
                 df %>% filter(Scale=='long'),
                 df %>% filter(Scale=='short', Year < 2012),
                 df %>% filter(Scale=='short', Year > 2011),
                 df %>% filter(Scale=='short'),
                 df %>% filter(Year < 2012),
                 df %>% filter(Year >= 2012),
                 df)
names(datasets) <- c('long_early','long_late','long_all','short_early','short_late','short_all','combined_early','combined_late','combined_all')

# I want 2 models: unadjusted, adjusted. I want 2 outcomes: unscaled, scaled
all_out_woutlier <- data.frame()
for (dsname in names(datasets)){
  ds <- datasets[[dsname]]
  for (adj in c('Model 1','Model 2')){
    for (scaled in c('Unscaled','Scaled')){
      if (!(str_detect(dsname, 'combined') & scaled=='Unscaled')){ # Only run unscaled models when not combining long and short form
        if (adj == 'Model 1' & scaled == 'Unscaled'){model <- lm('IUS_Mean ~ Year_shifted', data = ds, weights = ds$N)}
        else if (adj == 'Model 1' & scaled == 'Scaled'){model <- lm('IUS_Mean_SDunit ~ Year_shifted', data = ds, weights = ds$N)}
        else if (adj == 'Model 2' & scaled == 'Unscaled'){model <- lm('IUS_Mean ~ Year_shifted + MeanAge + PercFem', data = ds, weights = ds$N)}
        else if (adj == 'Model 2' & scaled == 'Scaled'){model <- lm('IUS_Mean_SDunit ~ Year_shifted + MeanAge + PercFem', data = ds, weights = ds$N)}
        else if (adj == 'Model 1' & scaled == 'Item'){model <- lm('IUS_Mean_item ~ Year_shifted', data = ds, weights = ds$N)}
        else if (adj == 'Model 2' & scaled == 'Item'){model <- lm('IUS_Mean_item ~ Year_shifted + MeanAge + PercFem', data = ds, weights = ds$N)}
        else {model <- 'ERROR'}
        out <- broom::tidy(model, conf.int = T)
        out$N <- nobs(model)
        out$ds <- dsname
        out$model <- sprintf('%s, %s',adj, scaled)
        all_out_woutlier <- rbind(all_out_woutlier, out)
      }
    }
  }
}

t2_woutlier <- all_out_woutlier %>%
  mutate(effect = sprintf('%.2f (%.2f, %.2f)',estimate, conf.low, conf.high)) %>%
  dplyr::select(ds, model, term, effect) %>%
  pivot_wider(id_cols = c('ds','term'), names_from = model, values_from = effect) %>%
  dplyr::select(ds, term, `Model 1, Unscaled`, `Model 2, Unscaled`, `Model 1, Scaled`, `Model 2, Scaled`)
t2_woutlier %>% write.csv(paste0(out_dir, 'eTable3_with_outliers.csv'))

# Print string with Ns per model
model_Ns <-
  all_out_woutlier %>%
  distinct(ds, model, N) %>%
  separate_wider_delim(cols = 'model', delim = ', ', names = c('Model', 'Adjustment')) %>%
  dplyr::select(-Adjustment) %>%
  distinct() %>%
  separate_wider_delim(cols = 'ds', delim = '_', names = c('Scale','Time')) %>%
  mutate(Scale = str_replace_all(Scale, "long", "Long form"),
         Scale = str_replace_all(Scale, "short", "Short form"),
         Scale = str_replace_all(Scale, "combined", "Both forms"),
         Time = str_replace_all(Time, "early", "1992-2011"),
         Time = str_replace_all(Time, "late", "2012-2022"),
         Time = str_replace_all(Time, "all", "1992-2022"))
string_for_paper <- ''
for (rowi in 1:nrow(model_Ns)){
  row <- model_Ns[rowi,]
  row_string <- sprintf('%s, %s, %s: k = %i',row$Scale, row$Time, row$Model, row$N)
  # print(row_string)
  string_for_paper <- paste(string_for_paper, row_string, sep = '; ')
}

# Save to word
ft2_woutlier <- t2_woutlier %>%
  flextable() %>%
  bold(part = "header") %>%
  autofit() %>%                              # fit columns to content
  set_table_properties(layout = "autofit") %>%  # let table expand with content
  valign(val = "center")                 # vertical align cells to center
read_docx() %>%
  body_add_flextable(ft2_woutlier) %>%
  print(target = paste0(out_dir, 'eTable3_with_outliers.docx'))

