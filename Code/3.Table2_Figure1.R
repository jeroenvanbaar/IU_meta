# Table 2 - regression results - and figure 1
# Q: did IU increase over time in North-American college samples?

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
library(cowplot)
library(ggExtra)
library(here)

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

# Regression models
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
all_out <- data.frame()
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
        all_out <- rbind(all_out, out)
      }
    }
  }
}

# Table 2
t2 <- all_out %>%
  mutate(effect = sprintf('%.2f (%.2f, %.2f)',estimate, conf.low, conf.high)) %>%
  dplyr::select(ds, model, term, effect) %>%
  pivot_wider(id_cols = c('ds','term'), names_from = model, values_from = effect) %>%
  dplyr::select(ds, term, `Model 1, Unscaled`, `Model 2, Unscaled`, `Model 1, Scaled`, `Model 2, Scaled`)
t2 %>% write.csv(paste0(out_dir, 'Table2.csv'))

# Print string with Ns per model
model_Ns <-
  all_out %>%
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

# Save table 2 to Word
ft2 <- t2 %>%
  flextable() %>%
  bold(part = "header") %>%
  autofit() %>%                              # fit columns to content
  set_table_properties(layout = "autofit") %>%  # let table expand with content
  valign(val = "center")                 # vertical align cells to center

read_docx() %>%
  body_add_flextable(ft2) %>%
  print(target = paste0(out_dir, 'Table2.docx'))

# Figure 1 - combined
pal <- brewer.pal(5,'Dark2')
# A
fig1a <- ggplot(df %>% filter(Scale == 'long'),
                aes(x = Year, y = IUS_Mean, weight = N)) +
  geom_point(alpha = .5, aes(size = N), color = pal[1]) +
  geom_smooth(method = 'loess',color = pal[1], fill = pal[1]) +
  guides(size = "none") +
  scale_x_continuous(name = 'Year of data collection', breaks = seq(1995,2025,5), limits = c(1991.5, 2022.5)) +
  scale_y_continuous(name = 'Intolerance of uncertainty') +
  ggtitle('Long-form IUS') +
  theme_cowplot()
fig1a_marg <- ggMarginal(fig1a, type="histogram", margins = 'x', fill = pal[1], color = pal[1], alpha = .5, size = 10)
fig1b <- ggplot(df %>% filter(Scale == 'short'),
                aes(x = Year, y = IUS_Mean, weight = N)) +
  geom_point(alpha = .5, aes(size = N), color = pal[2]) +
  geom_smooth(method = 'loess',color = pal[2], fill = pal[2]) +
  guides(size = "none") +
  scale_x_continuous(name = 'Year of data collection', breaks = seq(1995,2025,5), limits = c(1991.5, 2022.5)) +
  scale_y_continuous(name = 'Intolerance of uncertainty') +
  ggtitle('Short-form IUS') +
  theme_cowplot()
fig1b_marg <- ggMarginal(fig1b, type="histogram", margins = 'x', fill = pal[2], color = pal[2], alpha = .5, size = 10)
top_row <- plot_grid(fig1a_marg, fig1b_marg, labels = 'AUTO', label_size = 16, align = 'hv', axis = 'l')
fig1c <- ggplot(df, aes(x = Year, y = IUS_Mean_SDunit, weight = N, group = Scale, color = Scale, fill = Scale)) +
  geom_point(alpha = .5, aes(size = N)) +
  geom_smooth(method = 'loess', aes(group = NA), color = 'gray20', fill = 'gray20') +
  geom_vline(xintercept = 2025, linetype = 'dotted') +
  guides(size = "none") +
  scale_fill_discrete(palette = pal, breaks = c('long','short')) +
  scale_color_discrete(palette = pal, breaks = c('long','short')) +
  scale_x_continuous(name = 'Year of data collection', breaks = seq(1995,2025,5), limits = c(1991.5, 2025.5)) +
  scale_y_continuous(name = 'Intolerance of uncertainty') +
  ggtitle('Combined sample (SD units)') +
  labs(fill = 'IUS form', color = 'IUS form') +
  theme_cowplot()
fig1c_marg <- ggMarginal(fig1c, type="histogram", margins = 'x',
                         groupColour = TRUE, groupFill = TRUE, size = 10)
plot_grid(top_row, fig1c_marg, labels = c('','C'), label_size = 16, nrow = 2, rel_heights = c(1,1.8))
ggsave(paste0(out_dir, 'Figure1.pdf'), width = 8, height = 10)

