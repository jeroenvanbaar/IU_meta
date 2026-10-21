# Outlier removal

rm(list=ls())
library(tidyverse)
library(finalfit)
library(kableExtra)
library(officer)
library(flextable)
library(broom)
library(here)

# Load data
base_dir <- here('../')
print(base_dir)
in_dir <- paste0(base_dir, 'Data/')
out_dir <- paste0(base_dir, 'Results/')
df <- read_csv(paste0(in_dir, "final_dataset.csv")) %>%
  arrange(Source, YearPub, Study) %>%
  mutate(PercFem = PropFem*100,
         Year_shifted = Year - 1990,
         PubYear_shifted = YearPub - 1990)
df.original <- df
print(dim(df))

# Detect outliers
another.round = T
i = 1
while (another.round){
  df <- df %>%
    group_by(Scale) %>%
    mutate(
      fit = predict(lm(IUS_Mean ~ Year_shifted, weights = N)),
      resid = IUS_Mean - fit,
      z_resid = resid / sd(resid, na.rm = TRUE),
      outlier = abs(z_resid) > 3
    ) %>%
    ungroup()
  outliers <- df %>% filter(outlier) %>% dplyr::select(Study,Year,IUS_Mean)
  n.outliers <- nrow(outliers)
  inliers <- df %>% filter(!outlier)
  print(sprintf('Round %i, detected %i outliers: ',i,n.outliers))
  print(outliers)
  i <- i + 1
  another.round = n.outliers > 0
  df <- inliers
}

# Plot outliers
df$inlier <- T
df.out <- df.original %>% left_join(df)
df.out$inlier[is.na(df.out$inlier)] <- F
df.out$Outlier <- !df.out$inlier
ggplot(data = df.out, aes(x = Year, y = IUS_Mean, color = Outlier)) +
  geom_point(aes(size = N)) +
  facet_wrap(~Scale, nrow = 1, scales = 'free_y', labeller = labeller(Scale = c(long = 'Long-form IUS', short = 'Short-form IUS'))) +
  scale_y_continuous(name = 'Intolerance of uncertainty') +
  scale_color_discrete(palette = c('black','red'))
# Must create output directory first
ggsave(paste0(out_dir, 'FigureS1_outliers.pdf'), width = 10, height = 5)

# Stats
df.out %>%
  group_by(Outlier) %>%
  summarize(total_N = sum(N)) %>%
  mutate(pct = total_N/sum(total_N)*100)

# Save
write_csv(df, paste0(in_dir, "final_dataset_clean.csv"))
