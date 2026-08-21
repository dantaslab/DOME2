
# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggpubr)


# Load data --------------------------------------------------------------------

metadata.df <- read.table(file="data/DOME-16S-Nasal-Metadata.csv", header=TRUE, sep=",")

cohort.pal <- c(
  "Cow" = "#763a28",
  "Farmer" = "#26607f",
  "Non-Farmer" = "#5c5d59"
)

# 1. Sampling timeline figure -----------------------------------------------------

### Tidy data

df <- metadata.df %>%
  mutate(subjectID = paste0(Subject,"-",Site)) 

df$Collection.Date <- as.Date(df$Collection.Date, "%Y-%m-%d")

# Sort y-axis by first sample collection date
df_sorted <- df %>%
  ungroup() %>%
  mutate(subjectID = fct_reorder(subjectID, Collection.Date, min))

# # Split by Cohort for shorter plots
# df_Cow <- df %>%
#   ungroup() %>%
#   filter(Cohort == "Cow") %>%
#   mutate(subjectID = fct_reorder(subjectID, Collection.Date, min))
# 
# df_Farmer <- df %>%
#   ungroup() %>%
#   filter(Cohort == "Farmer") %>%
#   mutate(subjectID = fct_reorder(subjectID, Collection.Date, min))
# 
# df_Nonfarmer <- df %>%
#   ungroup() %>%
#   filter(Cohort == "Non-Farmer") %>%
#   mutate(subjectID = fct_reorder(subjectID, Collection.Date, min))

# Metadata summary
df_summary <- df %>%
  ungroup() %>%
  group_by(Cohort) %>%
  summarize(subjects = length(unique(subjectID)))

### Plot figure

# All in one long plot
p1 <- ggplot(df_sorted, aes(x=Collection.Date, y=subjectID, color=Cohort, group=subjectID)) +
  geom_point(size=1) +
  geom_line(size=0.5) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust =1),
    axis.ticks.y = element_blank(),
    legend.position = "right"
  ) +
  #facet_wrap(~ Cohort) +
  scale_color_manual(values=cohort.pal) +
  labs(
    x="Collection Date",
    y="Subject") +
  scale_x_date(date_labels = "%b-%Y",
               date_breaks = "3 months", limits = as.Date(c("2019-02-28", "2022-05-31")))


ggsave(p1, filename="reports/p1a_timeline_all_v1.pdf", height=22, width=6)

# # Split into separate, smaller plots (could not do facet_wrap because leaves blanks in the y-axis, defeating the whole purpose of doing this)
# p2a <- ggplot(df_Cow, aes(x=Collection.Date, y=subjectID, color=Cohort, group=subjectID)) +
#   geom_point(size=1) +
#   geom_line(size=0.5) +
#   theme_bw() +
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.text.x = element_text(angle = 45, hjust =1),
#     axis.ticks.y = element_blank(),
#     legend.position = "none"
#   ) +
#   scale_color_manual(values=cohort.pal) +
#   scale_x_date(date_labels = "%b-%Y",
#                date_breaks = "3 months", limits = as.Date(c("2019-02-28", "2022-05-31")))
# 
# p2b <- ggplot(df_Farmer, aes(x=Collection.Date, y=subjectID, color=Cohort, group=subjectID)) +
#   geom_point(size=1) +
#   geom_line(size=0.5) +
#   theme_bw() +
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.text.x = element_text(angle = 45, hjust =1),
#     axis.ticks.y = element_blank(),
#     legend.position = "none"
#   ) +
#   scale_color_manual(values=cohort.pal) +
#   scale_x_date(date_labels = "%b-%Y",
#                date_breaks = "3 months", limits = as.Date(c("2019-02-28", "2022-05-31")))
# 
# p2c <- ggplot(df_Nonfarmer, aes(x=Collection.Date, y=subjectID, color=Cohort, group=subjectID)) +
#   geom_point(size=1) +
#   geom_line(size=0.5) +
#   theme_bw() +
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.text.x = element_text(angle = 45, hjust =1),
#     axis.ticks.y = element_blank(),
#     legend.position = "none"
#   ) +
#   scale_color_manual(values=cohort.pal) +
#   scale_x_date(date_labels = "%b-%Y",
#                date_breaks = "3 months", limits = as.Date(c("2019-02-28", "2022-05-31")))
# 
# 
# # Export individual plots:
# ggsave(p2a, filename="reports/p2a_timeline_v1.pdf", height=13, width=4)
# ggsave(p2b, filename="reports/p2b_timeline_v1.pdf", height=4, width=4)
# ggsave(p2c, filename="reports/p2c_timeline_v1.pdf", height=4, width=4)
