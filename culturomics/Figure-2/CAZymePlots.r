library(tidyr)
library(ggplot2)
#library(ggh4x)
library(dplyr)
library(RColorBrewer)
library(tidyselect)
library(tidyr)
library(stringr)
library(ggpubr)
#library(labdsv)
library(vegan)
library(readr)
library(ape)
library(lme4)
#library(lsmeans)
library(scales)
library(igraph)

# # isolate_cazyme_counts <- read.csv("../input/DOME-Isolates-Subject-Total-CAZyme-Count.csv")
# # isolate_cazyme_counts <- read.csv("MSA-DOME-Isolates-Subject-CAZyme-Count.csv")
# isolate_cazyme_counts <- read.csv("../input/DOME-Isolates-Subject-Mucin-CAZyme-Count.csv")
# 
# isolate_cazyme_counts <- isolate_cazyme_counts%>%
#   rename(Cohort = subject.type, Media = media, CAZyme.Count = cazyme.count)
# 
# head(isolate_cazyme_counts)
# 
# concatenated_fastani <- read.csv("..//input/DOME-Isolates-fastANI.csv")
# 
# head(concatenated_fastani)
# 
cohort_colors <- c("Cow" = "#990006", "Farmer" = "#386EC2", "Non-Farmer" = "#B5B5B2")
# 
# metadata <- read.csv("~/projects/Combined-DOME/Pre-DADA2-DOME-16S-Nasal-Metadata.csv")
# 
# metadata <- metadata %>%
#   dplyr::mutate(Sample.ID = stringr::str_remove(Sample.ID, "^19-"))
# 
# head(metadata)
# 
# isolate_cazyme_counts <- isolate_cazyme_counts %>%
#  left_join(metadata, by = c("Sample.ID", "Cohort"))
# 
# head(isolate_cazyme_counts)
# 
# isolate_cazyme_counts <- isolate_cazyme_counts %>%
# mutate(Genus = sub(" .*", "", classification))
# 
# head(isolate_cazyme_counts)
# 
# unique(isolate_cazyme_counts$Genus)
# 
# length(unique(isolate_cazyme_counts$Genus))
# 
# isolate_cazyme_counts_with_fastani <- isolate_cazyme_counts %>%
#   left_join(concatenated_fastani, by = c("dome.number" = "Isolate.1"))
# 
# head(isolate_cazyme_counts_with_fastani)
# 
# dim(isolate_cazyme_counts_with_fastani)
# 
# colnames(isolate_cazyme_counts_with_fastani)
# 
# dim(isolate_cazyme_counts_with_fastani %>% filter(dome.number != Isolate.2))
# 
# colnames(isolate_cazyme_counts_with_fastani)
# 
# # 1️⃣ Remove self-comparisons
# no_self <- isolate_cazyme_counts_with_fastani %>%
#   filter(dome.number != Isolate.2)
# 
# # 2️⃣ Keep only high ANI pairs (≥ 99.999%)
# high_ani_pairs <- no_self %>%
#   filter(ANI >= 99.999)
# 
# # 3️⃣ Initialize list for dereplicated isolates
# derep_list <- list()
# 
# # 4️⃣ Loop over each Sample.ID
# for(sample_id in unique(isolate_cazyme_counts_with_fastani$Sample.ID)) {
#   
#   # Isolates in this sample
#   sample_isolates <- isolate_cazyme_counts_with_fastani %>%
#     filter(Sample.ID == sample_id) %>%
#     pull(dome.number)
#   
#   # Edges for this sample
#   edges <- high_ani_pairs %>%
#     filter(Sample.ID == sample_id) %>%
#     select(dome.number, Isolate.2) %>%
#     distinct()
#   
#   # Cluster isolates if edges exist
#   if(nrow(edges) > 0){
#     g <- graph_from_edgelist(as.matrix(edges), directed = FALSE)
#     comps <- components(g)
#     
#     reps <- tibble(
#       isolate = names(comps$membership),
#       cluster = comps$membership
#     ) %>%
#       group_by(cluster) %>%
#       slice(1) %>%  # pick one representative per cluster
#       pull(isolate)
#   } else {
#     reps <- character(0)
#   }
#   
#   # Add singletons (isolates with no edges)
#   singletons <- setdiff(sample_isolates, c(edges$dome.number, edges$Isolate.2))
#   
#   # Combine cluster reps + singletons
#   derep_list[[sample_id]] <- c(reps, singletons)
# }
# 
# # 5️⃣ Combine and ensure Dome.Number is unique
# dereplicated_isolates <- isolate_cazyme_counts %>%
#   filter(dome.number %in% unlist(derep_list)) %>%
#   distinct(dome.number, .keep_all = TRUE)  # ensures one row per Dome.Number
# 
# dim(dereplicated_isolates)
# 
# head(dereplicated_isolates)
# 
# dereplicated_isolates
# 
# length(unique(dereplicated_isolates$dome.number))
# 
# table(dereplicated_isolates$classification)
# 
# table(dereplicated_isolates$Media)
# 
# # write.csv(arrange(dereplicated_isolates, dome.number), "..//output/DOME-Dereplicated-Isolates-Total-CAZymes.csv", row.names = FALSE)
# 
# # write.csv(arrange(dereplicated_isolates, dome.number), "..//output/DOME-Dereplicated-Isolates-Mucin-CAZymes.csv", row.names = FALSE)
# 
comparison_groups = list(c("Cow", "Farmer"),c("Non-Farmer", "Farmer"), c("Cow", "Non-Farmer") )

#total CAZymes
total_cazymes_dereplicated_isolates <- read.csv("/Users/gdlab/Library/CloudStorage/Box-Box/2024_Dome2.0/Manuscript/Metadata & Data/Figures/251207_SriFixCAZymeJitter_Figure2Band2BSupp/Figure-2B-Supplement/DOME-Dereplicated-Isolates-Total-CAZymes.csv")
msa_total_cazymes_dereplicated <- total_cazymes_dereplicated_isolates %>% filter(Media == "MSA")

msa_total_cazyme_only_plot <- ggplot(msa_total_cazymes_dereplicated, aes(x = Cohort, y = CAZyme.Count, fill = Cohort)) +
  geom_boxplot(outlier.size = 1, outliers = FALSE) +
  geom_jitter(aes(fill = Cohort), width = 0.25, size = 0.50, height = 0) +
  # facet_wrap(~ Media, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = cohort_colors) +
  stat_compare_means(
    method = "wilcox.test",
    comparisons = comparison_groups,
    aes(group = Cohort.Label),
    label = "p.signif"
  ) +
  theme_bw() +
  guides(fill = "none", shape = "none") +
  theme(
    panel.grid = element_blank(),
    # strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white", face = "bold", size = 10),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_blank(),
    legend = NULL,
    text = element_text(family = "Helvetica", size = 12)
  ) +
  ylab("Total CAZyme Count (All Types)")

#ggplot2::ggsave("/Users/gdlab/Library/CloudStorage/Box-Box/2024_Dome2.0/Manuscript/Metadata & Data/Figures/251207_SriFixCAZymeJitter_Figure2Band2BSupp/SriJitterFix/251207_DOME-Isolate-Total-CAZymes-MSA.pdf", msa_total_cazyme_only_plot, width = 5, height = 6, dpi = 320, unit = "in")

# head(msa_total_cazyme_only_plot)
# 
# msa_total_cazyme_wilcox_results <- compare_means(
#   CAZyme.Count ~ Cohort,
#   data = msa_total_cazymes_dereplicated,
#   method = "wilcox.test",
#   comparisons = comparison_groups,
#   p.adjust.method = "BH"
# )
# 
# msa_total_cazyme_wilcox_results <- msa_total_cazyme_wilcox_results %>%
# select(group1, group2, p.adj) %>%
# rename(
#     Group1 = group1,
#     Group2 = group2
# )
# 
# msa_total_cazyme_wilcox_results
# 
# write.csv(msa_total_cazyme_wilcox_results, "..//output/DOME-MSA-Total-CAZyme-Cohort-Wilcoxon-Results.csv", row.names = FALSE)
# 

#Just Mucin CAZymes
dereplicated_isolates<- read.csv("/Users/gdlab/Library/CloudStorage/Box-Box/2024_Dome2.0/Manuscript/Metadata & Data/Figures/251207_SriFixCAZymeJitter_Figure2Band2BSupp/Figure-2B/DOME-Dereplicated-Isolates-Mucin-CAZymes.csv")

msa_only_dereplicated_isolates <- dereplicated_isolates %>%
    filter(Media == "MSA")
mucin_only_dereplicated_isolates <- dereplicated_isolates %>%
    filter(Media == "Mucin")

# msa_media_mucin_cazyme_wilcox_results <- compare_means(
#   CAZyme.Count ~ Cohort,
#   data = msa_only_dereplicated_isolates,
#   method = "wilcox.test",
#   comparisons = comparison_groups,
#   p.adjust.method = "BH"
# )
# 
# msa_media_mucin_cazyme_wilcox_results <- msa_media_mucin_cazyme_wilcox_results %>%
# select(group1, group2, p.adj) %>%
# rename(
#     Group1 = group1,
#     Group2 = group2
# )
# 
# msa_media_mucin_cazyme_wilcox_results
# 
# mucin_media_mucin_cazyme_wilcox_results <- compare_means(
#   CAZyme.Count ~ Cohort,
#   data = mucin_only_dereplicated_isolates,
#   method = "wilcox.test",
#   comparisons = comparison_groups,
#   p.adjust.method = "BH"
# )
# 
# mucin_media_mucin_cazyme_wilcox_results <- mucin_media_mucin_cazyme_wilcox_results %>%
# select(group1, group2, p.adj) %>%
# rename(
#     Group1 = group1,
#     Group2 = group2
# )
# 
# mucin_media_mucin_cazyme_wilcox_results
# 
# write.csv(msa_media_mucin_cazyme_wilcox_results, "..//output/DOME-MSA-Media-Mucin-CAZymes-Cohort-Wilcoxon-Results.csv", row.names = FALSE)
# write.csv(mucin_media_mucin_cazyme_wilcox_results, "..//output/DOME-Mucin-Media-Mucin-CAZymes-Cohort-Wilcoxon-Results.csv", row.names = FALSE)

# combined_cazyme_plot <- ggplot(dereplicated_isolates, aes(x = Cohort, y = CAZyme.Count, fill = Cohort)) +
#   geom_boxplot(outlier.size = 1, outliers = FALSE) +
#   geom_jitter(aes(fill = Cohort), width = 0.25, size = 0.50) +
#   facet_wrap(~ Media, ncol = 2, scales = "free_y") +
#   scale_fill_manual(values = cohort_colors) +
#   stat_compare_means(
#     method = "wilcox.test",
#     comparisons = comparison_groups,
#     aes(group = Cohort.Label),
#     label = "p.signif"
#   ) +
#   theme_bw() +
#   guides(fill = "none", shape = "none") +
#   theme(
#     panel.grid = element_blank(),
#     strip.background = element_rect(fill = "black"),
#     strip.text = element_text(color = "white", face = "bold", size = 10),
#     axis.text.x = element_text(size = 8),
#     axis.title.x = element_blank(),
#     legend = NULL,
#     text = element_text(family = "Helvetica", size = 12)
#   ) +
#   ylab("Total Number of CAZymes")

# ggplot2::ggsave("Final-Figures/DOME-Isolate-Total-CAZymes-By-Media-Type.pdf", combined_cazyme_plot, width = 8, height = 6, dpi = 320, unit = "in")

msa_cazyme_plot <- ggplot(msa_only_dereplicated_isolates, aes(x = Cohort, y = CAZyme.Count, fill = Cohort)) +
  geom_boxplot(outlier.size = 1, outliers = FALSE) +
  geom_jitter(aes(fill = Cohort), width = 0.25, size = 0.50, height = 0) +
  # facet_wrap(~ Media, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = cohort_colors) +
  stat_compare_means(
    method = "wilcox.test",
    comparisons = comparison_groups,
    aes(group = Cohort.Label),
    label = "p.signif"
  ) +
  theme_bw() +
  guides(fill = "none", shape = "none") +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white", face = "bold", size = 10),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_blank(),
    legend = NULL,
    text = element_text(family = "Helvetica", size = 12)
  ) +
  ylab("Number of Mucin CAZymes")

mucin_cazyme_plot <- ggplot(mucin_only_dereplicated_isolates, aes(x = Cohort, y = CAZyme.Count, fill = Cohort)) +
  geom_boxplot(outlier.size = 1, outliers = FALSE) +
  geom_jitter(aes(fill = Cohort), width = 0.25, size = 0.50, height = 0) +
  # facet_wrap(~ Media, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = cohort_colors) +
  stat_compare_means(
    method = "wilcox.test",
    comparisons = comparison_groups,
    aes(group = Cohort.Label),
    label = "p.signif"
  ) +
  theme_bw() +
  guides(fill = "none", shape = "none") +
  theme(
    panel.grid = element_blank(),
    # strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white", face = "bold", size = 10),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_blank(),
    legend = NULL,
    text = element_text(family = "Helvetica", size = 12)
  ) +
  ylab("Number of Mucin CAZymes")

#ggplot2::ggsave("/Users/gdlab/Library/CloudStorage/Box-Box/2024_Dome2.0/Manuscript/Metadata & Data/Figures/251207_SriFixCAZymeJitter_Figure2Band2BSupp/SriJitterFix/251207_DOME-Isolate-Mucin-CAZymes-MSA.pdf", msa_cazyme_plot, width = 5, height = 6, dpi = 320, unit = "in")

#ggplot2::ggsave("/Users/gdlab/Library/CloudStorage/Box-Box/2024_Dome2.0/Manuscript/Metadata & Data/Figures/251207_SriFixCAZymeJitter_Figure2Band2BSupp/SriJitterFix/251207_DOME-Isolate-Mucin-CAZymes-Mucin.pdf", mucin_cazyme_plot, width = 5, height = 6, dpi = 320, unit = "in")

# # # 0. remove self comparisons
# # no_self <- isolate_cazyme_counts_with_fastani %>%
# #   filter(dome.number != Isolate.2) %>%
# #   filter(Media == "MSA")
# 
# # 1. normalize cohort labels (trim + title case) and recode Office Worker -> Non-Farmer
# no_self <- no_self %>%
#   mutate(
#     Subject.Type.1 = str_to_title(trimws(Subject.Type.1)),
#     Subject.Type.2 = str_to_title(trimws(Subject.Type.2)),
#     Subject.Type.1 = if_else(Subject.Type.1 == "Office Worker", "Non-Farmer", Subject.Type.1),
#     Subject.Type.2 = if_else(Subject.Type.2 == "Office Worker", "Non-Farmer", Subject.Type.2)
#   )
# 
# # 2. detect inconsistent reciprocals (OPTIONAL but recommended)
# # Create a canonical pair key and then find groups with >1 unique ANI or inconsistent subject-type combos
# no_self_check <- no_self %>%
#   rowwise() %>%
#   mutate(pair_id = paste(sort(c(dome.number, Isolate.2)), collapse = "_")) %>%
#   ungroup()
# 
# inconsistencies <- no_self_check %>%
#   group_by(pair_id) %>%
#   summarize(
#     n_rows = n(),
#     n_unique_ani = n_distinct(round(ANI, 5)),        # tiny tolerance
#     n_subjcombos = n_distinct(paste(Subject.Type.1, Subject.Type.2, sep = "|"))
#   ) %>%
#   filter(n_rows > 1 & (n_unique_ani > 1 | n_subjcombos > 1))
# 
# # inspect `inconsistencies` — if it's empty, safe to dedupe. If not, fix those pairs manually.
# 
# # 3. collapse reciprocal pairs (keep first occurrence)
# unique_pairs <- no_self_check %>%
#   distinct(pair_id, .keep_all = TRUE)    # keeps the first row for each unordered pair
# 
# # 4. now define ComparisonGroup (Cow vs Farmer/Non-Farmer)
# ani_filtered <- unique_pairs %>%
#   mutate(
#     ComparisonGroup = case_when(
#       (Subject.Type.1 == "Cow" & Subject.Type.2 == "Farmer") |
#         (Subject.Type.2 == "Cow" & Subject.Type.1 == "Farmer") ~ "Farmer",
#       (Subject.Type.1 == "Cow" & Subject.Type.2 == "Non-Farmer") |
#         (Subject.Type.2 == "Cow" & Subject.Type.1 == "Non-Farmer") ~ "Non-Farmer",
#       TRUE ~ NA_character_
#     )
#   ) %>%
#   filter(!is.na(ComparisonGroup))
# 
# # quick counts to confirm
# count_by_group <- ani_filtered %>% count(ComparisonGroup)
# print(count_by_group)
# 
# table(unique_pairs$Subject.Type.1)
# table(unique_pairs$Subject.Type.2)
# 
# all_subjects <- c(unique_fastani$Isolate.1, unique_fastani$Isolate.2)
# all_types <- c(unique_fastani$Subject.Type.1, unique_fastani$Subject.Type.2)
# table(all_types)
# 
# ani_filtered %>%
#   group_by(ComparisonGroup) %>%
#   summarize(
#     unique_cows = n_distinct(if_else(Subject.Type.1 == "Cow", Isolate.1, Isolate.2)),
#     total_pairs = n()
#   )
# 
# head(ani_filtered)
# 
# dim(ani_filtered)
# 
# 
# 
# cow_summary <- ani_filtered %>%
#   rowwise() %>%
#   mutate(CowID = if_else(Subject.Type.1 == "Cow", dome.number, Isolate.2)) %>%
#   ungroup() %>%
#   group_by(CowID, ComparisonGroup) %>%
#   summarize(
#     median_ANI = median(ANI),
#     .groups = "drop"
#   )
# 
# head(cow_summary)
# 
# table(cow_summary$ComparisonGroup)
# 
# # 1️⃣ Remove self-comparisons
# # no_self <- isolate_cazyme_counts_with_fastani %>%
# #   filter(dome.number != Isolate.2) %>%
# 
# no_self <- isolate_cazyme_counts_with_fastani %>%
#   filter(dome.number != Isolate.2)
#   # filter(str_detect(classification, "staph"))
# 
# 
# # 2️⃣ Standardize cohort labels
# no_self <- no_self %>%
#   mutate(
#     Subject.Type.1 = str_to_title(trimws(Subject.Type.1)),
#     Subject.Type.2 = str_to_title(trimws(Subject.Type.2)),
#     Subject.Type.1 = if_else(Subject.Type.1 == "Office Worker", "Non-Farmer", Subject.Type.1),
#     Subject.Type.2 = if_else(Subject.Type.2 == "Office Worker", "Non-Farmer", Subject.Type.2)
#   )
# 
# # 3️⃣ Deduplicate reciprocal pairs
# no_self <- no_self %>%
#   rowwise() %>%
#   mutate(pair_id = paste(sort(c(dome.number, Isolate.2)), collapse = "_")) %>%
#   ungroup() %>%
#   distinct(pair_id, .keep_all = TRUE)
# 
# # 4️⃣ Define ComparisonGroup
# ani_filtered <- no_self %>%
#   mutate(
#     ComparisonGroup = case_when(
#       (Subject.Type.1 == "Cow" & Subject.Type.2 == "Farmer") |
#         (Subject.Type.2 == "Cow" & Subject.Type.1 == "Farmer") ~ "Farmer",
#       (Subject.Type.1 == "Cow" & Subject.Type.2 == "Non-Farmer") |
#         (Subject.Type.2 == "Cow" & Subject.Type.1 == "Non-Farmer") ~ "Non-Farmer",
#       TRUE ~ NA_character_
#     )
#   ) %>%
#   filter(!is.na(ComparisonGroup))
# 
# # 5️⃣ Identify cow isolate
# cow_summary <- ani_filtered %>%
#   rowwise() %>%
#   mutate(CowID = if_else(Subject.Type.1 == "Cow", dome.number, Isolate.2)) %>%
#   ungroup() %>%
#   group_by(CowID, ComparisonGroup) %>%
#   summarize(
#     median_ANI = median(ANI),
#     n_pairs = n(),      # optional: number of comparisons contributing
#     .groups = "drop"
#   )
# 
# # 6️⃣ Restrict to cows that have BOTH groups
# cow_summary_balanced <- cow_summary %>%
#   group_by(CowID) %>%
#   filter(n_distinct(ComparisonGroup) == 2) %>%
#   ungroup()
# 
# head(cow_summary_balanced)
# 
# table(cow_summary_balanced$ComparisonGroup)
# 
# balanced_ani <- ggplot(cow_summary_balanced, aes(x = ComparisonGroup, y = median_ANI, fill = ComparisonGroup)) +
#   geom_boxplot(alpha = 0.8, width = 0.6, outlier.shape = NA) +
#   stat_compare_means(comparisons = list(c("Farmer", "Non-Farmer")), method = "wilcox.test", label = "p.signif") +
#   scale_fill_manual(values = c("Farmer" = "#4DBBD5", "Non-Farmer" = "#E64B35")) +
#   labs(
#     y = "Average Nucleotide Identity (ANI)",
#   ) +
#   theme_bw() +
#   theme(
#     panel.grid = element_blank(),               # remove gridlines
#     strip.background = element_rect(fill = "black"),
#     strip.text = element_text(color = "white", face = "bold", size = 10),
#     axis.text.x = element_text(size = 8),   # rotate x labels
#     axis.title.x = element_blank(),
#     legend = NULL
#   )
# 
# 
# ggplot2::ggsave(filename = "Final-Figures/Balanced-ANI-Pairwise-Comparison.pdf", balanced_ani, width = 6, height = 8, units = "in")
# 
# msa_staph_ani_pairwise_plot <- ggplot(ani_filtered, aes(x = ComparisonGroup, y = ANI, fill = ComparisonGroup)) +
#   geom_boxplot(alpha = 0.8, width = 0.6, outlier.shape = NA) +
#   stat_compare_means(comparisons = list(c("Farmer", "Non-Farmer")), method = "wilcox.test", label = "p.signif") +
#   scale_fill_manual(values = c("Farmer" = "#4DBBD5", "Non-Farmer" = "#E64B35")) +
#   labs(
#     y = "Average Nucleotide Identity (ANI)",
#   ) +
#   theme_bw() +
#   theme(
#     panel.grid = element_blank(),               # remove gridlines
#     strip.background = element_rect(fill = "black"),
#     strip.text = element_text(color = "white", face = "bold", size = 10),
#     axis.text.x = element_text(size = 8),   # rotate x labels
#     axis.title.x = element_blank(),
#     legend = NULL
#   )
# 
# 
# ggplot2::ggsave(filename = "Final-Figures/MSA-Staph-Mock-ANI-Pairwise-Comparison.pdf", msa_ani_pairwise_plot, width = 6, height = 8, units = "in")
# 
# head(isolate_cazyme_counts_with_fastani)


