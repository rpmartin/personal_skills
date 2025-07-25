
library(tidyverse)
library(ggplot2)
library(plotly)
library(janitor)
library(vroom)
library(here)
library(readxl)
library(tibble)
library(openxlsx)

# create list
eo <- list()

# Load skills data
skills_df <- read_excel(here("data","skills_data_for_career_profiles_2025-06-09.xlsx")) |>
  clean_names()|>
  rename(Occupation = noc2021_title, Skill = skills_competencies, Importance = importance_score) |>
  mutate(noc_5=str_pad(noc2021, 5, pad="0"), .before="noc2021")|>
  select(noc_5, Occupation, Skill, Importance )

teer_group_labels <- tribble(
  ~TEER, ~teer_group,
  "0", "Management",
  "1", "University",
  "2", "College/Apprenticeship",
  "3", "College/Apprenticeship",
  "4", "High School or less",
  "5", "High School or less"
)


# NOC groups

noc_major_map <- tribble(
  ~NOC_Major, ~NOC_Description,
  "0", "0 – Legislative and senior management",
  "1", "1 – Business, finance, administration",
  "2", "2 – Natural/applied sciences",
  "3", "3 – Health occupations",
  "4", "4 – Education, law, social, government",
  "5", "5 – Art, culture, recreation, sport",
  "6", "6 – Sales and service",
  "7", "7 – Trades, transport, equipment",
  "8", "8 – Natural resources, agriculture",
  "9", "9 – Manufacturing and utilities"
)


skill_clusters <- tribble(
  ~Skill, ~Skill_Group,
  "Active Learning", "Analytical",
  "Active Listening", "Analytical",
  "Complex Problem Solving", "Analytical",
  "Critical Thinking", "Analytical",
  "Instructing", "Analytical",
  "Judgment and Decision Making", "Analytical",
  "Learning Strategies", "Analytical",
  "Reading Comprehension", "Analytical",
  "Speaking", "Analytical",
  "Systems Analysis", "Analytical",
  "Systems Evaluation", "Analytical",
  "Writing", "Analytical",

  "Coordination", "Management",
  "Management of Financial Resources", "Management",
  "Management of Material Resources", "Management",
  "Management of Personnel Resources", "Management",
  "Monitoring", "Management",
  "Time Management", "Management",

  "Equipment Maintenance", "Technical",
  "Equipment Selection", "Technical",
  "Installation", "Technical",
  "Operation and Control", "Technical",
  "Operations Monitoring", "Technical",
  "Quality Control Analysis", "Technical",
  "Repairing", "Technical",
  "Troubleshooting", "Technical",

  "Mathematics", "STEM",
  "Operations Analysis", "STEM",
  "Programming", "STEM",
  "Science", "STEM",
  "Technology Design", "STEM",

  "Negotiation", "Social",
  "Persuasion", "Social",
  "Service Orientation", "Social",
  "Social Perceptiveness", "Social"
)

# Change skill cluster to a wide format (for export to MS Excel)
# Add a row number within each Skill_Group
skill_clusters_indexed <- skill_clusters %>%
  group_by(Skill_Group) %>%
  mutate(row = row_number()) %>%
  ungroup()

# Pivot to wide format using the row index
eo$skill_clusters_tbl <- skill_clusters_indexed %>%
  pivot_wider(
    names_from = Skill_Group,
    values_from = Skill
  ) %>%
  select(-row) 


# Merge NOC and TEER groups to skills data

skills_df <- skills_df %>%
  mutate(TEER = as.character(substr(noc_5, 2, 2)),
         NOC_Major = substr(as.character(noc_5), 1, 1)
  ) |>
  left_join(teer_group_labels, by = "TEER") |>
  left_join(noc_major_map, by = "NOC_Major") |>
  left_join(skill_clusters, by = "Skill")


# Labelling NA values

skills_df <- skills_df %>%
  mutate(Skill_Group = ifelse(is.na(Skill_Group), "Other", Skill_Group))


# Load Job Openings and group by occupation (total job openings)

job_openings_df <- read_excel(here("data","job_openings_occupation.xlsx"), skip=3)|>
  filter(NOC !="#T",
         `Geographic Area`=="British Columbia",
         Variable=="Job Openings")|>
  pivot_longer(cols=starts_with("2"),
               names_to="year",
               values_to="value")|>
  group_by(Occupation=Description) |>
  summarize(Openings_10yr = sum(value, na.rm = TRUE), .groups = "drop")|>
  arrange(Occupation)|>
  mutate(Occupation = str_replace_all(Occupation, "Seniors managers - public and private sector", "Senior managers - public and private sector"))

skills_jo_df <- skills_df |>
  left_join(job_openings_df, by = "Occupation")|>
  mutate(Openings_10yr = replace_na(Openings_10yr, 0))


# Data Analysis and Summary

# Step 1: Average importance per occupation-skill group (using first Openings_10yr per occupation)
cluster_scores <- skills_jo_df %>%
  group_by(Occupation, Skill_Group, teer_group) %>%
  summarise(
    average_importance = mean(Importance, na.rm = TRUE),
    Openings_10yr = first(Openings_10yr),
    .groups = "drop"
  ) |> 
  filter(!is.na(average_importance)) # Removing Trappers and Hunters with NA values

# Check quartile values
quantile(cluster_scores$average_importance, probs = c(0.25, 0.5, 0.75))

# Distribution charts of the raw and average importance skills

# Step 1: Combine data
combined_df <- bind_rows(
  skills_jo_df %>% mutate(type = "Individual Skill", score = Importance),
  cluster_scores %>% mutate(type = "Skill Cluster", score = average_importance)
)

# Step 2: Get percentiles in the Individual Skill distribution
percentile_lookup <- ecdf(skills_jo_df$Importance)
p25 <- percentile_lookup(25)
p50 <- percentile_lookup(50)

# Step 3: Map those percentiles to the Skill Cluster distribution
mapped_thresholds <- quantile(cluster_scores$average_importance, probs = c(p25, p50), na.rm = TRUE)
names(mapped_thresholds) <- c("Mapped_25", "Mapped_50")


# Step 4: Create a data frame for vertical lines and labels
vlines_df <- tibble(
  type = c("Individual Skill", "Individual Skill", "Skill Cluster", "Skill Cluster"),
  label = c("Score 25", "Score 50", 
            paste0("Mapped: ", round(mapped_thresholds[1], 1)), 
            paste0("Mapped: ", round(mapped_thresholds[2], 1))),
  x = c(25, 50, mapped_thresholds[1], mapped_thresholds[2]),
  linetype = c("25th", "50th", "25th", "50th")
)

# Step 5: Plot
plot_dist <- ggplot(combined_df, aes(x = score, fill = type)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = vlines_df, aes(xintercept = x, linetype = linetype, color = type), size = 0.8) +
  geom_text(
    data = vlines_df,
    aes(x = x, y = 0.002, label = label),  # Lowered y-position
    angle = 90, vjust = -0.5, hjust = 0,
    size = 3, color = "black", show.legend = FALSE
  ) +
  scale_linetype_manual(values = c("25th" = "dashed", "50th" = "dotted")) +
  labs(
    title = "Comparison of Skill Importance Distributions",
    subtitle = "Vertical lines show original thresholds (Individual Skill) and their mapped equivalents (Skill Cluster)",
    x = "Importance Score", y = "Density",
    fill = "Type", linetype = "Percentile", color = "Type"
  )


plot_dist


# Step 2: Categorize importance level
# Categorize skill cluster scores based on mapped thresholds
cluster_scores <- cluster_scores %>%
  mutate(
    Importance_Rank = case_when(
      average_importance < mapped_thresholds["Mapped_25"] ~ "Less Important",
      average_importance < mapped_thresholds["Mapped_50"] ~ "Moderately Important",
      TRUE ~ "Important"
    )
  )


# Step 5: Set fixed order for Skill_Group on the x-axis
fixed_order <- c("Technical","STEM","Management", "Social","Analytical" )
cluster_scores$Skill_Group <- factor(cluster_scores$Skill_Group, levels = fixed_order)

# Step 6: Set fixed order for Importance_Level in the legend
cluster_scores$Importance_Rank <- factor(cluster_scores$Importance_Rank, 
                                                        levels = c("Less Important","Moderately Important", "Important" ))

eo$by_teer <- cluster_scores|>
  group_by(teer_group, Skill_Group, Importance_Rank)|>
  summarise(
    Total_Openings = sum(Openings_10yr, na.rm = TRUE),
    Occupation_Count = n())|>
  group_by(teer_group, Skill_Group)|>
  mutate(
    Total_Openings_All = sum(Total_Openings),
    Percent_Openings = round((Total_Openings / Total_Openings_All * 100), 0)
  )

eo$all_teer <- cluster_scores|>
  group_by(Skill_Group, Importance_Rank)|>
  summarise(
    Total_Openings = sum(Openings_10yr, na.rm = TRUE),
    Occupation_Count = n())|>
  group_by(Skill_Group)|>
  mutate(
    Total_Openings_All = sum(Total_Openings),
    Percent_Openings = round((Total_Openings / Total_Openings_All * 100), 0)
  )

# Step 7: Plot (Stacked Percentage Bar Chart)
all_occs_chart <-  ggplot(eo$all_teer, aes(x = Percent_Openings , y = Skill_Group, fill = Importance_Rank, label = Percent_Openings)) +
  geom_col(position = "stack") +
  geom_text(aes(label = scales::percent(Percent_Openings / 100, accuracy = 1)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  # Keep desired legend order: Important → Moderate → Less
  scale_fill_manual(
    breaks = c("Important", "Moderately Important", "Less Important"),
    values = c(
      "Important" = "#2ca02c",
      "Moderately Important" = "#1f77b4",
      "Less Important" = "gray")) +
  labs(
    title =  "All Occupations: Percentage of Job Openings by Skill Cluster and Importance",
    x = "Percent of 10-Year Job Openings",
    y = "",
    fill = "Importance Rank"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

all_occs_chart

# Plot by Facet Wrap

upper_teer <- eo$by_teer |> filter(teer_group %in% c("Management", "University"))
lower_teer <- eo$by_teer |> filter(teer_group %in% c("College/Apprenticeship", "High School or less"))



plot_upper <- ggplot(upper_teer, aes(
  x = Percent_Openings,
  y = Skill_Group,
  fill = Importance_Rank
)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = scales::percent(Percent_Openings / 100, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    size = 3, color = "white"
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(
    breaks = c("Important", "Moderately Important", "Less Important"),
    values = c(
      "Important" = "#2ca02c",
      "Moderately Important" = "#1f77b4",
      "Less Important" = "gray"
    )
  ) +
  facet_wrap(~teer_group) +
  labs(
    title = "Management and Occupations Requiring University degree",
    x = "Percent of 10-Year Job Openings",
    y = "",
    fill = "Importance Rank"
  ) +
  theme_minimal()

plot_upper

plot_lower <- ggplot(lower_teer, aes(
  x = Percent_Openings,
  y = Skill_Group,
  fill = Importance_Rank
)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = scales::percent(Percent_Openings / 100, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    size = 3, color = "white"
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(
    breaks = c("Important", "Moderately Important", "Less Important"),
    values = c(
      "Important" = "#2ca02c",
      "Moderately Important" = "#1f77b4",
      "Less Important" = "gray"
    )
  ) +
  facet_wrap(~teer_group) +
  labs(
    title = "Occupations Requiring College/Apprenticeship and High School or less",
    x = "Percent of 10-Year Job Openings",
    y = "",
    fill = "Importance Rank"
  ) +
  theme_minimal()

plot_lower

# Exporing output plots in rds
write_rds(all_occs_chart, here("out", "all_occs_chart.rds"))
write_rds(plot_upper, here("out", "plot_upper.rds"))
write_rds(plot_lower, here("out", "plot_lower.rds"))
write_rds(plot_dist, here("out", "plot_dist.rds"))

# table export
eo |> write.xlsx(here::here("out", "skill_clusters_data.xlsx"), overwrite = TRUE)



