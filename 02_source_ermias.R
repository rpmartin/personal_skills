
library(tidyverse)
library(ggplot2)
library(plotly)
library(janitor)
library(vroom)
library(here)
library(readxl)
library(tibble)


# Load skills data
skills_df <- read_excel(here("data","skills_data_for_career_profiles_2025-06-09.xlsx")) |>
  clean_names()|>
  rename(Occupation = noc2021_title, Skill = skills_competencies, Importance = importance_score) |>
  mutate(noc_5=str_pad(noc2021, 5, pad="0"), .before="noc2021")|>
  select(noc_5, Occupation, Skill, Importance )


# TEER groups

teer_labels <- tribble(
  ~TEER, ~TEER_Description,
  "0", "TEER 0 – Management occupations",
  "1", "TEER 1 – University degree",
  "2", "TEER 2 – College or apprenticeship (2+ years)",
  "3", "TEER 3 – College or apprenticeship (<2 years)",
  "4", "TEER 4 – Secondary school",
  "5", "TEER 5 – No formal education"
)

# Regroup TEER into four groups

teer_group_labels <- tribble(
  ~TEER, ~teer_group,
  "0", "TEER 0 – Management occupations",
  "1", "TEER 1 – University degree",
  "2", "TEER 2&3 – College or apprenticeship",
  "3", "TEER 2&3 – College or apprenticeship",
  "4", "TEER 4&5 – Secondary school or less",
  "5", "TEER 4&5 – Secondary school or less"
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
  left_join(job_openings_df, by = "Occupation") |>
  mutate(Openings_10yr = replace_na(Openings_10yr, 0))


# Data Analysis and Summary


# Step 1: Weighted importance per occupation-skill group (using first Openings_10yr per occupation)
cluster_scores <- skills_jo_df %>%
  group_by(Occupation, Skill_Group, teer_group) %>%
  summarise(
    weighted_importance = weighted.mean(Importance, Openings_10yr, na.rm = TRUE),
    Openings_10yr = first(Openings_10yr),
    .groups = "drop"
  )


# Step 2: Categorize importance level
cluster_scores <- cluster_scores %>%
  mutate(
    Importance_Rank = case_when(
      weighted_importance >= 50 ~ "Important",
      weighted_importance >= 25 ~ "Moderately Important",
      TRUE ~ "Less Important"
    )
  )




teer_table <- function(x = NULL) {
  y <- cluster_scores %>%
    { if (!is.null(x)) filter(., teer_group == x) else . } |>
    group_by(Skill_Group, Importance_Rank) |>
    summarise(
      Total_Openings = sum(Openings_10yr, na.rm = TRUE),
      Occupation_Count = n(),  # Or use n_distinct(Occupation)
      .groups = "drop"
    ) |>
    group_by(Skill_Group) |>
    mutate(
      Total_Openings_All = sum(Total_Openings),
      Percent_Openings = round((Total_Openings / Total_Openings_All * 100), 0)
    ) |>
    ungroup()

  # Fixed order for Skill_Group
  fixed_order <- c("Technical", "STEM", "Management", "Social", "Analytical")
  y$Skill_Group <- factor(y$Skill_Group, levels = fixed_order)

  # Fixed order for Importance_Rank
  y$Importance_Rank <- factor(
    y$Importance_Rank,
    levels = c("Less Important","Moderately Important", "Important")
  )

  return(y)
}


# Facet_wrap Option
# Create summaries and assign facet group
management  <- teer_table("TEER 0 – Management occupations") |> mutate(teer_group = "Management")
university  <- teer_table("TEER 1 – University degree") |> mutate(teer_group = "University")
college     <- teer_table("TEER 2&3 – College or apprenticeship") |> mutate(teer_group = "College/Apprenticeship")
highschool  <- teer_table("TEER 4&5 – Secondary school or less") |> mutate(teer_group = "High School or less")
all_teer <- bind_rows(management, university, college, highschool)
all_teer_nofilter <- teer_table()


# Step 7: Plot (Stacked Percentage Bar Chart)
all_occs_chart <-  ggplot(all_teer_nofilter, aes(x = Percent_Openings , y = Skill_Group, fill = Importance_Rank, label = Percent_Openings)) +
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

upper_teer <- all_teer |> filter(teer_group %in% c("Management", "University"))
lower_teer <- all_teer |> filter(teer_group %in% c("College/Apprenticeship", "High School or less"))



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

write_rds(all_occs_chart, here("out", "all_occs_chart.rds"))
write_rds(plot_upper, here("out", "plot_upper.rds"))
write_rds(plot_lower, here("out", "plot_lower.rds"))





