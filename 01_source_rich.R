library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(openxlsx)
library(conflicted)
conflicts_prefer(dplyr::filter)
source(here("R","functions.R"))
ro <- list() #all of the objects created by richs code... to avoid namespace collisions

ro$hoo <- read_excel(here("data","high-opportunity-occupations-bc-and-regions.xlsx"),
                  sheet = "HOO BC")|>
  select(noc_5=NOC)|>
  mutate(noc_5=str_remove_all(noc_5, "#"))

ro$skills <- read_excel(here("data","skills_data_for_career_profiles_2025-06-09.xlsx"))|>
  clean_names()|>
  mutate(noc_5=str_pad(noc2021, 5, pad="0"), .before="noc2021")|>
  select(noc_5, skills_competencies, importance_score)|>
  group_by(skills_competencies)|>
  nest(score_by_noc = c(noc_5, importance_score))

ro$hist_emp <- read_excel(here("data",
                            "Labour force status for 5 digit NOC (41229 split)2015-2024.xlsx"),
                       skip=3,
                       sheet="Employed")|>
  pivot_longer(cols=starts_with("2"),
               names_to="year",
               values_to="employed")|>
  clean_names()|>
  filter(noc_5!="Total")|>
  mutate(year=as.numeric(year),
         noc_5=if_else(str_detect(noc_5, "^0001[1-5]$"), "00018", noc_5),
         class_title=if_else(str_detect(noc_5, "00018"), "Senior managers - public and private sector", class_title),
         source="LFS")|>
  group_by(noc_5, class_title, year, source)|>
  summarize(employed=sum(employed, na.rm=TRUE))

ro$future_emp <- read_excel(here("data","employment_occupation.xlsx"), skip=3)|>
  filter(NOC !="#T",
         `Geographic Area`=="British Columbia")|>
  pivot_longer(cols=starts_with("2"),
               names_to="year",
               values_to="employed")|>
  clean_names()|>
  mutate(year=as.numeric(year),
         noc_5=str_remove_all(noc, "#"),
         source="LMO")|>
  select(noc_5, class_title=description, year, source, employed)

ro$emp <- bind_rows(ro$hist_emp, ro$future_emp)

ro$emp_prop <- ro$emp|>
  group_by(year, source)|>
  mutate(prop= employed/sum(employed, na.rm=TRUE))|>
  select(-employed)|>
  nest(prop_by_noc=c(noc_5, class_title,  prop))

ro$joined <- crossing(ro$skills, ro$emp_prop)|>
  mutate(joined=map2(score_by_noc, prop_by_noc, inner_join))

ro$crossed <- ro$joined|>
  mutate(weighted_average=map_dbl(joined, rich_weighted_average))|>
  select(skills_competencies, year, source, weighted_average)|>
  group_by(skills_competencies, source)|>
  nest()|>
  mutate(cagr= map_dbl(data, rich_cagr),
         last= map_dbl(data, rich_last)
  )|>
  select(-data)

#weighted means over time----------------------------------

ro$emp_teer <- ro$emp|>
  mutate(teer=str_sub(noc_5,2,2),
         teer=case_when(
           teer %in% c("0", "1") ~ "TEER 0&1",
           teer %in% c("2", "3") ~ "TEER 2&3",
           teer %in% c("4", "5") ~ "TEER 4&5",
           TRUE ~ "error"))|> #collapse teer to 3 groups 0&1, 2&3, 4&5
  group_by(year, source, teer)|>
  summarise(employed=sum(employed, na.rm=TRUE))|>
  group_by(teer, source)|>
  nest()|>
  mutate(cagr = map_dbl(data, ~ {
    df <- .x
    start_value <- df$employed[1]
    end_value <- df$employed[nrow(df)]
    years <- nrow(df) - 1
    cagr_value <- ((end_value / start_value)^(1 / years)) - 1
    return(cagr_value)
  })) |>
  unnest(data)

ro$skills_unnested <- ro$skills|>
  unnest(score_by_noc)

ro$not_hoo <- ro$skills_unnested|>
  ungroup()|>
  select(noc_5)|>
  distinct()|>
  anti_join(ro$hoo)

ro$most_recent_emp <- ro$hist_emp|>
  ungroup()|>
  filter(year==max(year))

ro$hoo_weights <- ro$most_recent_emp|>
  filter(noc_5 %in% ro$hoo$noc_5)|>
  mutate(prop=employed/sum(employed, na.rm=TRUE))|>
  select(noc_5, prop)

ro$not_hoo_weights <- ro$most_recent_emp|>
  filter(noc_5 %in% ro$not_hoo$noc_5)|>
  mutate(prop=employed/sum(employed, na.rm=TRUE))|>
  select(noc_5, prop)

ro$hoo_skills <- ro$skills_unnested|>
  right_join(ro$hoo_weights, by="noc_5")|>
  group_by(skills_competencies)|>
  summarize(weighted_average = sum(importance_score * prop, na.rm = TRUE))|>
  mutate(group = "High Opportunity Occupations")|>
  na.omit()

ro$not_hoo_skills <- ro$skills_unnested|>
  right_join(ro$not_hoo_weights, by="noc_5")|>
  group_by(skills_competencies)|>
  summarize(weighted_average = sum(importance_score * prop, na.rm = TRUE))|>
  mutate(group = "Other Occupations")

ro$skills <- bind_rows(ro$hoo_skills, ro$not_hoo_skills)

ro$diffs <- ro$skills|>
  pivot_wider(names_from=group, values_from=weighted_average)|>
  mutate(absolute_diff = `High Opportunity Occupations` - `Other Occupations`,
         relative_diff= `High Opportunity Occupations` / `Other Occupations`) |>
  arrange(desc(relative_diff))

#the plots-----------------------------------

ro$cagr01 <- scales::percent(ro$emp_teer$cagr[ro$emp_teer$teer=="TEER 0&1" & ro$emp_teer$source=="LFS"][1], accuracy=0.1)
ro$cagr23 <- scales::percent(ro$emp_teer$cagr[ro$emp_teer$teer=="TEER 2&3" & ro$emp_teer$source=="LFS"][1], accuracy=0.1)
ro$cagr45 <- scales::percent(ro$emp_teer$cagr[ro$emp_teer$teer=="TEER 4&5" & ro$emp_teer$source=="LFS"][1], accuracy=0.1)

ro$emp_teer_plt <- ggplot(ro$emp_teer, aes(x=year, y=employed, fill=teer)) +
  geom_area(alpha=.5) +
  geom_vline(xintercept = 2024.5) +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(labels=scales::comma_format())+
  theme_minimal()+
  labs(title="British Columbia Employment by TEER category",
       subtitle=paste0("Historically, TEERs 0&1 growing rapidly (", ro$cagr01, ") TEERs 2&3 constant share (",ro$cagr23,"), TEERs 4&5 no growth (",ro$cagr45,")\nForecast: ordinal ranking of growth rates the same, but muted in magnitude."),
       fill=NULL,
       x=NULL,
       y=NULL)

ro$crossed_plt1 <- ggplot(ro$crossed|>filter(source=="LFS"), aes(x=last, y=fct_reorder(skills_competencies, last))) +
  geom_col(alpha=.5)+
  labs(title="Weighted averages skills:",
       subtitle="Current (2024) employment proportions for weights.",
       x=NULL,
       y=NULL,
       caption = "Source: Skills data (ONET), Employment data (LFS)")+
  theme_minimal()+
  theme(text=element_text(size=12))

ro$crossed_plt2 <- ggplot(ro$crossed|>filter(source=="LFS"), aes(x=cagr, y=fct_reorder(skills_competencies, cagr))) +
  geom_col(alpha=.5)+
  labs(title="Historic growth (2015-2024) in weighted average skills",
       x=NULL,
       y=NULL,
       caption = "Source: Skills data (ONET), Employment data (LFS)") +
  scale_x_continuous(labels=scales::percent_format(accuracy=0.1))+
  theme_minimal()+
  theme(text=element_text(size=12))

ro$crossed_plt3 <- ggplot(ro$crossed|>filter(source=="LMO"), aes(x=cagr, y=fct_reorder(skills_competencies, cagr))) +
  geom_col(alpha=.5)+
  labs(title="Future growth (2025-2035) in weighted average skills",
       x=NULL,
       y=NULL,
       caption = "Source: Skills data (ONET), Employment data (LMO)") +
  scale_x_continuous(labels=scales::percent_format(accuracy=0.1))+
  theme_minimal()+
  theme(text=element_text(size=12))

ro$skills_plt <- ro$skills|>
  ggplot(aes(x=weighted_average, y=fct_reorder(skills_competencies, weighted_average), fill=group)) +
  geom_col(position="dodge", alpha=.5, width=.7) +
  labs(title="Comparison between weighted average skill scores:",
       subtitle="High opportunity occupations vs. other occupations",
       x=NULL,
       y=NULL,
       caption = "Source: Skills data (ONET), Employment data (LMO 2025) ") +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(text=element_text(size=12)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.box = "horizontal")

ro$diffs_plt1 <- ggplot(ro$diffs, aes(x=relative_diff, y=fct_reorder(skills_competencies, relative_diff))) +
  geom_vline(xintercept=1, color="grey70", lwd=.5)+
  geom_col(alpha=.5)+
  labs(title="Relative skill differences:",
  subtitle="High opportunity occupations vs. other occupations.",
       x=NULL,
       y=NULL,
       caption = "Source: Skills data (ONET), Employment data (LMO) ") +
  theme_minimal() +
  theme(text=element_text(size=12))

ro$diffs_plt2 <- ggplot(ro$diffs, aes(x=absolute_diff, y=fct_reorder(skills_competencies, absolute_diff))) +
  geom_col(alpha=.5)+
  labs(title="Absolute skill differences:",
       subtitle="High Opportunity Occupations vs. other occupations.",
       x=NULL,
       y=NULL,
       caption = "Source: Skills data (ONET), Employment data (LMO) ") +
  theme_minimal() +
  theme(text=element_text(size=12))

ro <- enframe(ro)|>
  filter(name %in% c("emp_teer", "crossed", "skills", "diffs",
              "emp_teer_plt", "crossed_plt1", "crossed_plt2",
              "crossed_plt3", "skills_plt", "diffs_plt1",
              "diffs_plt2"))

ro|>
  deframe()|>
  write_rds(here("out", "richs_objects.rds"))

ro|>
  filter(name %in% c("emp_teer", "crossed", "skills", "diffs"))|>
  mutate(name=case_when(name=="emp_teer"~"Skills Fig 1",
                        name=="crossed"~"Skills Figs 2,3&4",
                        name=="skills"~"Skills Fig 5",
                        name=="diffs"~"Skills Figs 6&7"))|>
  arrange(name)|>
  deframe()|>
  write.xlsx(file = here("out","skills_data.xlsx"))


#




