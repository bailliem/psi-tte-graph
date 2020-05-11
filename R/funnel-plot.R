library(tidyverse)
library(broom)
library(survival)
library(here)

# load data
ADTTE <- read_csv(here("data", '2020-04-08-psi-vissig-adtte.csv'))

# create age groups
# set up treatment comparisons
model_data <-
  ADTTE %>%
  mutate(
    age_grp = case_when(
      AGE >= 75 ~ '4',
      AGE >= 65 & AGE < 75 ~ '3',
      AGE >= 55 & AGE < 65 ~ '2',
      AGE < 55 ~ '1'
    ),
    compare1 = factor(
      TRT01P,
      levels = c(
        "tablemab x 52 weeks",
        "vismab x 52 weeks",
        "tablemab + vismab 52 weeks",
        "tablemab x 12 week -> vismab 34 weeks"
      )
    ),
    compare2 = factor(
      TRT01P,
      levels = c(
        "vismab x 52 weeks",
        "tablemab + vismab 52 weeks",
        "tablemab x 12 week -> vismab 34 weeks"
      )
    ),
    compare3 = factor(
      TRT01P,
      levels = c("tablemab + vismab 52 weeks",
                 "tablemab x 12 week -> vismab 34 weeks")
    )
  )

## calculate tidy hr for all comparisons
## calculate tidy hr for all comparisons
calc_tidy_hr <- function(indata) {
  
  bigN <-
    indata %>%
    filter(CNSR == 0) %>%
    group_by(TRT01P) %>%
    tally() %>%
    mutate(group = TRT01P) %>%
    ungroup()
  
  
  td1 <-
    coxph(Surv(AVAL, CNSR == 0) ~ compare1 ,  data = indata) %>%
    tidy(exponentiate = TRUE) %>%
    mutate(compare = stringr::str_replace(term, "compare1", "tablemab x 52 weeks vs. "),
           group = stringr::str_remove(term, "compare1"),
           TRT01P = "tablemab x 52 weeks")
  
  td2 <-
    coxph(Surv(AVAL, CNSR == 0) ~ compare2 ,  data = indata) %>%
    tidy(exponentiate = TRUE) %>%
    mutate(compare = stringr::str_replace(term, "compare2", "vismab x 52 weeks vs. "),
           group = stringr::str_remove(term, "compare2"),
           TRT01P = "vismab x 52 weeks")
  
  td3 <-
    coxph(Surv(AVAL, CNSR == 0) ~ compare3 ,  data = indata) %>%
    tidy(exponentiate = TRUE) %>%
    mutate(compare = stringr::str_replace(term, "compare3", "tablemab + vismab 52 weeks vs. "),
           group = stringr::str_remove(term, "compare3"),
           TRT01P = "tablemab + vismab 52 weeks")
  
  td <- bind_rows(td1, td2, td3) %>%
    left_join(bigN %>% select(n, TRT01P) %>% mutate(n.left = n), by = "TRT01P") %>%
    left_join(bigN %>% select(n, group) %>% mutate(n.right = n), by = "group") %>%
    mutate(n.events = n.left + n.right)
  
  
  return(td)
}


get_subgroup <- function(in_data, subgroup){
  out_data <- 
    in_data %>%
    group_by({{subgroup}}) %>%
    nest() %>%
    mutate(subgrps = map(data, ~calc_tidy_hr(.x))) %>%
    tidyr::unnest(subgrps) %>%
    mutate(subgroup = rlang::as_name(enquo(subgroup)),
           subgroup.term = {{subgroup}})
  
  return(out_data)
}


## overall 
all_data <- calc_tidy_hr(model_data) %>%
  mutate(
    subgroup = "Overall",
    subgroup.term  = "All patients"
  )

STR01_data <- get_subgroup(model_data, STR01)
STR02_data <- get_subgroup(model_data, STR02)
age_grp_data <- get_subgroup(model_data, age_grp)
plot_data <- bind_rows(all_data, STR01_data, STR02_data, age_grp_data)

plot_data %>% glimpse()

plot_data %>% 
  ggplot(aes(x = n.events, y = estimate, group = subgroup, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_linerange() +
  geom_point(
    data = transform(plot_data, compare = NULL),
    #aes(time, estimate, group = group2),
    size = 0.75,
    color = "#000000",
    alpha = 0.15
  ) +
  geom_linerange(data = transform(plot_data, compare = NULL),     size = 0.75,
                 color = "#000000",
                 alpha = 0.15) +
  scale_y_log10() + 
  facet_wrap(~ compare, ncol = 3) +
  # set up basic theme 
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  # Set the entire chart region to a light gray color
#  theme(panel.background = element_rect(fill = color_background, color = color_background)) +
#  theme(plot.background = element_rect(fill = color_background, color = color_background)) +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 0.35)) 




plot_data %>% 
  ggplot(aes(x = n.events, y = estimate, group = subgroup, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_linerange() +
  geom_point(
    data = transform(plot_data, compare = NULL),
    #aes(time, estimate, group = group2),
    size = 0.75,
    color = "#000000",
    alpha = 0.15
  ) +
  geom_linerange(data = transform(plot_data, compare = NULL),     size = 0.75,
                 color = "#000000",
                 alpha = 0.15) +
  scale_y_log10() + 
  facet_grid(compare ~ subgroup) +
  # set up basic theme 
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 0.35)) 

