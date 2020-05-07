
plot <-
  td %>%
  ggplot(aes(
    time,
    estimate,
    group = group,
    ymin = conf.low,
    ymax = conf.high
  )) +
  geom_step(
    data = transform(td2, group = NULL),
    aes(time, estimate, group = group2),
    size = 0.75,
    color = "#000000",
    alpha = 0.15
  ) +
  geom_ribbon(alpha = 0.1, fill = "red") +
  geom_step(color = "red") +
  geom_text(data = risk_data, mapping =  aes(x = time, y = y_pos, label = value, group = group)) +
  #  geom_point(alph = 0.5, size = 0.1) +
  labs(title = title,
       subtitle = subtitle,
       caption = source) +
  xlab(x_axis) +
  ylab(y_axis) +
  #paper_theme() +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  facet_wrap(~ group, scales = 'free', ncol = 2) 

plot


a <- risk_data %>% 
  filter(group == "tablemab + vismab 52 weeks") %>%
  ggplot() +
  geom_text(aes(x = time, y = var_type, label = value, group = group), size = 1) +
#  geom_step(data = td, mapping = aes(x = time, y = estimate, group = group, ymin = conf.low, ymax = conf.high)) +
  facet_wrap(~ group, scales = 'free', ncol = 2) + 
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) 
  

b <-
  td %>%
  filter(group == "tablemab + vismab 52 weeks") %>%
  ggplot(aes(
    time,
    estimate,
    group = group,
    ymin = conf.low,
    ymax = conf.high
  )) +
  geom_step(
    data = transform(td2, group = NULL),
    aes(time, estimate, group = group2),
    size = 0.75,
    color = "#000000",
    alpha = 0.15
  ) +
  geom_ribbon(alpha = 0.1, fill = "red") +
  geom_step(color = "red") +
  #  geom_point(alph = 0.5, size = 0.1) +
  labs(title = title,
       subtitle = subtitle,
       caption = source) +
  xlab(x_axis) +
  ylab(y_axis) +
  #paper_theme() +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) 


#+
#  facet_wrap(~ group, scales = 'free', ncol = 2) 
b

library(patchwork)

a / b




plot + geom_text(data = risk_data, aes(x = time, y = y_pos, label = value, group = group))


## numbers of risk as table of annotation?
td %>% 
  ggplot(aes(time, estimate, group = strata)) + 
  geom_line() +
  geom_point(data = td %>% filter(n.censor > 0), aes(time, estimate), alpha = 0.5) +
  geom_text(data = n_risk, aes(x = Time, y = row, label = Nrisk, group = Arm)) +
  theme_minimal()



library(ggrepel)
td %>% 
  ggplot(aes(time, estimate, group = strata)) + 
  geom_line() +
  geom_text_repel(data = td %>% group_by(strata) %>% slice(n()),
                  aes(label = strata),
                  size = 4.5,
                  point.padding = .2,
                  box.padding = .3,
                  force = 1,
                  min.segment.length = 0
  )


risk_data

#%>%
#  tidyr::pivot_longer(cols = starts_with("n."), names_to = "var_type", values_to = "value", values_drop_na = FALSE) %>%
#  dplyr::mutate(
#    y_pos = ifelse(var_type == "n.risk", 0, -0.2),
#    value = ifelse(is.na(value), 0, value)
#  )

risk_data


surv_tab <-
  n_risk %>% 
  ggplot(aes(x = time, y = var , label = value)) + 
  geom_text() + 
  facet_wrap(~ group)

surv_tab

risk_data %>% 
  ggplot(aes(x = time, y = var_type, label = value, group = group)) +
  geom_text() + 
  facet_wrap(~ group, ncol = 1) +
  theme_minimal()


plot <-
  td %>%
  ggplot(aes(
    time,
    estimate,
    group = group,
    ymin = conf.low,
    ymax = conf.high
  )) +
  geom_step(
    data = transform(td2, group = NULL),
    aes(time, estimate, group = group2),
    size = 0.75,
    color = "#000000",
    alpha = 0.15
  ) +
  geom_ribbon(alpha = 0.1, fill = "red") +
  geom_step(color = "red") +
  #  geom_point(alph = 0.5, size = 0.1) +
  labs(title = title,
       subtitle = subtitle,
       caption = source) +
  xlab(x_axis) +
  ylab(y_axis) +
  #paper_theme() +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  facet_wrap(~ group, scales = 'free', ncol = 1) 

plot





### annotation could be p-values cox model
#geom_text_repel(data = td %>% group_by(group) %>% slice(n()),
#                aes(label = group),
#                size = 4.5,
#                point.padding = .2,
#                box.padding = .3,
#                force = 1,
#                min.segment.length = 0
#) +




############################################
## Cox model
############################################

fit <-
  survfit(Surv(AVAL, CNSR == 0) ~ TRT01P,
          data = ADTTE,
          conf.type = 'log-log')
cfit <- coxph(Surv(AVAL, CNSR == 0) ~ TRT01P, data = ADTTE)
tidy(cfit, exponentiate = TRUE)



