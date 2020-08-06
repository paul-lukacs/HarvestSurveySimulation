library(tidyverse)
# Cost calculations ============================================================
t <- 1.33           # Average cost/survey for deer harvest (telephone)
i <- 5000            # internet

s1_cost <- s1_est %>% 
  group_by(resp_bias, resp_rate) %>% 
  summarise(
    response = sum(mean_respond, mean_fusresp),
    telephone = response * t,
    scen = "1"
  )

s2_cost <- s2_est %>% 
  group_by(resp_bias, resp_rate) %>% 
  summarise(
    response = mean_respond,
    telephone = response * t,
    scen = "2"
  )

s3_cost <- s3_est %>% 
  group_by(resp_bias, resp_rate) %>% 
  summarise(
    response = sum(mean_respond, mean_fusresp, na.rm = T),
    telephone = response * t,
    scen = "3"
    )
    
s4_cost <- s4_est %>% 
  group_by(resp_bias, resp_rate) %>% 
  summarise(
    response = mean_respond,
    telephone = response * t,
    scen = "4"
  )

s5_cost <- s5_est %>% 
  group_by(resp_bias, resp_rate) %>% 
  summarise(
    response = sum(mean_respond, mean_fusresp, na.rm = T),
    telephone = response * t,
    scen = "5"
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

all_costs <- bind_rows(s1_cost, s2_cost, s3_cost, s4_cost, s5_cost)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cost_line <- function(dat) {
  mycolor <- c("#75b4dd", "#ddd175", "#9f75dd", "black", "#b4dd75", "#dd75a4", 
               "#dd9f75", "#75ddd1", "grey", "hot pink")
  ggplot(dat, aes(x = resp_rate, group = scen, color = scen)) +
    geom_line(aes(y = mean_cost), size = 1) +
    scale_color_manual(values = mycolor) +
    labs(x = "Response rate",
         y = "Cost",
         title = "Costs of scenarios when no response bias is present",
         color = "Scenario") +
    scale_y_continuous(labels=scales::dollar_format()) +
    theme(text = element_text(size = 14),
          legend.position = "right",
          plot.title = element_text("serif", face = "bold", size = 16),
          plot.subtitle = element_text("serif", size = 12),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "light grey"),
          panel.grid.major.x = element_line(color = NA),
          axis.line.y = element_line(color = "white"),
          legend.title = element_text(size = 12),
          legend.background = element_rect(fill = "#e8e8e8", color = "black"),
          plot.title.position = "plot")
}

all_costs %>% 
  filter(resp_bias == "1") %>% 
  group_by(scen, resp_rate) %>% 
  mutate(scen = case_when(
    scen == "1" ~ "1: Mandatory harvest reporting\nwith follow up\n",
    scen == "2" ~ "2: Simple random sample\n",
    scen == "3" ~ "3: Simple random sample\nwith follow up\n",
    scen == "4" ~ "4: Voluntary\n",
    scen == "5" ~ "5: Voluntary with follow up",
  )) %>% 
  summarise(mean_cost = mean(telephone)) %>% 
  cost_line()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cost_bar <- function(dat) {
  mycolor <- c("#75b4dd", "#ddd175", "#9f75dd", "black", "#b4dd75", "#dd75a4", 
               "#dd9f75", "#75ddd1", "grey", "hot pink")
  ggplot(dat, aes(x = resp_bias, fill = resp_rate, color = resp_rate)) +
    geom_col(aes(y = mean_cost), position = "dodge") +
    facet_wrap(~scen, ncol = 2) +
    scale_color_manual(values = mycolor) +
    scale_fill_manual(values = mycolor) +
    labs(x = "Response Bias",
         y = "Cost",
         title = "Costs of scenarios",
         fill = "Response rate") +
    guides(color = FALSE, fill = guide_legend(nrow = 1)) +
    scale_y_continuous(labels=scales::dollar_format()) +
    theme(text = element_text(size = 14),
          legend.position = c(0.75, 0.15),
          plot.title = element_text("serif", face = "bold", size = 16),
          plot.subtitle = element_text("serif", face = "italic", size = 12),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "light grey"),
          panel.grid.major.x = element_line(color = NA),
          axis.line.y = element_line(color = "white"),
          legend.title = element_text(size = 12),
          legend.background = element_rect(fill = "#e8e8e8", color = "black"),
          plot.title.position = "plot")
}

all_costs %>% 
  group_by(scen, resp_bias, resp_rate) %>% 
  mutate(scen = case_when(
    scen == "1" ~ "1: Mandatory with follow up",
    scen == "2" ~ "2: Simple random sample",
    scen == "3" ~ "3: Simple random sample with follow up",
    scen == "4" ~ "4: Voluntary",
    scen == "5" ~ "5: Voluntary with follow up",
  )) %>% 
  summarise(mean_cost = mean(telephone)) %>% 
  cost_bar()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cost_facetedline <- function(dat) {
  ggplot(dat, aes(x = resp_rate, group = resp_bias, color = resp_bias)) +
    geom_line(aes(y = mean_cost), size = 1) +
    facet_wrap(~scen) +
    scale_color_manual(values = c("#75b4dd", "#ddd175", "#9f75dd", "black", 
                                  "#b4dd75", "#dd9f75", "#75ddd1")) +
    labs(x = "Response rate",
         y = "Cost",
         title = "Costs of scenarios",
         color = "Response bias") +
    scale_y_continuous(labels=scales::dollar_format()) +
    theme(text = element_text(size = 14),
          legend.position = c(0.75, 0.1),
          plot.title = element_text("serif", face = "bold", size = 16),
          plot.subtitle = element_text("serif", size = 12),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "light grey"),
          panel.grid.major.x = element_line(color = NA),
          axis.line.y = element_line(color = "white"),
          legend.title = element_text(size = 12),
          legend.background = element_rect(fill = "#e8e8e8", color = "black"),
          plot.title.position = "plot")
}

all_costs %>% 
  group_by(scen, resp_bias, resp_rate) %>% 
  mutate(
    scen = case_when(
    scen == "1" ~ "1: Mandatory harvest reporting with follow up",
    scen == "2" ~ "2: Simple random sample",
    scen == "3" ~ "3: Simple random sample with follow up",
    scen == "4" ~ "4: Voluntary",
    scen == "5" ~ "5: Voluntary with follow up",
  )) %>% 
  summarise(mean_cost = mean(telephone)) %>% 
  cost_facetedline()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

percent_response <- function(dat) {
  ggplot(dat, aes(x = resp_rate, group = scen, color = scen)) +
    geom_line(aes(y = p_resp), size = 1) +
    scale_color_manual(values = c("#75b4dd", "#ddd175", "#9f75dd", "black", 
                                  "#b4dd75")) +
    labs(x = "Response rate",
         y = "Percent of population responding",
         title = "Overall response rates",
         color = "Scenario") +
    scale_y_continuous(labels=scales::label_percent()) +
    theme(
      text = element_text(size = 14),
      legend.position = "right",
      plot.title = element_text("serif", face = "bold", size = 16),
      plot.subtitle = element_text("serif", size = 12),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white"),
      panel.grid = element_line(color = "light grey"),
      panel.grid.major.x = element_line(color = NA),
      axis.line.y = element_line(color = "white"),
      legend.title = element_text(size = 12),
      legend.background = element_rect(fill = "#e8e8e8", color = "black"),
      plot.title.position = "plot"
    ) 
}

all_costs %>% 
  filter(resp_bias == "1") %>% 
  group_by(scen, resp_rate) %>% 
  mutate(scen = case_when(
    scen == "1" ~ "1: Mandatory harvest reporting\nwith follow up\n",
    scen == "2" ~ "2: Simple random sample\n",
    scen == "3" ~ "3: Simple random sample\nwith follow up\n",
    scen == "4" ~ "4: Voluntary\n",
    scen == "5" ~ "5: Voluntary with follow up",
  )) %>% 
  summarise(p_resp = response / 8000) %>% 
  percent_response()

#~~~~~~~~~~~~~~~~~~~~~~~~~
# COLOR VIEWER
data.frame(x = 1:10, y = 1) %>% 
  ggplot(aes(x = as.character(x), fill = as.character(x))) +
  geom_bar()+
  scale_fill_manual(values = c("#75b4dd", "#ddd175", "#9f75dd", "black", 
                               "#b4dd75", "#dd75a4","#dd9f75", "#75ddd1", 
                               "grey", "hot pink")
                     )
