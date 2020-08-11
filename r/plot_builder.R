# MARE line for all scenarios ==================================================
MAREplot_all <- function(dat) {
  ggplot(dat, aes(x = resp_bias, group = scen, color = scen)) +
    geom_line(aes(y = MARE), size = 1) +
    ylim(0, 0.16) +
    labs(x = "Response bias",
         y = "Mean absolute relative error",
         title = "Harvest estimate error",
         color = "Scenario") +
    scale_color_manual(values = c("#75b4dd", "#ddd175", "#9f75dd", 
                                  "black", "#b4dd75")) +
    theme(text = element_text(size = 14),
          legend.position = c(0.25, 0.75),
          plot.title = element_text("serif", face = "bold", size = 16),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "light grey"),
          panel.grid.major.x = element_line(color = NA),
          panel.grid.minor.x = element_line(color = NA),
          axis.line.y = element_line(color = "white"),
          legend.title = element_text(size = 12),
          legend.background = element_rect(fill = "#e8e8e8", color = "black"),
          plot.title.position = "plot")
}
all_ests %>% 
  mutate(
    scen = case_when(
      scen == "1" ~ "1: Mandatory harvest reporting with follow up",
      scen == "2" ~ "2: Simple random sample",
      scen == "3" ~ "3: Simple random sample with follow up",
      scen == "4" ~ "4: Voluntary",
      scen == "5" ~ "5: Voluntary with follow up",
    )) %>%
  group_by(scen, resp_bias) %>% 
  summarise(MARE = mean(MARE)) %>% 
  MAREplot_all()


# RRMSE line all scenarios ============================================
RRMSEplot_all <- function(dat) {
  mycolor <- c("#75b4dd", "#ddd175", "#9f75dd", "black", "#b4dd75", "#dd75a4", 
               "#dd9f75", "#75ddd1", "grey", "hot pink")
  ggplot(dat, aes(x = resp_bias, group = scen, color = scen)) +
    geom_line(aes(y = RRMSE), size = 1) +
    xlim(0.8, 1.4) +
    labs(x = "Response bias",
         y = "Relative root mean squared error",
         title = "Relative root mean squared error by scenario",
         color = "Scenario") +
    scale_color_manual(values = mycolor) +
    theme(text = element_text(size = 14),
          legend.position = c(0.25, 0.7),
          plot.title = element_text("serif", face = "bold", size = 16),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "light grey"),
          panel.grid.major.x = element_line(color = NA),
          panel.grid.minor.x = element_line(color = NA),
          axis.line.y = element_line(color = "white"),
          legend.title = element_text(size = 12),
          legend.background = element_rect(fill = "#e8e8e8", color = "black"),
          plot.title.position = "plot")
}

all_ests %>% 
  mutate(resp_bias = as.double(resp_bias),
         scen = case_when(
           scen == "1" ~ "1: mandatory harvest reporting with follow up",
           scen == "2" ~ "2: simple random sample",
           scen == "3" ~ "3: simple random sample with follow up",
           scen == "4" ~ "4: voluntary reporting",
           scen == "5" ~ "5: voluntary with follow up",
         )) %>% 
  group_by(scen, resp_bias) %>% 
  summarise(RRMSE = mean(RRMSE)) %>% 
  RRMSEplot_all()

# Faceted Bar ==================================================================
MAREplot_facet <- function(dat) {
  mycolor <- c("#75b4dd", "#ddd175", "#9f75dd", "black", "#b4dd75", "#dd75a4", 
               "#dd9f75", "#75ddd1", "grey", "hot pink")
  ggplot(dat, aes(x = resp_bias, fill = resp_rate, color = resp_rate)) +
    geom_col(aes(y = MARE), position = "dodge") +
    facet_wrap(~scen, nrow = 2) +
    scale_fill_manual(values = mycolor) +
    scale_color_manual(values = mycolor) +
    labs(x = "Response bias",
         y = "Mean absolute relative error",
         fill = "Response rate") +
    guides(color = FALSE, fill = guide_legend(ncol = 2)) +
    theme(text = element_text(size = 14),
          legend.position = c(0.1, 0.85),
          plot.title = element_text("serif", face = "bold"),
          plot.subtitle = element_text("serif", face = "italic"),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "light grey"),
          panel.grid.major.x = element_line(color = NA),
          axis.line.y = element_line(color = "white"),
          legend.title = element_text(size = 12),
          legend.background = element_rect(fill = "#e8e8e8", color = "black"),
          plot.title.position = "plot")
}
all_ests %>% 
  filter(scen != "1") %>% 
  mutate(scen = case_when(
    scen == 1 ~ paste0("Scenario ", scen, 
                      ": mandatory with follow up"),
    scen == 2 ~ paste0("Scenario ", scen, 
                      ": simple random sample"),
    scen == 3 ~ paste0("Scenario ", scen, 
                      ": simple random sample with follow up"),
    scen == 4 ~ paste0("Scenario ", scen, 
                      ": voluntary"),
    scen == 5 ~ paste0("Scenario ", scen, 
                      ": voluntary with follow up") 
  )
  )%>% 
  group_by(scen, resp_bias, resp_rate) %>% 
  summarise(MARE = mean(MARE)) %>% 
  MAREplot_facet()

# Precision, line ==============================================================
SEplot_line <- function(dat) {
  mycolor <- c("#75b4dd", "#ddd175", "#9f75dd", "black", "#b4dd75", "#dd75a4", 
               "#dd9f75", "#75ddd1", "grey", "hot pink")
  ggplot(dat, aes(x = resp_bias, group = scen, color = scen)) +
    geom_line(aes(y = mean_SE), size = 1) +
    scale_color_manual(values = mycolor) +
    labs(x = "Response Bias",
         y = "SE",
         title = "Standard error of estimates",
         color = "Scenario") +
    theme(text = element_text(size = 14),
          legend.position = "right",
          plot.title = element_text("serif", face = "bold", size = 16),
          plot.subtitle = element_text("serif", face = "italic", size = 12),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "light grey"),
          panel.grid.major.x = element_line(color = NA),
          axis.line.y = element_line(color = "white"),
          legend.title = element_text(size = 12),
          legend.background = element_rect(fill = NA, color = "black"),
          legend.key = element_rect(fill = NA),
          plot.title.position = "plot")
}
all_ests %>% 
  
  group_by(scen, resp_bias) %>% 
  mutate(scen = case_when(
           scen == "1" ~ "1: mandatory harvest\nreporting with follow up\n",
           scen == "2" ~ "2: simple random sample\n",
           scen == "3" ~ "3: simple random sample\nwith follow up\n",
           scen == "4" ~ "4: voluntary reporting\n",
           scen == "5" ~ "5: voluntary with follow up",
         )) %>% 
  summarise(mean_SE = mean(mean_SE)) %>% 
  SEplot_line()

# MARE faceted line ============================================================

MAREplot_facetedline <- function(dat) {
  ggplot(dat, aes(x = resp_rate, group = resp_bias, color = resp_bias)) +
    facet_wrap(~scen, ncol = 2) +
    geom_line(aes(y = MARE), size = 1) +
    labs(x = "Response rate",
         y = "Mean absolute relative error",
         color = "Response bias") +
    scale_color_manual(
      values = c("#db3946", "#b675c9", "#b4dd75", "#75ddd1",
                 "orange","#5c5c5c", "#dd75a4")
    ) +
    guides(color = guide_legend(ncol = 2)) +
    theme(
      text = element_text(size = 14),
      legend.position = c(0.75, 0.1),
      plot.title = element_text("serif", face = "bold", size = 16),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white"),
      panel.grid = element_line(color = "light grey"),
      panel.grid.minor.x = element_line(color = NA),
      axis.line.y = element_line(color = "white"),
      legend.title = element_text(size = 12),
      legend.background = element_rect(fill = "#e8e8e8", color = "black"),
      plot.title.position = "plot"
    )
}
all_ests %>% 
  mutate(
    scen = case_when(
      scen == "1" ~ "Scenario 1: mandatory harvest reporting with follow up",
      scen == "2" ~ "Scenario 2: simple random sample",
      scen == "3" ~ "Scenario 3: simple random sample with follow up",
      scen == "4" ~ "Scenario 4: voluntary",
      scen == "5" ~ "Scenario 5: voluntary with follow up",
    )) %>% 
  group_by(scen, resp_bias, resp_rate) %>% 
  summarise(MARE = mean(MARE)) %>% 
  MAREplot_facetedline()

# MAPE faceted line ============================================================

MAPE_facetedline <- function(dat) {
  ggplot(dat, aes(x = resp_rate, group = resp_bias, color = resp_bias)) +
    facet_wrap(~scen, ncol = 2) +
    geom_line(aes(y = MAPE), size = 1) +
    labs(x = "Response rate",
         y = "Mean absolute percent error",
         color = "Response bias") +
    scale_color_manual(
      values = c("#db3946", "#b675c9", "#b4dd75", "#75ddd1",
                 "orange","#5c5c5c", "#dd75a4")
    ) +
    guides(color = guide_legend(ncol = 2)) +
    scale_y_continuous(labels = scales::label_percent()) +
    theme(
      text = element_text(size = 14),
      legend.position = c(0.75, 0.1),
      plot.title = element_text("serif", face = "bold", size = 16),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white"),
      panel.grid = element_line(color = "light grey"),
      panel.grid.minor.x = element_line(color = NA),
      axis.line.y = element_line(color = "white"),
      legend.title = element_text(size = 12),
      legend.background = element_rect(fill = NA, color = "black"),
      legend.key = element_rect(fill = NA),
      plot.title.position = "plot"
    )
}
all_ests %>% 
  mutate(
    scen = case_when(
      scen == "1" ~ "Scenario 1: mandatory harvest reporting with follow up",
      scen == "2" ~ "Scenario 2: simple random sample",
      scen == "3" ~ "Scenario 3: simple random sample with follow up",
      scen == "4" ~ "Scenario 4: voluntary reporting",
      scen == "5" ~ "Scenario 5: voluntary with follow up",
    )) %>% 
  group_by(scen, resp_bias, resp_rate) %>% 
  summarise(MAPE = mean(MARE)) %>% 
  MAPE_facetedline()

# SE faceted line ==============================================================

SEplot_facetedline <- function(dat) {
  mycolor <- c("#75b4dd", "#ddd175", "#9f75dd", "black", "#b4dd75", "#dd75a4", 
               "#dd9f75", "#75ddd1", "grey", "hot pink")
  ggplot(dat, aes(x = resp_rate, group = resp_bias, color = resp_bias)) +
    facet_wrap(~scen, ncol = 2) +
    geom_line(aes(y = SE), size = 1) +
    labs(x = "Response rate",
         y = "SE",
         color = "Response bias") +
    scale_color_manual(values = mycolor) +
    guides(color = guide_legend(ncol = 2)) +
    theme(
      text = element_text(size = 14),
      legend.position = c(0.75, 0.1),
      plot.title = element_text("serif", face = "bold", size = 16),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white"),
      panel.grid = element_line(color = "light grey"),
      panel.grid.minor.x = element_line(color = NA),
      axis.line.y = element_line(color = "white"),
      legend.title = element_text(size = 12),
      legend.background = element_rect(fill = "#e8e8e8", color = "black"),
      plot.title.position = "plot"
    )
}
all_ests %>% 
  mutate(
    scen = case_when(
      scen == "1" ~ "Scenario 1: mandatory harvest reporting with follow up",
      scen == "2" ~ "Scenario 2: simple random sample",
      scen == "3" ~ "Scenario 3: simple random sample with follow up",
      scen == "4" ~ "Scenario 4: voluntary",
      scen == "5" ~ "Scenario 5: voluntary with follow up",
    )) %>% 
  group_by(scen, resp_bias, resp_rate) %>% 
  summarise(SE = mean(mean_SE)) %>% 
  SEplot_facetedline()


