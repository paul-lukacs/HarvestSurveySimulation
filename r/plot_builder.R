# Scenario 1 ====================================================================

MAREplot <- function(dat, s) {
  ggplot(dat, aes(x = resp_rate)) +
    geom_col(aes(y = MARE), color = "black", fill = "grey") +
    labs(x = "Response rate",
         y = "Mean absolute relative error",
         title = paste("Mean absolute relative error for Scenario", s)) +
    theme(text = element_text(size = 16),
          plot.title = element_text("serif", face = "bold", size = 16),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "light grey"),
          panel.grid.major.x = element_line(color = NA),
          axis.line.y = element_line(color = "white"),
          plot.title.position = "plot")
}

# Line plot for scenarios 2-5 ==================================================
MAREplot_all <- function(dat) {
  mycolor <- c("#75b4dd", "#ddd175", "#9f75dd", "black", "#b4dd75", "#dd75a4", 
               "#dd9f75", "#75ddd1", "grey", "hot pink")
  ggplot(dat, aes(x = resp_bias, group = scen, color = scen)) +
    geom_line(aes(y = MARE), size = 1) +
    xlim(0.8, 1.4) +
    labs(x = "Response bias",
         y = "Mean absolute relative error",
         title = "Mean absolute relative error by scenario",
         color = "Scenario") +
    scale_color_manual(values = mycolor) +
    theme(text = element_text(size = 14),
          legend.position = c(0.1, 0.7),
          plot.title = element_text("serif", face = "bold", size = 16),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "light grey"),
          panel.grid.major.x = element_line(color = NA),
          panel.grid.minor.x = element_line(color = "white"),
          axis.line.y = element_line(color = "white"),
          legend.title = element_text(face = "italic", size = 12),
          legend.background = element_rect(fill = "#e8e8e8", color = "black"),
          plot.title.position = "plot")
}
all_ests %>% 
  mutate(resp_bias = as.double(resp_bias)) %>%
  filter(scen != "1") %>% 
  group_by(scen, resp_bias) %>% 
  summarise(MARE = mean(MARE)) %>% 
  MAREplot_all()


# Bar plot for scenarios 2-5 ===================================================
MAREplot_all <- function(dat) {
  mycolor <- c("#75b4dd", "#ddd175", "#9f75dd", "black", "#b4dd75", "#dd75a4", 
               "#dd9f75", "#75ddd1", "grey", "hot pink")
  ggplot(dat, aes(x = resp_bias, fill = scen, color = scen)) +
    geom_col(aes(y = MARE), position = "dodge") +
    scale_fill_manual(values = mycolor) +
    scale_color_manual(values = mycolor) +
    labs(x = "Response Bias",
         y = "MARE",
         title = "Mean Absolute Relative Error by Scenario and Response Bias",
         fill = "Scenario") +
    guides(color = FALSE) +
    theme(legend.position = "right",
          plot.title = element_text("serif", face = "bold", size = 16),
          plot.subtitle = element_text("serif", face = "italic", size = 12),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "light grey"),
          panel.grid.major.x = element_line(color = NA),
          axis.line.y = element_line(color = "white"),
          legend.title = element_text(face = "italic", size = 9),
          legend.background = element_rect(fill = "#e8e8e8", color = "black"),
          plot.title.position = "plot")
}
           
all_ests %>% 
  filter(scen != "1") %>% 
  group_by(scen, resp_bias) %>% 
  summarise(MARE = mean(MARE)) %>% 
  MAREplot_all()

# Line plot for RRMSE all scenarios ============================================
RRMSEplot_all <- function(dat) {
  mycolor <- c("#75b4dd", "#ddd175", "#9f75dd", "black", "#b4dd75", "#dd75a4", 
               "#dd9f75", "#75ddd1", "grey", "hot pink")
  ggplot(dat, aes(x = resp_bias, group = scen, color = scen)) +
    geom_line(aes(y = RRMSE), size = 1) +
    xlim(0.8, 1.4) +
    labs(x = "Response bias",
         y = "RRMSE",
         title = "Relative root mean squared error",
         color = "Scenario") +
    scale_color_manual(values = mycolor) +
    theme(legend.position = "right",
          plot.title = element_text("serif", face = "bold", size = 16),
          plot.subtitle = element_text("serif", face = "italic", size = 12),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "light grey"),
          panel.grid.major.x = element_line(color = NA),
          panel.grid.minor.x = element_line(color = NA),
          axis.line.y = element_line(color = "white"),
          legend.title = element_text(face = "italic", size = 9),
          legend.background = element_rect(fill = "#e8e8e8", color = "black"),
          plot.title.position = "plot")
}

all_ests %>% 
  filter(scen != "1") %>% 
  mutate(resp_bias = as.double(resp_bias)) %>% 
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
          legend.title = element_text(face = "italic", size = 12),
          legend.background = element_rect(fill = "#e8e8e8", color = "black"),
          plot.title.position = "plot")
}
all_ests %>% 
  filter(scen != "1") %>% 
  mutate(scen = case_when(
    scen == 1 ~ paste0("Scenario ", scen, 
                      ": mandatory reporting for successful hunters"),
    scen == 2 ~ paste0("Scenario ", scen, 
                      ": simple random sample without follow up"),
    scen == 3 ~ paste0("Scenario ", scen, 
                      ": simple random sample with follow up"),
    scen == 4 ~ paste0("Scenario ", scen, 
                      ": voluntary response without follow up"),
    scen == 5 ~ paste0("Scenario ", scen, 
                      ": voluntary response with follow up") 
  )
  )%>% 
  group_by(scen, resp_bias, resp_rate) %>% 
  summarise(MARE = mean(MARE)) %>% 
  MAREplot_facet()

# Precision ====================================================================
SEplot_facet <- function(dat) {
  mycolor <- c("#75b4dd", "#ddd175", "#9f75dd", "black", "#b4dd75", "#dd75a4", 
               "#dd9f75", "#75ddd1", "grey", "hot pink")
  ggplot(dat, aes(x = resp_bias, fill = scen, color = scen)) +
    geom_col(aes(y = mean_SE), position = "dodge") +
    scale_fill_manual(values = mycolor) +
    scale_color_manual(values = mycolor) +
    labs(x = "Response Bias",
         y = "SE",
         title = "Standard error of estimates",
         fill = "Scenario") +
    guides(color = FALSE) +
    theme(legend.position = "right",
          plot.title = element_text("serif", face = "bold", size = 16),
          plot.subtitle = element_text("serif", face = "italic", size = 12),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "light grey"),
          panel.grid.major.x = element_line(color = NA),
          axis.line.y = element_line(color = "white"),
          legend.title = element_text(face = "italic", size = 9),
          legend.background = element_rect(fill = "#e8e8e8", color = "black"),
          plot.title.position = "plot")
}
all_ests %>% 
  group_by(scen, resp_bias) %>% 
  summarise(mean_SE = mean(mean_SE)) %>% 
  SEplot_facet()

# Precision, line ====================================================================
SEplot_line <- function(dat) {
  mycolor <- c("#75b4dd", "#ddd175", "#9f75dd", "black", "#b4dd75", "#dd75a4", 
               "#dd9f75", "#75ddd1", "grey", "hot pink")
  ggplot(dat, aes(x = resp_bias, group = scen, color = scen)) +
    geom_line(aes(y = mean_SE), size = 1) +
    scale_color_manual(values = mycolor) +
    labs(x = "Response Bias",
         y = "SE",
         title = "Standard error of estimates",
         fill = "Scenario") +
    theme(legend.position = "right",
          plot.title = element_text("serif", face = "bold", size = 16),
          plot.subtitle = element_text("serif", face = "italic", size = 12),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "light grey"),
          panel.grid.major.x = element_line(color = NA),
          axis.line.y = element_line(color = "white"),
          legend.title = element_text(face = "italic", size = 9),
          legend.background = element_rect(fill = "#e8e8e8", color = "black"),
          plot.title.position = "plot")
}
all_ests %>% 
  group_by(scen, resp_bias) %>% 
  summarise(mean_SE = mean(mean_SE)) %>% 
  SEplot_line()

