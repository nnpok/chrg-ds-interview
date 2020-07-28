###################################
# Colford-Hubbard Research Group
# Technical interview
#
# Bar chart comparing vaccination counts
# in each year and site 
###################################

library(tidyverse)

# Kaiser vaccination data
kaiser_data = read.csv("kaiser_vacc.csv") %>% 
  filter(year != "2014-2015") %>% 
  mutate(site = if_else(site == "comparison", "Comparison", "Intervention"))

# Shoo the Flu vaccination data
stf_data = read.csv("stf_vacc.csv") %>% 
  mutate(year = paste0(substr(flu_season, 1, 5), substr(flu_season, 8,9)),
         site = "Intervention",
         program = "Shoo the Flu",
         agecat = "5-12",
         n = as.numeric(str_replace(n, ",", ""))) %>% 
  select(-flu_season)

# Join data and create pretty labels
data = full_join(kaiser_data, stf_data) 

data = data %>% 
  mutate(program = factor(program, levels=c("Shoo the Flu", "Kaiser")),
         color = paste0("Vaccination by ", program, "-", site)) %>% 
  mutate(color = as.character(color)) %>% 
  mutate(color = if_else(color == "Vaccination by Shoo the Flu-Intervention",
                        "Vaccination by Shoo the Flu",
                        color)) %>%
  mutate(color = factor(color, levels=c("Vaccination by Shoo the Flu", 
                                        "Vaccination by Kaiser-Intervention", 
                                        "Vaccination by Kaiser-Comparison",
                                        "Vaccination by Shoo the Flu-Comparison"))) 

##################################
# Bar plot 
##################################

plot_vacc_count = function(data, ylab_text) {
  
  p = ggplot(data, aes(x=year, y=n)) + 
    
    geom_bar(aes(site, fill=color),
             position="stack",
             stat="identity",
             width = 0.75,
             alpha = 0.8) + 
    
    facet_wrap(~year, nrow=1, strip.position = "bottom") + 
    
    # colors correspond in order of levels defined for 'color' column in data
    scale_fill_manual(values=c("#555f61", "#8c979a", "#dadedf")) +
    
    theme_minimal() + 
    
    xlab("Influenza season") +
    ylab(ylab_text) +
    
    geom_vline(xintercept=.4,
               lty="dashed",
               color = c("#00000000", "#00000000", "#00000000", "black", "#00000000", "#00000000")) +
    
    scale_x_discrete(breaks = NULL) +
    scale_y_continuous(breaks = seq(0, 40000, 10000)) +
    
    expand_limits(y = c(0, 40000)) +
     
    theme(axis.text.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.position = c(0.5, -0.20),
          panel.spacing = unit(0, "lines"),
          axis.text.y = element_text(size = 15),  
          axis.title.x = element_text(size = 15, vjust=-3),
          axis.title.y = element_text(size = 15, vjust=5),
          strip.text.x = element_text(size = 12),
          plot.margin = margin(20, 10, 100, 30)) +
    
    guides(fill=guide_legend(nrow=1, byrow=TRUE, reverse=TRUE))
  
  return(p)
}

fig_vacc_count = plot_vacc_count(data, ylab_text = "Population vaccinated")
fig_vacc_count

ggsave("fig_vaccination_by_year.png", fig_vacc_count, width = 12, height = 8)
