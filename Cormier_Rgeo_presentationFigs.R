library(tidyverse)
library(glue)
library(treemapify)
library(ggsci)
library(wordcloud)
library(wordcloud2)
library(ggraph)
library(igraph)
library(paletteer)
library(ggdark)

# # # # User Variables # # # #
survey_file <- 'https://raw.githubusercontent.com/tacormier/rstudio-conf-2020/master/data/Embracing_R_in_the_Geospatial_Community_Surveyresponses_cleaner.csv'
# You can skip this if you comment out/take out all of the ggsave lines.
fig_dir <- '[your/output/directory/]' 
# # # # Function(s) # # # #

prep_prog_lang <- function (df) {
  df_prep <- df %>% mutate(prog_languages = replace(prog_languages, prog_languages %in% c('bash', 'Bash', 'Shell scripts'), "Shell"),
       prog_languages = replace(prog_languages, prog_languages %in% c('C#', 'C++'), 'C (or variant)'),
       prog_languages = replace(prog_languages, prog_languages == "Idl  + envi", 'IDL'),
       prog_languages = replace(prog_languages, prog_languages %in% c('sql', 'T-SQL', 'TSQL', 'PGSQL', 'PostGIS PL\\pgSQL', 'SQL'), 'SQL Variant'),
       prog_languages = replace(prog_languages, prog_languages == 'Mapbasic', 'MapBasic'),
       prog_languages = replace(prog_languages, prog_languages == 'R (option does not respond)', 'R')
  )
  return(df_prep)
}
  
# # # # Data Prep # # # # 
dir.create(fig_dir, showWarnings = F, recursive = T)

survey <- readr::read_csv(survey_file)
survey$yrs_experience <- factor(survey$yrs_experience, levels = c("0 - 2 years", 
                                                                  "3 - 5 years",
                                                                  "6 - 10 years",
                                                                  "11 - 15 years",
                                                                  "16 - 20 years",
                                                                  "> 20 years"))

# # # # VISUALS # # # #

# 1. Summarize Roles - who answered the survey? Stack on code Y/N
role_df <- survey %>% 
  group_by(role_simplified, scripting) %>%
  tally()

ggplot(role_df, aes(x=reorder(role_simplified, n), y=n, fill=scripting)) + 
  geom_bar(stat='identity', position='stack') +
  xlab('Role') +
  ylab('Number of responses') +
  coord_flip() +
  scale_fill_manual(values=c('#ED5747', '#09587C')) +
  theme_gray(base_size = 20) 

role_plot <- glue('{fig_dir}/survey_roles_byScripting.png')
ggsave(filename = role_plot, width = 9, height = 4)

####

# 2. Roles cut by use R y/n (these are respondents who answered yes to scripting only)
roleR_df <- survey %>% 
  group_by(role_simplified, r_user) %>%
  tally() %>%
  drop_na()
   

ggplot(roleR_df, aes(x=reorder(role_simplified, n), y=n, fill=r_user)) + 
  geom_bar(stat='identity', position='stack') +
  xlab('Role') +
  ylab('Number of responses') +
  coord_flip() +
  scale_fill_manual(values=c('#ED5747', '#09587C')) +
  guides(fill = guide_legend(title = 'R User', reverse=T)) +
  theme_gray(base_size = 20) 

roleR_plot <- glue('{fig_dir}/survey_roles_byRuser.png')
ggsave(filename = roleR_plot, width = 9, height = 4)

####

# 3. Of the people who said yes to scripting, but no to R, what are they using?
noR_df <- separate_rows(survey, prog_languages, sep=',') %>% 
  prep_prog_lang() %>%
  filter(scripting == 'Yes' & r_user == 'No', prog_languages != "" & prog_languages != 'R') %>%
  group_by(prog_languages) %>%
  tally()

ggplot(noR_df, aes(x=reorder(prog_languages, n), y=n)) + 
  geom_bar(stat='identity', fill='#09587C') +
  xlab('Role') +
  ylab('Number of responses') +
  coord_flip() +
  guides(fill = guide_legend(reverse=T)) +
  theme_gray(base_size = 20) 

noR_plot <- glue('{fig_dir}/survey_prog_lang_noR.png')
ggsave(filename = noR_plot, width = 9, height = 4)

####
# 4. Dendrogram of tools by Role (color=role, circle size=tool popularity)

# Code adapted from here: https://www.r-graph-gallery.com/339-circular-dendrogram-with-ggraph.html
#  and here: https://github.com/jkaupp/tidytuesdays/blob/master/2020/week3/R/analysis.R

# Get top tools (>10 users named them)
top_tools <- separate_rows(survey, analysis_tools, sep = ",") %>% 
  group_by(analysis_tools) %>% 
  tally() %>% 
  filter(n > 10) %>% 
  mutate(analysis_tools = ifelse(analysis_tools == "Google Earth Engine", "GEE", analysis_tools),
         analysis_tools = ifelse(analysis_tools == "Esri products", "Esri", analysis_tools))

# Tally up tools by role (just the top tools) 
surv_tools <- separate_rows(survey, analysis_tools, sep = ",") %>% 
  group_by(role_simplified, analysis_tools) %>% 
  tally(name = "value") %>% 
  filter(analysis_tools %in% top_tools$analysis_tools) %>% 
  mutate(value = value/sum(value)) %>% 
  arrange(role_simplified, analysis_tools) %>% 
  ungroup() %>% 
  mutate(group_id = group_indices(., role_simplified),
         analysis_tools2 = glue('{analysis_tools}-{group_id}'))

# surv_tools <- survey
surv1 <- tibble(from = "origin", to = unique(surv_tools$role_simplified))
surv2 <- tibble(from = surv_tools$role_simplified, to = surv_tools$analysis_tools2)

edges <- surv1 %>% 
  bind_rows(surv2) %>% 
  arrange(from, to)

# create a vertices obj. One line per object of our hierarchy
vertices = tibble(
  name_grouped = unique(c(as.character(edges$from), as.character(edges$to))) , stringsAsFactors = F)

vertices <- vertices %>% 
  left_join(surv_tools, by = c("name_grouped" = "analysis_tools2")) %>% 
  select(name_grouped, value)

vertices$name <- sub("*-[0-9]{1,}", "", vertices$name_grouped)

# Add a column with the group of each name. It will be useful later to color points
vertices$group = edges$from[ match( vertices$name_grouped, edges$to ) ]

# Add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
# calculate the ANGLE of the labels
vertices$id=NA
myleaves=which(is.na( match(vertices$name, edges$from) ))
nleaves=length(myleaves)
vertices$id[ myleaves ] = seq(1:nleaves)

# Create a graph object
geo_graph <- graph_from_data_frame( edges, vertices=vertices )

# Make the plot
ggraph(geo_graph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(colour="grey") +
  scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_text(aes(x = x*1.3, y=y*1.3, filter = leaf, label=name, angle = -((-node_angle(x, y)+90)%%180)+90, hjust = ifelse(between(node_angle(x,y), 90, 270), 0, 1), colour=group), size=4, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=group, size=value, alpha=0.2)) +
  scale_size_continuous( range = c(0.1,10) ) +
  # For some reason, dark_theme_void did nothing, so had to manually remove axis labels below.
  dark_theme_classic() + 
  scale_colour_paletteer_d("vapoRwave::vapoRwave") +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()) +
  # guides(alpha = "none",
  #        size = "none") +
expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

toolByRole_plot <- glue('{fig_dir}/survey_toolsByRole_dendrogram.png')
ggsave(filename = toolByRole_plot, width = 8, height = 8)

####

# 5. Summarize Sector, cut by years of experience
sector_df1 <- survey %>% 
  group_by(sector_simplified) %>%
  tally(name='n_sec')

sector_df2 <- survey %>%
  group_by(sector_simplified, yrs_experience) %>%
  tally(name = 'n_yrs')

sector_df <- sector_df2 %>%
  left_join(sector_df1)

sector_df$yrs_experience <- factor(sector_df$yrs_experience, levels = rev(levels(sector_df$yrs_experience)))


ggplot(sector_df, aes(x=reorder(sector_simplified, n_sec), y=n_yrs, fill=yrs_experience, order=yrs_experience)) + 
  geom_bar(stat='identity', position='stack', color='lightgray', size=0.25) +
  xlab('Sector') +
  ylab('Number of responses') +
  coord_flip() +
  scale_fill_nejm() +
  guides(fill = guide_legend(title = 'Experience', reverse=T)) +
  theme_gray(base_size = 20)

sector_plot <- glue('{fig_dir}/survey_sector_byYearsExp.png')
ggsave(filename = sector_plot, width = 9, height = 5)

####

# 6. Years of experience bar plot 
sector_df3 <- survey %>%
  group_by(yrs_experience) %>%
  tally(name = 'n_yrs')

ggplot(sector_df3, aes(x=yrs_experience, y=n_yrs)) +
  geom_bar(stat='identity', width = 0.75, position='dodge', fill='#09587C') +
  ylab('number of responses') +
  xlab('years of experience') +
  theme_gray(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

yrs_exp_bplot <- glue('{fig_dir}/survey_yrsExp_barplot.png')
ggsave(filename = yrs_exp_bplot, width = 5, height = 5)

####

# 6B. years of experience treemap (a fun alternative to a bar or pie chart)
ggplot(sector_df3, aes(area=n_yrs, fill = yrs_experience)) +
  geom_treemap(color='gray12', show.legend = F) +
  geom_treemap_text(aes(label=yrs_experience), color='gray12', size=15, place='center') +
  guides(fill=guide_legend(title = "Years of Experience")) +
  scale_fill_startrek()

yrs_exp_tplot <- glue('{fig_dir}/survey_yrsExp_treeplot.png')
ggsave(filename = yrs_exp_tplot, width = 5, height = 5)
  
####

# 7. Do years of experience affect whether a person has scripted before? 
# Are newer folks more likely to use programming, or more experienced pple?
# Or are more experienced folks more likely to have turned to coding out of necessity by now?
# Answer: not really
ggplot(survey, aes(x=yrs_experience, fill=scripting)) +
  geom_bar(width=0.75) +
  ylab('number of responses') +
  xlab('years of experience') +
  scale_fill_manual(values=c('#ED5747', '#09587C')) +
  theme_gray(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

yrs_exp_scripting_bplot <- glue('{fig_dir}/survey_yrsExp_scripting_bplot.png')
ggsave(filename = yrs_exp_scripting_bplot, width = 8, height = 7)

####

# 8. Summarize/tally up analysis tools (top 15 tools)
analysis_tools <- separate_rows(survey, analysis_tools, sep = ",") %>%
  group_by(analysis_tools) %>%
  tally() %>%
  top_n(15, n)

ggplot(analysis_tools, aes(x=reorder(analysis_tools, n), y=n, fill=n)) +
  geom_bar(stat='identity', show.legend = F) +
  xlab('tools used in analysis') +
  ylab('number of responses') +
  coord_flip() +
  theme_gray(base_size = 20) +
  scale_fill_gradient(low='gray10', high= '#09587C')

analysis_plot <- glue('{fig_dir}/analysis_tools_bplot.png')
ggsave(filename = analysis_plot, width = 10, height = 6)

####

# 9. Summarize/tally up cartography tools
carto_tools <- separate_rows(survey, carto_tools, sep = ",") %>%
  group_by(carto_tools) %>%
  tally() %>%
  filter(carto_tools != "Not applicable - I do not make visualizations") %>%
  top_n(15, n)

ggplot(carto_tools, aes(x=reorder(carto_tools, n), y=n, fill=n)) +
  geom_bar(stat='identity', show.legend = F) +
  xlab('tools used in cartography') +
  ylab('number of responses') +
  coord_flip() +
  theme_gray(base_size = 20) +
  scale_fill_gradient(low='gray10', high= '#09587C')

carto_plot <- glue('{fig_dir}/carto_tools_bplot.png')
ggsave(filename = carto_plot, width = 10, height = 6)

####

# 10. Of the scripting folks, how many are or have ever been an R user?
# I can't look at another barplot. Sorry to the graphics purists!
# Donut plot - everyone loves donuts!
scripting <- survey %>% 
  select(scripting, r_user) %>%
  group_by(scripting, r_user) %>%
  tally(name = 'n_responses') %>%
  ungroup() %>%
  add_column(label = c('No Code', 'No R', 'R')) %>%
  mutate(frac = n_responses / sum(n_responses),      # Compute percentages
         ymax = cumsum(frac),                        # Compute the cumulative percentages (top of each rectangle)
         ymin = c(0, head(ymax, n=-1)),              # Compute the bottom of each rectangle
         label_pos = (ymax + ymin) / 2,             # Compute label position                   
         label_val = glue('{label}: {n_responses}') # Compute a good label
  )

  
ggplot(scripting, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=label)) +
  geom_rect() +
  geom_text( x=1.65, aes(y=label_pos, label=label_val, color=label), size=10) + # x here controls label position (inner / outer)
  scale_fill_manual(values=c('gray15','#ED5747','#09587C')) +
  scale_color_manual(values=c('gray15','#ED5747','#09587C')) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none",
    # panel.grid.major = element_blank(), 
    # panel.grid.minor = element_blank(),
    # panel.background = element_rect(fill = "transparent",colour = NA),
    # plot.background = element_rect(fill = "transparent",colour = NA)
  ) 

r_user_plot <- glue('{fig_dir}/r_user_radial_bplot.png')
ggsave(filename = r_user_plot, width = 8, height = 8, bg = "transparent")

# Programming languages
# clean up
prog_lang <- separate_rows(survey, prog_languages, sep = ",") %>%
  prep_prog_lang() %>%
  filter(!prog_languages %in% c("", NA)) %>%
  group_by(prog_languages) %>%
  tally(name='n_users') %>%
  top_n(10, wt = n_users)
  
ggplot(prog_lang, aes(area=n_users, fill = prog_languages)) +
  geom_treemap(color='gray10', show.legend = F) +
  geom_treemap_text(aes(label=glue('{prog_languages} ({n_users})')), color='gray10', size=15, place='center') +
  guides(fill=guide_legend(title = "Programming Languages")) + 
  scale_fill_d3("category20")

prog_lang_tplot <- glue('{fig_dir}/survey_progLang_treeplot.png')
ggsave(filename = prog_lang_tplot, width = 7, height = 7)

####

# 11. Tasks people do w/ R
task_df <- separate_rows(survey, r_tasks_simplified, sep = ",") %>% 
  group_by(r_tasks_simplified) %>% 
  tally() %>% 
  drop_na() %>% 
  arrange(-n)

ggplot(task_df, aes(reorder(r_tasks_simplified, -n), n)) +
  geom_bar(stat='identity', width = 0.75, position='dodge', fill='#09587C') +
  ylab('number of responses') +
  xlab('R tasks') +
  theme_gray(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

task_plot <- glue('{fig_dir}/survey_Rtasks.png')
ggsave(filename = task_plot, width = 7, height = 5)

####

# 12. R packages people use will be super helpful
map_png <- '/Users/tinacormier/Documents/presentations/RStudio2020/presentation_figures/global_map01.png'

survey$r_packages_clean <- tolower(survey$r_packages_clean)

r_pckg <- separate_rows(survey, r_packages_clean, sep = ",") %>%
  group_by(r_packages_clean) %>%
  tally(name = 'freq') %>%
  drop_na() %>% 
  filter(r_packages_clean != "" & r_packages_clean != "don't know") %>%
  arrange(-freq) %>%
  rename(word = r_packages_clean)

color <-  c('gray15','#999999','#ED5747','#ED5747','#ED5747','#ED5747','#ED5747','#ED5747','#ED5747',
            '#09587C','#09587C','#09587C','#09587C','#09587C','#09587C','#09587C')

wc <- wordcloud(r_pckg$word, 
          r_pckg$freq, 
          colors = color, 
          min.freq = 2, 
          scale=c(10, 0.8)
          )

