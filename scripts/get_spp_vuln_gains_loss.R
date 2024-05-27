
# Load libraries ----------------------------------------------------------
library(pacman)
p_load(crayon, dplyr, ggplot2, scales, glue, qs)

# path <- './inputs/species_info'
# # Read ACAD values file (file modified to keep only columns of interest 
# #(e.g. BCR, group, Scientific.Name, X.pop.b, AI.b, X.pop.w) 
# acad <- read.csv(glue('{path}/Regional_ACAD_AR.csv'))
# 
# 
# # Read the file containing 4letter species code. 
# #Downloaded from https://www.birdpop.org/pages/birdSpeciesCodes.php
# codes <- read.csv(glue('{path}/Sp_Codes_2024.csv'))
# 
# # Join the two data frames based on the Scientific Name column
# ACAD_df <- acad %>% left_join(codes %>% select(SCINAME, SPEC), 
#                                by = c('Scientific.Name' = 'SCINAME'))
# 
# # Check if there are any species that don't have 4-letter code
# nocode_sp <- ACAD_df[is.na(ACAD_df$SPEC), ]
# unique(nocode_sp$Common.Name)
# 
# #add the codes using the file from: https://www.birdpop.org/docs/misc/Alpha_codes_eng.pdf
# ACAD_df <- ACAD_df %>% 
#   mutate(SPEC = ifelse(Common.Name == 'Cordilleran Flycatcher', 'COFL', SPEC),
#          SPEC = ifelse(Common.Name == 'Crimson-collared Grosbeak', 'CCGF', SPEC),
#          SPEC = ifelse(Common.Name == 'Paint-billed Crake', 'PBCR', SPEC),
#          SPEC = ifelse(Common.Name == 'Ocellated Crake', 'OCCR', SPEC),
#          SPEC = ifelse(Common.Name == 'Tiny Hawk', 'TIHA', SPEC),
#          SPEC = ifelse(Common.Name == 'Blue-crowned Manakin', 'THAN', SPEC),
#          SPEC = ifelse(Common.Name == 'Thicket Antpitta', 'OCCR', SPEC),
#          SPEC = ifelse(Common.Name == 'Golden-crowned Flycatcher', 'GOCF', SPEC),
#          )
# 
# #check again 
# nocode_sp <- any(is.na(ACAD_df$SPEC))

#save file 
#qs::qsave(ACAD_df, './inputs/regionalACAD_AR_codes.qs')
groups <- c('arctic', 'aridlands', 'boreal_forests', 'coastal', 'eastern_forests',
            'generalists', 'grasslands', 'marshlands', 'subtropical', 'urban', 'waterbirds',
            'western_forests')
#Function to process ACAD and plot each group by BCR
process_and_plot <- function(group){
  message(crayon::blue(glue('Processing group: {group}')))
  
 ACAD_df <- qs::qread('./inputs/regionalACAD_AR_codes.qs')

 data<- qs::qread(glue('./outputs/vulnerabilityTbl/vulTbl_{group}.qs'))

# Create base plot  ------------------------------------------------------------
 # make a vector of colors
colors <- c('#0072B2', '#009E73', '#E69F00', '#D55E00', 'red')

# make a plot base
plot <-  ggplot() +
  geom_abline(intercept = 0.4, slope = 1, linewidth = 0.3, col = '#0072B2') +
  geom_abline(intercept = -0.1, slope = 1, linewidth = 0.3, col = '#009E73') +
  geom_abline(intercept = -0.3, slope = 1, linewidth = 0.3, col = '#E69F00') +
  geom_abline(intercept = -0.5, slope = 1, linewidth = 0.3, col = '#D55E00') +
  # geom_vline(xintercept = c(-0.1, -0.3, -0.5), linetype = 'solid', linewidth = 0.3, col = 'blue') +
  coord_cartesian(xlim = c(-0.1, 1.025), ylim = c(-0.05, 1.0)) +
  scale_x_continuous(breaks = seq(0, 1, 0.25), labels = c('0.0', '', '0.5', '', '1.0')) +
  scale_y_continuous(breaks = seq(0, 1.5, 0.25), labels = c('0.0', '', '0.5', '', '1.0', '', '1.5')) +
  xlab('\nProportion range loss') + ylab('Proportion range gain\n') +  
  ggtitle('Climate change vulnerability classifications\n',
          subtitle = str_to_title(glue('{group}'))) +
  theme_bw() + 
  theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0.5), 
        plot.subtitle = element_text(size = 14, face ='bold'),
        text = element_text(size = 13, face = 'plain'), 
        axis.title = element_text(size = 14, face = 'plain'), 
        panel.grid.major = element_line(colour = 'grey90', size = 0.1), 
        panel.grid.minor = element_blank(), 
        strip.background = element_rect(fill = 'azure3'),
        strip.text.x = element_text(size = 12, face = 'bold'))
#browser()
# format data
vuln <- data %>% filter(!(gain == 0 & loss == 0 & never == 1) & 
                          !is.na(gain) & !is.na(loss) & !is.na(never) &
                          CVb2 != 'No' & bcrID != 100) 

# Convert bcrID  and BCR to factor in the vuln and ACAD data frames
vuln <- vuln %>%
  mutate(bcrID = as.factor(bcrID))

ACAD_df <- ACAD_df %>% 
  mutate(BCR = as.factor(BCR))

#First, filter vuln based on matching species and bcrID from ACAD
filtered_vuln <- vuln %>%
  semi_join(ACAD_df, by = c("species" = "SPEC", 'bcrID' = 'BCR'))

#Second, filter by those species that have a value in X.pop.b and AI.b. column from ACAD
result <- filtered_vuln %>%
  semi_join(ACAD_df %>% filter(!is.na(X.pop.b) & !is.na(AI.b.)), 
            by = c("species" = "SPEC", "bcrID" = "BCR")) %>%
  mutate(included = TRUE)

# Identify the excluded rows
excluded_sp <- filtered_vuln %>%
  anti_join(ACAD_df %>% filter(!is.na(X.pop.b) & !is.na(AI.b.)), 
            by = c("species" = "SPEC", "bcrID" = "BCR")) %>%
  mutate(included = FALSE)

# Save excluded species 
qs::qsave(excluded_sp, glue('./tables/excluded_sp_{group}.qs'))

# plot vulnerability across groups
p <- plot + geom_point(data=result, aes(x = loss, y=  gain, 
                                        colour = factor(CVb2)), alpha = 0.8, size = 3) +
  scale_color_manual(values=colors, name='Vulnerability') +
  facet_wrap(~bcrID)

# create directory for saving the plots
 if(!dir.exists(out_dir)){
  dir.create(out, recursive = TRUE)}

out_dir <- './figs'

ggsave(filename= glue('{out_dir}/bcr_vulnerability_acad_filter_{group}.png'), p,
       height = 16, width = 20, dpi = 400)

message(crayon::magenta("Saved vulnerability plot"))

# Function to create top ten gains and losses plot ------------------------

create_top_ten_plot <- function(result, measure, group, out_dir){
  measure_col <- sym(measure)
  message(crayon:: blue(glue("Getting top ten data for {measure}")))
  result <- result %>%
    mutate(gain = gain * 100, loss = loss * 100)
  
  top_ten_bcr <- result %>%
    arrange(desc(!!measure_col)) %>%
    group_by(bcrID) %>%
    slice_head(n = 10) %>%
    ungroup()
  message(blue(glue("Created top ten data for {measure}")))
  
  plot <- ggplot(top_ten_bcr, aes(x = reorder(species, !!measure_col), y = !!measure_col, fill = species)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ggtitle(glue('Top 10 {measure} species {group}')) +
    labs(x = "Species", y = str_to_title(measure)) +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0.5), 
          plot.subtitle = element_text(size = 14, face ='bold'),
          text = element_text(size = 13, face = 'plain'), 
          axis.title = element_text(size = 14, face = 'plain'), 
          panel.grid.major = element_line(colour = 'grey90', size = 0.1), 
          panel.grid.minor = element_blank(), 
          strip.background = element_rect(fill = 'azure3'),
          strip.text.x = element_text(size = 12, face = 'bold')) +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    facet_wrap( ~ bcrID, scales = "free_y")
  
  ggsave(filename= glue('{out_dir}/top10_{measure}_{group}.png'), plot,
         height = 16, width = 20, dpi = 400) 
}
  # create directory for saving the plots
  out_dir <- './figs/gainsLoss'
  if(!dir.exists(out_dir)) {dir.create(out, recursive = TRUE)}
  
  # Top ten plots for gain
  create_top_ten_plot(result, "gain", group, out_dir)
  
  # Top ten plots for loss
  create_top_ten_plot(result, "loss", group, out_dir)
  message(crayon::cyan(glue("Done {group}!")))
}

# Apply the function to each group
lapply(groups, process_and_plot)





# 
# # Plot for greatest gain (produces barplot for species )
# gainPlot <- ggplot(ten_gain, aes(x = reorder(species, gain), y = gain, fill = species)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   ggtitle(glue('Top 10  winner species {group}')) +
#   labs(x = "Species", y = "Gain") +
#   theme_minimal()
# 
# # Get the top ten species with the greatest gain by BCR
# top_ten_gain_bcr <- result %>%
#   arrange(desc(gain)) %>%
#   group_by(bcrID) %>%
#   slice_head(n = 10) %>%
#   ungroup()
# 
# # Plot for greatest gain
# gainPlot_bcr <- ggplot(top_ten_gain_bcr, aes(x = reorder(species, gain), y = gain, fill = species)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   ggtitle(glue('Top 10  winner species {group}')) +
#   labs(x = "Species", y = "Gain") +
#   theme_bw() +
#   theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0.5), 
#         plot.subtitle = element_text(size = 14, face ='bold'),
#         text = element_text(size = 13, face = 'plain'), 
#         axis.title = element_text(size = 14, face = 'plain'), 
#         panel.grid.major = element_line(colour = 'grey90', size = 0.1), 
#         panel.grid.minor = element_blank(), 
#         strip.background = element_rect(fill = 'azure3'),
#         strip.text.x = element_text(size = 12, face = 'bold')) +
#   scale_y_continuous(labels = percent_format(scale = 1)) +
#   facet_wrap( ~ bcrID, scales = "free_y") 
# 
# # create directory for saving the plots
# out_dir <- './figs/gainsLoss'
# if(!dir.exists(out_dir)) {dir.create(out, recursive = TRUE)}
# 
# 
# ggsave(filename= glue('{out_dir}/top10_gains_{group}.png'), gainPlot_bcr,
#        height = 16, width = 20, dpi = 400)
# 
# # Get the top ten species with the greatest loss by BCR
# top_ten_loss_bcr <- result %>%
#   arrange(desc(loss)) %>%
#   group_by(bcrID) %>%
#   slice_head(n = 10) %>%
#   ungroup()
# 
# # Plot for greatest gain
# lossPlot_bcr <- ggplot(top_ten_loss_bcr, aes(x = reorder(species, loss), y = loss, fill = species)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   ggtitle(glue('Top 10 losser species {group}')) +
#   labs(x = "Species", y = "Loss") +
#   theme_bw() +
#   theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0.5), 
#         plot.subtitle = element_text(size = 14, face ='bold'),
#         text = element_text(size = 13, face = 'plain'), 
#         axis.title = element_text(size = 14, face = 'plain'), 
#         panel.grid.major = element_line(colour = 'grey90', size = 0.1), 
#         panel.grid.minor = element_blank(), 
#         strip.background = element_rect(fill = 'azure3'),
#         strip.text.x = element_text(size = 12, face = 'bold')) +
#   scale_y_continuous(labels = percent_format(scale = 1)) +
#   facet_wrap( ~ bcrID, scales = "free_y") 
# 
# # create directory for saving the plots
# out_dir <- './figs/gainsLoss'
# if(!dir.exists(out_dir)) {dir.create(out, recursive = TRUE)}
# 
# 
# ggsave(filename= glue('{out_dir}/top10_loss_{group}.png'), lossPlot_bcr,
#        height = 16, width = 20, dpi = 400)
