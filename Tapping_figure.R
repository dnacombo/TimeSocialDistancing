# Tapping figure

# Country selection
params <- list(c("JP", "IT","FR","CA","GR","DE","GB","TR","IN","CO"), "Tapping")
names(params) <- c("Country", "ExperimentName")

print(params$Country)
Countries <- countrycode(params$Country, origin = 'iso2c', destination = 'country.name')
Countries

# ITI calculation function
ITIs_free <- function(orig) {

  ITIs_tbl <- orig %>%
    mutate(
      Reaction_Time = as.numeric(Reaction_Time),
      Session = as.factor(Session),
      UniqueName = as.factor(Unique_Name),
      Run = as.factor(Run),
      PID = as.factor(PID),
    ) %>%
    clean_names() %>%
    filter(!(is.na(reaction_time)) & !(is.na(zone_type))) %>%
    group_by(country,
             session,
             pid,
             run,
             randomise_blocks) %>%
    mutate(aux_RT = if_else(zone_type=="continue_button",
                            reaction_time,
                            0),
           cum_aux_RT = cumsum(aux_RT),
           cumm_RT = if_else(zone_type=="continue_button",
                             cum_aux_RT,
                             if_else(zone_type=="response_keyboard_single",
                                     cum_aux_RT + reaction_time,
                                     0)),
           Tap = if_else(zone_type=="response_keyboard_single",
                         cumm_RT,
                         NA_real_),
           Stim = if_else(zone_type=="continue_button",
                          cumm_RT,
                          NA_real_),
           Stim_aux = lag(Stim),
           Tap_aux = lag(Tap),
           tap_nbr = row_number()) %>%
    mutate(ITI = if_else(zone_type=="response_keyboard_single",
                         Tap - Tap_aux,
                         NA_real_)) %>%
    select(-c(aux_RT,cum_aux_RT))
  return(ITIs_tbl)
}

# Files loading and appending
ITI_tbl = NULL
for (c in Countries) {
  filename <- list.files(path = here("Data_analysis/data"),
                         pattern = paste0("data-", params$ExperimentName, "_", c, "_*.*"))
  cat("---\n")
  cat(paste0(c,'\n'))
  cat(paste0(here("Data_analysis/data", filename),'\n'))
  load(here("Data_analysis/data", filename))
  ITIs_country <- ITIs_free(TSDdata) # ITI calculation
  ITI_tbl <- rbind(ITI_tbl, ITIs_country)
}

# Calculate median ITI per trial and filtering greater than 1 and smaller than 5000 (5s).
# Counting how many ITIs smaller than 50ms (button kept pressed) there are and discarding
# the trial if it has more than 30 of those.
summITI_free_trial <- ITI_tbl %>%
  filter(between(ITI, 1, 5000)) %>%
  group_by(country, session, pid, run) %>%
  summarise(mITI = median(ITI),
            n_low = sum(ITI<50)) %>%
  filter(n_low < 30) %>%
  ungroup()

# Outlier detection
outliersITI <- outliers_mad(x = summITI_free_trial$mITI)

summITI_free_trial <- summITI_free_trial %>%
  filter(between(mITI, outliersITI$limits[1], outliersITI$limits[2]) )

# Summary session and pid
summITI_free <- summITI_free_trial %>%
  group_by(country, session, pid) %>%
  summarise(mITI = median(mITI)) %>%
  ungroup()

# n by country and session
ITI_n_country_free <- summITI_free %>%
  group_by(country, session) %>%
  summarise(N = n(),
            medITI = median(mITI)) %>%
  ungroup()

ITI_n_session_free <- summITI_free %>%
  group_by(session) %>%
  summarise(N = n(),
            medITI = median(mITI)) %>%
  ungroup()

# Figure only session 1 ----

# Plotting
p1_paper_ISI <- summITI_free %>%
  filter(session == "S1") %>%
  # rbind(tibble(country = "AR", run = "InSync_Cont", pid = NA, 
  #              mITI = NA, session = "S1", n_low = 0)) %>%
  rbind(tibble(country = "AR", pid = NA, 
               mITI = NA, session = "S1")) %>%
  ggplot(aes(x = mITI/1000,
             y = reorder(country, desc(country)))) +
  stat_interval() +
  geom_text(data = ITI_n_country_free %>% filter(session == "S1"),
            aes(x = medITI/1000,
                y = country,
                label = as.character(paste0("n=",N))),
            nudge_y = .4,
            size = 3,
            color = "#0B585F") +
  facet_grid(. ~ session) +
  scale_color_carto_d(palette = "Mint") +
  theme_pubclean() +
  scale_x_continuous(limits = c(0, max(summITI_free$mITI)/1000)) +
  labs(x = "ITI (s)",
       y = "Country") +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major.x = element_line(color = "grey90"),
        panel.grid.major.y = element_line(linetype = "solid", color = "grey90"),
        plot.margin = margin(t = 0, r = 0, b = 6, l = 0, unit = "pt")) 

# New facet label names for dose variable
session.labs <- c("Free tapping")
names(session.labs) <- c("S1")

p2_paper_ISI <- summITI_free %>%
  filter(session == "S1") %>%
  ggplot(aes(x = mITI/1000)) +
  geom_histogram(aes(x = mITI/1000,
                     y = ..density..),
                 bins = 30,
                 fill = "#FDE0C5",
                 color = darken("#FDE0C5", 0.2)) +
  stat_pointinterval(aes(x = mITI/1000), 
               y = 0,
               fill = "#FDE0C5",
               color = "#EB4940") +
  geom_label(data = ITI_n_session_free %>% filter(session == "S1") ,
             aes(x = medITI/1000,
                 y = -0.5,#-1,
                 label = as.character(paste0("n=", N, 
                                             "\nmedian=", round(medITI/1000, digits = 2), " s"))),
             nudge_y = .4,
             size = 3,
             vjust = 1,
             color = "#EB4940") +
  theme_pubclean() +
  facet_grid(. ~ session,
             labeller = labeller(session = session.labs)) +
  scale_x_continuous(limits = c(0, max(summITI_free$mITI)/1000)) +
  # scale_y_continuous(limits = c(-3, 7),
  #                    breaks = 0) +
  scale_y_continuous(breaks = 0) +
  labs(x = NULL,
       y = "Pooled") +
  theme_pubclean() +
  theme(legend.position = "none",
        strip.background = element_rect(colour = "#B3D9CC",
                                        fill = "#B3D9CC",),
        axis.title.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color = "grey90", linetype = "solid"),
        panel.grid.major.x = element_line(color = "grey90"),
        plot.margin = margin(t = 6, r = 0, b = 0, l = 0, unit = "pt"))

fig_paper_free_tapping <- p2_paper_ISI / p1_paper_ISI + plot_layout(heights = c(1, 2))

