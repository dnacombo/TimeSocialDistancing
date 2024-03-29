# TapVis panel (synchronization + continuation)

# Country selection
params <- list(c("AR", "FR", "JP","IT","CA","GR","DE","GB","TR","IN","CO"), "TapVis")
names(params) <- c("Country", "ExperimentName")

print(params$Country)
Countries <- countrycode(params$Country, origin = 'iso2c', destination = 'country.name')
Countries

# data preprocessing
prepro <- function(orig) {
  data_prepro <- orig %>%
    mutate(
      Reaction_Time = as.numeric(Reaction_Time),
      Session = as.factor(Session),
      UniqueName = as.factor(Unique_Name),
      Run = as.factor(Run),
      PID = as.factor(PID)) %>%
    clean_names() %>%
    filter(!(is.na(reaction_time)) & !(is.na(zone_type))) %>%
    group_by(country, session, pid, run, randomise_blocks) %>%
    mutate(aux_RT = if_else(zone_type=="gonogo",
                            reaction_time,
                            0),
           cum_aux_RT = cumsum(aux_RT),
           cumm_RT = if_else(zone_type=="gonogo",
                             cum_aux_RT,
                             if_else(zone_type=="response_keyboard_single",
                                     cum_aux_RT + reaction_time,
                                     0)),
           Tap = if_else(zone_type=="response_keyboard_single",
                         cumm_RT,
                         NA_real_),
           Stim = if_else(zone_type=="gonogo",
                          cumm_RT,
                          NA_real_),
           Stim_aux = lag(Stim),
           Tap_aux = lag(Tap)) %>%
    fill(Stim_aux, .direction = "down") %>%
    fill(Tap_aux, .direction = "down") %>%
    mutate(Stim = if_else(display=="OutSync",
                          Stim + (Stim_aux-Stim)/2,
                          Stim),
           ITI = if_else(zone_type=="response_keyboard_single",
                         Tap - Tap_aux,
                         NA_real_)) %>%
    ungroup() %>%
    group_by(country, session, pid, run, display) %>%
    mutate(tap_nbr = cumsum(!is.na(Tap)),
           n_taps = sum(!is.na(Tap)),
           n_stims = sum(!is.na(Stim))) %>%
    mutate(tap_nbr = if_else(!is.na(Tap),tap_nbr,NA_integer_)) %>%
    ungroup() %>%
    group_by(country, session, pid, run, randomise_blocks) %>%
    mutate(event_idx_min = min(event_index,na.rm=TRUE)) %>%
    mutate(disp_type = case_when(display %in% c("InSync","OutSync")           ~ "synch",
                                 display == "Debrief"                         ~ "debrief",
                                 display %in% c("InSync_Cont","OutSync_Cont") ~ "cont",
                                 display %in% c("InSync_Conf","OutSync_Conf") ~ "conf",
                                 TRUE ~ "inst")) %>%
    group_by(country, session, pid, run, disp_type) %>%
    mutate(event_idx_max = max(event_idx_min,na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(randomise_order = if_else(display=="Debrief", "NA",
                                     if_else(event_idx_min<event_idx_max, "1",
                                             "2"))) %>%
    select(-c(event_idx_min,event_idx_max)) %>%
    ungroup() %>%
    select(all_of(c("country_name", "country", "session", "unique_name", "run", "pid", "event_index",
                    "experiment_id", "zone_type", "reaction_time", "response", "randomise_blocks",
#                    "display", "disp_type", "stringency_index", "n_stims", "n_taps", "tap_nbr",
                    "display", "disp_type", "n_stims", "n_taps", "tap_nbr",
                    "participant_os", "participant_browser",
                    "handedness", "sex", "age", "randomise_order", "Stim", "Tap", "ITI")))

  return(data_prepro)
}

# Files loading and appending ----------------------------------------------------------------------------------------
data_prepro_tbl = NULL
for (c in Countries) {
#  filename <- list.files(path = here("data"),
  filename <- list.files(path = here("Data_analysis/data"),
                pattern = paste0("data-", params$ExperimentName, "_", c, "_*.*"))
  cat("---\n")
  cat(paste0(c,'\n'))
#  cat(paste0(here("data", filename),'\n'))
#  load(here("data", filename))
  cat(paste0(here("Data_analysis/data", filename),'\n'))
  load(here("Data_analysis/data", filename))
  data_prepro_country <- prepro(TSDdata) # ITI calculation
  data_prepro_tbl <- rbind(data_prepro_tbl, data_prepro_country)
}

# # Make OS and browser names uniform
# data_prepro_tbl <- data_prepro_tbl %>%
#   mutate(Operating_System = case_when(grepl("Windows",participant_os) ~ "Windows",
#                                       grepl("Mac",participant_os) ~ "Mac",
#                                       grepl("Linux",participant_os) | grepl("Ubuntu",participant_os) ~ "Linux",
#                                       TRUE ~ "Other"),
#          Browser = case_when(grepl("Chrome",participant_browser) ~ "Chrome",
#                              grepl("Firefox",participant_browser) ~ "Firefox",
#                              grepl("Edge",participant_browser) ~ "Edge",
#                              TRUE ~ "Other"))

# Participants failing -----------------------------------------------
data_prepro_tbl <- data_prepro_tbl %>%
  # FR
  filter(pid != 1292928) %>%
  filter(pid != 1381670) %>%
  # JP
  filter(pid != 4072160)
  
# Outliers glitches ------------------------------------------------------------
outliers_glitches_synch <- data_prepro_tbl %>%
  filter(
    # remove very long trials (glitch? twice the expected number of stimuli)
    (disp_type=="synch" & n_stims > 100) |
    # remove trials with very long key presses -> too many responses)s
    (disp_type=="synch" & n_taps > 100))

outliers_glitches_cont <- data_prepro_tbl %>%
  filter(
    # remove trials with very long key presses -> too many responses)s
    (disp_type=="cont" & n_taps > 300))


# Remove outlier synchronization trials (and continuation trials corresponding to removed synch trials)
data_prepro_tbl <- anti_join(data_prepro_tbl, outliers_glitches_synch, by=c("country","session","pid","run","randomise_blocks"))
# Remove outlier continuation trials
data_prepro_tbl <- anti_join(data_prepro_tbl, outliers_glitches_cont, by=names(data_prepro_tbl))
# data_prepro_tbl <- anti_join(data_prepro_tbl, outliers_glitches, by=names(data_prepro_tbl))


## Asyn ------------------------------------------------------------------------
## Outliers transient ----------------------------------------------------------
transient_length <- 5
outliers_transient <- data_prepro_tbl %>%
  # remove first taps in synchronization
  filter(disp_type=="synch" & tap_nbr <= transient_length)

data_prepro_tbl <- anti_join(data_prepro_tbl, outliers_transient, by=names(data_prepro_tbl))


## Phasediff -------------------------------------------------------------------
phasediff_tbl <- data_prepro_tbl %>%
  filter(disp_type=="synch") %>%
  group_by(country, pid, session, run, randomise_blocks, randomise_order, display) %>%
  nest() %>%
  mutate(stims = purrr::map(data, ~.x$Stim[.x$zone_type=="gonogo"]),
         responses = purrr::map(data, ~.x$Tap[.x$zone_type!="gonogo"]),
         phasediffs.df = purrr::map2(stims, responses, ~compute_phasediff(.x, .y))
  ) %>%
  select(-c(data, stims, responses)) %>%
  unnest(cols = phasediffs.df) %>%
  ungroup()

phase_measures_tbl <- phasediff_tbl %>%
  group_by(country, session, pid, run, randomise_blocks, randomise_order, display) %>%
  summarize(
    # Phase-Locking Value (Lachau et al 1999, adapted to paced finger tapping)
    plv=abs(sum(phasor,na.rm=TRUE))/n(),
    # Circular Mean
    circ_mean=mean.circular(phase_diff_pi,na.rm=TRUE),
    # Circular Standard Deviation
    circ_sd=sd.circular(phase_diff_pi,na.rm=TRUE),
    # Circular-Linear Regression (Kempter et al 2012)
    cl.regress(event_time,phase_diff_mod)) %>%
  ungroup()

circmean_stats <- phase_measures_tbl %>%
  group_by(country, session, run, randomise_blocks, display) %>%
  summarize(out_limit = boxplot.stats(as.numeric(circ_mean))$stats[c(1,5)]) %>%
  ungroup()

# Trial-level measures ---------------------------------------------------------
asynchronies_tbl <- data_prepro_tbl %>%
  filter(display %in% c("InSync", "OutSync")) %>%
  group_by(country,
           pid,
           session,
           run,
           randomise_blocks,
           randomise_order,
           display) %>%
  nest() %>%
  mutate(stims = purrr::map(data,
                     ~.x$Stim[.x$zone_type=="gonogo"]),
         responses = purrr::map(data,
                         ~.x$Tap[.x$zone_type=="response_keyboard_single"]),
         asyns.df = purrr::map2(stims,
                         responses,
                         ~compute_asyn(.x, .y)),
  ) %>%
  select(-c(data, stims, responses)) %>%
  unnest(cols = c(asyns.df)) %>%
  mutate(asyn_rel = asyn - mean(asyn,na.rm=TRUE)) %>%
  ungroup()

asyn_measures_tbl <- asynchronies_tbl %>%
  group_by(country,session,pid,run,randomise_blocks,randomise_order,display) %>%
  summarize(
    # trial mean asynchrony
    asyn_mean=mean(asyn,na.rm=TRUE),
    # trial standard deviation of asynchronies
    asyn_sd=sd(asyn,na.rm=TRUE),
    # trial median asynchrony
    asyn_median=median(asyn,na.rm=TRUE),
    # trial median asynchrony
    asyn_mad=mad(asyn,na.rm=TRUE),
    # number of assigned responses per trial
    nasyn=sum(!is.na(asyn)),
    nbr_large_asyns = ifelse(first(display=="InSync"),
                             sum(asyn > 200,na.rm=TRUE),
                             sum(asyn < -200,na.rm=TRUE))) %>%
  ungroup()

clustering_tbl <- asyn_measures_tbl %>%
  select(country,session,pid,run,display, randomise_blocks,
         asyn_mean,asyn_sd,asyn_median,asyn_mad,nbr_large_asyns) %>%
  group_by(country,session,display) %>%
  drop_na() %>%
  mutate(mclust_fn(tibble(asyn_median,asyn_mad,nbr_large_asyns),
                   ncnters=1:9,select=3,above=TRUE,threshold=15)) %>%
  ungroup()


## Remove outliers of synchronization ------------------------------------------
# In the following chunks, outliers are removed with anti_join. Importantly,
# the "by" argument removes both the detected synchronization trial (InSync or
# OutSync) and the corresponding continuation trial (InSync_Cont or
# OutSync_Cont) that are grouped by the randomise_blocks variable


## Outliers slope --------------------------------------------------------------
# free-running/phase-drifting (non-synchronized subjects): phase makes at least
# half of a full rotation (pi) during the trial (60000 ms)
slope_thr <- pi/60000
outliers_slope <- phase_measures_tbl %>%
  filter(abs(phase_slope)>slope_thr)

data_prepro_tbl <- anti_join(data_prepro_tbl, outliers_slope, by=c("country","session","pid","run","randomise_blocks"))

## Outliers circmean -----------------------------------------------------------
# subjects confusing instructions: mean phase outliers (e.g. OutSync when InSync)
outliers_circmean <- phase_measures_tbl %>%
  group_by(country, session, run, display) %>%
  filter(!do.call(between,c(list(as.numeric(circ_mean)),boxplot.stats(as.numeric(circ_mean))$stats[c(1,5)]))) %>%
  ungroup()

data_prepro_tbl <- anti_join(data_prepro_tbl, outliers_circmean, by=c("country","session","pid","run","randomise_blocks"))

## Outliers plv ----------------------------------------------------------------
# exceedingly variable trials (low-outliers of the PLV distribution)
outliers_plv <- phase_measures_tbl %>%
  group_by(country, session, run, display) %>%
  filter(plv < boxplot.stats(plv)$stats[1]) %>%
  ungroup()

data_prepro_tbl <- anti_join(data_prepro_tbl,outliers_plv, by=c("country","session","pid","run","randomise_blocks"))

## Outliers circsd -------------------------------------------------------------
# exceedingly variable trials (hi-outliers of the Circ_SD distribution)
outliers_circsd <- phase_measures_tbl %>%
  group_by(country, session, run, display) %>%
  filter(circ_sd > boxplot.stats(circ_sd)$stats[5]) %>%
  ungroup()

data_prepro_tbl <- anti_join(data_prepro_tbl, outliers_circsd, by=c("country","session","pid","run","randomise_blocks"))

## Outliers clustering ---------------------------------------------------------
# subjects reacting instead of synchronizing:
outliers_cluster <- clustering_tbl %>%
  group_by(country,session,display) %>%
  filter(cluster %in% clust_out[[1]]) %>%
  ungroup()

data_prepro_tbl <- anti_join(data_prepro_tbl, outliers_cluster, by=c("country","session","pid","run","randomise_blocks"))

## Outliers nasyn --------------------------------------------------------------
# trials with too few surviving responses (half the number of stimuli or less)
outliers_nasyn <- asyn_measures_tbl %>%
  filter(nasyn<30)

data_prepro_tbl <- anti_join(data_prepro_tbl, outliers_nasyn, by=c("country","session","pid","run","randomise_blocks"))


## Figure distribution ---------------------------------------------------------
asynchronies_tbl <- data_prepro_tbl %>%
  filter(disp_type == "synch") %>%
  group_by(country, pid, session, run, randomise_order, display) %>%
  nest() %>%
  mutate(stims = purrr::map(data, ~.x$Stim[.x$zone_type=="gonogo"]),
         responses = purrr::map(data, ~.x$Tap[.x$zone_type=="response_keyboard_single"]),
         asyns.df = purrr::map2(stims, responses,~compute_asyn(.x, .y))
  ) %>%
  select(-c(data, stims, responses)) %>%
  unnest(cols = c(asyns.df)) %>%
  mutate(asyn_rel = asyn - mean(asyn,na.rm=TRUE)) %>%
  ungroup() %>%
  drop_na()



## Base figure Asyn .-----------------------------------------------------------
# Summary by subject
summAsyn_trial <- asynchronies_tbl %>%
  group_by(country, session, display, pid, randomise_order) %>%
  summarise(m_asyn = median(asyn)) %>%
  ungroup()

summAsyn <- summAsyn_trial %>%
  group_by(country, session, display, pid) %>%
  summarise(m_asyn = median(m_asyn)) %>%
  ungroup()

# get info about OS and Browser
aux_Asyn_os_browser <- data_prepro_tbl %>%
  group_by(country, session, display, pid) %>%
  # Make OS and browser names uniform
  summarise(Operating_System = case_when(first(grepl("Windows",participant_os)) ~ "Windows",
                                         first(grepl("Mac",participant_os)) ~ "Mac",
                                         first(grepl("Linux",participant_os) | grepl("Ubuntu",participant_os)) ~ "Linux",
                                         TRUE ~ "Other"),
            Browser = case_when(first(grepl("Chrome",participant_browser)) ~ "Chrome",
                                first(grepl("Firefox",participant_browser)) ~ "Firefox",
                                first(grepl("Edge",participant_browser)) ~ "Edge",
                                TRUE ~ "Other")) %>%
  ungroup()
# add info about os and browser to tibble
summAsyn_os_browser <- inner_join(summAsyn, aux_Asyn_os_browser, by=c("country", "session", "display", "pid"))
summAsyn_os_browser <- filter(summAsyn_os_browser, display=="InSync")

# Summary OS
#summAsyn_os <- summAsyn_trial_os_browser %>%
summAsyn_os <- summAsyn_os_browser %>%
  group_by(Operating_System) %>%
  summarise(meanmAsyn = mean(m_asyn), # mean across trials of mean trial asynchrony
            sterrmAsyn = stats::sd(m_asyn, na.rm=TRUE)/sqrt(n()),
            N = n()) %>%
  ungroup()
# Reorder factor levels
summAsyn_os$Operating_System <- factor(summAsyn_os$Operating_System,
                                      c("Windows", "Mac", "Linux", "Other"))

# Summary Browser
#summAsyn_browser <- summAsyn_trial_os_browser %>%
summAsyn_browser <- summAsyn_os_browser %>%
  group_by(Browser) %>%
  summarise(meanmAsyn = mean(m_asyn), # mean across trials of mean trial asynchrony
            sterrmAsyn = stats::sd(m_asyn, na.rm=TRUE)/sqrt(n()),
            N = n()) %>%
  ungroup()
# Reorder factor levels
summAsyn_browser$Browser <- factor(summAsyn_browser$Browser,
                                       c("Chrome", "Firefox", "Edge", "Other"))



# n by country and session
asyn_n_country <- summAsyn %>%
  group_by(country, session, display) %>%
  summarise(N = n(),
            med_asyn = median(m_asyn))

asyn_n_session <- summAsyn %>%
  group_by(session, display) %>%
  summarise(N = n(),
            med_asyn = median(m_asyn))



# ITI - Free tapping -----------------------------------------------------------
# Summary by country, session and pid
summITI_trial <- data_prepro_tbl %>%
  filter(disp_type=="cont") %>%
  filter(between(ITI, 1, 5000)) %>%
  group_by(country, session, display, pid, randomise_blocks) %>%
  summarise(mITI = median(ITI),
            n_low = sum(ITI<50)) %>%
  filter(n_low < 30) %>%
  ungroup()

# Outlier detection
# Hacer por country session y display
outliersITI <- outliers_mad(x = summITI_trial$mITI, threshold = 6)

summITI_trial <- summITI_trial %>%
  filter(between(mITI, outliersITI$limits[1], outliersITI$limits[2])) %>%
  ungroup()

# Summary by country, session and pid
summITI <- summITI_trial %>%
  group_by(country, session, display, pid) %>%
  summarise(mITI = median(mITI)) %>%
  ungroup()

# add info about OS and Browser
summITI_os_browser <- inner_join(summITI, aux_Asyn_os_browser, by=c("country", "session", "display", "pid"))
summITI_os_browser <- filter(summITI_os_browser, display=="InSync_Cont")

# Summary OS
summITI_os <- summITI_os_browser %>%
  group_by(Operating_System) %>%
  summarise(meanmITI = mean(mITI),
            sterrmITI = stats::sd(mITI, na.rm=TRUE)/sqrt(n()),
            N = n()) %>%
  ungroup()
# Reorder factor levels
summITI_os$Operating_System <- factor(summITI_os$Operating_System,
                                       c("Windows", "Mac", "Linux", "Other"))

# Summary Browser
summITI_browser <- summITI_os_browser %>%
  group_by(Browser) %>%
  summarise(meanmITI = mean(mITI),
            sterrmITI = stats::sd(mITI, na.rm=TRUE)/sqrt(n()),
            N = n()) %>%
  ungroup()
# Reorder factor levels
summITI_browser$Browser <- factor(summITI_browser$Browser,
                                   c("Chrome", "Firefox", "Edge", "Other"))


# n by country and session
ITI_n_country <- summITI %>%
  group_by(country, session, display) %>%
  summarise(N = n(),
            medITI = median(mITI)) %>%
  ungroup()

ITI_n_session <- summITI %>%
  group_by(session, display) %>%
  summarise(N = n(),
            medITI = median(mITI)) %>%
  ungroup()


## Figure only session 1 -------------------------------------------------------
## ITI ----

display_filter <- "InSync_Cont" # "InSync" o "OutSync"

p1_paper_cont <- summITI %>%
  filter((display == display_filter) & (session == "S1")) %>%
  ggplot(aes(x = mITI/1000,
             y = reorder(country, desc(country)))) +
  stat_interval() +
  geom_text(data = ITI_n_country %>%  filter((display == display_filter) & (session == "S1")),
            aes(x = medITI/1000,
                y = country,
                label = as.character(paste0("n=",N))),
            nudge_y = .4,
            size = 3,
            color = "#0B585F") +
  facet_grid(. ~ session) +
  scale_color_carto_d(palette = "Mint") +
  theme_pubclean() +
  # scale_x_continuous(limits = c(min(summITI$mITI)/1000, 
  #                               max(summITI$mITI)/1000),
  #                     breaks = c(0.5, 1, 1.5)) +
  scale_x_continuous(limits = c(0.5,1.5),
                     breaks = c(0.75, 1, 1.25)) +
  labs(x = "ITI (s)",
       y = NULL) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey90"),
        panel.grid.major.y = element_line(linetype = "solid", color = "grey90"),
        plot.margin = margin(t = 0, r = 0, b = 6, l = 0, unit = "pt")) 

session.labs <- c("Continuation")
names(session.labs) <- c("S1")

p2_paper_cont <- summITI %>%
  filter((display == display_filter) & (session == "S1")) %>%
  ggplot(aes(x = mITI/1000)) +
  geom_histogram(aes(x = mITI/1000,
                     y = ..density..), 
                 bins = 25,
                 fill = "#FDE0C5",
                 color = darken("#FDE0C5", 0.2)) +
  stat_pointinterval(aes(x = mITI/1000), 
                     y = 0,
                     fill = "#FDE0C5",
                     color = "#EB4940") +
  geom_label(data = ITI_n_session %>%  filter((display == display_filter) & (session == "S1")),
             aes(x = medITI/1000,
                 y = -1,
                 label = as.character(paste0("n=", N, 
                                             "\nmedian=", round(medITI/1000, digits = 2), " s"))),
             nudge_y = .4,
             size = 3,
             vjust = 1,
             color = "#EB4940") +
  facet_grid(. ~ session,
             labeller = labeller(session = session.labs)) +
  # scale_x_continuous(limits = c(min(summITI$mITI)/1000,
  #                               max(summITI$mITI)/1000),
  scale_x_continuous(limits = c(0.5,1.5),
                     breaks = c(0.75, 1, 1.25),
                      labels = c("0.75", "1", "1.25")) +
  scale_y_continuous(limits = c(-3, 7),
                     breaks = 0) +
  labs(x = NULL,
       y = NULL) +
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

fig_paper_continuation <- p2_paper_cont / p1_paper_cont + plot_layout(heights = c(1, 2))


# Asyn -------------------------------------------------------------------------

display_filter <- "InSync" # "InSync" o "OutSync"

p1_paper_asyn <- summAsyn %>%
  filter((display == display_filter) & (session == "S1")) %>%
  ggplot(aes(x = m_asyn/1000,
             y = reorder(country, desc(country)))) +
  stat_interval() +
  geom_text(data = asyn_n_country %>% filter((display == display_filter) & (session == "S1")),
            aes(x = med_asyn/1000,
                y = country,
                label = as.character(paste0("n=",N))),
            nudge_y = .4,
            size = 3,
            color = "#0B585F") +
  facet_grid(. ~ session) +
  scale_color_carto_d(palette = "Mint") +
  theme_pubclean() +
  scale_x_continuous(limits = c(min(summAsyn$m_asyn)/1000, 
                                max(summAsyn$m_asyn)/1000),
                     breaks = c(-0.25, 0, 0.25),
                     labels = c("-0.25", "0", "0.25")) +
  labs(x ="Asynchrony (s)",
       y = NULL) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey90"),
        panel.grid.major.y = element_line(linetype = "solid", color = "grey90"),
        plot.margin = margin(t = 0, r = 0, b = 6, l = 0, unit = "pt")) 

session.labs <- c("Synchronization")
names(session.labs) <- c("S1")

p2_paper_asyn  <- summAsyn %>%
  filter((display == display_filter) & (session == "S1")) %>%
  ggplot(aes(x = m_asyn/1000)) +
  geom_histogram(aes(x = m_asyn/1000,
                     y = ..density..), 
                 bins = 25,
                 fill = "#FDE0C5",
                 color = darken("#FDE0C5", 0.2)) +
  stat_pointinterval(aes(x = m_asyn/1000), 
                     y = 0,
                     fill = "#FDE0C5",
                     color = "#EB4940") +
  geom_label(data = asyn_n_session %>% filter((display == display_filter) & (session == "S1")),
             aes(x = med_asyn/1000,
                 y = -1,
                 label = as.character(paste0("n=", N, 
                                             "\nmedian=", round(med_asyn/1000, digits = 2), " s"))),
             nudge_y = .4,
             size = 3,
             vjust = 1,
             color = "#EB4940") +
  facet_grid(. ~ session,
             labeller = labeller(session = session.labs)) +
  scale_x_continuous(limits = c(min(summAsyn$m_asyn)/1000,
                                max(summAsyn$m_asyn)/1000),
                     breaks = c(-0.25, 0, 0.25),
                    labels = c("-0.25", "0", "0.25")) +
  scale_y_continuous(limits = c(-3, 7),
                     breaks = 0) +
  labs(x = NULL,
       y = NULL) +
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

fig_paper_synchronization <- p2_paper_asyn / p1_paper_asyn + plot_layout(heights = c(1, 2))



# Supplementary Figure: split by OS and Browser
# All sessions, countries, etc
p1_supp_Asynqty_browser <- summAsyn_browser %>%
  ggplot(aes(x=Browser, y=N, label=N)) + 
  geom_bar(stat="identity") + 
  geom_label(size=3) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title="Split by Browser", y="Count")

p1_supp_Asynqty_os <- summAsyn_os %>%
  ggplot(aes(x=Operating_System, y=N, label=N)) + 
  geom_bar(stat="identity") + 
  geom_label(size=3) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title="Split by OS", y="Count")

p1_supp_Asynvalue_browser <- summAsyn_browser %>%
  ggplot(aes(x=Browser, y=meanmAsyn, fill=Browser)) + 
  geom_bar(stat="identity") + 
  geom_linerange(aes(ymin=meanmAsyn-sterrmAsyn, ymax=meanmAsyn+sterrmAsyn, colour=Browser)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y="mean trial Asynchrony (ms)")

p1_supp_Asynvalue_os <- summAsyn_os %>%
  ggplot(aes(x=Operating_System, y=meanmAsyn, fill=Operating_System)) + 
  geom_bar(stat="identity") + 
  geom_linerange(aes(x=Operating_System, ymin=meanmAsyn-sterrmAsyn, ymax=meanmAsyn+sterrmAsyn, colour=Operating_System)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y="mean trial Asynchrony (ms)")


p1_supp_ITI_qty_browser <- summITI_browser %>%
  ggplot(aes(x=Browser, y=N, label=N)) + 
  geom_bar(stat="identity") + 
  geom_label(size=3) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title="Split by Browser", y="Count")

p1_supp_ITI_qty_os <- summITI_os %>%
  ggplot(aes(x=Operating_System, y=N, label=N)) + 
  geom_bar(stat="identity") + 
  geom_label(size=3) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title="Split by OS", y="Count")

p1_supp_ITI_value_browser <- summITI_browser %>%
  ggplot(aes(x=Browser, y=meanmITI, fill=Browser)) + 
  geom_bar(stat="identity") + 
  geom_linerange(aes(ymin=meanmITI-sterrmITI, ymax=meanmITI+sterrmITI, colour=Browser)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y="median trial ITI (ms)")

p1_supp_ITI_value_os <- summITI_os %>%
  ggplot(aes(x=Operating_System, y=meanmITI, fill=Operating_System)) + 
  geom_bar(stat="identity") + 
  geom_linerange(aes(x=Operating_System, ymin=meanmITI-sterrmITI, ymax=meanmITI+sterrmITI, colour=Operating_System)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y="median trial ITI (ms)")



