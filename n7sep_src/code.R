
### 1. SETTING UP ####

rm(list=ls())
library(purrr)
library(dplyr)
library(stringr)
library(lubridate)
library(readr)
library(tibble)
library(tidyr)
library(viridis)
library(ggplot2)
library(ggrepel)
library(broom)
library(forcats)
library(survey)
library(survival)
library(gridExtra)
library(scales)
library(cowplot)

# Load data ----
surv_com_week <- read.csv("Data/data1_surv_com_week.csv")
bl_exp <- read.csv("Data/data2_bl_exp.csv")
daily <-  read.csv("Data/data3_daily.csv") %>%
  mutate(day_date=as.Date(day_date))
  

# CODEBOOK ----
# Variables
# id_hh - household id
# id - individual id (household id + adult/child + number)

# Dataframes
# surv_com_week - each household for every week, whether completed survey or not
# bl_exp - individual-level data collected on enrollment
# daily - daily symptom reporting by each individual
### --- ------ --- ###


# Weekly survey completion ----

# proportion completed by individual sign up
surv_com_ind_sum <-
  surv_com_week %>% 
  group_by(id_hh) %>% 
  summarise(
    total=n(), 
    complete=length(which(wk_complete=="Complete"))
  ) %>%
  mutate(
    prop_complete=complete/total
  ) 

# Participants and follow-up time included ----

# include participants with at least 75% weekly surveys completed
bl <-
  bl_exp %>% 
  left_join(
    surv_com_ind_sum %>% select(id_hh, prop_complete), 
    by="id_hh"
  ) %>%
  filter(prop_complete>=0.75)

# follow-up time for these adults
dy<-
  daily %>%
  semi_join(bl, by="id") 

# Combine age groups ----

bl<-
  bl %>%
  mutate(
    agegrp2=case_when(
      agegrp %in% c("<=5", "6-16") ~ "<=16",
      agegrp %in% c("17-35", "36-55") ~ "17-55",
      agegrp %in% c("56-70") ~ "56-70",
      TRUE ~ ">70"
    )
  )


# Identification of infection syndromes ----

#New infection syndrome defined as:
# ill (any) = YES AND
# at least X days with no symptoms preceeding, 
# where X is "lagdays" argument
# (i.e. number of days after which symptom would be considered a new infection)

# Person days at risk (denominator for incidence calculations):
# excluded from follow up if:
# first [lagdays] of start of follow-up (to exclude prevalent episodes)
# if there is a prevalent episode, exclude the days of the prevalent episode
# first [lagdays] of start of follow-up (to exclude prevalent episodes)
# if there is a prevalent episode, exclude the days of the prevalent episode
# assume non-response weeks have no symptoms


# day of followup

dy<-
  dy %>%
  group_by(id) %>%
  mutate(
    date_first=min(day_date)
  ) %>%
  ungroup() %>%
  mutate(days_fup=as.numeric(day_date-date_first+1)) %>%
  select(-date_first)

## add month to day data
dy<-
  dy %>% mutate(month=month(day_date), year=year(day_date))


# function for assigning labels to incident symptoms
set_var_names1_day<-function (.data, values) 
{
  if (!all(names(values) %in% names(.data))) 
    stop("some variables not found in .data")
  for (v in names(values)) labelled::var_label(.data[[v]]) <- values[[v]]
  .data
}


# function for identifying symptom episodes
## lagday is the number of days of no symptoms (MORE THAN) in between days with symptoms that still count as the same episode
## nonspec = TRUE if wanting to include nonspecific symptoms - however, the episode would still have to have at least one day of reporting the specific symptom
## nonspec = FALSE if not wanting to include nonspecific symptoms
## nonspecvar is the list of nonspecific symptoms that are included 

## variables created:
### name of symptom 
### number of lagdays - eg 7
### _new = start of incident symptom episodes (i.e. excluding those ongoing at start of follow up)
### _start = start of any episode (i.e. including those ongoing at start of follow up)
### _end = end of any episode
### _id = id of episode (this runs across whole of episode - including any missed days/ days with no sympt that are within lagday)
### _exclfu = days to exclude from follow up (at start of follow up) for calculating incidence - i.e. ongoing symptom episodes from start of follow up (and the days before them if relevant). Also everything execept the first day of a new symptom episode (can't have a new infection whilst one is ongoing).
### _incident_ep = days that thare part of an incident episode (non-incident episodes still have an ID)


new_episode_fn<-function(df, sym, lagday, nonspec=TRUE, nonspecvar){
  sym<-enquo(sym)
  nonspecvar<-enquo(nonspecvar)
  nsvarname<-ifelse(nonspec==TRUE, paste0("__", quo_name(nonspecvar), "_"), "__")
  
  # names of new vars
  sym_name_new<-paste0(quo_name(enquo(sym)), nsvarname, lagday, "_new")
  sym_name_start<-paste0(quo_name(enquo(sym)), nsvarname, lagday, "_start")
  sym_name_end<-paste0(quo_name(enquo(sym)), nsvarname, lagday, "_end")
  sym_name_id<-paste0(quo_name(enquo(sym)), nsvarname, lagday, "_id")
  sym_name_exclfu<-paste0(quo_name(enquo(sym)), nsvarname, lagday, "_exclfu")
  sym_name_incident_ep<-paste0(quo_name(enquo(sym)), nsvarname, lagday, "_incident_ep")
  
  df %>%
    # create combined var - specific and non-specific symptoms
    mutate(sym_temp=case_when(
      nonspec==T & (!!nonspecvar==TRUE | !! sym==1) ~1, 
      nonspec==F & (!! sym==1) ~1
    )
    ) %>%
    # identify next and previous date ill
    arrange(id, sym_temp, week, day_date) %>%
    group_by(id, sym_temp) %>%
    mutate(
      prev_day_ill_date=lag(day_date, 1), 
      prev_day_ill_date=if_else(sym_temp==1, prev_day_ill_date, ymd(NA_real_)), 
      days_since_ill=day_date-prev_day_ill_date, 
      next_day_ill_date=lead(day_date, 1), 
      next_day_ill_date=if_else(sym_temp==1, next_day_ill_date, ymd(NA_real_)), 
      days_until_ill=next_day_ill_date-day_date
    ) %>% ungroup() %>%
    arrange(id, week, day_date) %>% 
    group_by(id) %>% 
    # calculate number of days between symptoms
    mutate(
      ill_where_n=ifelse(sym_temp==1, 1, 0), 
      ill_where_n=cumsum(ill_where_n), 
      ill_where_n=ifelse(sym_temp==1, ill_where_n, NA), 
      prev_day_ill=lag(sym_temp, 1),
      day_lag=lag(day_date, 1), 
      # identify start of symptom episode - needs to be longer than ladgay since prev symptom
      ill_where_start=ifelse(
        (sym_temp==1 & days_since_ill>lagday | 
           sym_temp==1 & is.na(days_since_ill)), 1, NA
      ), 
      # identify end of symptom episode - needs to be longer than lagday until next symptom
      ill_where_end=ifelse(
        (sym_temp==1 & days_until_ill>lagday |
           sym_temp==1 & is.na(days_until_ill)), 1, NA
      ),
      # identify new symptom epsiodes - needs to be longer than lagday since prev symptom
      ## and also longer than lagday since start of follow up
      ill_where_new=ifelse(
        (sym_temp==1 & days_since_ill>lagday |
           sym_temp ==1 & is.na(days_since_ill) & days_fup>lagday), 1, NA
      ), 
      # generate illness id - assign to duration of symptom episode for all new episodes
      ## is unique within individual
      ill_where_id=cumsum(coalesce(ill_where_start,0)), 
      ill_where_end_id=cumsum(coalesce(ill_where_end,0)),
      ill_where_id=case_when(
        ill_where_id==ill_where_end_id & (ill_where_end==0 | is.na(ill_where_end)) ~ NA_real_, 
        ill_where_id==ill_where_end_id & ill_where_end==1 ~ ill_where_id,
        ill_where_id!=ill_where_end_id ~ ill_where_id
      )
    ) %>%
    group_by(id, ill_where_id) %>%
    mutate(
      # check if there are any specific symptoms
      any_specif_sym=ifelse(sum(!!sym, na.rm=T)>=1, TRUE, FALSE)) %>% 
    ungroup() %>% 
    group_by(id) %>% 
    # recode illness vars as NA if no specific symptoms during episode
    mutate_at(vars(ill_where_id, ill_where_n, ill_where_start, ill_where_end, ill_where_new), 
              funs(ifelse(any_specif_sym==FALSE, NA,. 
              ))) %>%
    # exclude days from follow up if first illness occurs during lagdays
    ## exclude until end of the first 
    mutate(
      first_ill_start=min(days_fup[ill_where_id==min(ill_where_id, na.rm=T)], na.rm = T), 
      first_ill_end=max(days_fup[ill_where_id==min(ill_where_id, na.rm=T)], na.rm=T), 
      exclude_where_fup=ifelse(first_ill_start<=lagday & days_fup<=first_ill_end, TRUE, FALSE)
    ) %>% 
    group_by(id, ill_where_id) %>%
    # identify incident illness episodes
    mutate(
      ill_where_incident=
        ifelse(
          !is.na(ill_where_new[days_fup==min(days_fup)]), TRUE, FALSE
        )
    ) %>%
    ungroup() %>%
    # for days after an illness, identify last date was ill
    mutate(
      previll=if_else(ill_where_end==1, day_date, ymd(NA_real_))
    ) %>%
    arrange(id, day_date) %>%
    group_by(id) %>%
    fill(previll, .direction="down") %>%
    mutate(
      previll=if_else(!is.na(ill_where_id), ymd(NA_real_), previll), 
      previll=day_date-previll, 
      exclude_where_fup=ifelse(days_fup<=lagday, TRUE, exclude_where_fup)
    ) %>%
    ungroup() %>%
    select(-sym_temp, -ill_where_end_id, -day_lag, -prev_day_ill_date, -next_day_ill_date, -first_ill_start, -first_ill_end, 
           -prev_day_ill,  -days_until_ill, -days_since_ill, -any_specif_sym, -ill_where_n, -previll) %>% 
    rename(
      !! sym_name_new := ill_where_new, 
      !! sym_name_start := ill_where_start, 
      !! sym_name_end := ill_where_end, 
      !! sym_name_id := ill_where_id, 
      !! sym_name_exclfu := exclude_where_fup, 
      !! sym_name_incident_ep := ill_where_incident
    ) 
}


# create cough, fever, and cough/fever variables
## note that coughfever_any means cough and/or fever (at least one of the two symptoms)

dy<-
  dy %>%
  unite(col="cough_any", coughdry, coughphl, 
        na.rm=T, remove=F, sep="") %>%
  unite(col="coughfever_any", coughdry, coughphl, fever,  
        na.rm=T, remove=F, sep="") %>%
  mutate(
    cough_any=case_when(str_detect(cough_any, "1|2|3") ~ 1, 
                        TRUE ~ 0),
    fever_any=case_when(str_detect(as.numeric(fever), "1|2|3") ~ 1, 
                        TRUE ~ 0),
    coughfever_any=case_when(str_detect(coughfever_any, "1|2|3") ~ 1, 
                             TRUE ~ 0)
  )

# identify infection syndromes
dy<-
  dy %>% 
  new_episode_fn(cough_any, lagday=10, nonspec=F) %>%
  new_episode_fn(fever_any, lagday=10, nonspec=F) %>%
  new_episode_fn(coughfever_any, lagday=10, nonspec=F) 
# note that this gives the same id number (value = which symptomatic period it is for that individual, 
# i.e. 1 = first, 2= second etc.) to each day of follow-up

# Incidence episodes ----

# ids of incident infection syndromes

incident_ep_ids_fn<-function(epid, epincid){ 
  epid<-enquo(epid) 
  epincid<-enquo(epincid)
  
  dy %>% 
    select(id, !!epid, !!epincid) %>%
    filter(!is.na(!!epid), !!epincid==TRUE) %>%
    distinct() %>%
    select(id, !!epid) %>%
    gather(episode_cat, episode_id, -id)
} # takes participant id, event id and logical for episode symptom columns, 
# takes unique episodes (i.e. just one row per incident), then puts in tidy form
# with category for type of episode and episode id. So now have 1 line per episode per person.

incident_ep_ids<-
  incident_ep_ids_fn(epid=cough_any__10_id, epincid=cough_any__10_incident_ep) %>%
  bind_rows(
    incident_ep_ids_fn(epid=fever_any__10_id, epincid=fever_any__10_incident_ep)
  ) %>%
  bind_rows(
    incident_ep_ids_fn(epid=coughfever_any__10_id, epincid=coughfever_any__10_incident_ep)
  ) 

### 2. Summarise episodes at level of individual (WITH month and region) ----

sum_episodes_individ_fn_region_month<-function(sym){
  sym_var=sym
  dayslag_var=str_split(str_extract(quo_name(sym_var),  "_(\\d)+"), "_")[[1]][2]
  sym_name=str_match(quo_name(sym_var),  "(.*)__")[,2]
  incident_ep_var=rlang::sym(paste0(quo_name(sym_var), "_new"))
  incident_daysfu_var=rlang::sym(paste0(quo_name(sym_var), "_exclfu"))
  
  dy %>% 
    group_by(id, month) %>%
    select(id, !!incident_ep_var, !!incident_daysfu_var, month) %>%
    summarise(
      symptom_type=sym_name,
      episode_name=sym_var,
      incident_episodes=sum(!!incident_ep_var, na.rm=T), 
      incident_ep_daysfu=length(which(!!incident_daysfu_var==FALSE))
    ) %>%
    left_join(bl %>% select(id, sex, agegrp, agegrp2, region), by="id") %>%
    mutate(sex=as.character(sex), 
           agesex=paste(sex, agegrp, sep="_"),
           regsexage=paste(region,sex, agegrp,sep="_"),
           age2sex=paste(sex, agegrp2, sep="_"),
           regsexage2=paste(region,sex, agegrp2,sep="_"),
           regsex=paste(region,sex,sep="_")) %>%
    filter(sex %in% c("Male", "Female"), 
           !is.na(agegrp), !is.na(region)
    )
}

sym_sum_names<-c("cough_any__10",  
                 "fever_any__10",
                 "coughfever_any__10")

sum_episodes_individ_region_month<-
  map_df(sym_sum_names, sum_episodes_individ_fn_region_month)

# remove if few days of follow up (because of prevalent infections)
sum_episodes_individ_region_month<-sum_episodes_individ_region_month %>% filter(incident_ep_daysfu>=10) 

### 3. mid-2019 ONS population figures ----

ONS_england <- read_csv("Data/ONS_England_mid-2019_extracted_sex-reg-age.csv") %>%
  rename(region=Name, sex=Sex) %>%
  select(-Geography1) %>%
  gather(key="age", value="n", -region,-sex) %>%
  mutate(sex = case_when(
    sex == "M" ~ "Male",
    sex == "F" ~ "Female"))

# 3a. England: For sex and regions weights (all ages)
eng_regsex_com_pop <- ONS_england %>%
  filter(age=="All ages") %>%
  select(-age)%>%
  mutate(prop=n/sum(n))%>%
  mutate(regsex=paste(region,sex, sep="_"), Freq=n) %>%
  select(-region, -sex) %>%
  select(regsex, Freq) 

# 3b. England: For agegrp, sex and regions weights
eng_regsexage_com_pop <- ONS_england %>%
  filter(age!="All ages") %>%
  mutate(age=as.numeric(age),
    agegrp2 = case_when(
      age<=16 ~ "<=16",
      age>=17 & age<=55 ~ "17-55",
      age>55 & age<=70 ~ "56-70",
      age>70 ~ ">70"
    )) %>%
  group_by(region, sex, agegrp2) %>%
  summarise(n=sum(n)) %>%
  mutate(prop=n/sum(n)) %>%
  mutate(regsexage2=paste(region,sex,agegrp2, sep="_"), Freq=n) %>%
  ungroup() %>%
  select(regsexage2, Freq) 

# 3c. England: For agegrp and sex weights
eng_agesex_com_pop <- ONS_england %>%
  filter(age!="All ages") %>%
  mutate(age=as.numeric(age),
  agegrp2 = case_when(
    age<=16 ~ "<=16",
    age>=17 & age<=55 ~ "17-55",
    age>55 & age<=70 ~ "56-70",
    age>70 ~ ">70"
  )) %>%
  group_by(sex, agegrp2) %>%
  summarise(n=sum(n)) %>%
  mutate(prop=n/sum(n))%>%
  mutate(age2sex=paste(sex,agegrp2, sep="_"), Freq=n) %>%
  ungroup() %>%
  select(age2sex, Freq)   

# 3d. UK: For agegrp and sex weights
UK_agesex_com_pop <- read_csv("Data/ONS_UK_mid-2019_extracted_sex-age.csv") %>%
  rename(region=Name, sex=Sex) %>%
  select(-Geography1) %>%
  gather(key="age", value="n", -region,-sex) %>%
  mutate(sex = case_when(
    sex == "M" ~ "Male",
    sex == "F" ~ "Female")) %>%
  filter(age!="All ages") %>%
  mutate(age=as.numeric(age),
  agegrp2 = case_when(
    age<=16 ~ "<=16",
    age>=17 & age<=55 ~ "17-55",
    age>55 & age<=70 ~ "56-70",
    age>70 ~ ">70"
  )) %>%
   group_by(sex, agegrp2) %>%
   summarise(n=sum(n)) %>%
   mutate(prop=n/sum(n))%>%
   mutate(age2sex=paste(sex,agegrp2, sep="_"), Freq=n) %>%
   ungroup() %>%
   select(age2sex, Freq) 

### 4. Incidence rates stratified by age group and month, weighted by sex and region for England pop structure ----

agegrps <- unique(sum_episodes_individ_region_month$agegrp2)

results_strat <- tibble(agegrp = rep(agegrps, each=12), month=rep(1:12, length(agegrps))) %>%
  slice(rep(1:(length(agegrps)*12), 3)) %>%
  mutate(symptoms = rep(sym_sum_names, each = length(agegrps)*12), inc = 0, lci =0, uci = 0)

for (ik in 1:length(sym_sum_names)){
  for (i in 1:length(agegrps)){
    sym=sym_sum_names[ik]
    age=agegrps[i]
    des<-svydesign(ids=~1, data=sum_episodes_individ_region_month %>% filter(agegrp2==age, episode_name==sym))
    des_rake<-rake(design=des, sample.margins = list(~regsex), population.margins = list(eng_regsex_com_pop))
    m1<-svyglm(incident_episodes~ -1 + as.factor(month) + offset(log(incident_ep_daysfu)), design=des_rake, family=quasipoisson())
    
    # weekly incidence per 100,000
    results_strat$inc[results_strat$agegrp==age & results_strat$symptoms==sym] <- 700000*exp(coef(m1))
    results_strat$lci[results_strat$agegrp==age & results_strat$symptoms==sym] <- 700000*exp(confint(m1)[,1])
    results_strat$uci[results_strat$agegrp==age & results_strat$symptoms==sym] <- 700000*exp(confint(m1)[,2])
  }
  
}

### 5. Incidence rates stratified by month, weighted by age, sex and region for England pop structure ----

results_adj <- tibble(month=rep(1:12, 3)) %>%
  mutate(symptoms = rep(sym_sum_names, each = 12), inc = 0, lci =0, uci = 0)

for (ik in 1:length(sym_sum_names)){
    sym=sym_sum_names[ik]
    des<-svydesign(ids=~1, data=sum_episodes_individ_region_month %>% filter(episode_name==sym))
    des_rake<-rake(design=des, sample.margins = list(~regsexage2), population.margins = list(eng_regsexage_com_pop))
    m1<-svyglm(incident_episodes~ -1 + as.factor(month) + offset(log(incident_ep_daysfu)), design=des_rake, family=quasipoisson())
    
    # weekly incidence per 100,000
    results_adj$inc[results_adj$symptoms==sym] <- 700000*exp(coef(m1))
    results_adj$lci[results_adj$symptoms==sym] <- 700000*exp(confint(m1)[,1])
    results_adj$uci[results_adj$symptoms==sym] <- 700000*exp(confint(m1)[,2])
}

# combine all rates together
results_adj$agegrp <- "All ages"

results_ENG <- bind_rows(results_strat, results_adj)

### 5b. Incidence rates stratified by month, weighted by age and sex for England pop structure ----

# To check that not weighting by region does not affect results significantly (as weighting by region not possible for UK)
results_adj_agesex_check <- tibble(month=rep(1:12, 3)) %>%
  mutate(symptoms = rep(sym_sum_names, each = 12), inc = 0, lci =0, uci = 0)

for (ik in 1:length(sym_sum_names)){
  sym=sym_sum_names[ik]
  des<-svydesign(ids=~1, data=sum_episodes_individ_region_month %>% filter(episode_name==sym))
  des_rake<-rake(design=des, sample.margins = list(~age2sex), population.margins = list(eng_agesex_com_pop))
  m1<-svyglm(incident_episodes~ -1 + as.factor(month) + offset(log(incident_ep_daysfu)), design=des_rake, family=quasipoisson())
  
  # weekly incidence per 100,000
  results_adj_agesex_check$inc[results_adj$symptoms==sym] <- 700000*exp(coef(m1))
  results_adj_agesex_check$lci[results_adj$symptoms==sym] <- 700000*exp(confint(m1)[,1])
  results_adj_agesex_check$uci[results_adj$symptoms==sym] <- 700000*exp(confint(m1)[,2])
}

# compare with region weighting rates
compare_results <- bind_rows(results_adj %>% mutate(type = "regagesex"),
                             results_adj_agesex_check %>% mutate(type = "agesex"))
ggplot(data=compare_results %>% filter(symptoms=="coughfever_any__10")) +
  geom_point(aes(month, inc, col=type), position=position_dodge(width=0.5)) +
  geom_errorbar(aes(month, inc, ymin=lci, ymax=uci, col=type), position=position_dodge(width=0.5)) + 
  scale_x_continuous(limits=c(0.5,12.5), breaks=c(1:12)) + ylab("Weekly incidence per 100,000") + xlab("Month")

# these are similar, so we weight to full UK population

### 5c. Incidence rates stratified by month, weighted by age and sex for the UK pop structure ----

results_UK <- tibble(month=rep(1:12, 3)) %>%
  mutate(symptoms = rep(sym_sum_names, each = 12), inc = 0, lci =0, uci = 0)

for (ik in 1:length(sym_sum_names)){
  sym=sym_sum_names[ik]
  des<-svydesign(ids=~1, data=sum_episodes_individ_region_month %>% filter(episode_name==sym))
  des_rake<-rake(design=des, sample.margins = list(~age2sex), population.margins = list(UK_agesex_com_pop))
  m1<-svyglm(incident_episodes~ -1 + as.factor(month) + offset(log(incident_ep_daysfu)), design=des_rake, family=quasipoisson())
  
  # weekly incidence per 100,000
  results_UK$inc[results_UK$symptoms==sym] <- 700000*exp(coef(m1))
  results_UK$lci[results_UK$symptoms==sym] <- 700000*exp(confint(m1)[,1])
  results_UK$uci[results_UK$symptoms==sym] <- 700000*exp(confint(m1)[,2])
}

### 6. Dates ----

results_ENG$date <- c(as.Date("2020-01-01", "%Y-%m-%d"))
results_UK$date <- c(as.Date("2020-01-01", "%Y-%m-%d"))
for (i in 1:12){
  ifelse(i<10, k <- paste0("0",i), k<- as.character(i))
  ifelse(i<7, 
         results_UK$date[results_UK$month==i] <-as.Date(paste0("2021-",k,"-",1, "%Y-%m-%d")),
         results_UK$date[results_UK$month==i] <-as.Date(paste0("2020-",k,"-",1, "%Y-%m-%d"))
         )
  ifelse(i<7, 
         results_ENG$date[results_ENG$month==i] <-as.Date(paste0("2021-",k,"-",1, "%Y-%m-%d")),
         results_ENG$date[results_ENG$month==i] <-as.Date(paste0("2020-",k,"-",1, "%Y-%m-%d"))
  )
}

results_ENG$year <- as.factor(year(results_ENG$date))
results_UK$year <- as.factor(year(results_UK$date))

date_start <- as.Date("2020-07-01","%Y-%m-%d")  
date_end <- as.Date("2021-06-01","%Y-%m-%d")
date_breaks_vec <- seq.Date(date_start, date_end, "month")

date_start_ENG <- as.Date("2020-07-01","%Y-%m-%d")  
date_end_ENG <- as.Date("2021-06-01","%Y-%m-%d")
date_breaks_vec_ENG <- seq.Date(date_start_ENG, date_end_ENG, "month")

### REVISION: Calculate reduction relative to historical baseline using Virus Watch data ----
bw_compare <- results_ENG %>%
  filter(agegrp == "All ages", month %in% 7:12, symptoms == "coughfever_any__10") %>%
  select(bw_inc = inc)

VW_prop_neg <- read_csv("Data/VW_monthly_cough_prop.csv") %>%
  mutate(prop_neg = prop_neg/100,
         month = case_when(
           month == "Sep" ~ "Sept",
           TRUE ~ month
         )
         )

vw <- read_csv("Data/Virus_watch_results_adj_month_2020-12-22.csv") %>%
  left_join(VW_prop_neg, by="month") %>%
  cbind(bw_compare) %>%
  mutate(inc_reduction = (bw_inc - inc*prop_neg)/bw_inc) %>%
  filter(month != "Dec")

median(vw$inc_reduction)
range(vw$inc_reduction)

### 7. Testing capacity analysis ----

UK_pop_mid_2019 <- 66796807 # Total population - source: ONS mid-2019

# estimated daily incidence in the UK
results_diag <- results_UK %>%
  mutate(inc = inc*UK_pop_mid_2019/700000, lci = lci*UK_pop_mid_2019/700000, uci = uci*UK_pop_mid_2019/700000)

# UK government estimated UK laboratory testing capacity on 6-12th Aug as: (source: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/910738/Test_and_Trace_Week11_v3.pdf)
UKlab_capacity <- tibble(type = c("Pillar1", "Pillar2", "Pillar1+2", "Pillar3", "Pillar4"),
                         capacity = c(579418/7, 880000/7, 579418/7 + 880000/7, 840000/7, 63700/7))


## Capacity exceedence relative to Pillars 1 and 2 ----
# calculate by how much daily demand exceeds capacity for a range of PROPTEST (Proportion of Population Requesting tests) values
# this is done for i) assumption of zero COVID-19 demand and ii) additional COVID-19 demand due to scenarios C1 to C4.
c.f.prop <- 0.875  # from ZOE app (https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)31281-2/fulltext#sec1)

shape_param <- 0.8
exceed_daily<- tibble(month=1:12, month_date = results_diag %>% filter(symptoms=="coughfever_any__10") %>% pull(date),
                ref_month=c(6,6,6,5:2,1:5),
                inc = results_diag %>% filter(symptoms=="coughfever_any__10") %>% pull(inc),
                lci = results_diag %>% filter(symptoms=="coughfever_any__10") %>% pull(lci),
                uci = results_diag %>% filter(symptoms=="coughfever_any__10") %>% pull(uci)) %>%
  slice(rep(1:12, 4)) %>%
  mutate(
    covid_factor = rep(c(0.05,0.1,0.15,0.2), each= 12), 
    CF_eff=covid_factor/exp(6^shape_param)*exp(ref_month^shape_param),
    c.f.prop = c.f.prop,
    covid= inc*CF_eff) %>%
  slice(rep(1:48, 4)) %>%
  mutate(PR = rep(c(0.4,0.6,0.8,1), each=48),
         covid_request = inc*CF_eff*c.f.prop*PR) %>%
  mutate(daily_exceed_pillar1_2 = UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"] - inc*PR,
         daily_exceed_pillar1_2_lci = UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"] - lci*PR,
         daily_exceed_pillar1_2_uci = UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"] - uci*PR,
         exceed_pillar1_2_y_n = as.factor(ifelse(daily_exceed_pillar1_2>=0, "no", "yes")),
         daily_exceed_pillar1_2_covid = UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"] - inc*PR - covid_request,
         daily_exceed_pillar1_2_lci_covid = UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"] - lci*PR - covid_request,
         daily_exceed_pillar1_2_uci_covid = UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"] - uci*PR- covid_request,
         exceed_pillar1_2_y_n_covid = as.factor(ifelse(daily_exceed_pillar1_2_covid>=0, "no", "yes"))
  )
  
exceed_daily$exceed_pillar1_2_y_n <- relevel(exceed_daily$exceed_pillar1_2_y_n, "yes")
exceed_daily$exceed_pillar1_2_y_n_covid <- relevel(exceed_daily$exceed_pillar1_2_y_n_covid, "yes")

# Create stacked demand data for Figure 6 ----
dat.stacked <- exceed_daily %>%
  mutate(baseline_demand = inc*PR ,
         total_demand = inc*PR + covid_request,
         total_demand_lci = lci*PR + covid_request,
         total_demand_uci = uci*PR + covid_request) %>%
  select(month_date, covid_factor, PR, baseline_demand, total_demand, total_demand_lci, total_demand_uci, covid_request)

dat.stacked_plot <- dat.stacked %>%
  select(month_date, covid_factor, PR, baseline_demand, covid_request) %>%
  gather(-month_date, -covid_factor, -PR, value="demand", key = "type") %>%
  mutate(type=relevel(as.factor(type), "baseline_demand")) %>%
  left_join(dat.stacked %>% select(-baseline_demand, -covid_request), by=c("month_date","covid_factor", "PR"))

# REVISION: Create stacked demand data for Figure 7 ----
adj.inc <- 0.27 # 29% of historic incidence
dat.stacked.adj <- exceed_daily %>%
  mutate(baseline_demand = inc*PR*adj.inc ,
         total_demand = inc*PR*adj.inc + covid_request,
         total_demand_lci = lci*PR*adj.inc + covid_request,
         total_demand_uci = uci*PR*adj.inc + covid_request) %>%
  select(month_date, covid_factor, PR, baseline_demand, total_demand, total_demand_lci, total_demand_uci, covid_request)

dat.stacked_plot.adj <- dat.stacked.adj %>%
  select(month_date, covid_factor, PR, baseline_demand, covid_request) %>%
  gather(-month_date, -covid_factor, -PR, value="demand", key = "type") %>%
  mutate(type=relevel(as.factor(type), "baseline_demand")) %>%
  left_join(dat.stacked.adj %>% select(-baseline_demand, -covid_request), by=c("month_date","covid_factor", "PR"))

### 8. Graphs ----

# plotting parameters
txtsize <- 25
txtsize_figs1_3 <- 30
pntsize <- 5
linesize <- 1

##### Figure 1 - All-age incidence rates for cough or fever stratified by month (England) ----
p.all.ages <- ggplot(results_ENG %>% filter(agegrp=="All ages" & symptoms=="coughfever_any__10"), aes(x=date, y=inc)) + 
  geom_errorbar(aes(date, inc, ymin=lci, ymax=uci), size = linesize) +
  geom_point(aes(date, inc), size = pntsize, shape=21, fill = "#56B4E9", col="black") +
  geom_line(aes(date,inc), linetype=2, col ="#999999", size = linesize) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(limits=c(0,7000), breaks=c((0:7)*1000),labels=comma) +
  ylab("Incidence rate per 100,000-person-week") + xlab("") + theme_bw() + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, size=txtsize), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize_figs1_3), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

jpeg(filename="4_Outputs/fig1_incidences_coughfever_allages_england.jpeg", width=10*1.68, height=10, units = "in", res=300)
p.all.ages
dev.off()


##### Figure 2 - Non-COVID-19 cough or fever cases (UK) with Pillar 1 and 2 capacity ----
labtxtsize <- 8
ylab.daily.cap <- c(100*1:6)
  
p.diag.daily.capacity <- ggplot(results_diag %>% filter(symptoms=="coughfever_any__10"), aes(x=date, y=inc)) + 
  geom_errorbar(aes(date, inc, ymin=lci, ymax=uci), size = linesize) +
  geom_point(aes(date, inc), size = pntsize, shape=21, fill = "#56B4E9", col="black") +
  geom_line(aes(date,inc), linetype=2, col ="#999999", size = linesize) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = c(0,paste0(ylab.daily.cap, "K")),
                     breaks = c(0,ylab.daily.cap)*1e3, 
                     limits = c(0,6e5)) + 
  ylab("Cough or fever cases per day") + xlab("") + theme_bw() + 
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar2"], 
             col="#e34a33", linetype=2) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize_figs1_3), 
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  scale_linetype_manual(name = "limit", 
                        guide = guide_legend(override.aes = list(linetype = c(2, 1)))) + 
  geom_label(
    label="UK Pillar 2", 
    x=as.Date("2021-1-1","%Y-%m-%d"),
    y=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar2"],
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = 0.35,
    size = labtxtsize,
    color = "#e34a33",
    fill="white"
  ) + 
  geom_label(
    label="UK Pillars 1+2", 
    x=as.Date("2021-1-1","%Y-%m-%d"),
    y=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"],
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = 0.35,
    size = labtxtsize,
    color = "#e34a33",
    fill="white"
  )

jpeg(filename="4_Outputs/fig2_demand_capacity_UK.jpeg", width=10*1.68, height=10, units = "in", res=300)
p.diag.daily.capacity
dev.off()

##### Figure 3 - Daily capacity exceedance for range of PROPTEST values, assuming zero additional COVID-19 demand (UK) ----
ylab.exceed.daily1 <- c(seq(-2,2,1)*100)
ylab.exceed.daily1.lab <- paste0(ylab.exceed.daily1, "K")
ylab.exceed.daily1.lab[ylab.exceed.daily1.lab=="0K"] <- "0"
ylab.exceed.daily2 <- c(seq(-4,2,2)*100)
ylab.exceed.daily2.lab <- paste0(ylab.exceed.daily2, "K")
ylab.exceed.daily2.lab[ylab.exceed.daily2.lab=="0K"] <- "0"

p1.exceed <- ggplot(exceed_daily %>% filter(PR==0.4, covid_factor==0.05), aes(x=month_date, y=daily_exceed_pillar1_2)) + 
  geom_bar(stat="identity",fill="#00BFC4", col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily1.lab,
                     breaks = ylab.exceed.daily1*1e3,
                     limits=c(range(ylab.exceed.daily1*1e3))) + 
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2, ymin=daily_exceed_pillar1_2_lci, ymax=daily_exceed_pillar1_2_uci), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "a) PROPTEST=40%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

p2.exceed <- ggplot(exceed_daily %>% filter(PR==0.6, covid_factor==0.05), aes(x=month_date, y=daily_exceed_pillar1_2)) + 
  geom_bar(stat="identity", aes(fill=exceed_pillar1_2_y_n), col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily1.lab,
                     breaks = ylab.exceed.daily1*1e3,
                     limits=c(range(ylab.exceed.daily1*1e3))) + 
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2, ymin=daily_exceed_pillar1_2_lci, ymax=daily_exceed_pillar1_2_uci), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "b) PROPTEST=60%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

p3.exceed <- ggplot(exceed_daily %>% filter(PR==0.8, covid_factor==0.05), aes(x=month_date, y=daily_exceed_pillar1_2)) + 
  geom_bar(stat="identity", aes(fill=exceed_pillar1_2_y_n), col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily2.lab,
                     breaks = ylab.exceed.daily2*1e3,
                     limits=c(range(ylab.exceed.daily2*1e3))) + 
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2, ymin=daily_exceed_pillar1_2_lci, ymax=daily_exceed_pillar1_2_uci), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "c) PROPTEST=80%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

p4.exceed <- ggplot(exceed_daily %>% filter(PR==1, covid_factor==0.05), aes(x=month_date, y=daily_exceed_pillar1_2)) + 
  geom_bar(stat="identity", aes(fill=exceed_pillar1_2_y_n), col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily2.lab,
                     breaks = ylab.exceed.daily2*1e3,
                     limits=c(range(ylab.exceed.daily2*1e3))) + 
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2, ymin=daily_exceed_pillar1_2_lci, ymax=daily_exceed_pillar1_2_uci), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "d) PROPTEST=100%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

jpeg(filename="4_Outputs/fig3_capacity_exceedance_zeroCOVID_UK.jpeg", width=11*1.68, height=10, units = "in", res=300)
grid.arrange(p1.exceed, p2.exceed, p3.exceed, p4.exceed, nrow = 2,
                         left = textGrob("Remaining testing capacity per day", rot = 90, vjust = 1, gp = gpar(cex = 2.2)))
dev.off()

##### Figure 4 - COVID-19 incidences for scenarios C1 to C4 (UK) ----
ylab.covid.all <- seq(0,80,20)
ylab.covid.lab.all <- c(0,paste0(ylab.covid.all[2:length(ylab.covid.all)], "K"))

# all together
p.covid_all <- ggplot(exceed_daily %>% filter(PR==1), aes(x=month_date, y=covid)) +
  geom_bar(stat="identity",aes(fill=as.factor(covid_factor)), col="black", position="dodge") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.covid.lab.all,
                     breaks = ylab.covid.all*1e3,
                     limits=c(range(ylab.covid.all*1e3))) +
  xlab("") +
  ylab("Cases per day") + theme_bw() + 
  theme(panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize_figs1_3),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        legend.position=c(0.97,0.93), legend.justification=c(1, 1),
        legend.key.size = unit(2,"line"),
        legend.background = element_rect(size=0.5, linetype="solid", 
                                        colour ="black"),
       legend.direction="horizontal") + 

  scale_fill_discrete(name="",
                      breaks=c("0.05", "0.1", "0.15", "0.2"),
                      labels=c("C1", "C2", "C3", "C4"))

jpeg(filename="4_Outputs/fig4_covid_demand_UK.jpeg", width=11*1.68, height=10, units = "in", res=300)
p.covid_all
dev.off()

##### Figure 5 - Daily capacity exceedence for range of PR values, for COVID-19 scenario C2 (UK) ----
p1.exceed.covid <- ggplot(exceed_daily %>% filter(PR==0.4, covid_factor==0.1), aes(x=month_date, y=daily_exceed_pillar1_2_covid)) + 
  geom_bar(stat="identity", fill="#00BFC4", col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily1.lab,
                     breaks = ylab.exceed.daily1*1e3,
                     limits=c(range(ylab.exceed.daily1*1e3))) +  
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2_covid, ymin=daily_exceed_pillar1_2_lci_covid, ymax=daily_exceed_pillar1_2_uci_covid), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2_covid), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "a) PROPTEST=40%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

p2.exceed.covid <- ggplot(exceed_daily %>% filter(PR==0.6, covid_factor==0.1), aes(x=month_date, y=daily_exceed_pillar1_2_covid)) + 
  geom_bar(stat="identity", aes(fill=exceed_pillar1_2_y_n_covid), col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily1.lab,
                     breaks = ylab.exceed.daily1*1e3,
                     limits=c(range(ylab.exceed.daily1*1e3))) + 
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2_covid, ymin=daily_exceed_pillar1_2_lci_covid, ymax=daily_exceed_pillar1_2_uci_covid), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2_covid), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "b) PROPTEST=60%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

p3.exceed.covid <- ggplot(exceed_daily %>% filter(PR==0.8, covid_factor==0.1), aes(x=month_date, y=daily_exceed_pillar1_2_covid)) + 
  geom_bar(stat="identity", aes(fill=exceed_pillar1_2_y_n_covid), col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily2.lab,
                     breaks = ylab.exceed.daily2*1e3,
                     limits=c(min(ylab.exceed.daily2*1e3)-10e3,max(ylab.exceed.daily2*1e3))) + 
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2_covid, ymin=daily_exceed_pillar1_2_lci_covid, ymax=daily_exceed_pillar1_2_uci_covid), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2_covid), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "c) PROPTEST=80%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

p4.exceed.covid <- ggplot(exceed_daily %>% filter(PR==1, covid_factor==0.1), aes(x=month_date, y=daily_exceed_pillar1_2_covid)) + 
  geom_bar(stat="identity", aes(fill=exceed_pillar1_2_y_n_covid), col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily2.lab,
                     breaks = ylab.exceed.daily2*1e3,
                     limits=c(min(ylab.exceed.daily2*1e3)-10e3,max(ylab.exceed.daily2*1e3))) + 
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2_covid, ymin=daily_exceed_pillar1_2_lci_covid, ymax=daily_exceed_pillar1_2_uci_covid), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2_covid), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "d) PROPTEST=100%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

jpeg(filename="4_Outputs/fig5_capacity_exceedance_withCOVID_UK.jpeg", width=11*1.68, height=10, units = "in", res=300)
grid.arrange(p1.exceed.covid, p2.exceed.covid, p3.exceed.covid, p4.exceed.covid, nrow = 2,
             left = textGrob("Remaining testing capacity per day", rot = 90, vjust = 1, gp = gpar(cex = 2.2)))
dev.off()


# Figure 6 - total demand PROPTEST=0.8, for COVID-19 scenarios C1-C4 (UK) ----
ylab.exceed.dailyfig7alt <- c((0:5)*100)
ylab.exceed.dailyfig7alt.lab <- paste0(ylab.exceed.dailyfig7alt, "K")
ylab.exceed.dailyfig7alt.lab[ylab.exceed.dailyfig7alt.lab=="0K"] <- "0"

lab7alt <- 5.5
labsize7alt <- 0.45
labheight <- UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"] + 7000

stackC1 <- ggplot(dat.stacked_plot %>% filter(covid_factor==0.05, PR==0.8), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "a) C1") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

stackC2 <- ggplot(dat.stacked_plot %>% filter(covid_factor==0.1, PR==0.8), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "b) C2") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

stackC3 <- ggplot(dat.stacked_plot %>% filter(covid_factor==0.15, PR==0.8), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "c) C3") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

stackC4 <- ggplot(dat.stacked_plot %>% filter(covid_factor==0.2, PR==0.8), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "d) C4") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

library(gridExtra)
library(grid)
library(lemon)  
jpeg(filename="4_Outputs/fig6_demand_with_PR80.jpeg", width=11*1.68, height=10, units = "in", res=300)
grid_arrange_shared_legend(stackC1, stackC2, stackC3, stackC4, ncol = 2, nrow = 2, position='top', 
                           left = textGrob("Tests per day", rot = 90, vjust = 1, gp = gpar(cex = 2.2)))
dev.off()

# Figure 7 - REVISION: total demand PROPTEST=0.8, for COVID-19 scenarios C1-C4 (UK) WITH 73% LOWER BASELINE COUGH/FEVER ----
ylab.exceed.dailyfig7alt <- c((0:3)*100)
ylab.exceed.dailyfig7alt.lab <- paste0(ylab.exceed.dailyfig7alt, "K")
ylab.exceed.dailyfig7alt.lab[ylab.exceed.dailyfig7alt.lab=="0K"] <- "0"

lab7alt <- 5.5
labsize7alt <- 0.45
labheight <- UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"] + 7000

stackC1 <- ggplot(dat.stacked_plot.adj %>% filter(covid_factor==0.05, PR==0.8), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "a) C1") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

stackC2 <- ggplot(dat.stacked_plot.adj %>% filter(covid_factor==0.1, PR==0.8), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "b) C2") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

stackC3 <- ggplot(dat.stacked_plot.adj %>% filter(covid_factor==0.15, PR==0.8), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "c) C3") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

stackC4 <- ggplot(dat.stacked_plot.adj %>% filter(covid_factor==0.2, PR==0.8), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "d) C4") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

library(gridExtra)
library(grid)
library(lemon)  
jpeg(filename="4_Outputs/fig7REVISION_demand_with_PR80.jpeg", width=11*1.68, height=10, units = "in", res=300)
grid_arrange_shared_legend(stackC1, stackC2, stackC3, stackC4, ncol = 2, nrow = 2, position='top', 
                           left = textGrob("Tests per day", rot = 90, vjust = 1, gp = gpar(cex = 2.2)))
dev.off()



### 7. SUPPLEMENTARY MATERIAL PLOTS ####

##### Figure S2.1 - Incidence rates for cough or fever stratified by month and age group (England) ----
p1 <- ggplot(results_ENG %>% filter(agegrp=="<=16" & symptoms=="coughfever_any__10"), aes(x=date, y=inc)) + 
  geom_errorbar(aes(date, inc, ymin=lci, ymax=uci), size = linesize) +
  geom_point(aes(date, inc), size = pntsize, shape=21, fill = "#56B4E9", col="black") +
  geom_line(aes(date,inc), linetype=2, col ="#999999", size = linesize) +
  ggtitle(label = "a) 0-16 years") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(limits=c(0,15000),labels=comma) +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, size=txtsize), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize)) 
p2 <- ggplot(results_ENG %>% filter(agegrp=="17-55" & symptoms=="coughfever_any__10"), aes(x=date, y=inc)) + 
  geom_errorbar(aes(date, inc, ymin=lci, ymax=uci), size = linesize) +
  geom_point(aes(date, inc), size = pntsize, shape=21, fill = "#56B4E9", col="black") +
  geom_line(aes(date,inc), linetype=2, col ="#999999", size = linesize) +
  ggtitle(label = "b) 17-55 years", ) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(limits=c(0,8500),labels=comma) +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, size=txtsize), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize)) 

p3 <- ggplot(results_ENG %>% filter(agegrp=="56-70" & symptoms=="coughfever_any__10"), aes(x=date, y=inc)) + 
  geom_errorbar(aes(date, inc, ymin=lci, ymax=uci), size = linesize) +
  geom_point(aes(date, inc), size = pntsize, shape=21, fill = "#56B4E9", col="black") +
  geom_line(aes(date,inc), linetype=2, col ="#999999", size = linesize) +
  ggtitle(label = "c) 56-70 years", ) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(limits=c(0,8500),labels=comma) +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, size=txtsize), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize)) 

p4 <- ggplot(results_ENG %>% filter(agegrp==">70" & symptoms=="coughfever_any__10"), aes(x=date, y=inc)) + 
  geom_errorbar(aes(date, inc, ymin=lci, ymax=uci), size = linesize) +
  geom_point(aes(date, inc), size = pntsize, shape=21, fill = "#56B4E9", col="black") +
  geom_line(aes(date,inc), linetype=2, col ="#999999", size = linesize) +
  ggtitle(label = "d) >70 years") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(limits=c(0,8500),labels=comma) +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, size=txtsize), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize))

jpeg(filename="4_Outputs/figS2_1_incidences_coughfever_agestrat_england.jpeg", width=11*1.68, height=10, units = "in", res=300)
grid.arrange(arrangeGrob(p1, p2, p3, p4, nrow = 2, 
                         left = textGrob("Incidence rate per 100,000-person-week", rot = 90, vjust = 1, gp = gpar(cex = 2.2))))
dev.off()

## Figure S3.1 CFi graph ----

CFi <- ggplot(exceed_daily) + geom_line(aes(x=month_date, y= CF_eff, colour=as.factor(covid_factor)), ) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  ylab("CFi") + xlab("") + 
  scale_color_manual(values=c("#F8766D","#7CAE00","#00BFC4","#C77CFF"), 
                     breaks=c("0.05", "0.1", "0.15", "0.2"),
                     labels=c("C1", "C2","C3","C4"),
                     name="Scenario") + theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        legend.key.size = unit(2,"line"), legend.direction="vertical",
        legend.background = element_rect(size=0.5, linetype="solid", 
                                         colour ="black"),
        legend.position=c(0.15, 0.9), legend.justification =c(0,1),
        text = element_text(size=txtsize_figs1_3), plot.title = element_text(hjust = 0.5, size=txtsize_figs1_3),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) 

jpeg(filename="4_Outputs/figS3_1_CFi_plot.jpeg", width=11*1.68, height=10, units = "in", res=300)
CFi
dev.off()

##### Figure S4.1 - Daily capacity exceedence for range of PROPTEST values, COVID-19 scenario C1 (UK) ----
ylab.exceed.daily1 <- c(seq(-2,2,1)*100)
ylab.exceed.daily1.lab <- paste0(ylab.exceed.daily1, "K")
ylab.exceed.daily1.lab[ylab.exceed.daily1.lab=="0K"] <- "0"
ylab.exceed.daily2 <- c(seq(-4,2,2)*100)
ylab.exceed.daily2.lab <- paste0(ylab.exceed.daily2, "K")
ylab.exceed.daily2.lab[ylab.exceed.daily2.lab=="0K"] <- "0"

p1.exceed.covid.5 <- ggplot(exceed_daily %>% filter(PR==0.4, covid_factor==0.05), aes(x=month_date, y=daily_exceed_pillar1_2_covid)) + 
  geom_bar(stat="identity",fill="#00BFC4", col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily1.lab,
                     breaks = ylab.exceed.daily1*1e3,
                     limits=c(range(ylab.exceed.daily1*1e3))) +  
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2_covid, ymin=daily_exceed_pillar1_2_lci_covid, ymax=daily_exceed_pillar1_2_uci_covid), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2_covid), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "a) PROPTEST=40%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

p2.exceed.covid.5 <- ggplot(exceed_daily %>% filter(PR==0.6, covid_factor==0.05), aes(x=month_date, y=daily_exceed_pillar1_2_covid)) + 
  geom_bar(stat="identity", aes(fill=exceed_pillar1_2_y_n_covid), col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily1.lab,
                     breaks = ylab.exceed.daily1*1e3,
                     limits=c(range(ylab.exceed.daily1*1e3))) + 
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2_covid, ymin=daily_exceed_pillar1_2_lci_covid, ymax=daily_exceed_pillar1_2_uci_covid), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2_covid), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "b) PROPTEST=60%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

p3.exceed.covid.5 <- ggplot(exceed_daily %>% filter(PR==0.8, covid_factor==0.05), aes(x=month_date, y=daily_exceed_pillar1_2_covid)) + 
  geom_bar(stat="identity", aes(fill=exceed_pillar1_2_y_n_covid), col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily2.lab,
                     breaks = ylab.exceed.daily2*1e3,
                     limits=c(min(ylab.exceed.daily2*1e3),max(ylab.exceed.daily2*1e3))) + 
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2_covid, ymin=daily_exceed_pillar1_2_lci_covid, ymax=daily_exceed_pillar1_2_uci_covid), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2_covid), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "c) PROPTEST=80%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

p4.exceed.covid.5 <- ggplot(exceed_daily %>% filter(PR==1, covid_factor==0.05), aes(x=month_date, y=daily_exceed_pillar1_2_covid)) + 
  geom_bar(stat="identity", aes(fill=exceed_pillar1_2_y_n_covid), col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily2.lab,
                     breaks = ylab.exceed.daily2*1e3,
                     limits=c(min(ylab.exceed.daily2*1e3),max(ylab.exceed.daily2*1e3))) + 
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2_covid, ymin=daily_exceed_pillar1_2_lci_covid, ymax=daily_exceed_pillar1_2_uci_covid), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2_covid), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "d) PROPTEST=100%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

jpeg(filename="4_Outputs/figS4_1_capacity_exceedance_with_COVID_UK_scenarioC1.jpeg", width=11*1.68, height=10, units = "in", res=300)
grid.arrange(p1.exceed.covid.5, p2.exceed.covid.5, p3.exceed.covid.5, p4.exceed.covid.5, nrow = 2,
             left = textGrob("Remaining testing capacity per day", rot = 90, vjust = 1, gp = gpar(cex = 2.2)))
dev.off()

##### Figure S4.2 - Daily capacity exceedence for range of PROPTEST values, COVID-19 scenario C3 (UK) ----

p1.exceed.covid.15 <- ggplot(exceed_daily %>% filter(PR==0.4, covid_factor==0.15), aes(x=month_date, y=daily_exceed_pillar1_2_covid)) + 
  geom_bar(stat="identity",fill="#00BFC4", col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily1.lab,
                     breaks = ylab.exceed.daily1*1e3,
                     limits=c(range(ylab.exceed.daily1*1e3))) +  
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2_covid, ymin=daily_exceed_pillar1_2_lci_covid, ymax=daily_exceed_pillar1_2_uci_covid), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2_covid), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "a) PROPTEST=40%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

p2.exceed.covid.15 <- ggplot(exceed_daily %>% filter(PR==0.6, covid_factor==0.15), aes(x=month_date, y=daily_exceed_pillar1_2_covid)) + 
  geom_bar(stat="identity", aes(fill=exceed_pillar1_2_y_n_covid), col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily1.lab,
                     breaks = ylab.exceed.daily1*1e3,
                     limits=c(range(ylab.exceed.daily1*1e3))) + 
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2_covid, ymin=daily_exceed_pillar1_2_lci_covid, ymax=daily_exceed_pillar1_2_uci_covid), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2_covid), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "b) PROPTEST=60%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

p3.exceed.covid.15 <- ggplot(exceed_daily %>% filter(PR==0.8, covid_factor==0.15), aes(x=month_date, y=daily_exceed_pillar1_2_covid)) + 
  geom_bar(stat="identity", aes(fill=exceed_pillar1_2_y_n_covid), col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily2.lab,
                     breaks = ylab.exceed.daily2*1e3,
                     limits=c(min(ylab.exceed.daily2*1e3),max(ylab.exceed.daily2*1e3))) + 
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2_covid, ymin=daily_exceed_pillar1_2_lci_covid, ymax=daily_exceed_pillar1_2_uci_covid), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2_covid), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "c) PROPTEST=80%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

p4.exceed.covid.15 <- ggplot(exceed_daily %>% filter(PR==1, covid_factor==0.15), aes(x=month_date, y=daily_exceed_pillar1_2_covid)) + 
  geom_bar(stat="identity", aes(fill=exceed_pillar1_2_y_n_covid), col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily2.lab,
                     breaks = ylab.exceed.daily2*1e3,
                     limits=c(min(ylab.exceed.daily2*1e3),max(ylab.exceed.daily2*1e3))) + 
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2_covid, ymin=daily_exceed_pillar1_2_lci_covid, ymax=daily_exceed_pillar1_2_uci_covid), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2_covid), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "d) PROPTEST=100%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

jpeg(filename="4_Outputs/figS4_2_capacity_exceedance_with_COVID_UK_scenarioC3", width=11*1.68, height=10, units = "in", res=300)
grid.arrange(p1.exceed.covid.15, p2.exceed.covid.15, p3.exceed.covid.15, p4.exceed.covid.15, nrow = 2,
             left = textGrob("Remaining testing capacity per day", rot = 90, vjust = 1, gp = gpar(cex = 2.2)))
dev.off()

##### Figure S5.3 - Daily capacity exceedence for range of PROPTEST values, COVID-19 scenario C4 (UK) ----
p1.exceed.covid.20 <- ggplot(exceed_daily %>% filter(PR==0.4, covid_factor==0.20), aes(x=month_date, y=daily_exceed_pillar1_2_covid)) + 
  geom_bar(stat="identity",fill="#00BFC4", col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily1.lab,
                     breaks = ylab.exceed.daily1*1e3,
                     limits=c(range(ylab.exceed.daily1*1e3))) +  
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2_covid, ymin=daily_exceed_pillar1_2_lci_covid, ymax=daily_exceed_pillar1_2_uci_covid), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2_covid), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "a) PROPTEST=40%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

p2.exceed.covid.20 <- ggplot(exceed_daily %>% filter(PR==0.6, covid_factor==0.20), aes(x=month_date, y=daily_exceed_pillar1_2_covid)) + 
  geom_bar(stat="identity", aes(fill=exceed_pillar1_2_y_n_covid), col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily1.lab,
                     breaks = ylab.exceed.daily1*1e3,
                     limits=c(range(ylab.exceed.daily1*1e3))) + 
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2_covid, ymin=daily_exceed_pillar1_2_lci_covid, ymax=daily_exceed_pillar1_2_uci_covid), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2_covid), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "b) PROPTEST=60%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

p3.exceed.covid.20 <- ggplot(exceed_daily %>% filter(PR==0.8, covid_factor==0.20), aes(x=month_date, y=daily_exceed_pillar1_2_covid)) + 
  geom_bar(stat="identity", aes(fill=exceed_pillar1_2_y_n_covid), col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily2.lab,
                     breaks = ylab.exceed.daily2*1e3,
                     limits=c(min(ylab.exceed.daily2*1e3),max(ylab.exceed.daily2*1e3))) + 
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2_covid, ymin=daily_exceed_pillar1_2_lci_covid, ymax=daily_exceed_pillar1_2_uci_covid), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2_covid), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "c) PROPTEST=80%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

p4.exceed.covid.20 <- ggplot(exceed_daily %>% filter(PR==1, covid_factor==0.20), aes(x=month_date, y=daily_exceed_pillar1_2_covid)) + 
  geom_bar(stat="identity", aes(fill=exceed_pillar1_2_y_n_covid), col="black") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.daily2.lab,
                     breaks = ylab.exceed.daily2*1e3,
                     limits=c(min(ylab.exceed.daily2*1e3),max(ylab.exceed.daily2*1e3))) + 
  geom_errorbar(aes(month_date, daily_exceed_pillar1_2_covid, ymin=daily_exceed_pillar1_2_lci_covid, ymax=daily_exceed_pillar1_2_uci_covid), size = linesize) +
  geom_point(aes(month_date, daily_exceed_pillar1_2_covid), size=2) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "d) PROPTEST=100%") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

jpeg(filename="4_Outputs/figS3_3_capacity_exceedance_with_COVID_UK_scenarioC4.jpeg", width=11*1.68, height=10, units = "in", res=300)
grid.arrange(p1.exceed.covid.20, p2.exceed.covid.20, p3.exceed.covid.20, p4.exceed.covid.20, nrow = 2,
             left = textGrob("Remaining testing capacity per day", rot = 90, vjust = 1, gp = gpar(cex = 2.2)))
dev.off()

##### Figure S5.1 - total demand PROPTEST=0.4, for COVID-19 scenarios C1-C4 (UK) ----
ylab.exceed.dailyfig7alt <- c((0:3)*100)
ylab.exceed.dailyfig7alt.lab <- paste0(ylab.exceed.dailyfig7alt, "K")
ylab.exceed.dailyfig7alt.lab[ylab.exceed.dailyfig7alt.lab=="0K"] <- "0"

lab7alt <- 5.5
labsize7alt <- 0.45
labheight <- UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"] + 7000

stackC1 <- ggplot(dat.stacked_plot %>% filter(covid_factor==0.05, PR==0.4), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "a) C1") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

stackC2 <- ggplot(dat.stacked_plot %>% filter(covid_factor==0.1, PR==0.4), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "b) C2") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

stackC3 <- ggplot(dat.stacked_plot %>% filter(covid_factor==0.15, PR==0.4), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "c) C3") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

stackC4 <- ggplot(dat.stacked_plot %>% filter(covid_factor==0.2, PR==0.4), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "d) C4") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

jpeg(filename="4_Outputs/figS5_1_demand_with_PR40.jpeg", width=11*1.68, height=10, units = "in", res=300)
grid_arrange_shared_legend(stackC1, stackC2, stackC3, stackC4, ncol = 2, nrow = 2, position='top', 
                           left = textGrob("Tests per day", rot = 90, vjust = 1, gp = gpar(cex = 2.2)))
dev.off()

##### Figure S5.2 - total demand PROPTEST=0.6, for COVID-19 scenarios C1-C4 (UK) ----
ylab.exceed.dailyfig7alt <- c((0:4)*100)
ylab.exceed.dailyfig7alt.lab <- paste0(ylab.exceed.dailyfig7alt, "K")
ylab.exceed.dailyfig7alt.lab[ylab.exceed.dailyfig7alt.lab=="0K"] <- "0"

labheight <- UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"] + 7000

stackC1 <- ggplot(dat.stacked_plot %>% filter(covid_factor==0.05, PR==0.6), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "a) C1") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

stackC2 <- ggplot(dat.stacked_plot %>% filter(covid_factor==0.1, PR==0.6), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "b) C2") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

stackC3 <- ggplot(dat.stacked_plot %>% filter(covid_factor==0.15, PR==0.6), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "c) C3") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

stackC4 <- ggplot(dat.stacked_plot %>% filter(covid_factor==0.2, PR==0.6), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "d) C4") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

jpeg(filename="4_Outputs/figS5_2_demand_with_PR60.jpeg", width=11*1.68, height=10, units = "in", res=300)
grid_arrange_shared_legend(stackC1, stackC2, stackC3, stackC4, ncol = 2, nrow = 2, position='top', 
                           left = textGrob("Tests per day", rot = 90, vjust = 1, gp = gpar(cex = 2.2)))
dev.off()

##### Figure S5.3 - total demand PROPTEST=1, for COVID-19 scenarios C1-C4 (UK) ----
ylab.exceed.dailyfig7alt <- c((0:7)*100)
ylab.exceed.dailyfig7alt.lab <- paste0(ylab.exceed.dailyfig7alt, "K")
ylab.exceed.dailyfig7alt.lab[ylab.exceed.dailyfig7alt.lab=="0K"] <- "0"

labheight <- UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"] + 7000

stackC1 <- ggplot(dat.stacked_plot %>% filter(covid_factor==0.05, PR==1), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "a) C1") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

stackC2 <- ggplot(dat.stacked_plot %>% filter(covid_factor==0.1, PR==1), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "b) C2") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

stackC3 <- ggplot(dat.stacked_plot %>% filter(covid_factor==0.15, PR==1), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "c) C3") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

stackC4 <- ggplot(dat.stacked_plot %>% filter(covid_factor==0.2, PR==1), aes(x=month_date, y=demand, fill=type)) + 
  geom_bar(stat="identity", col="black", position="stack") +
  scale_fill_manual(values=c("#00A9FF","#FF61CC"), labels=c("Baseline", "COVID-19")) +
  geom_hline(yintercept=UKlab_capacity$capacity[UKlab_capacity$type=="Pillar1+2"], 
             col="#e34a33", linetype=1) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b\n%Y", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(labels = ylab.exceed.dailyfig7alt.lab,
                     breaks = ylab.exceed.dailyfig7alt*1e3,
                     limits=c(range(ylab.exceed.dailyfig7alt*1e3))) +  
  geom_errorbar(aes(month_date, ymin=total_demand_lci, ymax=total_demand_uci), size = linesize) +
  geom_point(aes(month_date, total_demand), size=2, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype=1) +
  ggtitle(label = "d) C4") +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.title = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize), plot.title = element_text(hjust = 0.5, size=txtsize),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  geom_label(
    label="Capacity", 
    x=as.Date("2021-6-5","%Y-%m-%d"),
    y=labheight,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = labsize7alt,
    size = lab7alt,
    color = "#e34a33",
    fill="white"
  )

jpeg(filename="4_Outputs/figS5_3_demand_with_PR100.jpeg", width=11*1.68, height=10, units = "in", res=300)
grid_arrange_shared_legend(stackC1, stackC2, stackC3, stackC4, ncol = 2, nrow = 2, position='top', 
                           left = textGrob("Tests per day", rot = 90, vjust = 1, gp = gpar(cex = 2.2)))
dev.off()


##### Figure S6.1 - Incidence rates for cough stratified by month and age group (England) ----
p1 <- ggplot(results_ENG %>% filter(agegrp=="<=16" & symptoms=="cough_any__10"), aes(x=date, y=inc)) + 
  geom_errorbar(aes(date, inc, ymin=lci, ymax=uci), size = linesize) +
  geom_point(aes(date, inc), size = pntsize, shape=21, fill = "#56B4E9", col="black") +
  geom_line(aes(date,inc), linetype=2, col ="#999999", size = linesize) +
  ggtitle(label = "a) 0-16 years") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(limits=c(0,15000),labels=comma) +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, size=txtsize), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize)) 
p2 <- ggplot(results_ENG %>% filter(agegrp=="17-55" & symptoms=="cough_any__10"), aes(x=date, y=inc)) + 
  geom_errorbar(aes(date, inc, ymin=lci, ymax=uci), size = linesize) +
  geom_point(aes(date, inc), size = pntsize, shape=21, fill = "#56B4E9", col="black") +
  geom_line(aes(date,inc), linetype=2, col ="#999999", size = linesize) +
  ggtitle(label = "b) 17-55 years", ) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(limits=c(0,8500),labels=comma) +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, size=txtsize), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize)) 

p3 <- ggplot(results_ENG %>% filter(agegrp=="56-70" & symptoms=="cough_any__10"), aes(x=date, y=inc)) + 
  geom_errorbar(aes(date, inc, ymin=lci, ymax=uci), size = linesize) +
  geom_point(aes(date, inc), size = pntsize, shape=21, fill = "#56B4E9", col="black") +
  geom_line(aes(date,inc), linetype=2, col ="#999999", size = linesize) +
  ggtitle(label = "c) 56-70 years", ) +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(limits=c(0,8500),labels=comma) +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, size=txtsize), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize)) 

p4 <- ggplot(results_ENG %>% filter(agegrp==">70" & symptoms=="cough_any__10"), aes(x=date, y=inc)) + 
  geom_errorbar(aes(date, inc, ymin=lci, ymax=uci), size = linesize) +
  geom_point(aes(date, inc), size = pntsize, shape=21, fill = "#56B4E9", col="black") +
  geom_line(aes(date,inc), linetype=2, col ="#999999", size = linesize) +
  ggtitle(label = "d) >70 years") +
  scale_x_date(lab = format(date_breaks_vec, 
                            ifelse(date_breaks_vec == as.Date("2021-01-01", "%Y-%m-%d") | date_breaks_vec ==as.Date("2020-07-01", "%Y-%m-%d"),
                                   "%b", "%b")), 
               breaks = date_breaks_vec, limits=c(date_start-15, date_end+15)) +
  scale_y_continuous(limits=c(0,8500),labels=comma) +
  ylab("") +
  xlab("") + theme_bw() + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, size=txtsize), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize))

jpeg(filename="4_Outputs/figS6_1_incidences_cough_agestrat_england.jpeg", width=11*1.68, height=10, units = "in", res=300)
grid.arrange(arrangeGrob(p1, p2, p3, p4, nrow = 2, 
                         left = textGrob("Incidence rate per 100,000-person-week", rot = 90, vjust = 1, gp = gpar(cex = 2.2))))
dev.off()

#### Figure S6.2 - All-age incidence rates for cough stratified by month (England) ----
p.all.ages.cough <- ggplot(results_ENG %>% filter(agegrp=="All ages" & symptoms=="cough_any__10"), aes(x=date, y=inc)) + 
  geom_errorbar(aes(date, inc, ymin=lci, ymax=uci), size = linesize) +
  geom_point(aes(date, inc), size = pntsize, shape=21, fill = "#56B4E9", col="black") +
  geom_line(aes(date,inc), linetype=2, col ="#999999", size = linesize) +
  scale_x_date(breaks = date_breaks_vec_ENG, date_labels = "%b",limits=c(date_start_ENG-15, date_end_ENG+15)) +
  scale_y_continuous(limits=c(0,7000), breaks=c((0:7)*1000), label=comma) +
  ylab("Incidence rate per 100,000-person-week") + xlab("") + theme_bw() + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, size=txtsize), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize_figs1_3), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

jpeg(filename="4_Outputs/figS6_2_incidences_cough_allages_england.jpeg", width=10*1.68, height=10, units = "in", res=300)
p.all.ages.cough
dev.off()

##### Figure S7.1 - All-age incidence rates for fever stratified by month (England) ----
p.all.ages.fever <- ggplot(results_ENG %>% filter(agegrp=="All ages" & symptoms=="fever_any__10"), aes(x=date, y=inc)) + 
  geom_errorbar(aes(date, inc, ymin=lci, ymax=uci), size = linesize) +
  geom_point(aes(date, inc), size = pntsize, shape=21, fill = "#56B4E9", col="black") +
  geom_line(aes(date,inc), linetype=2, col ="#999999", size = linesize) +
  scale_x_date(breaks = date_breaks_vec_ENG, date_labels = "%b",limits=c(date_start_ENG-15, date_end_ENG+15)) +
  scale_y_continuous(limits=c(0,2500), label=comma) +
  ylab("Incidence rate per 100,000-person-week") + xlab("") + theme_bw() + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, size=txtsize), panel.grid.minor.x = element_blank(), 
        text = element_text(size=txtsize_figs1_3), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

jpeg(filename="4_Outputs/figS7_1_incidences_fever_allages_england.jpeg", width=10*1.68, height=10, units = "in", res=300)
p.all.ages.fever
dev.off()
