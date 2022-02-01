

# libraries ---------------------------------------------------------------

library(tidyverse)
tprint <- 65  # default tibble print
options(tibble.print_max = tprint, tibble.print_min = tprint) # show up to tprint rows

# tools
library(fs)
library(readxl)
library(haven)
library(lubridate)
library(RColorBrewer)
library(RcppRoll)
library(fredr)
library(btools)

# graphics
library(scales)
library(ggbeeswarm)
library(patchwork)
library(gridExtra)
library(ggrepel)
library(ggbreak)
library(RColorBrewer)

# tables
library(knitr)
library(kableExtra)
library(DT)
library(gt)
library(gtsummary)


# notes, urls, etc. -------------------------------------------------------
# state level data
# path <- r"(C:\Users\donbo\Downloads\COVID-19_Vaccinations_in_the_United_States_Jurisdiction.csv)"


# locations ----
dld <- r"(C:\Users\donbo\Downloads\ctcvax)"  # download directory

# constants -------------------------------------------------------------------------
caption_left <- theme(plot.caption = element_text(hjust = 0))


# functions ---------------------------------------------------------------
stname <- function(stabbr) {
  stabbrs <- c(state.abb, "DC", "US")
  stnames <- c(state.name, "District of Columbia", "United States")
  stnames[match(stabbr, stabbrs)]
}

# get data ----------------------------------------------------------------
#.. county-level cumulative vaccinations ----
# https://www.cdc.gov/coronavirus/2019-ncov/vaccines/distributing/about-vaccine-data.html

# county level data
# https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh
# https://data.cdc.gov/api/views/8xkx-amqh/rows.csv?accessType=DOWNLOAD

# this does not work well:
# url <- "https://data.cdc.gov/api/views/8xkx-amqh/rows.csv?accessType=DOWNLOAD"
# url <- "https://data.cdc.gov/api/views/8xkx-amqh/rows.csv?accessType=DOWNLOAD&bom=true&format=true"
# download.file(url, file.path(dld, "rows.csv"), mode="wb")

# I was not successful in downloading programmatically - it is possible but takes long
# better to download outside of R from url below and then read.
# url <- "https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh"
 
fname <- "COVID-19_Vaccinations_in_the_United_States_County_2022-01-31.csv"
path <- file.path(dld, fname)

df <- read_csv(path)
dim(df)  # 1.3m, 51 cols
glimpse(df)
# tbl_summary(df) too much output
# ages 5+, 12+, 18+, 65+

# clean and save a subset -------------------------------------------------
df2 <- df %>%
  rename(date=Date,
         mmrw=MMWR_week,
         fips=FIPS,
         state=Recip_State,
         county=Recip_County,
         metro=Metro_status,
         svi=SVI_CTGY,
         dose118p=Administered_Dose1_Recip_18Plus,
         poppct18p=Administered_Dose1_Recip_18PlusPop_Pct,
         pop18p=Census2019_18PlusPop,
         pop=Census2019) %>%
  mutate(date=as.Date(date, format="%m/%d/%Y")) %>%
  arrange(date, state, county)
summary(df2)
ns(df2)

df3 <- df2 %>%
  select(date, mmrw,
         fips, state, county,
         metro, svi,
         dose118p,
         poppct18p,
         pop18p,
         pop)
summary(df3)

# create a factor for SVI
# svi: CDC Social Vulnerability Index (SVI) rank categorization where:
# A = 0– 0.25 SVI rank
# B = 0.2501–0.50 SVI rank
# C = 0.5001–0.75 SVI rank
# D = 0.7501–1.0 SVI rank
df3 %>%
  filter(state=="NY", date==max(date)) %>%
  arrange(desc(poppct18p)) %>%
  select(county, date, poppct18p, svi)
# Bronx has a D so that must be highest quartile of vulnerability
# Saratoga has an A so that must be lowest quartile of vulnerability
# a little surprising that Nassau County has an A
# wonder how well this really discriminates
# note that Unknown county is not coded

vax <- df3 %>%
  mutate(svif=factor(
    svi, 
    levels=c("A", "B", "C", "D"),
    labels=c("Quartile 1: Least vulnerability",
             "Quartile 2: 2nd-least vulnerability",
             "Quartile 3: 2nd-most vulnerability",
             "Quartile 4: Greatest vulnerability"))) %>%
  relocate(svif, .after = svi)
glimpse(vax)
saveRDS(vax, here::here("data", "vax.rds"))
# memory()
# rm(df, df2, df3)

# svi: CDC Social Vulnerability Index (SVI) rank categorization where:
# A = 0– 0.25 SVI rank
# B = 0.2501–0.50 SVI rank
# C = 0.5001–0.75 SVI rank
# D = 0.7501–1.0 SVI rank"


# explore -----------------------------------------------------------------
vax <- readRDS(here::here("data", "vax.rds"))
summary(vax)
summary(vax %>% filter(date >= "2021-07-01", date <= "2021-12-31"))
count(vax, metro)
count(vax, svi, svif)

vax %>%
  group_by(date) %>%
  summarise(dose118p=sum(dose118p, na.rm=TRUE)) %>%
  ggplot(aes(date, dose118p)) +
  geom_line() +
  geom_point() +
  geom_smooth() +
  ggtitle("total 18+ dose1 vaccinations")
# something caused bump up around Nov; a few other small bumps

vax %>%
  group_by(date) %>%
  summarise(n.notna=sum(!is.na(dose118p))) %>%
  ggplot(aes(date, n.notna)) +
  geom_line() +
  geom_point() +
  geom_smooth() +
  ggtitle("# of not NA counties")
# falls 10+% Jan-Jul 2021 then rises, weird

vax %>%
  group_by(date) %>%
  filter(!is.na(dose118p), !is.na(pop18p)) %>%
  summarise(dose118p=sum(dose118p),
            pop18p=sum(pop18p)) %>%
  mutate(poppct18p=dose118p / pop18p) %>%
  ggplot(aes(date, poppct18p)) +
  geom_line() +
  geom_point() +
  geom_smooth() +
  ggtitle("vaccination rate 18+")
# falls 10+% Jan-Jul 2021 then rises, weird

# metro non/metro
vax %>%
  filter(!is.na(metro)) %>%
  group_by(date, metro) %>%
  filter(!is.na(dose118p), !is.na(pop18p)) %>%
  summarise(dose118p=sum(dose118p),
            pop18p=sum(pop18p), .groups="drop") %>%
  mutate(poppct18p=dose118p / pop18p) %>%
  ggplot(aes(date, poppct18p, colour=metro)) +
  geom_line() +
  geom_point() +
  ggtitle("vaccination rate 18+, metro/non-metro")
# metro much higher than nonmetro

# how does svi vary by state?
svicheck <- vax %>% 
  filter(date=="2021-09-30") %>%  # around the middle of our period
  filter(dose118p > 0, !str_detect(county, "Unknown")) %>% # make sure it has usable data
  group_by(state, svi) %>%
  summarize(n=n(), pop=sum(pop, na.rm=TRUE),
            .groups = "drop") %>%
  group_by(state) %>%
  mutate(pctn=n / sum(n),
         pctpop=pop / sum(pop)) %>%
  select(state, svi, pctpop) %>%
  pivot_wider(names_from = svi, 
              values_from = pctpop,
              values_fill = 0) %>%
  arrange(desc(D))
svicheck
# state       A       B      C       D  `NA`
# <chr>   <dbl>   <dbl>  <dbl>   <dbl> <dbl>
# 1 NM    0       0       0      1           0
# 2 RI    0.397   0       0      0.603       0
# 3 MS    0       0.155   0.244  0.600       0
# 4 CA    0.0187  0.292   0.172  0.517       0
# 5 LA    0       0.126   0.443  0.431       0
# ...
# 39 MN    0.425   0.446   0.114  0.0148      0
# 40 IL    0.240   0.223   0.524  0.0136      0
# 41 IA    0.444   0.472   0.0788 0.00533     0
# 42 UT    0.163   0.813   0.0191 0.00477     0
# 43 CT    0.138   0.372   0.490  0           0

# we need to know number of A and D counties in each state
# for our time period
svicheck2 <- vax %>% 
  filter(date=="2021-09-30") %>%  # around the middle of our period
  filter(dose118p > 0, !str_detect(county, "Unknown")) %>% # make sure it has usable data
  group_by(state, svi) %>%
  summarize(n=n(),
            .groups = "drop") %>%
  pivot_wider(names_from = svi, 
              values_from = n,
              values_fill = 0) %>%
  arrange(desc(D))
svicheck2
# state     A     B     C     D  `NA`
# <chr> <int> <int> <int> <int> <int>
# 1 GA       14    14    35    82     0
# 2 MS        0     4    23    55     0
# 3 LA        0     5    14    45     0
# 4 NC        4    26    26    44     0
# 5 KY        6    28    43    42     0
# 6 AR        1     9    25    40     0
# 7 OK        3    19    17    37     0
# 8 AL        2     8    27    30     0
# 9 FL        4    15    19    29     0
# 10 VA       41    43    21    27     0


# construct data by week in July-Dec 2021 ---------------------------------
vweeks <- vax %>%
  filter(date >= "2021-07-01", 
         date <= "2021-12-31",
         state %in% state.abb,
         !str_detect(county, "Unknown")) %>%
  mutate(day=day(date), 
         nmonth=month(date)) %>%
  filter(day %in% c(7, 14, 21)) %>%
  # compute incremental vaccinations between weeks
  arrange(state, county, date) %>%
  group_by(state, county) %>%
  mutate(vaxpct=dose118p / pop18p, # we need more precision than reported
         newvax=vaxpct - lag(vaxpct)) %>%
  filter(day != 7) %>%
  ungroup %>%
  mutate(week=factor(day,
                     levels=c(14, 21),
                     labels=c("ctcm1", "ctcp1")), # minus or plus 1 week
         month=factor(nmonth,
                      levels=7:12,
                      labels=month.abb[7:12])
         )
stcos <- count(vweeks, state, county) 
stcos %>% filter(state=="GA")

# inspect the results to make sure they are right
# GA, MS, LA, NC, KY all look worth looking at as high D states
# st <- "TX"; cnty <- "Harris"  # no county data until Nov
st <- "GA"; cnty <- "Decatur"
vweeks %>%
  filter(state==st, str_detect(county, cnty)) %>%
  write_csv(here::here("temp.csv"))

# how much did vax rates change in a single month in a single state
vweeks %>%
  filter(state=="NY", week %in% c("ctcm1", "ctcp1"), month=="Jul") %>%
  select(month, county, svi, week, newvax) %>%
  pivot_wider(names_from = week, values_from = newvax) %>%
  ggplot(aes(ctcm1, ctcp1, colour=svi)) +
  geom_point() +
  geom_abline(slope=1)
  
sts <- c("RI", "MS", "CA", "NM")  # top 4 states %pop in D svi counties
# states with enough counties
sts <- c("CA", "MS", "TX", "GA")
sts <- c("GA", "MS", "LA", "NC")
sts <- c("GA", "NC", "KY", "VA")
pdata <- vweeks %>%
  filter(state %in% sts,
         week %in% c("ctcm1", "ctcp1"),
         svi %in% c("A", "D")) %>%
  select(month, state, county, svi, week, newvax) %>%
  pivot_wider(names_from = week, values_from = newvax) %>%
  mutate(change=ctcp1 - ctcm1,
         pch=change / ctcm1)

p <- pdata %>%
  filter(month=="Sep") %>%
  ggplot(aes(ctcm1, ctcp1, colour=svi)) +
  geom_point() +
  geom_abline(slope=1) +
  facet_wrap(~state, ncol=2, scales="free")
p


#.. find states with large numbers of A and D social vulnerability counties ----
# and large % of pop in D counties

# how does svi vary by state?
svicheck <- vax %>% 
  filter(date=="2021-09-30") %>%  # around the middle of our period
  filter(dose118p > 0, !str_detect(county, "Unknown")) %>% # make sure it has usable data
  group_by(state, svi) %>%
  summarize(n=n(), pop=sum(pop, na.rm=TRUE),
            .groups = "drop") %>%
  group_by(state) %>%
  mutate(pctn=n / sum(n),
         pctpop=pop / sum(pop)) %>%
  select(state, svi, pctpop) %>%
  pivot_wider(names_from = svi, 
              values_from = pctpop,
              values_fill = 0) %>%
  arrange(desc(D))
svicheck
# state       A       B      C       D  `NA`
# <chr>   <dbl>   <dbl>  <dbl>   <dbl> <dbl>
# 1 NM    0       0       0      1           0
# 2 RI    0.397   0       0      0.603       0
# 3 MS    0       0.155   0.244  0.600       0
# 4 CA    0.0187  0.292   0.172  0.517       0
# 5 LA    0       0.126   0.443  0.431       0
# ...
# 39 MN    0.425   0.446   0.114  0.0148      0
# 40 IL    0.240   0.223   0.524  0.0136      0
# 41 IA    0.444   0.472   0.0788 0.00533     0
# 42 UT    0.163   0.813   0.0191 0.00477     0
# 43 CT    0.138   0.372   0.490  0           0

# we need to know number of A and D counties in each state
# for our time period
svigroups <- vax %>% 
  filter(date=="2021-09-30") %>%  # around the middle of our period
  filter(dose118p > 0, !str_detect(county, "Unknown")) %>% # make sure it has usable data
  filter(state %in% state.abb) %>%
  group_by(state, svi) %>%
  summarize(n=n(),
            pop18p=sum(pop18p, na.rm=TRUE),
            .groups = "drop") %>%
  group_by(state) %>%
  mutate(poppct=pop18p / sum(pop18p)) %>%
  select(-pop18p) %>%
  pivot_wider(names_from = svi, values_from = c(n, poppct),
              values_fill = 0)

svigroups %>%
  filter(n_A >= 5, n_D >= 5) %>%
  arrange(desc(poppct_D))

  

sts <- c("GA", "NC", "KY", "VA")
sts <- c("GA", "TN", "KY", "VA")
pdata <- vweeks %>%
  filter(state %in% sts,
         week %in% c("ctcm1", "ctcp1"),
         svi %in% c("A", "D")) %>%
  mutate(stname=stname(state)) %>%
  select(month, state, stname, county, svi, svif, week, dose118p) %>%
  pivot_wider(names_from = week, values_from = dose118p) %>%
  mutate(change=ctcp1 - ctcm1,
         pch=change / ctcm1,
         outlier=abs(pch) > .4) %>% # based on prior inspection of data
  na.omit()
summary(pdata)

capt1 <- "States selected have the largest percentages of their population in greatest-vulnerability counties,"
capt2 <- "\namong states with at least 5 counties in least- and greatest- social vulnerability categories."

capt3 <- "\nNote: Each dot is a county. Outliers (n=8) with absolute % change greater than 40% are excluded."
capt <- paste0(capt1, capt2 ,"\n", capt3)

gtitle <- "% change in 1st-dose vaccinations from week before advance CTC deposit date to week after"
gsubtitle <- "Selected states, population age 18+"

p <- pdata %>%
  filter(!outlier) %>%
  ggplot(aes(month, pch, colour=svif)) +
  geom_point(position=position_dodge(width = 0.40), size=1) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(name="% change from week before to week after",
                     breaks=seq(-1, 1, .02),
                     labels = label_percent(accuracy=1)) +
  scale_colour_manual(values=c("darkgreen", "blue")) +
  facet_wrap(~stname, ncol=2, scales="free") +
  ggtitle(gtitle,
          subtitle=gsubtitle) +
  labs(x=NULL,
       colour="Social vulnerability",
       caption=capt) +
  theme_bw() +
  caption_left
p
ggsave(filename = here::here("results", "dose1pch_bysvi.png"),
       plot=p, height=6, width=10, scale=1)


p <- vweeks %>%
  filter(state %In% c("NY", week %in% c("ctcm1", "ctcp1")) %>%
  select(month, county, svi, week, newvax) %>%
  pivot_wider(names_from = week, values_from = newvax) %>%
  ggplot(aes(ctcm1, ctcp1, colour=svi)) +
  geom_point() +
  geom_abline(slope=1) +
  facet_wrap(~month, ncol=2, scales="free")
p

ggsave(filename = here::here("results", "plot.png"), plot=p, width=8, height=8)


# weekly vaccinations, metro/nonmetro -------------------------------------
pbase <- vax %>%
  filter(!is.na(metro)) %>%
  arrange(fips, state, county, date) %>%
  group_by(fips, state, county) %>%
  mutate(wpoppct18p=(poppct18p - lag(poppct18p, 7)) / 7) %>%
  ungroup

pbase %>%
  filter(state=="NY", str_detect(county, "Albany")) %>%
  tail(20)

pdata <- pbase %>%
  group_by(state, metro, date) %>%
  summarise(wpoppct18p_mdn=median(wpoppct18p, na.rm=TRUE),
            wpoppct18p_mn=mean(wpoppct18p, na.rm=TRUE), .groups="drop")

pdata %>%
  filter(state %in% c("CA", "FL", "NY", "SC")) %>%
  filter(date >= "2021-06-15", date <= "2022-01-15") %>%
  ggplot(aes(date, wpoppct18p_mn, colour=metro)) +
  geom_line() +
  geom_point() +
  facet_wrap(~state, ncol=2)
  
  

#  filter(state=="NY", str_detect_any(county, c("Albany", "Washington", "Chauttauqua")))

pdata %>%
  ggplot(aes(date, wpoppct18p, colour=county)) +
  geom_line() +
  geom_point()
  group_by(date, metro) %>%
  filter(!is.na(dose118p), !is.na(pop18p)) %>%
  mutate(wpoppct18p=)
  summarise(dose118p=sum(dose118p),
            pop18p=sum(pop18p), .groups="drop") %>%
  mutate(poppct18p=dose118p / pop18p) %>%
  ggplot(aes(date, poppct18p, colour=metro)) +
  geom_line() +
  geom_point() +
  ggtitle("vaccination rate 18+, metro/non-metro")




#.. daily weekly monthly ----
statecos <- count(df2, state, county)
statecos %>%
  filter(state=="SC")

sccos <- statecos %>%
  filter(state=="SC", county!="Unknown County")

df3 <- df2 %>%
  select(date, state, county, dose1=Administered_Dose1_Recip_18PlusPop_Pct) %>%
  arrange(state, county, date) %>%
  group_by(state, county) %>%
  mutate(dose1=dose1 / 100,
         daily=dose1 - lag(dose1, 1),
         weekly=(dose1 - lag(dose1, 7)) / 7,
         days30=(dose1 - lag(dose1, 30)) / 30) %>%
  ungroup

set.seed(1234)
nsamp <- 4
usecos <- sccos %>%
  slice_sample(n=nsamp) %>%
  arrange(county)
usecos




# sources ----
# https://data.cdc.gov/Vaccinations/COVID-19-Vaccine-Distribution-Allocations-by-Juris/saz5-9hgg

# allocations ----
pall <- "COVID-19_Vaccine_Distribution_Allocations_by_Jurisdiction_-_Pfizer.csv"
df <- read_csv(file.path(dld, pall))
dim(df) # 1764    4
glimpse(df)
df2 <- df %>%
  rename(week=2) %>%
  mutate(date=as.Date(week, format="%m/%d/%Y"))
tbl_summary(df2) # 28 weeks in 2021, ending June

#.. Vaccine hesitancy ----
# https://data.cdc.gov/Vaccinations/Vaccine-Hesitancy-for-COVID-19-County-and-local-es/q9mh-h2tw/data
vhurl <- "https://data.cdc.gov/api/views/djj9-kh3p/rows.csv?accessType=DOWNLOAD"
vhfn <- "Vaccine_Hesitancy_for_COVID-19__Public_Use_Microdata_Areas__PUMAs_.csv"
vhfn2 <- "Vaccine_Hesitancy_for_COVID-19__County_and_local_estimates.csv"
# download.file(vhurl, file.path(dld, vhfn))

vhdf <- read_csv(file.path(dld, vhfn))
glimpse(vhdf) # 2351 records
tmp <- vhdf %>%
  filter(State=="New York")
summary(tmp) # 145 records

vhdf2 <- read_csv(file.path(dld, vhfn2))
glimpse(vhdf2) # 2351 records



#.. Vaccine administration ----

tmp <- df3 %>%
  filter(state=="SC", county %in% usecos$county)

df4 <- df3 %>%
  pivot_longer(cols = c(daily, weekly, days30)) %>%
  mutate(name=factor(name,
                     levels=c("daily", "weekly", "days30"),
                     labels=c("daily", "weekly", "30days"))) %>%
  arrange(state, county, name)

p <- df4 %>%
  filter(state=="SC", county %in% usecos$county) %>%
  ggplot(aes(date, value, colour=name)) +
  geom_line() +
  geom_point(aes(size=name)) +
  scale_size_manual(values=c(.25, .75, .75)) +
  scale_colour_manual(values=c("lightgrey", "darkgreen", "blue")) +
  scale_y_continuous(name="percent of 18+ population",
                     limits=c(-0.001, 0.01),
                     breaks=seq(-1, 1, .01),
                     labels=label_percent(accuracy = .1)) +
  facet_wrap(~county, ncol = 2, scales="free") +
  ggtitle("Daily-average number of new 1st-dose age 18+ vaccinations as % of population  18+",
          subtitle = paste0("3 data frequencies, ",
                            nsamp, " randomly selected South Carolina counties",
                            "; truncated at +1% (many daily values are higher)")) +
  theme_bw()
p
ggsave("plot.png", plot=p, width=12, height = 8)

#.. poor rich ----
counties <- read_csv(
  "state, county, richpoor
SC, Allendale County, poor
SC, York County, rich
NJ, Cumberland County, poor 
NJ, Hunterdon County, rich
NY, Chautauqua County, poor
NY, Saratoga County, rich"
)

df4 %>%
  right_join(counties, by=c("state", "county")) %>%
  unite(scpr, state, county, richpoor, remove=FALSE) %>%
  group_by(scpr) %>%
  arrange(date) %>%
  mutate(ddose1=dose1age18p - lag(dose1age18p, 7),
         ddose1pct=dose1age18ppct - lag(dose1age18ppct, 7)) %>%
  ungroup %>%
  select(date, scpr, richpoor, dose1age18ppct, ddose1pct) %>%
  pivot_longer(-c(date, scpr, richpoor)) %>%
  mutate(namef=factor(name, 
                      levels=c("dose1age18ppct", "ddose1pct"),
                      labels=c("Cumulative vaccination rate",
                               "Change in vaccination rate from 7 days earlier"))) %>%
  arrange(date, scpr, namef) %>%
  filter(richpoor=="poor") %>%
  ggplot(aes(date, value, colour=scpr)) +
  geom_line() +
  geom_point() +
  ggtitle(paste0(st, ": First-dose administration rate, ages 18+")) +
  labs(x=NULL, y="% of age group") +
  facet_wrap(~namef, scales="free", ncol=1) +
  theme_bw()




# OLD ----
ns(df2)
tmp <- count(df2, state, county)
tmp2 <- tmp %>% filter(!str_detect(county, "Unknown"))
tmp2 %>% filter(state %in% state.abb)

tmp <- df2 %>% filter(state=="TX")

df2 %>%
  mutate(kuk=ifelse(str_detect(county, "Unknown"), "unknown", "known")) %>%
  group_by(state, kuk) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = kuk, values_from = n)

count(df2, SVI_CTGY)

df3 <- df2 %>%
  # filter(state=="NY", str_detect(county, "Washington")) %>%
  select(date, state, county,
         dose1age05p=Administered_Dose1_Recip_5Plus, 
         dose1age12p=Administered_Dose1_Recip_12Plus,
         dose1age18p=Administered_Dose1_Recip_18Plus,
         dose1age12ppct=Administered_Dose1_Recip_12PlusPop_Pct,
         dose1age18ppct=Administered_Dose1_Recip_18PlusPop_Pct,
         pop18p=Census2019_18PlusPop) %>%
  mutate(dose1age0511=naz(dose1age05p) - naz(dose1age05p),
         dose1age1217=naz(dose1age12p) - naz(dose1age18p),
         pop12p=dose1age12p / (dose1age12ppct / 100),
         pop18p=dose1age18p / (dose1age18ppct / 100),
         pop1217=naz(pop12p) - naz(pop18p),
         dose1age1217pct=dose1age1217 / pop1217 * 100)
ns(df3)
unique(df3$date)

df4 <- df3 %>%
  filter(dose1age1217 > 0, county != "Unknown County")


df5 <- df4 %>%
  filter(state=="CA", county=="Los Angeles County") 

df5 <- df4 %>%
  filter(state=="NY", str_detect(county, "Albany"))
summary(df5)


df5 %>%
  ggplot(aes(date, dose1age18ppct)) +
  geom_line() +
  geom_point()

count(df4, state)
count(df4 %>% filter(state=="SC"), county)

st <- "NY"; cnty <- "Nassau"
st <- "SC"; cnty <- "Darlington"
df4 %>%
  filter(state==st, str_detect(county, cnty)) %>%
  arrange(date) %>%
  mutate(ddose1=dose1age18p - lag(dose1age18p, 7)) %>%
  ggplot(aes(date, ddose1)) +
  geom_line() +
  geom_point()

# poor, rich counties -- list poor first
st <- "SC"; prcos <- c("Allendale County", "York County")
st <- "NJ"; prcos <- c("Cumberland County", "Hunterdon County")# c("Hunterdon County", "Cumberland County") # c("Cumberland County", "Hunterdon County")
st <- "NY"; prcos <- c("Chautauqua County", "Saratoga County")
df4 %>%
  filter(state==st, county %in% prcos) %>%
  mutate(cgroup=factor(county, 
                       levels=prcos,
                       labels=paste0(prcos, " (", c("poor", "rich"), ")"))) %>%
  group_by(cgroup) %>%
  arrange(date) %>%
  mutate(ddose1=dose1age18p - lag(dose1age18p, 7),
         ddose1pct=dose1age18ppct - lag(dose1age18ppct, 7)) %>%
  ungroup %>%
  select(date, cgroup, dose1age18ppct, ddose1pct) %>%
  pivot_longer(-c(date, cgroup)) %>%
  mutate(namef=factor(name, 
                      levels=c("dose1age18ppct", "ddose1pct"),
                      labels=c("Cumulative vaccination rate",
                               "Change in vaccination rate from 7 days earlier"))) %>%
  arrange(date, cgroup, namef) %>%
  ggplot(aes(date, value, colour=cgroup)) +
  geom_line() +
  geom_point() +
  ggtitle(paste0(st, ": First-dose administration rate, ages 18+")) +
  labs(x=NULL, y="% of age group") +
  facet_wrap(~namef, scales="free", ncol=1) +
  theme_bw()


# poor, rich counties -- list poor first
st <- "SC"; prcos <- c("Allendale County", "York County")
st <- "SC"; prcos <- c("Dillon County", "York County")
st <- "NJ"; prcos <- c("Cumberland County", "Hunterdon County")# c("Hunterdon County", "Cumberland County") # c("Cumberland County", "Hunterdon County")
st <- "NY"; prcos <- c("Chautauqua County", "Saratoga County")

counties <- read_csv(
"state, county, richpoor
SC, Allendale County, poor
SC, York County, rich
NJ, Cumberland County, poor 
NJ, Hunterdon County, rich
NY, Chautauqua County, poor
NY, Saratoga County, rich"
)

df4 %>%
  right_join(counties, by=c("state", "county")) %>%
  unite(scpr, state, county, richpoor, remove=FALSE) %>%
  group_by(scpr) %>%
  arrange(date) %>%
  mutate(ddose1=dose1age18p - lag(dose1age18p, 7),
         ddose1pct=dose1age18ppct - lag(dose1age18ppct, 7)) %>%
  ungroup %>%
  select(date, scpr, richpoor, dose1age18ppct, ddose1pct) %>%
  pivot_longer(-c(date, scpr, richpoor)) %>%
  mutate(namef=factor(name, 
                      levels=c("dose1age18ppct", "ddose1pct"),
                      labels=c("Cumulative vaccination rate",
                               "Change in vaccination rate from 7 days earlier"))) %>%
  arrange(date, scpr, namef) %>%
  filter(richpoor=="poor") %>%
  ggplot(aes(date, value, colour=scpr)) +
  geom_line() +
  geom_point() +
  ggtitle(paste0(st, ": First-dose administration rate, ages 18+")) +
  labs(x=NULL, y="% of age group") +
  facet_wrap(~namef, scales="free", ncol=1) +
  theme_bw()


df5 %>%
  ggplot(aes(date, pop18p)) +
  geom_line() +
  geom_point()


df5 %>%
  select(-ends_with("p")) %>%
  pivot_longer(-c(date, state, county)) %>%
  ggplot(aes(date, value, colour=name)) +
  geom_line() +
  geom_point() 
  
df5 %>%
  ggplot(aes(date, dose1age12ppct)) +
  geom_line() +
  geom_point() 

df5 %>%
  ggplot(aes(date, dose1age12ppct)) +
  geom_line() +
  geom_point() 


df3 %>%
  arrange(date)

# estimate the percents
baseday <- 16
test1 <- df %>%
  # setNames(str_to_lower(names(.))) %>%
  rename(date=Date,
         state=Recip_State,
         county=Recip_County) %>%
  mutate(date=as.Date(date, format="%m/%d/%Y"),
         month=month(date),
         day=mday(date)) %>%
  filter(date >= "2021-07-01", date <= "2021-12-31",
         day %in% c(baseday - 7, baseday, baseday + 7)) %>%
  select(state, county, date, month, day, d1pct=Administered_Dose1_Recip_18PlusPop_Pct)

test2 <- test1 %>%
  arrange(state, county, month, day) %>%
  group_by(state, county, month) %>%
  mutate(wpct=d1pct - lag(d1pct)) %>%
  ungroup


test3 <- test2 %>%
  filter(day %in% c(baseday, baseday + 7)) %>%
  mutate(type=factor(day, levels=c(baseday, baseday + 7), labels=c("pre", "post")))

tmp <- test3 %>%
  # filter(state=="NY", str_detect(county, "Chautauqua")) %>%
  filter(state=="NY")

test3 %>%
  # filter(state=="NY", str_detect(county, "Chautauqua")) %>%
  filter(state=="SC") %>%
  ggplot(aes(month, wpct, colour=type)) +
  geom_point()

tmp3 <- test3 %>%
  # filter(state=="NY", str_detect(county, "Chautauqua")) %>%
  filter(wpct != 0) %>%
  filter(state %in% c("NJ", "NY", "SC")) %>%
  select(state, county, month, type, wpct) %>%
  pivot_wider(names_from = type, values_from = wpct) %>%
  mutate(change=post - pre) %>%
  group_by(state, month) %>%
  summarise(n=n(), change=median(change), .groups="drop")



tmp4 <- test3 %>%
  # filter(state=="NY", str_detect(county, "Chautauqua")) %>%
  filter(wpct != 0) %>%
  filter(state %in% c("NJ", "NY", "SC")) %>%
  select(state, county, month, type, wpct) %>%
  pivot_wider(names_from = type, values_from = wpct) %>%
  mutate(change=post - pre)

mdns <- tmp4 %>%
  group_by(month) %>%
  summarise(change=mean(change))
mdns

tmp4 %>%
  ggplot(aes(month, change)) +
  geom_point() +
  geom_point(colour="red", data=. %>% group_by(month) %>% mutate(change=mean(change)))
  geom_hline(yintercept = 0)

