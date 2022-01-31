

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

# dirs ----
dld <- r"(C:\Users\donbo\Downloads\ctcvax)"

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

# https://www.cdc.gov/coronavirus/2019-ncov/vaccines/distributing/about-vaccine-data.html
# state level data
# path <- r"(C:\Users\donbo\Downloads\COVID-19_Vaccinations_in_the_United_States_Jurisdiction.csv)"

# county level data
# https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh
# https://data.cdc.gov/api/views/8xkx-amqh/rows.csv?accessType=DOWNLOAD

path <- file.path(dld, "COVID-19_Vaccinations_in_the_United_States_County.csv")
df <- read_csv(path)
dim(df)  # 1.3m, 51 cols
glimpse(df)
# tbl_summary(df)
# ages 5+, 12+, 18+, 65+

df2 <- df %>%
  # setNames(str_to_lower(names(.))) %>%
  rename(date=Date,
         state=Recip_State,
         county=Recip_County) %>%
  mutate(date=as.Date(date, format="%m/%d/%Y")) %>%
  arrange(date, state, county)
summary(df2)


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

