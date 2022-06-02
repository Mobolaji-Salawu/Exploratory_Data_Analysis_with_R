# Install required packages

install.packages("here")
install.packages("tidyverse")
install.packages("funModeling")
install.packages("skimr")
install.packages("janitor")
install.packages("corrplot")
install.packages("rstatix")


# Load required packages

library(here)
library(tidyverse)
library(funModeling)
library(skimr)
library(janitor)
library(corrplot)
library(rstatix)

# Import the required data into R


# Read data for East Asia and Pacific Region

east_asia_and_pacific <- read_csv(here("data", "WorldBank", 
                                       "East_Asia_and_Pacific.csv"), na = "..")

View(east_asia_and_pacific)

spec(east_asia_and_pacific)


# Clean column names, remove empty rows and duplicates if any

east_asia_and_pacific <- east_asia_and_pacific %>% 
                         clean_names() %>% 
                         remove_empty(which = "rows") %>% 
                         distinct()


# Inspect the imported data

head(east_asia_and_pacific)

str(east_asia_and_pacific)

colnames(east_asia_and_pacific)

skim_without_charts(east_asia_and_pacific)


# Create and populate a new column: region

east_asia_and_pacific <- east_asia_and_pacific %>% 
                        mutate(region = "East Asia and Pacific", 
                               .after = country_code)

head(east_asia_and_pacific)


# Read data for Europe and Central Asia Region

europe_and_central_asia <- read_csv(here("data", "WorldBank", 
                                         "Europe_and_Central_Asia.csv"), 
                                    na = "..")

View(europe_and_central_asia)

spec(europe_and_central_asia)


# Clean column names, remove empty rows and duplicates if any

europe_and_central_asia <- europe_and_central_asia %>% 
                           clean_names() %>% 
                           remove_empty(which = "rows") %>% 
                           distinct()


# Inspect the imported data

head(europe_and_central_asia)

str(europe_and_central_asia)

colnames(europe_and_central_asia)

skim_without_charts(europe_and_central_asia)


# Create and populate a new column: region

europe_and_central_asia <- europe_and_central_asia %>% 
                            mutate(region = "Europe and Central Asia", 
                                   .after = country_code)

head(europe_and_central_asia)


# Read data for Latin America and Caribbean

latin_america_and_caribbean <- read_csv(here("data", "WorldBank", 
                                             "Latin_America_and_Caribbean.csv"),
                                        na = "..")

View(latin_america_and_caribbean)

spec(latin_america_and_caribbean)


# Clean column names, remove empty rows and duplicates if any

latin_america_and_caribbean <- latin_america_and_caribbean %>% 
                               clean_names() %>% 
                               remove_empty(which = "rows") %>% 
                               distinct()


# Inspect the imported data

head(latin_america_and_caribbean)

str(latin_america_and_caribbean)

colnames(latin_america_and_caribbean)

skim_without_charts(latin_america_and_caribbean)


# Create and populate a new column: region

latin_america_and_caribbean <- latin_america_and_caribbean %>% 
                               mutate(region = "Latin America and Caribbean", 
                                      .after = country_code)

head(latin_america_and_caribbean)


# Read data for Middle East and North Africa

middle_east_and_north_africa <- read_csv(here("data", "WorldBank", 
                                              "Middle_East_and_North_Africa.csv"),
                                         na = "..")

View(middle_east_and_north_africa)

spec(middle_east_and_north_africa)       


# Clean column names and remove empty rows and duplicates if any

middle_east_and_north_africa <- middle_east_and_north_africa %>% 
                                clean_names() %>% 
                                remove_empty(which = "rows") %>% 
                                distinct()


# Inspect the imported data

head(middle_east_and_north_africa)

str(middle_east_and_north_africa)

colnames(middle_east_and_north_africa)

skim_without_charts(middle_east_and_north_africa)


# Create and populate a new column: region

middle_east_and_north_africa <- middle_east_and_north_africa %>% 
                                mutate(region = "Middle East and North Africa", 
                                       .after = country_code)

head(middle_east_and_north_africa)


# Read data for North America

north_america <- read_csv(here("data", "WorldBank", "North_America.csv"), 
                          na = "..")

View(north_america)

spec(north_america)


# Clean column names and remove empty rows and duplicates if any

north_america <- north_america %>% 
                 clean_names() %>% 
                 remove_empty(which = "rows") %>% 
                 distinct()


# Inspect the imported data

head(north_america)

str(north_america)

colnames(north_america)

skim_without_charts(north_america)


# Create and populate a new column: region

north_america <- north_america %>% 
                 mutate(region = "North America", .after = country_code)

head(north_america)


# Read data for South Asia

south_asia <- read_csv(here("data", "WorldBank", "South_Asia.csv"), 
                       na = "..")

View(south_asia)

spec(south_asia)


# Clean column names and remove empty rows and duplicates if any

south_asia <- south_asia %>% 
              clean_names() %>% 
              remove_empty(which = "rows") %>% 
              distinct()


# Inspect imported data

head(south_asia)

str(south_asia)

colnames(south_asia)

skim_without_charts(south_asia)


# Create and populate a new column: region

south_asia <- south_asia %>% 
              mutate(region = "South Asia", .after = country_code)

head(south_asia)


# Read data for Sub Saharan Africa

sub_saharan_africa <- read_csv(here("data", "WorldBank", "Sub_Saharan_Africa.csv"),
                              na = "..")

View(sub_saharan_africa)

spec(sub_saharan_africa)


# Clean column names and remove empty rows and duplicates if any

sub_saharan_africa <- sub_saharan_africa %>% 
                      clean_names() %>% 
                      remove_empty(which = "rows") %>% 
                      distinct()


# Inspect the imported data

head(sub_saharan_africa)

str(sub_saharan_africa)

colnames(sub_saharan_africa)

skim_without_charts(sub_saharan_africa)


# Create and populate a new column: region

sub_saharan_africa <- sub_saharan_africa %>% 
                      mutate(region = "Sub Saharan Africa", 
                             .after = country_code)

head(sub_saharan_africa)


# Combine all the 7 regions into one

world_region_all <- bind_rows(east_asia_and_pacific, europe_and_central_asia, 
                              latin_america_and_caribbean, 
                              middle_east_and_north_africa, north_america, 
                              south_asia, sub_saharan_africa)

View(world_region_all)


# Inspect the new table world_region_all

head(world_region_all)

str(world_region_all)

skim_without_charts(world_region_all)

tabyl(world_region_all, year)


# Remove the unwanted entries in year column and retain only the years 2010 - 2019

world_region_all <- world_region_all %>% 
                    filter(year %in% c(2010, 2011, 2012, 2013, 2014, 2015, 
                     2016, 2017, 2018, 2019))

skim_without_charts(world_region_all)

tabyl(world_region_all, year)


# Change the region and year columns to factor data type

world_region_all$region <- as_factor(world_region_all$region)

world_region_all$year <- as_factor(world_region_all$year)

levels(world_region_all$region)

levels(world_region_all$year)


# Read table with economic parameters of countries

world_economic_par <- read_csv(here("data", "WorldBank", 
                                    "Economic_Parameters.csv"))

View(world_economic_par)

spec(world_economic_par)


# Clean column names and remove empty rows and duplicates if any

world_economic_par <- world_economic_par %>%
                      clean_names() %>% 
                      remove_empty(which = "rows") %>% 
                      distinct()

head(world_economic_par)
              

# Select the required years in the column named time, 2010 - 2019

world_economic_par <- world_economic_par %>% 
                      filter(time %in% c(2010, 2011, 2012, 2013, 2014, 2015, 
                                         2016, 2017, 2018, 2019))


# Inspect the new table world_economic_par

head(world_economic_par)

str(world_economic_par)

skim_without_charts(world_economic_par)

tabyl(world_economic_par, time)


# Change the time column to factor data type

world_economic_par$time <- as_factor(world_economic_par$time)

levels(world_economic_par$time)


# Join world_region_all and world_economic_par into one table

sapply(c(world_region_all, world_economic_par), length)

world_dev_indicators <- left_join(world_region_all, world_economic_par, 
                        by = c(year = "time", year_code = "time_code", 
                        "country_name", "country_code", 
                        "labor_force_total_sl_tlf_totl_in",
                        "gni_per_capita_atlas_method_current_us_ny_gnp_pcap_cd"))


# Inspect the new table world_dev_indicators

View(world_dev_indicators)

head(world_dev_indicators)

str(world_dev_indicators)

skim_without_charts(world_dev_indicators)

tabyl(world_dev_indicators, year, region)


# Remove columns with more than 20% missing values

x <- as_tibble(skim_without_charts(world_dev_indicators)) %>% 
     filter(complete_rate >= 0.8) %>% 
     select(skim_variable, complete_rate)

y <- pivot_wider(x, names_from = skim_variable, values_from = complete_rate)

world_dev_indicators_2 <- world_dev_indicators %>% 
                          select(colnames(y), -year_code)

skim_without_charts(world_dev_indicators_2)


# Change the column names to shorter ones

world_dev_indicators_2 <- world_dev_indicators_2 %>% 
    rename(gni_per_capita = gni_per_capita_atlas_method_current_us_ny_gnp_pcap_cd, 
          labor_force_total = labor_force_total_sl_tlf_totl_in,
          unemployment_female_percent_of_female_labor_force = 
          unemployment_female_percent_of_female_labor_force_sl_uem_totl_fe_zs,
          unemployment_male_percent_of_male_labor_force = 
          unemployment_male_percent_of_male_labor_force_sl_uem_totl_ma_zs,
          unemployment_total_percent_of_total_labor_force = 
          unemployment_total_percent_of_total_labor_force_sl_uem_totl_zs,
          death_rate_crude_per_1_000 = death_rate_crude_per_1_000_people_sp_dyn_cdrt_in,
          life_expectancy_at_birth = life_expectancy_at_birth_total_years_sp_dyn_le00_in,
          population_growth_annual_percent = population_growth_annual_percent_sp_pop_grow,
          population_total = population_total_sp_pop_totl, 
          male_percent_total_population = population_male_percent_of_total_population_sp_pop_totl_ma_zs,
          female_percent_total_population = population_female_percent_of_total_population_sp_pop_totl_fe_zs,
          incidence_of_tuberculosis_per_100_000 = incidence_of_tuberculosis_per_100_000_people_sh_tbs_incd,
          inflation_consumer_prices_annual_percent = inflation_consumer_prices_annual_percent_fp_cpi_totl_zg,
          inflation_gdp_deflator_annual_percent = inflation_gdp_deflator_annual_percent_ny_gdp_defl_kd_zg,
          consumer_price_index_2010 = consumer_price_index_2010_100_fp_cpi_totl,
          gdp_per_capita_growth_annual_percent = gdp_per_capita_growth_annual_percent_ny_gdp_pcap_kd_zg,
          gdp_per_capita_constant_2010 = gdp_per_capita_constant_2010_us_ny_gdp_pcap_kd,
          gdp_per_capita_current = gdp_per_capita_current_us_ny_gdp_pcap_cd)

colnames(world_dev_indicators_2)


# Summary statistics of new table world_dev_indicators_2

world_dev_indicators_2 %>% 
  get_summary_stats(show = c("mean", "min", "max", "median"))



# Correlation matrix between all numeric columns

world_dev_pearson <- world_dev_indicators_2 %>% 
                     select(c(5:22)) %>% 
                     cor_mat(method = "pearson")

world_dev_spearman <- world_dev_indicators_2 %>% 
                      select(c(5:22)) %>% 
                      cor_mat(method = "spearman")

world_dev_kendall <- world_dev_indicators_2 %>% 
                     select(c(5:22)) %>% 
                     cor_mat(method = "kendall")

world_dev_pearson

world_dev_spearman

world_dev_kendall


# Distribution of values for basic indicators of economic activity

ggplot(world_dev_indicators_2) +
  geom_freqpoly(aes(gni_per_capita, colour = "orange")) +
  geom_freqpoly(aes(gdp_per_capita_constant_2010, colour = "green")) +
  geom_freqpoly(aes(gdp_per_capita_current, colour = "blue")) +
  scale_color_discrete(label = c("gni", "gdp constant", "gdp current")) +
  xlab(NULL)


# Correlation between basic indicators of economic activity

world_econ <- world_dev_indicators_2 %>% 
              select(5, 21, 22) %>% 
              as.matrix() %>% 
              rcorr(type = "spearman")

View(world_econ[["r"]])

View(world_econ[["P"]])

world_econ_P <- world_econ$P

world_econ_P[is.na(world_econ_P)] = 0

corrplot(world_econ$r, method = "number", p.mat = world_econ_P, sig.level = 0.05, 
         insig =  "pch", tl.cex = 0.7, number.cex = 1, cl.ratio = 0.2, 
         cl.cex = 0.7)


# Distribution of values for health indicators

ggplot(world_dev_indicators_2) +
  geom_freqpoly(aes(life_expectancy_at_birth, colour = "green")) +
  geom_freqpoly(aes(death_rate_crude_per_1_000, colour = "red")) +
  geom_freqpoly(aes(incidence_of_tuberculosis_per_100_000, colour = "orange")) +
  scale_color_discrete(label = c("life expectancy", "death rate", 
                                 "tuberculosis incidence")) +
  xlab(NULL)



# Correlation between indicators of economic activity and health indicators

world_econ_health <- world_dev_indicators_2 %>% 
                     select(5, 21, 22, 10, 11, 16) %>%
                     as.matrix() %>% 
                     rcorr(type = "spearman")

View(world_econ_health[["r"]])

View(world_econ_health[["P"]])

world_econ_health_P <- world_econ_health$P

world_econ_health_P[is.na(world_econ_health_P)] = 0

corrplot(world_econ_health$r, method = "number", p.mat = world_econ_health_P, 
         sig.level = 0.05,insig =  "pch", tl.cex = 0.7, number.cex = 0.8, 
         cl.ratio = 0.2, cl.cex = 0.7)


# Distribution of values for population indicators

ggplot(world_dev_indicators_2) +
  geom_freqpoly(aes(population_total, colour = "orange")) +
  geom_freqpoly(aes(male_percent_total_population, colour = "green")) +
  geom_freqpoly(aes(female_percent_total_population, colour = "blue")) +
  scale_color_discrete(label = c("total population", "male percent", 
                                 "female percent")) +
  xlab(NULL)


# Correlation between indicators of economic activity and population indicators

world_econ_pop <- world_dev_indicators_2 %>% 
                   select(5, 21, 22, 12, 13, 14, 15) %>% 
                   as.matrix() %>% 
                   rcorr(type = "spearman")
                   
View(world_econ_pop[["r"]])

View(world_econ_pop[["P"]])

world_econ_pop_P <- world_econ_pop$P

world_econ_pop_P[is.na(world_econ_pop_P)] = 0

corrplot(world_econ_pop$r, method = "number", p.mat = world_econ_pop_P, 
         sig.level = 0.05,insig =  "pch", tl.cex = 0.65, number.cex = 0.75, 
         cl.ratio = 0.2, cl.cex = 0.7)


# Distribution of values for labor force indicators

ggplot(world_dev_indicators_2) +
  geom_freqpoly(aes(labor_force_total, colour = "orange")) +
  geom_freqpoly(aes(unemployment_male_percent_of_male_labor_force, 
                    colour = "green")) +
  geom_freqpoly(aes(unemployment_female_percent_of_female_labor_force, 
                    colour = "blue")) +
  geom_freqpoly(aes(unemployment_total_percent_of_total_labor_force, 
                    colour = "red")) +
  scale_color_discrete(label = c("total labor force", "unemployment male", 
                                 "unemployment female", "unemployment total")) +
  xlab(NULL)


# Correlation between indicators of economic activity and labor force indicators

world_econ_lab <- world_dev_indicators_2 %>% 
                  select(5, 21, 22, c(6:9)) %>% 
                  as.matrix() %>% 
                  rcorr(type = "spearman")

View(world_econ_lab[["r"]])

View(world_econ_lab[["P"]])

world_econ_lab_P <- world_econ_lab$P

world_econ_lab_P[is.na(world_econ_lab_P)] = 0

corrplot(world_econ_lab$r, method = "number", p.mat = world_econ_lab_P, 
         sig.level = 0.05,insig =  "pch", tl.cex = 0.5, number.cex = 0.7, 
         cl.ratio = 0.2, cl.cex = 0.7)


# Distribution of values for inflation indicators

ggplot(world_dev_indicators_2) +
  geom_freqpoly(aes(inflation_consumer_prices_annual_percent, colour = "orange")) +
  geom_freqpoly(aes(inflation_gdp_deflator_annual_percent, colour = "green")) +
  geom_freqpoly(aes(consumer_price_index_2010, colour = "blue")) +
  scale_color_discrete(label = c("inflation consumer prices", "inflation gdp", 
                                 "consumer price index")) +
  xlab(NULL)


# Correlation between indicators of economic activity and inflation indicators

world_econ_inf <- world_dev_indicators_2 %>% 
                  select(5, 21, 22, c(17:19)) %>% 
                  as.matrix() %>% 
                  rcorr(type = "spearman")

View(world_econ_inf[["r"]])

View(world_econ_inf[["P"]])

world_econ_inf_P <- world_econ_inf$P

world_econ_inf_P[is.na(world_econ_inf_P)] = 0

corrplot(world_econ_inf$r, method = "number", p.mat = world_econ_inf_P, 
         sig.level = 0.05,insig =  "pch", tl.cex = 0.65, number.cex = 0.75, 
         cl.ratio = 0.2, cl.cex = 0.7)


# Basic indicators of economic activity by region

world_dev_indicators_2 %>% 
  select(4, 5, 21, 22) %>% 
  group_by(region) %>%
  drop_na() %>% 
  summarise(across(where(is.numeric), list(mean = mean, min = min, max = max)))


econ1 <- world_dev_indicators_2 %>% 
      ggplot(aes(region, gni_per_capita, color = region, fill = region)) +
      stat_summary(fun = "mean", geom = "bar", alpha = 0.5) +
      stat_summary(fun = "min", geom = "point") +
      stat_summary(fun = "max", geom = "point") +
      geom_line() +
      annotate("text", x = 2.1, y = 104370, label = "104,370", size = 2.5, 
               hjust = 0) +
      annotate("text", x = 5.1, y = 117725, label = "117,740", size = 2.5, 
               hjust = 0)+
      annotate("text", x = 7.1, y = 19100, label = "15,930", size = 2.5)+
      annotate("segment", x = 3.5, y = 32000, xend = 4, yend = 970, 
               arrow = arrow(length = unit(1.5, "mm"))) +
      annotate("text", x = 3.5, y = 36000, label = "940", size = 2.5) +
      theme(axis.text.x = element_blank()) +
      ylab("avg gni per capita") +
      scale_color_manual(values=c("yellow", "red", "darkblue", "green", "orange",
                                  "violet", "skyblue")) +
      scale_fill_manual(values=c("yellow", "red", "darkblue", "green", "orange",
                                 "violet", "skyblue"))


econ2 <- world_dev_indicators_2 %>%
      ggplot(aes(region, gdp_per_capita_constant_2010, color = region, 
                 fill = region)) +
      stat_summary(fun = "mean", geom = "bar", alpha = 0.5) +
      stat_summary(fun = "min", geom = "point") +
      stat_summary(fun = "max", geom = "point") +
      geom_line() +
      annotate("text", x = 2.1, y = 209225, label = "209,225", size = 2.5, 
               hjust = 0) +
      annotate("text", x = 5.1, y = 101875, label = "101,875", size = 2.5, 
               hjust = 0) +
      annotate("text", x = 7.1, y = 25000, label = "18,254", size = 2.5)+
      annotate("segment", x = 3.5, y = 32000, xend = 4, yend = 1500, 
               arrow = arrow(length = unit(1.5, "mm"))) +
      annotate("text", x = 3.5, y = 39000, label = "633", size = 2.5) +
      theme(axis.text.x = element_blank()) +
      ylab("avg gdp per capita constant 2010") +
      scale_color_manual(values=c("yellow", "red", "darkblue", "green", "orange",
                                  "violet", "skyblue")) +
      scale_fill_manual(values=c("yellow", "red", "darkblue", "green", "orange",
                                 "violet", "skyblue"))
  
  
econ3 <- world_dev_indicators_2 %>% 
      ggplot(aes(region, gdp_per_capita_current, color = region, fill = region)) +
      stat_summary(fun = "mean", geom = "col", alpha = 0.5) +
      stat_summary(fun = "min", geom = "point") +
      stat_summary(fun = "max", geom = "point") +
      geom_line() +
      annotate("text", x = 2.1, y = 190515, label = "190,513", size = 2.5, 
               hjust = 0) +
      annotate("text", x = 5.1, y = 117100, label = "117,099", size = 2.5, 
               hjust = 0) +
      annotate("text", x = 7.1, y = 27000, label = "21,711", size = 2.5)+
      annotate("segment", x = 3.5, y = 32000, xend = 4, yend = 1200, 
               arrow = arrow(length = unit(1.5, "mm"))) +
      annotate("text", x = 3.5, y = 39000, label = "824", size = 2.5) +
      theme(axis.text.x = element_blank()) +
      ylab("gdp per capita current") +
      scale_color_manual(values=c("yellow", "red", "darkblue", "green", "orange",
                              "violet", "skyblue")) +
      scale_fill_manual(values=c("yellow", "red", "darkblue", "green", "orange",
                              "violet", "skyblue"))


econ1
econ2
econ3


# Health indicators by region

world_dev_indicators_2 %>% 
  select(4, 10, 11, 16) %>% 
  group_by(region) %>%
  drop_na() %>% 
  summarise(across(where(is.numeric), list(mean = mean, min = min, max = max)))


health1 <- world_dev_indicators_2 %>% 
        select(3, 4, 11) %>% 
        group_by(region, year) %>%
        drop_na() %>% 
        summarise(avg_life_expectancy = mean(life_expectancy_at_birth)) %>% 
        ggplot(aes( year, avg_life_expectancy, group = region, color = region)) +
        geom_line() +
        theme(axis.text.x = element_text(angle = 30)) +
        scale_color_manual(values=c("yellow", "red", "darkblue", "green", 
                                    "orange", "violet", "skyblue"))


health2 <- world_dev_indicators_2 %>% 
        group_by(year, region) %>% 
        ggplot(aes(year, death_rate_crude_per_1_000, group = region, 
                   color = region)) +
        stat_summary(fun = "mean", geom = "line", size = 0.7) +
        theme(axis.text.x = element_text(angle = 30)) +
        scale_color_manual(values=c("yellow", "red", "darkblue", "green", 
                                    "orange", "violet", "skyblue"))


health3 <- world_dev_indicators_2 %>% 
        group_by(year, region) %>% 
        ggplot(aes(year, incidence_of_tuberculosis_per_100_000, group = region, 
                   color = region)) +
        stat_summary(fun = "mean", geom = "line", size = 0.7) +
        theme(axis.text.x = element_text(angle = 30)) +
        scale_color_manual(values=c("yellow", "red", "darkblue", "green", 
                                    "orange", "violet", "skyblue"))

health1
health2
health3


# Export world_dev_indicators_2 for further visualization

write_csv(world_dev_indicators_2, 
          "C:/Users/User/Documents/data/WorldBank/world_dev_indicators.csv")
