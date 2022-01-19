# Installing the coronavirus R package from CRAN -> install.packages("coronavirus")
# Update coronavirus package to get the most recent available data - An R session must be restarted to have the updates available

#library(coronavirus) 
#detach("package:coronavirus", unload = TRUE)
#coronavirus::update_dataset()

# Include packages
library(coronavirus) # coronavirus package
library(ggplot2) # data vizualizations/plots
library(treemapify) # use with ggplot2 for treemaps
library(ggpubr) # combine ggplot2 plots in multiple rows/cols
library(data.table) # working with tabular data
library(scales) # used for custom scaling on ggplot axes

#set env language to English (eg date in plots to be shown in English)
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")

# Read the vaccinations dataset
DT = fread("covid19_vaccine.csv", data.table = TRUE) #load vaccination data in data table
is.data.table(DT)

# Examine the vaccination dataset
head(DT) #inspect the dataframe's first few lines
#check the length of the dataframe (number of columns) = 18
initial_columns_number = length(DT)
#check the length of one of the columns => (number of rows) = 143629
dataset_rows_number = length(DT$date) 

end_date = max(DT$date) # last day for which data are available = 08-01-2022
start_date = min(DT$date) # first day for which data is available = 14-12-2020
# number of days for which we have data = 391 days
days_covered_in_dataset = end_date - start_date + 1 
# unique country regions = 191 (actual number  190 as "World" is included)
unique_country_regions = DT[, length(unique(country_region))] 

# Update data table with fully_vaccinated_ratio column, converting it to percentage with 1 decimal digit
DT = DT[, fully_vaccinated_ratio := round(100 * (people_fully_vaccinated / population), digits = 1)]

# And then with partially_vaccinated_ratio column, converting it to percentage with 1 decimal digit
DT = DT[, partially_vaccinated_ratio := round(100 * (people_partially_vaccinated / population), digits = 1)]

# Keep only country & world data - keep rows with NA value under province\_state
# Those are the aggregate values for each country
DT = DT[is.na(province_state)] # Filter out province_state values that are not NA (ie referring to provinces)
DT = DT[, province_state:=NULL] # Remove province_state column from datatable 

# Fix values of partially vaccinated ratio column that are over 100%
#returns rows indices of DT that fully vaccinated ratio exceeds 100%
which(DT[,fully_vaccinated_ratio]>100)
#returns rows indices of DT that partially vaccinated ratio exceeds 100
which(DT[,partially_vaccinated_ratio]>100)  
set(DT, i=which(DT[, partially_vaccinated_ratio]>100), j=19L, value=100) #set those values to 100
DT[order(-partially_vaccinated_ratio)] #show DT ordered by partially vaccinated ratio descending 
#check max value is now 100%
DT[partially_vaccinated_ratio > 0, max(partially_vaccinated_ratio)]

# Find missing continent values
DT[is.na(continent_name) & country_region != 'World', unique(country_region)]

# Fix missing continent values
which(DT[, country_region]=='Kosovo')
set(DT, i=which(DT[, country_region]=='Kosovo'), j=16L, value='Europe') #set continent name for Kosovo to Europe
set(DT, i=which(DT[, country_region]=='Kosovo'), j=17L, value='EU') #set continent code for Kosovo to EU
which(DT[, country_region]=='Sudan')
set(DT, i=which(DT[, country_region]=='Sudan'), j=16L, value='Africa') #set continent name for Sudan to Africa
set(DT, i=which(DT[, country_region]=='Sudan'), j=17L, value='AF') #set continent code for Sudan to AF


#---- SECTION 1 ----#

# People partially vaccinated progress - World
world_partial_data = DT[country_region == 'World', 
                        .(date, people_partially_vaccinated)]

partially_vacc_worldwide <-  
  ggplot(world_partial_data, aes(x=date)) +
  geom_area(aes(y=people_partially_vaccinated), alpha=0.5) +
  labs(title="World - Number of people partially vaccinated",
       subtitle="Vaccination progress on a global scale",
       x="Date", 
       y="People partially vaccinated") +
  theme(axis.text.x = element_text(angle=45)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = unit_format(unit = "Billions", scale = 1e-9))

partially_vacc_worldwide

#---- World Vaccinated Progress ----#
world_data = DT[country_region == 'World'] #World data by date
#keep 1 row for each country -> contains eg its population
countries_info = unique(DT, by=c('country_region')) 
# world_population=7.331.618.789, slightly above real value (~7.9 billion)
world_population = countries_info[country_region != 'World', 
                                  sum(population, na.rm=TRUE)] 


x <- ggplot(world_data, aes(x=date)) +
  geom_line(aes(y=round(100*people_partially_vaccinated/world_population, digits=1), 
                colour = 'Partially vaccinated'), size = 1) +
  geom_line(aes(y=round(100*people_fully_vaccinated/world_population, digits=1), 
                colour = 'Fully vaccinated'), size = 1) +
  scale_color_manual(name = "Legend", 
                     values = c("Partially vaccinated" = "darkblue", 
                                "Fully vaccinated" = "red")) +
  labs(title = 'World - Vaccination ratios',
       subtitle='Vaccination progress on a global scale',
       y = 'Vaccination ratios',
       x = 'Date') +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_y_continuous(labels = unit_format(unit = "%"))

x #display plot in RStudio

#---- World's Top Performers ----#

# TREEMAP WITH POPULATION HERE OF ALL COUNTRIES

#data of the last day recorded (exclude World aggregate data)
last_day_data = DT[date == "2022-01-08" & country_region != 'World'] 

treemap1 = ggplot(last_day_data, aes(area = population, 
                                     fill = partially_vaccinated_ratio, label = country_region)) +
  labs(title='Treemap of partially vaccinated ratio per country population',
       subtitle='Size of tile: population, Date = 08-01-2022', fill='Ratio %') +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre")

treemap2 = ggplot(last_day_data, aes(area = population,
                                     fill = fully_vaccinated_ratio, label = country_region)) +
  labs(title='Treemap of fully vaccinated ratio per country population',
       subtitle='Size of tile: population, Date = 08-01-2022', fill='Ratio %') +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre") 

# combine both treemaps in a single figure
figure <- ggarrange(treemap1, treemap2, ncol = 1, nrow = 2) #combine two ggplots
figure

# TOP 20 COUNTRIES IN POPULATION

last_day_data_top_20_population = last_day_data[order(-population)]
last_day_data_top_20_population = last_day_data_top_20_population[1:20]

barplot3<-ggplot(data=last_day_data_top_20_population,
                 aes(x=reorder(country_region, population), 
                     y=partially_vaccinated_ratio, fill=continent_name)) +
  geom_bar(stat='identity') +
  labs(title="Bar plot of 20 most populated countries' partially vaccinated ratios",
       subtitle='Date: 08-01-2022', fill='Continent',
       x='Country', y='partially vaccinated ratio %') +
  coord_flip() # Horizontal bar plot

barplot3


# TREEMAP WITH TOP 20 COUNTRIES IN PARTIALLY VACCINATION RATIOS

last_day_data_top_20_part_vacc_ratios = last_day_data[order(-partially_vaccinated_ratio)]
last_day_data_top_20_part_vacc_ratios = last_day_data_top_20_part_vacc_ratios[1:20]

#create a bar plot to compare their percentages (should look better than the treemap -> confusing a bit)
barplot4<-ggplot(data=last_day_data_top_20_part_vacc_ratios, 
                 aes(x=reorder(country_region, partially_vaccinated_ratio), 
                     y=partially_vaccinated_ratio, fill=continent_name)) +
  labs(title="Bar plot of 20 top countries' partially vaccinated ratios",
       subtitle='Date: 08-01-2022', fill='Continent',
       x='Country', y='partially vaccinated ratio %') +
  geom_bar(stat='identity') +
  coord_flip() # Horizontal bar plot

barplot4


# TREEMAP PER CONTINENT

#remove rows with NA partially_vaccinated_ratio
last_day_data_per_continent = na.omit(last_day_data, 
                                      cols="partially_vaccinated_ratio") 
last_day_data_per_continent = last_day_data_per_continent[, 
                                                          .(population_total = as.double(sum(population)), 
                                                            partially_vaccinated_ratio = round(100*sum(people_partially_vaccinated)/
                                                                                                 sum(population)),digits=1),
                                                          by='continent_name']

treemap5 = ggplot(last_day_data_per_continent, aes(area = population_total, 
                                                   fill = partially_vaccinated_ratio, label = continent_name)) +
  labs(title="Treemap of 6 continents' partially vaccinated ratios",
       subtitle='Tile size: population, Date: 08-01-2022', fill='Ratio %') +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre")

barplot5<-ggplot(data=last_day_data_per_continent, 
                 aes(x=reorder(continent_name, partially_vaccinated_ratio), 
                     y=partially_vaccinated_ratio, fill=partially_vaccinated_ratio)) +
  labs(title="Bar plot of 6 continents' partially vaccinated ratios",
       subtitle='Date: 08-01-2022', fill='Ratio %',
       x='Country', y='partially vaccinated ratio %') +
  geom_bar(stat='identity') +
  coord_flip() # Horizontal bar plot


figure <- ggarrange(treemap5, barplot5, ncol = 1, nrow = 2) #combine two ggplots
figure

# explore African countries numbers on the last day of the dataset 08-01-2022
africa_last_day_data = DT[continent_name == 'Africa' & date == '2022-01-08']
# there are some countries with NA values


barplot6<-ggplot(data=africa_last_day_data, 
                 aes(x=reorder(country_region, partially_vaccinated_ratio), 
                     y=partially_vaccinated_ratio, 
                     fill=population)) +
  labs(title="Bar plot of African countries' partially vaccinated ratios",
       subtitle='Date: 08-01-2022', fill='Population',
       x='Country', y='partially vaccinated ratio %') +
  scale_fill_continuous(labels = unit_format(unit = "Millions", scale = 1e-6))+
  geom_bar(stat='identity') +
  coord_flip() # Horizontal bar plot

barplot6 #display the barplot

# TREEMAP FOR EUROPEAN COUNTRIES

last_day_data_Europe = last_day_data[continent_name == 'Europe']

barplot7<-ggplot(data=last_day_data_Europe,
                 aes(x=reorder(country_region, partially_vaccinated_ratio), 
                 y=partially_vaccinated_ratio, fill=population)) +
  labs(title="Bar plot of European countries' partially vaccinated ratios",
       subtitle='Date: 08-01-2022', fill='Population',
       x='Country', y='partially vaccinated ratio %') +
  scale_fill_continuous(labels = unit_format(unit = "Millions", scale = 1e-6))+
  geom_bar(stat='identity') +
  coord_flip() # Horizontal bar plot

barplot7


# Greece versus rest of Europe / rest of Balkans

last_day_data_Greece = last_day_data[country_region == 'Greece']
partially_vacc_ratio_Greece = last_day_data_Greece[, partially_vaccinated_ratio]

last_day_data_Europe_excl_Greece = last_day_data_Europe[country_region != 'Greece']
mean_partially_vacc_ratio_Europe_excl_Greece = last_day_data_Europe_excl_Greece[, 
        round(100 * sum(people_partially_vaccinated)/sum(population), digits = 1)]

balkans = list('Albania',
               'Bosnia and Herzegovina',
               'Bulgaria',
               'Croatia',
               'Kosovo',
               'Montenegro',
               'North Macedonia',
               'Romania',
               'Serbia',
               'Slovenia')

last_day_data_Balkans_excl_Greece = last_day_data_Europe[
  country_region == balkans[1] | country_region == balkans[2] |
  country_region == balkans[3] | country_region == balkans[4] |
  country_region == balkans[5] | country_region == balkans[6] |
  country_region == balkans[7] | country_region == balkans[8] |
  country_region == balkans[9] | country_region == balkans[10]]

mean_partially_vacc_ratio_Balkans_excl_Greece = last_day_data_Balkans_excl_Greece[, 
          round(100 * sum(people_partially_vaccinated)/sum(population), digits = 1)]

country_or_countries = list('Greece', 'Rest of Balkans', 'Rest of Europe')
partially_vacc_ratios = list(partially_vacc_ratio_Greece, mean_partially_vacc_ratio_Balkans_excl_Greece,
                             mean_partially_vacc_ratio_Europe_excl_Greece)
df_Greece = data.frame(location = unlist(country_or_countries), 
                       partially_vaccinated_ratio = unlist(partially_vacc_ratios))
df_Greece



# Time Series Greece vs Europe / Balkans
Greece_time_series = DT[country_region == 'Greece']
Europe_time_series = DT[continent_name == 'Europe' & country_region != 'Greece', .(
  doses_admin = sum(doses_admin),
  people_partially_vaccinated = sum(people_partially_vaccinated),
  people_fully_vaccinated = sum(people_fully_vaccinated),
  population = sum(population),
  partially_vaccinated_ratio = round(100 * sum(people_partially_vaccinated)/sum(population), digits=1),
  fully_vaccinated_ratio = round(100 * sum(people_fully_vaccinated)/sum(population), digits=1) ) , by='date']
Balkan_time_series = DT[country_region != 'Greece' & (  country_region == balkans[1] | country_region == balkans[2] |
                                                          country_region == balkans[3] | country_region == balkans[4] |
                                                          country_region == balkans[5] | country_region == balkans[6] |
                                                          country_region == balkans[7] | country_region == balkans[8] |
                                                          country_region == balkans[9] | country_region == balkans[10]), .(
                                                            doses_admin = sum(doses_admin),
                                                            people_partially_vaccinated = sum(people_partially_vaccinated),
                                                            people_fully_vaccinated = sum(people_fully_vaccinated),
                                                            population = sum(population),
                                                            partially_vaccinated_ratio = round(100 * sum(people_partially_vaccinated)/sum(population), digits=1),
                                                            fully_vaccinated_ratio = round(100 * sum(people_fully_vaccinated)/sum(population), digits=1) ) , by='date']

#time series plotting

ggplot() + 
  geom_line(aes(x = Balkan_time_series[,date], y=Balkan_time_series[, partially_vaccinated_ratio], col="Rest of Balkans"), size=1) +
  geom_line(aes(x = Greece_time_series[,date],y=Greece_time_series[, partially_vaccinated_ratio], col="Greece"), size=1) +
  geom_line(aes(x = Europe_time_series[,date], y=Europe_time_series[, partially_vaccinated_ratio], col="Rest of Europe"), size=1) +
  labs(title="Time Series of Greece's partially vaccinated ratio", 
       subtitle="vs Rest of Europe and Rest of Balkans",
       x='Date',
       y='partially vaccinated ratio') +  # title and subtitle
  scale_color_manual(name="", 
                     values = c("Greece"="#00ba38", "Rest of Balkans"="#f8766d", "Rest of Europe"="#999333")) +  # line color
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_y_continuous(labels = unit_format(unit = "%"))


