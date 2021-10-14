# Effacement de la mémoire
rm(list=ls())
gc()

library(tidyverse)
library(gt)
library(maps)
library(sf)
library(showtext)


# Load fonts (for ggplot only)
font_add_google("Oswald","oswald")
font_add_google("Barlow Condensed","barlow")
# Automatically use {showtext} for plots
showtext_auto()

# Loading data-----------
# As both original files used to create this table are quite large, 
# I made a subset of those files with only data required for this table

# Animal production
# Full dataset available here: 
# https://www.fao.org/faostat/en/#data/QCL
# (Bulk download > All Data Normalized)
#data<-read_csv("Production_Crops_Livestock_E_All_Data_(Normalized).csv")

# Subset with only necessary data for this table:
data<-read_csv('https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/data/FAO_AnimalProductivity_subset.csv')


# Animal trades 
# Full dataset available here: 
# https://www.fao.org/faostat/en/#data/TCL
# (Bulk download > All Data Normalized)
#trade<-read_csv("Trade_Crops_Livestock_E_All_Data_(Normalized).csv")

# Subset with only necessary data for this table:
trade <-read_csv('https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/data/FAO_AnimalTrade_subset.csv')

# Data preparation---------------------

# Clean up some country names (for merging with world map)
data<-data%>%
  filter(Area!='China, mainland')%>%
  mutate(Area=case_when(
    Area=='Bolivia (Plurinational State of)'~'Bolivia',
    Area=='China, Taiwan Province of'~'Taiwan',
    Area=='China, Taiwan Province of'~'China',
    Area=='Congo'~'Republic of Congo',
    str_detect(Area,'Ivoire')~'Ivory Coast',
    Area=='Czechia'~'Czech Republic',
    Area=="Democratic People's Republic of Korea"~'North Korea',
    Area=='Iran (Islamic Republic of)'~'Iran',
    Area=='Republic of Korea'~'South Korea',
    Area=='Russian Federation'~'Russia',
    Area=='United Kingdom of Great Britain and Northern Ireland'~'UK',
    Area=='United Republic of Tanzania'~'Tanzania',
    Area=='United States of America'~'USA',
    Area=='Venezuela (Bolivarian Republic of)'~'Venezuela',
    Area=='Viet Nam'~'Vietnam',
    TRUE~Area
  ))%>%
  mutate(Element=case_when(
    Element=='Laying'~'Stocks',
    TRUE~Element
  ))

trade<-trade%>%
  filter(Year==2018)%>%
  filter(Area!='China, mainland')%>%
  mutate(Area=case_when(
    Area=='Bolivia (Plurinational State of)'~'Bolivia',
    Area=='China, Taiwan Province of'~'Taiwan',
    Area=='China, Taiwan Province of'~'China',
    Area=='Congo'~'Republic of Congo',
    str_detect(Area,'Ivoire')~'Ivory Coast',
    Area=='Czechia'~'Czech Republic',
    Area=="Democratic People's Republic of Korea"~'North Korea',
    Area=='Iran (Islamic Republic of)'~'Iran',
    Area=='Republic of Korea'~'South Korea',
    Area=='Russian Federation'~'Russia',
    Area=='United Kingdom of Great Britain and Northern Ireland'~'UK',
    Area=='United Republic of Tanzania'~'Tanzania',
    Area=='United States of America'~'USA',
    Area=='Venezuela (Bolivarian Republic of)'~'Venezuela',
    Area=='Viet Nam'~'Vietnam',
    TRUE~Area
  ))%>%
  filter(Element%in%c('Import Quantity','Export Quantity'))

# Get number of animals in 2019
dat19<-data%>%
  filter(Area=='World')%>%
  filter(str_detect(Unit,"Head"))%>%
  filter(Year==2019)%>%
  arrange(-Value)

# Description for the 10 most breed species in the world
animals<-c(
  'Sheep','Goats','Pigs','Cattle','Chickens',
  'Eggs Primary','Ducks','Turkeys',
  'Geese and guinea fowls','Rabbits and hares'
  )

# To convert number of heads to same units latter
heads<-c(1,1,1,1,1000,1000,1000,1000,1000,1000)

count_heads<-tibble(
  Item = animals,
  Factor = heads
)

# Only keep trade for most breeded animals
trade<-trade%>%
  mutate(Item_clean=case_when(
    Item=='Eggs, hen, in shell'~'Eggs Primary',
    # Only offals trade available for geese
    Item=='Offals, liver geese'~'Geese and guinea fowls',
    TRUE~Item
  ))%>%
  filter(Item_clean%in%animals)

# Description of each animal
categories <- cbind.data.frame(
  Item = animals,
  Description = c(
    # Sheep
    'Sheep can be raised for meat and milk, but also for wool production. Productivity per sheep has increased slightly over the past 30 years. 13% of sheep are in China.',
    
    # Goats
    'Goats are raised for milk and meat production, but also for wool and leather. Goat stocks are increasing faster than those of other ruminants. 14% are in India.',
    
    # Pigs
    'Pig meat is the most consumed meat in the world but stocks recently started to decline. There was an increase in productivity per animal in past 30 years. 44% of the pigs raised in the world are in China.',
    
    # Cattle
    "Cattle produce meat and milk. Cattle stocks are more stabble than for poultry. The world's leading breeder of cattle is Brazil (14% of the cattle).",
    
    # Chickens
    'Broiler chickens are bred and raised specifically for meat production. There are 26 billion chickens on earth, that is more than 3 chickens per human being. China raises 20% of all chickens.',
    
    # Laying hens
    'Laying hens are bred for egg production and raised over a longer period of time than chickens. Productivity per hen has not changed over the last 30 years. 38% of laying hens are in China.',

    # Ducks
    'Ducks are bred for meat production. The huge majority of farmed ducks are found in Asia (62% in China). Productivity per duck has risen sharply over the last decades.',
    
    # Turkeys
    'Turkeys are bred for meat production, mainly in the USA (56%), where this poultry is the favorite meal for celebrations like Christmas and Thanksgiving.',
    
    # Geese and guinea fowls
    "Geese and guinea fowl are not differentiated in the FAO statistics. Almost all of these poultries are in China (86%), where they are mainly bred for meat production.",
    
    # Rabits
    "Rabbits are raised for meat and fur. More than three quarters of the world's farmed rabbits are in China"
))

# Evolution of productions since 1990 (for plot)

milk <- c('Milk, whole fresh cow', 'Milk, whole fresh goat', 'Milk, whole fresh sheep')
eggs <- c('Eggs, hen, in shell')
meat <- c('Meat, cattle',	'Meat, chicken', 'Meat, duck', 'Meat, goat','Meat, goose and guinea fowl',
          'Meat, pig', 'Meat, rabbit','Meat, sheep','Meat, turkey')

animal_trends<-data%>%
  filter(Year>1989)%>%
  filter(Item%in%c(animals,meat,milk,eggs))%>%
  filter(!(Item %in% eggs & Element=='Stocks'))%>%
  mutate(Element=case_when(
    (Item %in% milk)&(Element=='Production') ~ 'Milk',
    (Item %in% eggs)&(Element=='Production') ~ 'Eggs',
    (Item %in% meat)&(Element=='Production') ~ 'Meat',
    TRUE~Element
  ))%>%
  filter(Element%in%c('Stocks','Milk','Eggs','Meat'))%>%
  # Rename Production with same name as Stocks
  mutate(Item=case_when(
    Item=='Milk, whole fresh cow'~'Cattle',
    Item=='Meat, cattle'~'Cattle',
    Item=='Milk, whole fresh goat'~'Goats',
    Item=='Meat, goat'~'Goats',
    Item=='Milk, whole fresh sheep'~'Sheep',
    Item=='Meat, sheep'~'Sheep',
    Item=='Eggs, hen, in shell'~'Eggs Primary',
    Item=='Meat, chicken'~'Chickens',
    Item=='Meat, duck'~'Ducks',
    Item=='Meat, goose and guinea fowl'~'Geese and guinea fowls',
    Item=='Meat, pig'~'Pigs',
    Item=='Meat, rabbit'~'Rabbits and hares',
    Item=='Meat, turkey'~'Turkeys',
    TRUE~Item
  ))%>%
  select(Area,Item,Element,Value,Year)%>%
  filter(Area=='World')%>%
  group_by(Item,Element)%>%
  mutate(Base=Value[Year==1990])%>%
  mutate(Ratio=Value/Base)%>%
  ungroup()

# Stocks of each animal in 2018 (for maps)
animal18<-data%>%
  filter(Year=='2018')%>%
  filter(Item%in%animals)%>%
  filter(Element=='Stocks')%>%
  select(Area,'Area Code',Item,Production=Value)%>%
  group_by(Item)%>%
  mutate(world_prod=Production[Area=='World'])%>%
  mutate(ratio_prod=Production/world_prod)%>%
  ungroup()%>%
  filter(get('Area Code')<1000)%>%
  select(-'Area Code')

# If you wanna take a look at main producers ranking...
#animal18%>%
#  filter(Item=='Rabbits and hares')%>%
#  arrange(-ratio_prod)

# Save highest ratio for map scales
# (China has 86% of goose stocks)
max_percent<-max(na.omit(animal18$ratio_prod))

# Top imports/exports per crop
# Top importer
import<-trade%>%
  filter(Element=='Import Quantity')%>%
  filter(get('Area Code')<1000)%>%
  filter(!(str_detect(Area,pattern='European')))%>%
  group_by(Item_clean)%>%
  # Keeping only top importer for the sake of clarity
  # but you may keep more by changing 'n' value
  slice_max(order_by = Value,n=1)%>%
  ungroup()

# Top exporter
export<-trade%>%
  filter(Element=='Export Quantity')%>%
  filter(get('Area Code')<1000)%>%
  filter(!(str_detect(Area,pattern='European')))%>%
  group_by(Item_clean)%>%
  # (Same as importer: keeping only top exporter)
  slice_max(order_by = Value,n=1)%>%
  ungroup()

# Preparing world map
states <- st_as_sf(maps::map(database="world", plot = FALSE, fill = TRUE))
country_to_remove <- c(
  'Antarctica','Greenland', 'French Southern and Antarctic Lands'
)
# Short function to create %!in% operator 
'%!in%' <- function(x,y)!('%in%'(x,y))

states <- states %>%
  mutate(Area=ID)%>%
  select(-ID)%>%
  filter(Area %!in% country_to_remove)

# Compute centroids from world map
# (to add country names to map)
centroids<-states%>%
  # Changing WGS84 to projeted CRS
  st_transform(4088)%>%
  st_centroid()%>%
  # Converting back to WGS84
  st_transform(4326)

# Some centroids geometry are off, we will manually correct that
st_geometry(centroids[centroids$Area=='Russia',]) <-  st_sfc(st_point(c(87.14749958861049, 63.685184215286306)))
st_geometry(centroids[centroids$Area=='USA',]) <-  st_sfc(st_point(c(-103.0814210978926, 40.898544852227886)))
st_geometry(centroids[centroids$Area=='Canada',]) <-  st_sfc(st_point(c(-111.292969,59.334666)))

# Moving other centroids for better labelling
st_geometry(centroids[centroids$Area=='Myanmar',]) <-  st_sfc(st_point(c(116.21602171139949,7.2705372388883625)))
st_geometry(centroids[centroids$Area=='Netherlands',]) <-  st_sfc(st_point(c(4.4763158153096745,56.447710450448525)))
st_geometry(centroids[centroids$Area=='Germany',]) <-  st_sfc(st_point(c(11.060888925884282,43.03451850449381)))
st_geometry(centroids[centroids$Area=='Poland',]) <-  st_sfc(st_point(c(21.527097070417252,57.84221001028442)))
st_geometry(centroids[centroids$Area=='Sudan',]) <-  st_sfc(st_point(c(23.786655,9.925251)))
st_geometry(centroids[centroids$Area=='Portugal',]) <-  st_sfc(st_point(c(-9.069803422285617,36.98549085058945)))

# Add centroids to data
import_cent<-import%>%
  select(Area,Item=Item_clean,Value)%>%
  left_join(centroids)%>%
  rename_with(.cols=c(Value,geom), ~ paste0(.x, "_imp"))

export_cent<-export%>%
  select(Area,Item=Item_clean,Value)%>%
  left_join(centroids)%>%
  rename_with(.cols=c(Value,geom), ~ paste0(.x, "_exp"))

# Functions for plotting------------

# Color palette (for world maps)
pal<-c(
  "#FFDF9F",
  "#BE524C",
  "#B74243",
  "#B0323A","#A82230","#A11327","#9A031E"
)
# As country with 0 production are not reported in the table,
# take the 'lowest color' of the palette for NA's 
na_pal<-pal[1]

# Palette for animal productions
prod_pal <- c(
  "Eggs" = "#E36414",
  "Meat" = "#9A031E", 
  "Milk" = "#60A561", 
  "Stocks" = "#0F4C5C" 
)

# Function to plot trends
trend <- function(data){
  
  tr <- ggplot(data,aes(x=Year,y=Ratio,color=Element))+
    # Create axis
    # x-axis
    annotate(geom='segment',x=2000,xend=2000,y=0.75,yend=0.85,color='dimgrey',lty='solid',size=2)+
    annotate(geom='segment',x=2010,xend=2010,y=0.75,yend=0.85,color='dimgrey',lty='solid',size=2)+
    annotate(geom='text',x=2000,y=0.6,label='2000',hjust=0.5,color='dimgrey',size=13,family='oswald')+
    annotate(geom='text',x=2010,y=0.6,label='2010',hjust=0.5,color='dimgrey',size=13,family='oswald')+
    # y-axis
    annotate(geom='segment',x=1990,xend=2019,y=1,yend=1,color='dimgrey',size=2)+
    annotate(geom='segment',x=1990,xend=2019,y=2.5,yend=2.5,color='dimgrey',lty='dotted',size=2)+
    annotate(geom='segment',x=1990,xend=2019,y=4,yend=4,color='dimgrey',lty='dotted',size=2)+
    annotate(geom='text',x=1990,y=2.70,label='+150 %',hjust=0,color='dimgrey',size=13,family='oswald')+
    annotate(geom='text',x=1990,y=4.30,label='+300 %',hjust=0,color='dimgrey',size=13,family='oswald')+
   
    # Add points and line
    geom_point(size=6)+
    geom_line(size=2.5)+
    
    # Set scales and theme
    scale_color_manual(values=prod_pal)+
    scale_y_continuous(limits=c(0.5,4.6))+
    guides(color='none')+
    theme_void()
  
  return(tr)

}

# Function for world map
mapping <- function(data){
  pl <- ggplot()+
    geom_sf(states,mapping=aes(),fill=na_pal,color=NA)+
    geom_sf(data,
      mapping=aes(geometry=geom,fill=ratio_prod), 
      color=NA,show.legend=FALSE)+
    geom_sf_text(
      data,mapping=aes(geometry=geom_exp,label = Area),
      color='#004643',size=15,family='barlow',fontface='bold')+
    geom_sf_text(
      data,mapping=aes(geometry=geom_imp,label = Area),
      color='#EF2D56',size=15,family='barlow',fontface='bold')+
    scale_fill_gradientn(colors=pal,na.value = na_pal,limits=c(0,max_percent))+
    
    theme_void()+
    theme(plot.margin=margin(0,0,0,0,"cm"))
  
  return(pl)

}



# Function to merge crop name + description
# (offers more customization than gtExtras::gt_merge_stack())
# Adapted from Tom Mock: 
# https://themockup.blog/posts/2020-10-31-embedding-custom-features-in-gt-tables/
combine_word <- function(number, Item, Heads, description){
  glue::glue(
    "<div style='line-height:16px; text-align:left'><span style='font-weight:bold; font-size:16px'>{number}. </span><span style='font-weight:bold;font-size:14px'>{Item}</span><span style='font-size:13px'> ({Heads} millions)</span></div>
     <div style='line-height:12px; text-align:left'><span style ='color:#2f2f2f;font-size:12px'>{description}</span></div>"
  )
}

# Merging tables ------

# 'Map' plotting functions and add columns with ggplot objects
crops_chart_column <-
  animal_trends %>%
  drop_na(Item)%>%
  group_by(Item) %>%
  nest()%>%
  mutate(
    chart = purrr::map(data,trend) 
  )%>%
  select(Item,chart)

crops_plot_column <-
  states %>%
  left_join(animal18)%>%
  complete(Area, Item, fill = list(Value = 0))%>%
  full_join(export_cent, by=c('Item','Area'))%>%
  full_join(import_cent, by=c('Item','Area'))%>%
  drop_na(Item)%>%
  group_by(Item) %>%
  nest()%>%
  mutate(
    plot = purrr::map(data,mapping) 
  )%>%
  select(Item,plot)

# Add these columns to main 'area' table
area<-dat19%>%
  filter(Item %in% animals)%>%
  left_join(count_heads)%>%
  left_join(categories)%>%
  mutate(
    total_heads=Value*Factor
  )%>%
  arrange(-total_heads)%>%
  mutate(
    # Convert to million heads
    Heads = round(total_heads/1000000)
  )%>%
  mutate(Pic=case_when(
    Item=='Cattle'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_animals/cow.png' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>",
    Item=='Chickens'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_animals/chicken.png' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>",
    Item=='Eggs Primary'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_animals/layin.png' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>",
    Item=='Sheep'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_animals/sheep.png' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>",
    Item=='Ducks'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_animals/duck.png' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>",
    Item=='Goats'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_animals/goat.png' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>",
    Item=='Pigs'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_animals/pig.png' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>",
    Item=='Turkeys'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_animals/turkey.png' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>",
    Item=='Geese and guinea fowls'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_animals/geese.png' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>",
    Item=='Rabbits and hares'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_animals/rabbit.png' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>"
  ))%>%
  select(Pic,Item,Heads,Description)

whole_table <- area%>%
  left_join(crops_plot_column)%>%
  left_join(crops_chart_column)


# Make table----------

# Format number of heads
whole_table$Heads<-format(whole_table$Heads,big.mark=",",small.mark='',trim=TRUE)

tb <- whole_table %>%
  mutate(Production = NA)%>%
  mutate(Trends = NA)%>%
  mutate(Number=seq(1,10,1))%>%
  mutate(Item=case_when(
    Item=='Eggs Primary'~'Laying hens',
    Item=='Rabbits and hares'~'Rabbits',
    TRUE~Item
  ))%>%
  # Merge all infos for each animal description
  mutate(
    combo = combine_word(Number,Item, Heads, Description),
    combo = purrr::map(combo, gt::html)
  )%>%
  # Select and order columns of interest
  select(Pic,combo,Trends,Production)%>%
  # Make table
  gt()%>%
  # Add pictures
  text_transform(
    locations = cells_body(columns=Pic),
    fn = function(x){
      purrr::map(
        whole_table$Pic, gt::html
      )
    }
  )%>%
  # Add plots of trends
  text_transform(
    locations = cells_body(columns=Trends),
    fn = function(x){
      purrr::map(
        whole_table$chart, ggplot_image, height = px(90), aspect_ratio = 1.2
      )
    }
  )%>%
  # Add maps of production
  text_transform(
    locations = cells_body(columns=Production),
    fn = function(x){
      purrr::map(
        whole_table$plot, ggplot_image, height = px(100), aspect_ratio = 2
      )
    }
  )%>% 
  # Add title and subtitle
  tab_header(
    title = "The big barnyard",
    subtitle=md("This table shows **the ten most important types of farm animals of the world** (excluding fishes), based on the number of heads for each category of the Food and Agriculture Organization (FAO) database. Poultry stocks have risen sharply over the past 30 years. In comparison, the numbers of ruminants such as cattle, sheep and goats have remained relatively stable, while pig stocks started to go down recently. In addition, for meat poultry, productivity per animal has greatly increased. A very large proportion of the farmed animals of the world is in China, while most of the trade in living animals is done within European countries.")
  )%>%
  # Add source note
  tab_source_note(
    source_note = md("**Data:** FAO | **Table:** @BjnNowak")
  )%>%
  # Change column names
  cols_label(
    Pic = 'Animals',
    combo = "",
    Trends = gt::html(
      "<span style='color:#0f4c5c'>Stocks</span><span style='color:#0f4c5c; font-size:11px'> (heads)</span><span style='color:black'> and</span>
       <br><span style='color:#9a031e'>Meat</span><span style='color:black'>, </span><span style='color:#60A561'>Milk</span><span style='color:black'> or </span><span style='color:#e36414'>Eggs</span>
        <br><span style='color:black'>production trends</span>"),
    Production = gt::html(
    "<span>Total country stocks</span> 
    <br><span style='font-size:11px'>(Up to </span><span style='color:#9A031E;font-size:11px'>86%</span><span style='font-size:11px'> of world production)</span>
       <br><span>and top </span><span style='color:#EF2D56'>importer</span><span style='color:black'>/</span><span style='color:#004643'>exporter</span>"),
  )%>%
  # Add footnotes
  tab_footnote(
    footnote = md("Heads number in animals description and in the maps refer to year 2018."),
    locations = cells_column_labels(columns = Pic)
  )%>%
  tab_footnote(
    footnote = md("Trends from 1990 to 2019 expressed as percentage of values for year 1990"),
    locations = cells_column_labels(columns = Trends)
  )%>%
  tab_footnote(
    footnote = md("Top importer and exporter refer to **living animals** trades in 2018 (except for geese, where figures for live animals were not available and labels refer to meat trades)"),
    locations = cells_column_labels(columns = Production)
  )%>%

  # Style
  # Set columns size
  cols_width(
    combo ~ px(240),
    Pic ~ px(100),
    Trends ~ px(120),
    Production ~px(200)
  )%>%
  # Title
  tab_style(
    locations = cells_title(groups = 'title'),
    style = list(
      cell_text(
        font=google_font(name = 'Bebas Neue'), 
        size='xx-large',weight='bold',align='left',
        color='#9A031E'
      )))%>%
  # Subtitle
  tab_style(
    locations = cells_title(groups = 'subtitle'),
    style = list(
      cell_text(
        font=google_font(name = 'Oswald'), 
        size='small',align='left'
      )))%>%
  # Header
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Anton"), 
        align = "left",v_align = "middle",size='small',color='black')),
    locations = cells_column_labels(Pic)
  )%>%
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Anton"), 
        align = "center",v_align = "middle",size='small',color='black')),
    locations = cells_column_labels(c(Trends,Production))
  )%>%
  # Columns
  tab_style(
    style = list(
      cell_text(font=google_font(name = "Oswald"),align = 'left'
      )),
    locations = cells_body(columns = c(combo)
    )
  )%>%
  # Highlight one row each two rows
  # to improve readability
  tab_style(
    style = list(cell_fill(color = "#f0f0f0")),
    locations = cells_body(rows = seq(1,9,2))
  )%>%
  # Footnote
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Roboto Condensed"
      ),style = "italic",size='small')),
    locations = cells_footnotes()
  )%>%
  # Source note
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Oswald"
      ),size='small')),
    locations = cells_source_notes()
  )%>%
  # General table options
  tab_options(
    data_row.padding = px(0),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table_body.hlines.style = "hidden",
    table_body.border.top.style = "solid",
    column_labels.border.bottom.style = "solid"
  )

# Print table
tb

# May export table as png with gtsave() but, for me, I lose
# custom fonts with that function. Better paste script in an 
# R markdown file and knit to html
# gtsave(tb,"CultivatedPlanet.png")


