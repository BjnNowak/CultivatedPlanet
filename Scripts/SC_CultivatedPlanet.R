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

# Crops production
# Full dataset available here: 
# https://www.fao.org/faostat/en/#data/QCL
# (Bulk download > All Data Normalized)
# data<-read_csv("Production_Crops_Livestock_E_All_Data_(Normalized).csv")

# Subset with only necessary data for this table:
data<-read_csv('https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/data/FAO_CropsProductivity_subset.csv')

# Food balance 
# Full dataset available here: 
# https://www.fao.org/faostat/en/#data/FBS
# (Bulk download > All Data Normalized)
# balance<-read_csv("FoodBalanceSheets_E_All_Data_(Normalized).csv")

# Subset with only necessary data for this table:
balance<-read_csv('https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/data/FAO_FoodBalance_subset.csv')

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
  ))

balance<-balance%>%
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


# Get area cultivated for each crop in 2019
dat19<-data%>%
  filter(Year==2018)%>%
  filter(Element=='Area harvested')%>%
  filter(Area=='World')%>%
  arrange(-Value)

# Description for the 10 most cultivated crops in the world
most_crops <- c('Wheat','Maize','Rice, paddy','Soybeans','Barley',
               'Sorghum','Seed cotton','Rapeseed','Beans, dry','Millet')

# Only keep balance for most crops
balance<-balance%>%
  mutate(Item_clean=case_when(
    Item=="Wheat and products"~'Wheat',
    Item=="Maize and products"~'Maize',
    Item=="Rice and products"~'Rice, paddy',
    Item=='Soyabeans'~'Soybeans',
    Item=='Barley and products'~'Barley',
    Item=='Sorghum and products'~'Sorghum',
    Item=='Cottonseed'~'Seed cotton',
    Item=='Rape and Mustardseed'~'Rapeseed',
    Item=='Beans'~'Beans, dry',
    Item=='Millet and products'~'Millet',
    TRUE~'Other'
  ))%>%
  filter(Item_clean%in%most_crops)

categories <- cbind.data.frame(
  Item = most_crops,
  Description = c(
    # Wheat
    'Originally from the Fertile Crescent, in the Middle East, wheat is now the most cultivated cereal in the world, mostly for human consumption. China is the main producer, with 17% of world production.',
    
    # Maize -> CORRECTED
    "Maize, first cultivated in México, is now widely grown, except in the coldest regions. It is mainly used for animal feed. The USA produces more than 30% of the world's harvest.",
    
    # Rice
    "Rice is the world's third major cereal but, unlike the others, it is still mostly grown in its historical area of origin: Asia. China and India produce more than half of total production.",
    
    # Soybeans -> CORRECTED
    'Soya is the most cultivated legume. First domesticated in China, it is now produced primarily in America (34% in Brazil) and exported throughout the world, mainly for animal feeding.',
    
    # Barley
    'Barley is mainly grown in temperate regions, for malting and animal feeding. Russia is the main producer, with 13% of global production.',
    
    # Sorghum -> CORRECTED
    'First domesticated in Africa, sorghum is frequently used as an alternative to maize in dry areas (12% of total production in Nigeria, just behind USA). It may also be used for bioenergy production.',

    # Cotton -> CORRECTED
    'Cotton, the leading textile crop, was domesticated in both the New World (México and South America) and Old World (Africa and Asia). Now China produces 29% of world production.',
    
    # Rapeseed
    'Rapeseed is grown for oil production. Once pressed, oilcakes are used for animal feeding. Canada is the main producer (26% of total production).',
    
    # Beans
    "Beans are the second most important legume species cultivated in the world. They are more frequently used for human consumption than soybeans. Myanmar is the world's largest producer (20%).",
    
    # Millet
    'Millet is a cereal generally cultivated in an extensive way, in dry regions. India is the main producer, with 36% of total production.'
))

# Evolution of surface and yield since 2019 (for plot)
crops_trends<-data%>%
  filter(Year>1989)%>%
  filter(Item%in%most_crops)%>%
  filter(Element%in%c('Yield','Area harvested'))%>%
  select(Area,Item,Element,Value,Year)%>%
  filter(Area=='World')%>%
  group_by(Item,Element)%>%
  mutate(Base=Value[Year==1990])%>%
  mutate(Ratio=Value/Base)%>%
  ungroup()

# Surface of each crop in 2019 (for maps)
crops19<-data%>%
  filter(Year=='2019')%>%
  filter(Item%in%most_crops)%>%
  filter(Element=='Production')%>%
  select(Area,'Area Code',Item,Production=Value)%>%
  group_by(Item)%>%
  mutate(world_prod=Production[Area=='World'])%>%
  mutate(ratio_prod=Production/world_prod)%>%
  ungroup()%>%
  filter(get('Area Code')<1000)%>%
  select(-'Area Code')

# If you wanna take a look at main producers ranking...
#crops19%>%
#  filter(Item=='Millet')%>%
#  arrange(-ratio_prod)

# Save highest ratio for map scales
# (India producing 36% of world Millet)
max_percent<-max(na.omit(crops19$ratio_prod))


# Top imports/exports per crop
# Top importer
import<-balance%>%
  filter(Element=='Import Quantity')%>%
  filter(get('Area Code')<1000)%>%
  group_by(Item_clean)%>%
  # Keeping only top importer for the sake of clarity
  # but you may keep more by changing 'n' value
  slice_max(order_by = Value,n=1)%>%
  ungroup()

# Top exporter
export<-balance%>%
  filter(Element=='Export Quantity')%>%
  filter(get('Area Code')<1000)%>%
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

# (Myanmar centroid is ok but slightly moving it south east so 
# names does not overlaps with India
st_geometry(centroids[centroids$Area=='Myanmar',]) <-  st_sfc(st_point(c(116.21602171139949,7.2705372388883625)))


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
  #"#DAD18A",
  #"#B5C375",
  "#91B561","#6CA74C","#479937","#228B22"
)
# As country with 0 production are not reported in the table,
# take the 'lowest color' of the palette for NA's 
na_pal<-pal[1]

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
    annotate(geom='segment',x=1990,xend=2019,y=1.5,yend=1.5,color='dimgrey',lty='dotted',size=2)+
    annotate(geom='segment',x=1990,xend=2019,y=2,yend=2,color='dimgrey',lty='dotted',size=2)+
    annotate(geom='text',x=1990,y=1.65,label='+50 %',hjust=0,color='dimgrey',size=13,family='oswald')+
    annotate(geom='text',x=1990,y=2.15,label='+100 %',hjust=0,color='dimgrey',size=13,family='oswald')+
   
    # Add points and line
    geom_point(size=6)+
    geom_line(size=2.5)+
    
    # Set scales and theme
    scale_color_manual(values=c('#7D0037','#1E3888'))+
    scale_y_continuous(limits=c(0.55,2.3))+
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
        #geom_sf_text(data,mapping=aes(geometry=geom_cent,label = Area),color='blue',size=15,family='fira')+
    
    scale_fill_gradientn(colors=pal,na.value = na_pal,limits=c(0,max_percent))+
    
    theme_void()+
    theme(plot.margin=margin(0,0,0,0,"cm"))
  
  return(pl)

}



# Function to merge crop name + description
# (offers more customization than gtExtras::gt_merge_stack())
# Adapted from Tom Mock: 
# https://themockup.blog/posts/2020-10-31-embedding-custom-features-in-gt-tables/
combine_word <- function(number, crop, surface, description){
  glue::glue(
    "<div style='line-height:16px; text-align:left'><span style='font-weight:bold; font-size:16px'>{number}. </span><span style='font-weight:bold;font-size:14px'>{crop}</span><span style='font-size:14px'> ({surface} Mha)</span></div>
     <div style='line-height:12px; text-align:left'><span style ='color:#2f2f2f;font-size:12px'>{description}</span></div>"
  )
}

# Merging tables ------

# 'Map' plotting functions and add columns with ggplot objects
crops_chart_column <-
  crops_trends %>%
  drop_na(Item)%>%
  group_by(Item) %>%
  nest()%>%
  mutate(
    chart = purrr::map(data,trend) 
  )%>%
  select(Item,chart)

crops_plot_column <-
  states %>%
  left_join(crops19)%>%
  complete(Area, Item, fill = list(Value = 0))%>%
  full_join(export_cent, by=c('Item','Area'))%>%
  full_join(import_cent, by=c('Item','Area'))%>%
  #bind_rows(export_cent)
  #bind_rows(import_cent)

  drop_na(Item)%>%
  group_by(Item) %>%
  nest()%>%
  mutate(
    plot = purrr::map(data,mapping) 
  )%>%
  select(Item,plot)

# Add these columns to main 'area' table

area<-dat19%>%
  filter(Item %in% most_crops)%>%
  left_join(categories)%>%
  mutate(
    # Convert to million hectares
    Surface = round(Value/1000000)
  )%>%
  select(Item,Description,Surface)

whole_table <- area%>%
  left_join(crops_plot_column)%>%
  left_join(crops_chart_column)%>%
  mutate(Item=case_when(
    Item=='Rice, paddy'~'Rice',
    Item=='Seed cotton'~'Cotton',
    Item=='Beans, dry'~'Beans',
    TRUE~Item
  ))%>%
  # Tried to make a 'seed shape' for crop pics with 8-points border-radius
  # but hard to decide for a shape since seeds of crops in the list have different shapes...
  mutate(Pic2=case_when(
    Item=='Wheat'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_Crops/Wheat2.png' style='border-radius: 0% 100% 15% 45% / 0% 45% 15% 100%;height:80px;'>",
    Item=='Maize'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_Crops/Corn2.png' style='border-radius: 0% 100% 15% 45% / 0% 45% 15% 100%;height:80px;'>",
    Item=='Rice'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_Crops/Rice2.png' style='border-radius: 0% 100% 15% 45% / 0% 45% 15% 100%;height:80px;'>",
    Item=='Soybeans'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_Crops/Soybeans2.png' style='border-radius: 0% 100% 15% 45% / 0% 45% 15% 100%;height:80px;'>",
    Item=='Barley'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_Crops/Barley2.png' style='border-radius: 0% 100% 15% 45% / 0% 45% 15% 100%;height:80px;'>",
    Item=='Sorghum'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_Crops/Sorghum2.png' style='border-radius: 0% 100% 15% 45% / 0% 45% 15% 100%;height:80px;'>",
    Item=='Cotton'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_Crops/Cotton2.png' style='border-radius: 0% 100% 15% 45% / 0% 45% 15% 100%;height:80px;'>",
    Item=='Rapeseed'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_Crops/Rapeseed2.png' style='border-radius: 0% 100% 15% 45% / 0% 45% 15% 100%;height:80px;'>",
    Item=='Beans'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_Crops/Beans2.png' style='border-radius: 0% 100% 15% 45% / 0% 45% 15% 100%;height:80px;'>",
    Item=='Millet'~"<img src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_Crops/Millet2.png' style='border-radius: 0% 100% 15% 45% / 0% 45% 15% 100%;height:80px;'>"
  ))

# Make table ------

tb<-whole_table%>%
  # Add empty columns to fill with images and plots
  mutate(Pic = NA)%>%
  mutate(Production = NA)%>%
  mutate(Trends = NA)%>%
  # Add number for crops ranking 
  mutate(Number=seq(1,10,1))%>%
  # Merge all infos for each crop description
  mutate(
    combo = combine_word(Number,Item, Surface, Description),
    combo = purrr::map(combo, gt::html)
  )%>%
  # Select and order columns of interest
  select(Pic,combo,-Item,-Description,Trends,Production)%>%
  # Make table
  gt()%>%
  # Add pictures
  text_transform(
    locations = cells_body(columns=Pic),
    fn = function(x){
      purrr::map(
        whole_table$Pic2, gt::html
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
    title = "Our cultivated planet",
    subtitle = md("This table shows **the ten most cultivated crops of the world**. Of 1,400 million hectares (Mha) of arable land, more than a third is covered by the three major cereals: wheat, maize and rice. Soybeans are the only other crop to be grown on more than 100 Mha, with strong growth over the last 30 years, while other crops covers much smaller areas. Although they do not always have the highest yields, the largest countries (USA, China, Russia, India, etc.) are the main grain producers, with more or less regional specificities depending on the crop. the largest producers are not always the largest exporters (as in the case of the USA for millet).")
  )%>%
  # Add footnotes
  tab_footnote(
    footnote = md("The areas occupied by each crop as well as the maps of the quantity produced by each country refer to year 2018."),
    locations = cells_column_labels(columns = Pic)
  )%>%
  tab_footnote(
    footnote = md("Trends from 1990 to 2019 expressed as percentage of values for year 1990"),
    locations = cells_column_labels(columns = Trends)
  )%>%
  # Add source note
  tab_source_note(
    source_note = md("**Data:** FAO | **Table:** @BjnNowak")
  )%>%
  # Change column names
  cols_label(
    Pic = 'Crops',
    combo = "",
    Trends = gt::html(
      "<span style='color:#1E3888'>Yield</span><span style='color:#1E3888; font-size:11px'> (per ha)</span><span style='color:black'> and</span>
       <br><span style='color:#7D0037'>Surface</span><span style='color:black'> evolution</span>"),
    Production = gt::html(
      "<div><span>Total country production</span></div> 
       <div style='font-size:11px'><span>(Up to </span><span style='color:#228B22'>36%</span><span style='color:black'> of world production)</span></div>
       <div><span>and top </span><span style='color:#EF2D56'>importer</span><span style='color:black'>/</span><span style='color:#004643'>exporter</span></div>"),
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
        color='forestgreen'
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
        align = "center",v_align = "middle",size='small',color='black')),
    locations = cells_column_labels(c(Trends,Production))
  )%>%
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Anton"), 
        align = "left",v_align = "middle",size='small',color='black')),
    locations = cells_column_labels(Pic)
  )%>%
  # Columns
  tab_style(
    style = list(
      cell_text(font=google_font(name = "Oswald"),align = 'left'
      )),
    locations = cells_body(columns = c(combo)
    )
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
  # Highlight one row each two rows
  # to improve readability
  tab_style(
    style = list(cell_fill(color = "#f0f0f0")),
    locations = cells_body(rows = seq(1,9,2))
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


