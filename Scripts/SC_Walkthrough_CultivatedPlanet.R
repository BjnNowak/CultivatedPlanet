
# Effacement de la mémoire
rm(list=ls())
gc()


# Load libraries
library(tidyverse)
library(gt)

# Create table
table <- tibble(
  # Name of the crop
  crop = c('Wheat'),
  # Area harvested (in Mha)
  surface = c(214),
  # Short description
  description = c(
    'Originally from the Fertile Crescent, in the Middle East, wheat is now the most cultivated cereal in the world, mostly for human consumption. China is the main producer, with 17% of world production.'
  )
)

# Make first table
out1 <- table%>%
  # Use row number as crop rank
  dplyr::mutate(rank=row_number())%>%
  # Select and order columns
  dplyr::select(rank,crop,surface,description)%>%
  # Print table
  gt::gt()

out1


table<-table%>%
  mutate(URL=case_when(
    crop=='Wheat'~"<img 
      src='https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/Pictures_crops/Wheat2.png' 
      style='border-radius: 50% 50% 50% 50%;height:80px;'
    >",
  ))%>%
  # Then use gt::html() to interpret text as HTML-formatted text
  mutate(
    picture = purrr::map(URL, gt::html)
  )

# Let's add one picture to this table


# Make second table
out2<-table%>%
  select(picture,crop,description)%>%
  gt()

out2

# Now let's do some text customization!

# We start by creating a function with desired text layout in HTML
format_text <- function(rank, crop, surface, description){
  glue::glue(   # Use glue to call R variables inside strings
    "<div style='line-height:16px; text-align:left'>
      <span style='font-weight:bold; font-size:16px'>{rank}. </span>
      <span style='font-weight:bold;font-size:14px'>{crop}</span>
      <span style='font-size:14px'> ({surface} Mha)</span>
    </div>
     <div style='line-height:12px; text-align:left'>
      <span style ='color:#2f2f2f;font-size:12px'>{description}</span>
    </div>"
  )
}
# Apply function to our table then convert to html
table <- table %>%
  mutate(rank=row_number())%>%
  mutate(
    txt_glue = format_text(rank,crop,surface,description),
    txt_html = purrr::map(txt_glue, gt::html)
  )
# Make third table
out3<-table%>%
  select(picture,txt_html)%>%
  gt()
out3

# finally, let's see how we can add a plot to the table

# Load table with data to create plot
data<-read_csv(
  'https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/data/FAO_CropsProductivity_subset.csv'
)

# Filter data and add ratio (compared to 1990)
data<-data%>%
  filter(Item=="Wheat")%>%
  filter(Element=="Area harvested")%>%
  filter(Area=="World")%>%
  filter(Year>1989)%>%
  group_by(Item,Element)%>%
  mutate(Base=Value[Year==1990])%>%
  mutate(Ratio=Value/Base)%>%
  ungroup()

# Try one minimal plot
ggplot(data,aes(x=Year,y=Ratio))+
  annotate(
    geom='text',x=2000,y=0.9,label='2000',
    hjust=0.5,color='dimgrey',size=13)+
  annotate(
    geom='text',x=2010,y=0.9,label='2010',
    hjust=0.5,color='dimgrey',size=13)+
  geom_line(color="forestgreen",size=2)+
  geom_point(color="forestgreen",size=5)+
  theme_void()


# Create function to draw plot
fun_plot <- function(data){
  trend <- ggplot(data,aes(x=Year,y=Ratio))+
    annotate(
      geom='text',x=2000,y=0.9,label='2000',
      hjust=0.5,color='dimgrey',size=13)+
    annotate(
      geom='text',x=2010,y=0.9,label='2010',
      hjust=0.5,color='dimgrey',size=13)+
    geom_line(color="forestgreen",size=2)+
    geom_point(color="forestgreen",size=5)+
    theme_void()
  return(trend)
}

data_plot<-data%>%
  # Group by crop
  group_by(Item)%>%
  # Use nest() to get one row by crop
  nest()%>%
  # Apply function
  mutate(
    gg = purrr::map(data,fun_plot) 
  )%>%
  select(crop=Item,gg)

data_plot

# Join column with ggplot object
table <- table%>%
  left_join(data_plot)

out3<-table%>%
  # Add empty columns to fill with ggplot object later
  mutate(Surface = NA)%>%
  select(Crop=picture,Description=txt_html,Surface)%>%
  # Make table
  gt()%>%
  # Add ggplot 
  gt::text_transform(
    # Where to add the plot
    locations = cells_body(columns=Surface),
    fn = function(x){
      purrr::map(
        # Apply ggplot_image on the column where
        # the ggplot object is stored to convert
        # it to HTML
        table$gg, gt::ggplot_image, 
        height = px(90), aspect_ratio = 1.2
  )})
out3

# BONUS!
# Change theme easily

library(gtExtras)

tb3%>%
  gt_theme_nytimes()
  

