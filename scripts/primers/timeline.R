rm(list = ls()) #clear environment

# install packages --------------------------------------------------------

library(scales)
library(tidyverse)
library(timevis)
library(readxl)
library(ggsci)
library(ggrepel) #for managing the text


# load data ---------------------------------------------------------------


#import historical timeline data
timeline.df <- read_excel("raw/timeline.xlsx") %>%
  arrange(year)

#set type as a factor for graphing
#timeline.df$type <- factor(timeline.df$type, 
                           #levels=c("Physical Modification","Ecology","Nutrient Cycling"),
 #                          ordered=TRUE)

# set random direction ----------------------------------------------------

random_direction<-TRUE

if (random_direction){
  
  # Set the heights we will use for our milestones.
  positions <- rep(c(1:5),times=10)
  
  
  # Set the directions we will use for our milestone (above and below)
  directions <- c(1, -1) 
  
  
  # Assign the positions & directions to each year from those set above.
  line_pos.df <- data.frame(
    "year"=unique(timeline.df$year),
    "position"=rep(positions, length.out=length(unique(timeline.df$year))),
    "direction"=rep(directions, length.out=length(unique(timeline.df$year))))
  
  #set position to positive and negative based on direction
  line_pos.df$position <- line_pos.df$position * line_pos.df$direction
  
  timeline.df$position <- NULL
  
  # Create columns with the specified positions and directions for each milestone event
  timeline.df <- full_join(timeline.df, line_pos.df, by="year") 
}


# use defined direction ---------------------------------------------------

minyear <- 1600
maxyear <- 2030
breaks <- 50

wholenumber <- function(x,n){
  
  result.list <- list()
  
  for (i in 1:length(x)){
    
    y <- x[i]/n
    
    if (y-round(y)==0){
      
      result <- TRUE
      
    } else {
      
      result <- FALSE
      
    }
    result.list <- append(result.list, result)
  }
  
  return(result.list)
}

#define years of x axis
year_range.df <- data.frame(year_range=c(minyear:maxyear)) %>%
  mutate(year_label=ifelse(wholenumber(year_range, breaks), year_range, NA),
         year_breaks=ifelse(wholenumber(year_range, 10), year_range, NA))


# plot --------------------------------------------------------------------

# offset the labels 0.2 away from scatter points
text_offset <- 0.2

# Let's use the absolute value since we want to add the text_offset and increase space away from the scatter points 
timeline.df$text_position <- (text_offset*ceiling((nchar(timeline.df$description)/20)) + abs(timeline.df$position)) * sign(timeline.df$position)

#timeline
ggplot(timeline.df,aes(x=year,y=position))+
  geom_hline(yintercept=0, color = "black", size=0.7)+
  geom_segment(data=year_range.df, aes(y=-0.2, yend=0.2, x=year_label, xend=year_label))+
  geom_segment(data=year_range.df, aes(y=-0.1, yend=0.1, x=year_breaks, xend=year_breaks))+
  geom_segment(aes(yend=0,xend=year), color='black', size=0.2)+
  geom_point(shape=21,fill="orange",size=3)+
  geom_text(data=year_range.df, aes(x=year_range,y=-0.5, label=year_label),size=3.5, color='black', angle=0)+
  geom_text(aes(y=text_position, label = str_wrap(description, 20)),
                  size=3, direction="y", show.legend=FALSE)+
  theme_classic()+
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank())+
  scale_shape_manual(values=21:25)+
  labs(fill=NULL, shape=NULL)
 

ggsave("figures/timeline.png",width=10, height=8)
