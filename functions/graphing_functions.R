
# calculate mean and sd ---------------------------------------------------

mean_sd <- function(data.df,group_by){
  mean.df <- data.df %>% 
    group_by(across(group_by)) %>%
    summarize_if(~!is.character(.),mean, na.rm=TRUE) %>%
    ungroup()
  
  sd.df <- data.df %>%
    group_by(across(group_by)) %>%
    summarize_if(is.numeric,sd,na.rm=TRUE) %>%
    ungroup()
  
  newdata.df <- left_join(mean.df,sd.df,by=group_by,suffix=c(".mean",".sd"))
  
  return(newdata.df)
}


# calculate long dataframe ------------------------------------------------

plot_longer <- function(data.df,long_cols){
  long.df <- data.df %>%
    pivot_longer(long_cols)
  
  long.df <- left_join(long.df,labs.df)
  
  factor_names <- long.df %>%
    pull(ylabs) %>%
    unique()
  
  long.df$ylabs <- factor(long.df$ylabs,levels=factor_names)
  
  factor_names <- long.df %>%
    pull(xlabs) %>%
    unique()
  
  long.df$xlabs <- factor(long.df$xlabs,levels=factor_names)
  
  return (long.df)
}

plot_longer_mean <- function(data.df,group_by,cols){
  mean.df <- data.df %>%
    select(c(group_by,paste0(cols,'.mean'))) %>%
    pivot_longer(all_of(paste0(cols,'.mean')),values_to="mean")
  
  temp_labs.df <- labs.df %>%
    mutate(name=paste0(name,".mean"))
  
  mean.df <- left_join(mean.df,temp_labs.df) %>%
    select(-name)
  
  sd.df <- data.df %>%
    select(c(group_by,paste0(cols,'.sd'))) %>%
    pivot_longer(all_of(paste0(cols,'.sd')),values_to="sd")
  
  temp_labs.df <- labs.df %>%
    mutate(name=paste0(name,".sd"))
  
  sd.df <- left_join(sd.df,temp_labs.df) %>%
    select(-name)
  
  long.df <- left_join(mean.df,sd.df)
  
  factor_names <- long.df %>%
    pull(ylabs) %>%
    unique()
  
  long.df$ylabs <- factor(long.df$ylabs,levels=factor_names)
  
  factor_names <- long.df %>%
    pull(xlabs) %>%
    unique()
  
  long.df$xlabs <- factor(long.df$xlabs,levels=factor_names)
  
  return (long.df)
}


# plot regression ---------------------------------------------------------

plot_regression <- function(data.df,x,y,group){
  x.mean <- paste0(x,".mean")
  y.mean <- paste0(y,".mean")
  x.sd <- paste0(x,".sd")
  y.sd <- paste0(y,".sd")
  
  x.lab <- labs.df %>%
    filter(name==x) %>%
    pull(xlabs)
  
  y.lab <- labs.df %>%
    filter(name==y) %>%
    pull(ylabs)
  
  group.lab <- labs.df %>%
    filter(name==group) %>%
    pull(xlabs)
  
  n_groups <- data.df %>%
    pull(get(group)) %>%
    unique() %>%
    length()
  
  if (n_groups<5){
    splot <- ggplot(data.df,aes_string(x.mean, y.mean, shape=group, color=group, group=group))+
      geom_point(fill=NA, alpha=0.7, na.rm=TRUE)+
      geom_smooth(color="black", se=FALSE, method="lm",formula="y~x",size=0.8, alpha=0.7, na.rm=TRUE)+
      geom_smooth(se=FALSE, method="lm",formula="y~x",size=0.4, alpha=0.7, na.rm=TRUE)+
      myregression+
      labs(x=parse(text=x.lab),y=parse(text=y.lab),
           color=NULL, shape=NULL
           )
  } else {
    splot <- ggplot(data.df,aes_string(x.mean, y.mean, color=group, group=group))+
      geom_point(shape=21,fill=NA,alpha=0.8, na.rm=TRUE)+
      geom_smooth(color="black",se=FALSE, method="lm",formula="y~x",size=0.8, alpha=0.7, na.rm=TRUE)+
      geom_smooth(se=FALSE, method="lm",formula="y~x",size=0.4, alpha=0.7, na.rm=TRUE)+
      myregression+
      labs(x=parse(text=x.lab),y=parse(text=y.lab),fill=NULL,
           color=NULL)
  }
  
  xhist <- axis_canvas(splot, axis = "x")+
    geom_density(data=data.df, aes_string(x.mean, fill=group),alpha=0.3, na.rm=TRUE)+
    marginaltheme
  yhist <- axis_canvas(splot, axis = "y", coord_flip=TRUE)+
    geom_density(data=data.df, aes_string(y.mean, fill=group),alpha=0.3, na.rm=TRUE)+
    marginaltheme+
    coord_flip()
  
  combined_plot <- insert_xaxis_grob(splot, xhist, position = "top") %>% insert_yaxis_grob(., yhist, position = "right")
  
  return (combined_plot)
}
