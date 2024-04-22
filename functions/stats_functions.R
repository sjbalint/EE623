
# make a custom qq-plot ---------------------------------------------------

qq_plot <- function(data.df, cols){
  
  result.list <- list()
  
  for (col in cols){
    qq.df <- data.frame(
      qqnorm(data.df[,col], plot.it=FALSE)
    )
    colnames(qq.df) <- c("x","y")
    qq.df$name <- col
    result.list <- append(result.list, list(qq.df))
  }
  
  qq.df <- bind_rows(result.list)
  
  print(
    ggplot(qq.df, aes(x,y,fill=name, shape=name))+
      theme_bw()+
      geom_point()+
      geom_abline()+
      theme(legend.position="top")+
      labs(x="Theoretical Quantiles",
           y="Sample Quantiles")+
      scale_fill_grey()+
      scale_shape_manual(values=c(21:25))
  )
}

# make a function to determine season -------------------------------------

get_season <- function(date_column,astronomical=TRUE) {
  
  if (astronomical){
    WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox 
  } else {
    WS <- as.Date("2012-12-00", format = "%Y-%m-%d") # December
    SE <- as.Date("2012-3-00",  format = "%Y-%m-%d") # March
    SS <- as.Date("2012-6-00",  format = "%Y-%m-%d") # June
    FE <- as.Date("2012-9-00",  format = "%Y-%m-%d") # September
  }
  
  # Convert dates from any year to 2012 dates
  fakedate <- as.Date(strftime(date_column, format="2012-%m-%d"))
  
  seasons <- c("Winter","Spring","Summer","Fall")
  
  results <- ifelse (fakedate >= WS | fakedate < SE, seasons[1],
                    ifelse (fakedate >= SE & fakedate < SS, seasons[2],
                            ifelse (fakedate >= SS & fakedate < FE, seasons[3], seasons[4])))
  
  results <- factor(results, levels=seasons)
  return(results)
}

# make a function to do the correlation -----------------------------------

monthly_wider <- function(data.df,mydepth,variable){
  temp.df <- data.df %>%
    mutate(date=format(datetime.est,"%Y%m")) %>%
    filter(depth==mydepth) %>%
    group_by(station,yearmonth) %>%
    select(c("date","station",variable)) %>%
    summarize_all(mean, na.rm=TRUE) %>%
    pivot_wider(names_from="station",values_from=variable) %>%
    mutate_if(is.numeric,round,2) %>%
    select(-date)
  
  return(temp.df)
}

station_matrix <- function(data.df,mydepth,variable){
  temp.df <- data.df %>%
    mutate(date=format(datetime.est,"%Y-%m-%d")) %>%
    filter(depth==mydepth) %>%
    group_by(station,date) %>%
    select(c("date","station",variable)) %>%
    summarize_all(median, na.rm=TRUE) %>%
    pivot_wider(names_from="station",values_from=variable) %>%
    mutate_if(is.numeric,round,2) %>%
    select(-date)
  
  return(temp.df)
}

mycorr <- function(wide.df){
  
  wide.df$date <- NULL
  
  temp.corr <- rcorr(as.matrix(wide.df))
  
  r2 <- as.data.frame(temp.corr$r) %>%
    round(2) %>%
    mutate(name="r2",
           station=row.names(.))
  
  p <- as.data.frame(temp.corr$P)%>%
    round(4) %>%
    mutate(name="p.value",
           station=row.names(.))
  
  n <- as.data.frame(temp.corr$n)%>%
    round(0) %>%
    mutate(name="n",
           station=row.names(.))
  
  temp.corr.df <- bind_rows(r2,p,n) %>%
    arrange(name,station)
  rownames(temp.corr.df) <- NULL
  
  return(temp.corr.df)
  
}


# function to remove outliers ---------------------------------------------

perform_IQR <- function(data, threshold=3){
  Q <- quantile(data, probs=c(.25, .75), na.rm = TRUE)
  iqr <- IQR(data, na.rm = TRUE)
  up <-  Q[2]+threshold*iqr # any values higher than this get removed
  low<- Q[1]-threshold*iqr # any values lower than this get removed. 
  #low is a negative number, so it doesn't really matter
  
  data <- ifelse(between(data,low,up), data, NA)
  
  return(data)
}
