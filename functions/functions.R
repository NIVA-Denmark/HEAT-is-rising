
#--DUPLICATES----
duplicate_check<- function(data){
  df<-data%>%group_by(across(c(-Longitude..degrees_east.,-Latitude..degrees_north.,-latitude_center,-longitude_center)))%>%
    summarise(n =n(),.groups = "drop") %>%
    filter(n>1)%>%
    select(-matches("QV"))
  check<-data.frame()
  for (col in names(df)[26:33]) {
    x<-all(is.na(df[[col]]))
    check <- rbind(check, data.frame(column = names(df[col]), NA_empty = x))
  }
  result <- check %>% filter(NA_empty == FALSE) %>% pull(column)
  
  if (length(result) == 0) {
    cat("\nAll columns have only NA values. There are no duplicates.")
    FALSE
  } else {
    res<-paste(result,collapse="\n")
    cat("Duplicates in columns:\n",res)
    TRUE
  }
}


duplicate_cluster_in_basins<- function(data){
  check<-data%>%
    group_by(ClusterID)%>%
    mutate(n=n())
  
  n<-sum(check$n>1)
  cat(n, "/",nrow(check)," = ", n/nrow(check)," (",n," clusters are represented more than once from ",nrow(check),")",sep="")
}

#--VISUALIZATION----

add_eea_grid<- function(grid,df,projection=st_crs(3035)){
  df1<-st_transform(df,projection)
  
  grid<-st_intersection(grid,df1)%>%
    mutate(area_km2=1e-6*as.numeric(sf::st_area(.)))
  return(grid)
}

