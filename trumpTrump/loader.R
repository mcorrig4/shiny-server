#######################
#####  LOADER.R  ######
#######################

load_data <- function(data.source) {
  library(shinyjs)
  library(rsparkling)
  library(sparklyr)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(reshape2)
  options(rsparkling.sparklingwater.version = "2.0.3")


  #sc <- spark_connect(master = "local", version = "2.0.0", hadoop_version="2.7")
  #crimes_tbl <- spark_read_csv(sc, "crimes", data.source)
  crimes_tbl <- NULL
    
  hide("loading_page")
  show("main_content")
  
  #spark_disconnect(sc)
  return(crimes_tbl)
}

