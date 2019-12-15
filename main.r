# Libraries
library(tidyverse)
# Download
download_data <- function(link) {
  # Create temp file to store data
  temp <- tempfile(pattern = "file", fileext = ".zip")
  # download and open zip, read csv inside
  download.file(link, mode = "wb", destfile = temp, method = "wininet")
  x <- unzip(temp, exdir = tempdir())
  data_set <- read.csv(x[2], skip = 3, header = TRUE)
  unlink(temp)
  return(data_set)
}
# Clean
prepare_data <- function(downloaded_data) {
  # remove variables with no data in 2000-2018
  downloaded_data %>%
    dplyr::select(-c(1:which(names(downloaded_data) == "X1999"), 64:65)) %>%
    as.data.frame(row.names = as.character(downloaded_data$Indicator.Code)) %>%
    na.omit() %>%
    t() %>%
    as.data.frame() %>%
    return()
}
# Plots
visualize_data <- function(data_table) {
  names(data_table) %>%
    str_split(pattern = "\\.") %>%
    map_chr(2) %>%
    table() %>%
    as.data.frame() %>%
    setNames(c("Indicator", "Frequency")) %>%
    ggplot(aes(x = reorder(Indicator, -Frequency), y = Frequency)) +
    geom_bar(stat = "identity") +
    labs(title = "Number of variable categories",
         subtitle = "Finland, 2000-2018") -> viz1
  print(viz1)
  data_table %>%
    select(NY.GDP.DEFL.KD.ZG, SL.UEM.TOTL.ZS) %>%
    mutate(Year = 2000:2018) %>%
    gather(Var, Val, -Year) %>%
    ggplot(aes(x = Year, y = Val, color = Var)) +
    geom_line() +
    theme(legend.position = "bottom") +
    labs(title = "Comparison of inflation and unemployment",
         subtitle = "Finland, 2000-2018") -> viz2
  print(viz2)
  data_table %>%
    select(SP.POP.TOTL, NY.GDP.PCAP.CD) %>%
    mutate(Year = 2000:2018) %>%
    ggplot(aes(x = SP.POP.TOTL, y = NY.GDP.PCAP.CD, col = Year)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE) +
    labs(x = "Population, total", y = "GDP per capita",
         title = "Relation of GDP per capita to population",
         subtitle = "Finland, 2000-2018") -> viz3
  print(viz3)
}
# Code execution
data_link <- "http://api.worldbank.org/v2/en/country/FIN?downloadformat=csv"
data_finland <- download_data(data_link)
data_finland_clean <- prepare_data(data_finland)
visualize_data(data_finland_clean)
