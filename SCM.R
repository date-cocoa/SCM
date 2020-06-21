# reset environment
rm(list = ls())

# define funcitons
library_import <- function(package_name){
  ###
  # install and load library
  # input: 
  #   package_name(str): package name you want to install and load
  ###
  tryCatch(
    {flag <- require(package_name, character.only = TRUE, quietly = TRUE)},
    warning = function(e){ # if warning is occuerd
      print(package_name)
      print('installing and loading this package...')
      install.packages(package_name, quiet = TRUE)
      library(package_name, character.only = TRUE)
      print('sucess!!')
    }
  )
  if (flag){
    print(package_name)
    print('this package is alrady imstalled')
    print('loading this package...')
    library(package_name, character.only = TRUE)
    print('sucess!!')
  }
}

# excution
library_import('tidyverse')
library_import('Synth')
data('basque')

plot_timeseries_basque <- 
  basque %>% 
  ggplot(aes(x = year, y = gdpcap, group = regionname)) +
  geom_line(alpha=0.5) +
  geom_line(data = basque %>% filter(regionname=='Basque Country (Pais Vasco)'), color='red')






