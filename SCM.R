#### reset environment ####
rm(list = ls())

#### define funcitons ####
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
      install.packages(package_name, quiet = TRUE, repos = "http://cran.us.r-project.org")
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

library_import('tidyverse')
library_import('Synth')

plot_timeseries <- function(data, red_region, file_path = NULL, save = FALSE){
  ###
  # plot gdbcap data over years 
  # input: 
  #   data(dataframe): data
  #   red_region(str): region you want to make red
  #   file_path(str): file path for saving plot (default = NULL)
  #   save(bool): wheter to save or not (default = FALSE)
  ###
  plot <- 
    data %>% 
    filter(regionname != "Spain (Espana)") %>% # omit Spain (Espana)
    ggplot(aes(x = year, y = gdpcap, group = regionname)) +
    geom_line(alpha=0.5) +
    geom_line(data = basque %>% filter(regionname=='Basque Country (Pais Vasco)'), color='red')  + 
    geom_vline(xintercept = 1970, linetype = 'dashed', color = 'purple') + 
    ggtitle('gdpcap over years (red = Basque)')
  
  if(save){
    ggsave(filename = file_path, width = 11, height = 7)
  } else{
    print(plot)
  }
}

make_dataprep <- function(data, treatment_region = 'Basque Country (Pais Vasco)'){
  ###
  # make dataprep  
  # input: 
  #   data(dataframe): data
  #   treatment_region(str): treatment region (default = 'Basque Country (Pais Vasco)')
  # output:
  #   dataprep_out(list): dataprep
  ###
  treatment.identifier <- 
    data %>% 
    filter(regionname == treatment_region) %>% 
    dplyr::select(regionno) %>% 
    .[1, 1]
  
  controls.identifier <- 
    c(2:18) %>% 
    .[. != treatment.identifier]
  
  dataprep_out <- dataprep(
    foo = data,
    predictors = c('school.illit', 
                   'school.prim', 
                   'school.med', 
                   'school.high', 
                   'school.post.high', 
                   'invest'),
    time.predictors.prior = 1964:1969,
    special.predictors = list(
      list('gdpcap', 1960:1969, 'mean'),
      list('sec.agriculture', seq(1961, 1969, 2), 'mean'), 
      list('sec.energy', seq(1961, 1969, 2), 'mean'),
      list('sec.industry', seq(1961, 1969, 2), 'mean'),
      list('sec.construction', seq(1961, 1969, 2), 'mean'),
      list('sec.services.venta', seq(1961, 1969, 2), 'mean'),
      list('sec.services.nonventa', seq(1961, 1969, 2), 'mean'),
      list('popdens', 1969, 'mean')
    ),
    dependent = 'gdpcap',
    unit.variable = 'regionno',
    unit.names.variable = 'regionname',
    time.variable = 'year',
    treatment.identifier = treatment.identifier,
    controls.identifier = controls.identifier,
    time.optimize.ssr = 1960:1969,
    time.plot = data$year %>% unique()
  )
  
  return(dataprep_out)
}

plot_results <- function(dataprep_out, synth_out, file_path = NULL, save = FALSE){
  ###
  # plot results (synthetic gdpcap vs actual gdpcap)
  # input: 
  #   dataprep_out(list): return of make_dataprep()
  #   synth_out(list): return of synth()
  #   file_path(str): file path for saving plot (default = NULL)
  #   save(bool): wheter to save or not (default = FALSE)
  ###
  plot <- 
    tibble(
    gdpcap = c(dataprep_out$Y0plot %*% synth_out$solution.w, dataprep_out$Y1plot),
    kind = c(rep('synthetic', length(1955:1997)), rep('actual', length(1955:1997))),
    year = rep(1955:1997, 2)
  ) %>% 
    ggplot() + 
    geom_line(aes(x = year, y = gdpcap, color = kind)) + 
    geom_vline(xintercept = 1970, linetype = 'dashed', color = 'purple') + 
    ggtitle('result of SCM (actual gdpcap vs synthetic gdpcap)')
  
  if(save){
    ggsave(filename = file_path, width = 11, height = 7)
  } else{
      print(plot)
    }
}

plot_gap <- function(dataprep_out, synth_out, file_path = NULL, save = FALSE){
  ###
  # plot results (gap between actual gdpcap and synthetic gdpcap)
  # input: 
  #   dataprep_out(list): return of make_dataprep()
  #   synth_out(list): return of synth()
  #   file_path(str): file path for saving plot (default = NULL)
  #   save(bool): wheter to save or not (default = FALSE)
  ###
  plot <- tibble(
    gap = c(dataprep_out$Y1plot - dataprep_out$Y0plot %*% synth_out$solution.w),
    year = rep(1955:1997)
  ) %>% 
    ggplot() +
    geom_line(aes(x = year, y = gap)) + 
    geom_hline(yintercept = 0, linetype = 'dashed') + 
    ylim(-1.5, 1.5) + 
    geom_vline(xintercept = 1970, linetype = 'dashed', color = 'purple') + 
    ggtitle('result of SCM (gap between actual gdpcap and synthetic gdpcap)')
  
  if(save){
    ggsave(filename = file_path, width = 11, height = 7)
  } else{
    print(plot)
  }
}

# TO DO: summary satatistics

##### excution ####
data('basque')
data <- basque

plot_timeseries(data = data, 
                red_region = 'Basque Country (Pais Vasco)', 
                file_path = './plot/plot_timeseries.png',
                save = TRUE)

dataprep_out <- 
  make_dataprep(data = data,
                treatment_region = 'Basque Country (Pais Vasco)')

synth_out <- synth(data.prep.obj = dataprep_out) # search for W

plot_results(dataprep_out = dataprep_out, synth_out = synth_out, file_path = './plot/plot_result.png', save = TRUE)
plot_gap(dataprep_out = dataprep_out, synth_out = synth_out, file_path = './plot/plott_gap.png', save = TRUE)


