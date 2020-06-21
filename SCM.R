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

plot_timeseries <- 
  basque %>% 
  filter(regionname != "Spain (Espana)") %>% # omit Spain (Espana)
  ggplot(aes(x = year, y = gdpcap, group = regionname)) +
  geom_line(alpha=0.5) +
  geom_line(data = basque %>% filter(regionname=='Basque Country (Pais Vasco)'), color='red')
ggsave(filename = './plot/plot_timeseries.png', width = 11, height = 7)

# TO DO summary 

##### excution ####
library_import('tidyverse')
library_import('Synth')
data('basque')

# make data frame for synth
dataprep_out <- dataprep(
  foo = basque,
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
  treatment.identifier = 17,
  controls.identifier = c(2:16, 18),
  time.optimize.ssr = 1960:1969,
  time.plot = basque$year %>% unique()
)

# search for W
synth_out <- synth(data.prep.obj = dataprep_out)

tibble(
  gdpcap = c(dataprep_out$Y0plot %*% synth_out$solution.w, dataprep_out$Y1plot),
  kind = c(rep('synthetic', length(1955:1997)), rep('actual', length(1955:1997))),
  year = rep(1955:1997, 2)
) %>% 
  ggplot() + 
  geom_line(aes(x = year, y = gdpcap, color = kind))

tibble(
  gap = c(dataprep_out$Y1plot - dataprep_out$Y0plot %*% synth_out$solution.w),
  year = rep(1955:1997)
) %>% 
  ggplot() +
  geom_line(aes(x = year, y = gap)) + 
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  ylim(-1.5, 1.5)

# synth_table <- synth.tab(dataprep.res = dataprep_out, synth.res = synth_out)





