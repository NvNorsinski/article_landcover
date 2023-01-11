
library(mountSTAT)


pfad_dr <- mountWinShare(
  server = "xwwp0070",
  share = "eo",
  folder = "Projekt"
)



library(sf)
library(terra)


setwd(paste0(pfad_dr,"/2021_STATeo/01_Data/"))
library(sf)

library(mlr3)

library(mlr3learners)
library(mlr3tuning)

library(data.table)
library(dplyr)
library(magrittr)
library(parallelly)
library(tidyr)


future::plan("multisession", workers = 14)


tools::psnice(value = 9)



st_drop_geometry <- function(x) {
  if(inherits(x,"sf")) {
    x <- st_set_geometry(x, NULL)
    class(x) <- 'data.frame'
  }
  return(x)
}

path_data0 = "02_labelled_prepared/Punkte_stat2austria/Eckartsau_data.gpkg"
path_data1 = "02_labelled_prepared/Punkte_stat2austria/Friesach_data.gpkg"
path_data2 = "02_labelled_prepared/Punkte_stat2austria/gletscher_data.gpkg"
path_data3 = "02_labelled_prepared/Punkte_stat2austria/Weitra_data.gpkg"
path_data4 = "02_labelled_prepared/Punkte_stat2austria/Villach_data.gpkg"
path_data5 = "02_labelled_prepared/Punkte_stat2austria/Tobaj_data.gpkg"
path_data6 = "02_labelled_prepared/Punkte_stat2austria/Ternitz_data.gpkg"
path_data7 = "02_labelled_prepared/Punkte_stat2austria/Mieming_data.gpkg"
path_data8 = "02_labelled_prepared/Punkte_stat2austria/Mattsee_data.gpkg"


data0 = st_read(path_data0)
data1 = st_read(path_data1)

data2 = st_read(path_data2)

data3 = st_read(path_data3)

data4 = st_read(path_data4)

data5 = st_read(path_data5)

data6 = st_read(path_data6)

data7 = st_read(path_data7)

data8 = st_read(path_data8)


data = bind_rows(data0, data1, data2, data3, data4, data5, data6, data7, data8)



data$Response<-replace(data$Response, data$Response == "bare rock and screes" , "Bare rock and screes")


unique(data$Response)
crs = st_crs(data)


data2 <- data %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1],
                y = sf::st_coordinates(.)[,2])


data2 = st_drop_geometry(data2)


data2$Response <- as.factor(data2$Response)


set.seed(42)

summary(data2)

sapply(data2, levels)

unique(data2$Response)

data2 = data2[,-2]

datax = data2[, c(-218, -219)]
#-------------------------------------------------------------------------------



task = mlr3::TaskClassif$new(id = "gemeinden",
                         backend = datax, target = "Response")


task$col_roles
#stratifizieren
task$col_roles$stratum = task$col_roles$target
task$strata

learner = mlr3::lrn("classif.rpart", predict_type = "prob")
learner$param_set

learner$param_set$ids()

search_space = ps(
  cp = p_dbl(lower = 0.001, upper = 0.7),
  minsplit = p_int(lower = 2, upper = 70)
)


search_space

rbindlist(generate_design_grid(search_space, 3)$transpose())


res_inner = mlr3::rsmp("repeated_cv", folds = 5, repeats = 2)


res_outer = mlr3::rsmp("repeated_cv", folds = 3, repeats =3)


measure = mlr3::msr("classif.acc")
measure



terminator = mlr3tuning::trm("evals", n_evals = 400)
terminator

tuner = mlr3tuning::tnr("grid_search", resolution = 10, batch_size = 50)

at = AutoTuner$new(
  learner = learner,
  resampling = res_inner,
  measure = measure,
  search_space = search_space,
  terminator = terminator,
  tuner = tuner
)
at


rr = mlr3::resample(task, at, res_outer, store_models = TRUE)

inner_result = extract_inner_tuning_results(rr)
inner_result

rr$score(measure)
rr$aggregate(measure)

txt = knitr::kable(inner_result, digits = 6, align = "c", "simple")

fileConn<-file("07_Predict_results/s1_S2_spatial_exp_al.txt")
writeLines(txt, fileConn)
