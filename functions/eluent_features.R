library(dplyr)

get_organic_percentage <- function(eluent_parameters, ret_time) {
  if (ret_time <= 0){
    organic <- eluent_parameters$B[1]
    return(organic)
  }
  gradient_length <- length(eluent_parameters$time)
  if (ret_time >= eluent_parameters$time[gradient_length]) {
    organic <- eluent_parameters$B[gradient_length]
    return(organic)
  }
  if (length(eluent_parameters$B) == 1) {
    organic <- eluent_parameters$B
    return(organic)
  }
  for (i in 1:length(eluent_parameters$B)) {
    if (ret_time < eluent_parameters$time[i]) {
      if (eluent_parameters$B[i] == eluent_parameters$B[i - 1]) {
        organic <- as.numeric(eluent_parameters$B[i])
      } else {
        gradient_boundaries <- eluent_parameters[(i - 1):i, ]
        lin_fit <- lm(B ~ time, data = gradient_boundaries)
        slope <- lin_fit$coefficients[2]
        intercept <- lin_fit$coefficients[1]
        organic <- as.numeric(ret_time * slope + intercept)
      }
      return(organic)
    }
  }
}

get_viscosity <- function(organic,organic_modifier){
  viscosity <- case_when(
    organic_modifier == "MeCN" ~ (-0.000103849885417527)*organic^2+0.00435719229180079*organic+0.884232851261593,
    organic_modifier == "MeOH" ~ (-0.00035908)*organic^2+0.031972067*organic+0.9027394,
    organic_modifier == "acetone" ~ (-0.000312773564735429)*organic^2+0.0246804181291766*organic+0.901746394945477)
  return(viscosity)
}

get_surface_tension <- function(organic,organic_modifier){
  sigma_water <- 71.76
  sigma_organic <- case_when(
    organic_modifier == "MeCN" ~ 27.86,
    organic_modifier == "MeOH" ~ 22.12,
    organic_modifier == "acetone" ~ 22.2
  )
  D_parameter <- case_when(
    organic_modifier == "MeCN" ~ -2.906,
    organic_modifier == "MeOH" ~ -2.245,
    organic_modifier == "acetone" ~ -2.5
  )
  E_parameter <- case_when(
    organic_modifier == "MeCN" ~ 7.138,
    organic_modifier == "MeOH" ~ 5.625,
    organic_modifier == "acetone" ~ 6.84
  )
  surface_tension <- sigma_water+D_parameter*sigma_water*(organic/100)+(E_parameter*sigma_organic-D_parameter*sigma_water-sigma_water)*(organic/100)^2+(sigma_organic-E_parameter*sigma_organic)*(organic/100)^3
  return(surface_tension)
}

get_polarity_index <- function(organic,organic_modifier){
  polarity_index <- case_when(
    organic_modifier == "MeCN" ~ (organic/100)*5.8+((100-organic)/100)*10.2,
    organic_modifier %in% c("MeOH","acetone") ~ (organic/100)*5.1+((100-organic)/100)*10.2)
  return(polarity_index)
}