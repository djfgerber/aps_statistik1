
absichern_pearson <- function(r,n){
  return(paste0("$t(",
                n-2,
                ") = ",
                round(r * sqrt((n-2)/(1-r^2)),2),
                "$"))
}
absichern_partiell <- function(r,n){
  return(paste0("$t(",
                n-3,
                ") = ",
                round(r * sqrt((n-3)/(1-r^2)),2),
                "$"))
}
