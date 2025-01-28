glass_delta <- function(jmv_desc, invert_groups = FALSE){
  if (invert_groups){
    return((jmv_desc$mean2 - jmv_desc$mean1)/ jmv_desc$sd1)
  }
  return((jmv_desc$mean1 - jmv_desc$mean2)/ jmv_desc$sd2)
}

report_p <- function(pval, digits = 3){
  lower_bound <- c("0.",rep(0,digits-1)) %>% 
                     str_c(collapse = "") %>% 
    str_c(1)
  if(pval < as.numeric(lower_bound)){
    return(str_c("p < ", lower_bound))
  }else{
    return(str_c("p = ",
                 round(pval,digits) %>% 
                   str_sub(start = 2L)))
  }
}