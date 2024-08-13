

convert_to_year_end <- function(year_input){
  
  year_input <- as.character(year_input)
  
  if(str_detect(year_input, "^\\d{4}$")){
    year <- substr(year_input, 3,4)
    year_ending <- paste0("20", year)
  } 
  else if (str_detect(year_input, "^\\d{4}-\\d{2}$")){
    year <- substr(year_input, 6,7)
    year_ending <- paste0("20", year)
  }
  else{
    stop("invalid year format")
  }
  
  year_ending <- dmy(paste0("30-Jun-", year_ending))
  return(year_ending)
  
}