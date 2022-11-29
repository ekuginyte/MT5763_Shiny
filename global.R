### Functions

# Dynmic dataset formatter
get.df <- function(format, df_0, choices){
  df <- filter(df_0, choice %in% choices)
  
  if (is_null(df) == FALSE) {
    if (format == "w"){
      df <- pivot_wider(df, names_from = "choice", values_from = "numeric")
    }
    return(df)
  } else {
    return(df_0)
  }
}