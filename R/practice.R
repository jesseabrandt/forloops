#' Column Means
#'
#' @param df is a dataframe
#'
#' @returns a named vector of the means of each column of df
#' @export
#'
#' @examples
#' col_means(mtcars)
col_means <- function(df){
  means <- numeric(0)
  for(c in 1:ncol(df)){
    means[c] = mean(df[[c]])
  }
  names(means) <- names(df)
  return(means)
}
#' Count NAs
#'
#' @param vec is a vector
#'
#' @returns the number of NAs in vec
#' @export
#'
#' @examples
#' count_na(c(1,1,NA,2,NA))
count_na <- function(vec){
  count = 0
  for(v in vec){
    if(is.na(v)){
      count = count + 1
    }
  }
  return(count)
}