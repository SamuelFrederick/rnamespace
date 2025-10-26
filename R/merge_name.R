#' matchName
#' 
#' @description Class for matching and deduplicating names, includes methods to fuzzy merge data on names as well.
#' 
#' @param classification_model string, default "SamFrederick/namematch1m". Denotes the model to use for classifying name pairs as matches
#' @param device string, default "cpu". Can be "cpu", "cuda", "mps"
#' @param tokenizer string, default "roberta-large". A tokenizer with which to tokenize names
#' @param filter filterName object. A filterName object to filter possible matches prior to matching
#' 
#' @importFrom reticulate import r_to_py
#' @importFrom tibble tibble
#' @importFrom R6 R6Class
#' 
#' @return filterName objects with methods for matching, merging, and deduplicating names
#' 
#' @export
#' 
#' @examples
#' matcher <- matchName$new(
#'      classification_model = 'SamFrederick/namematch1m', 
#'      device = 'cpu', 
#'      tokenizer = 'roberta-large'
#' )
#' matcher$predict_match('John Smith', 'Smith, Jonathan') 
matchName <- R6::R6Class("matchName", list(
  classification_model = 'SamFrederick/namematch1m', 
  device = 'cpu', 
  tokenizer = 'roberta-large',
  filter_match = NULL, 
  mn = NULL,
  initialize = function(classification_model = 'SamFrederick/namematch1m', device = 'cpu', tokenizer = 'roberta-large', filter = NULL){
    self$classification_model <- classification_model
    self$device <- device
    self$tokenizer <- tokenizer
    self$filter_match <- filter$fn
      
    self$mn <- ns$MatchName(
      classification_model = self$classification_model, 
      device = self$device, 
      tokenizer = self$tokenizer, 
      filter = filter$fn
    )
  }, 

  #' @description A method to predict whether names match one another
  #' @param name1 a string, the first name to be matched
  #' @param name2 a string, the second name to be matched
  #' @return the predicted probability that the names match
  #' @examples
  #' matcher <- matchName$new()
  #' matcher$predict_match("Jonathan Johnson", "John Johnson")
  predict_match = function(name1, name2) {
    self$mn$predict_match(name1, name2)
  }, 

  #' @description a method to retrieve the probabilities of matches between all pairs of names
  #' @param names1 a vector of strings. Containing the first set of names for prediction.
  #' @param names2 a vector of strings, optional. Containing the second set of names for prediction. If this is missing, function will generate all possible pairs with first set of names
  #' @param batch_size an integer, default 100L, the size of batches for prediction
  #' @return a tibble with columns name1, name2, and prob, where prob is the predicted probability of a match
  #' @examples
  #' matcher <- matchName$new()
  #' matcher$all_probs(
  #'    c("Alex Johnson", "John Johnson"),
  #'    c("John Johnson", "Johnson, Alexander", "Michael Frost")
  #' )
  all_probs = function(names1, names2 = NULL, batch_size = 100L) {
    batch_size <- as.integer(batch_size)
    if(all(is.null(names2))) names2 <- names1
    out <- self$mn$all_probs(names1, names2, batch_size = batch_size)
    return(tibble::as_tibble(out))
  }, 

  #' @description A method to join two datasets by fuzzy name merging
  #' 
  #' @param df1,df2 datasets to merge
  #' @param left_name_col a string, the column containing the name for fuzzy merging in df1
  #' @param right_name_col a string, optional, default NULL. The column containing the name for fuzzy merging in df2. If right_name_col is missing, defaults to left_name_col
  #' @param how a string, one of c('inner', 'outer', 'left', 'right')
  #' @param by a (named) vector containing the column names on which to exact merge the data. Column names in df1 correspond to names of vector if applicable. Follows dplyr's joining convention
  #' @param merge_threshold a double between 0 and 1, default 0.5.T he desired probability threshold to declare a match between two names.
  #' @param return_marginal a boolean, default TRUE. Whether to return a list containing the "marginal" name matches
  #' @param marginal a vector of doubles, default c(0.1, 0.9). The lower and upper bounds for "marginal" matches if return_marginal is TRUE
  #' @param batch_size an integer, default 100L, the size of batches for prediction
  #' @param crosswalk a boolean, default FALSE. Whether to return a crosswalk dataset (i.e., only return name columns and columns in by argument)
  #' 
  #' @rdname join_name
  #' 
  #' @return if return_marginal is FALSE, a tibble with the merged data and the probability of a match in the 'prob' column. If return_marginal is TRUE, a list with items 'merged' (containing the merged data and match probability) and 'marginal' (a dataset containing the "marginal" matches and their probability of matching)
  #' @examples
  #' matcher <- matchName$new()
  #' d1 <- tibble::tibble(
  #'      name1 = c("John Johnson", "Mark Alexander", "Cynthia Bastion")
  #' )
  #' d2 <- tibble::tibble(
  #'      name2 = c("Bastion, Cynthia J.", "Johnson, John Mark", "Alexander, Mark E.")
  #' )
  #' matcher$join_name(d1, d2, 'name1', 'name2', how = 'left')
  join_name = function(
    df1, 
    df2, 
    left_name_col, 
    right_name_col = NULL, 
    how,
    by = NULL, 
    merge_threshold = 0.5, 
    return_marginal = TRUE, 
    marginal = c(0.1, 0.9), 
    batch_size = 100L, 
    crosswalk = FALSE
  ) {
    if(all(is.null(marginal))) marginal <- c(0.1, 0.9)
    if(!all(marginal[1]>=0&marginal[2]<=1)) stop("marginal must be a vector containing two doubles between 0 and 1.")
    if(!merge_threshold>=0&merge_threshold<=1) stop("merge_threshold must be a double between 0 and 1.")

    batch_size <- as.integer(batch_size)
    if(!all(is.null(by))) {
      left_exact<- by
      if(!all(is.null(names(by)))) {
        right_exact <- names(by)
      } else {
        right_exact <- left_exact
      }
      right_exact[right_exact == ''] <- left_exact[right_exact == '']
      left_exact <- as.list(left_exact)
      right_exact <- as.list(right_exact)
    }else {
      left_exact <- right_exact <- NULL
    }
    df1 <- reticulate::r_to_py(df1)
    df2 <- reticulate::r_to_py(df2)
    out <- self$mn$merge_name(
      df1, 
      df2, 
      how = how, 
      left_name_col = left_name_col, 
      right_name_col = right_name_col, 
      left_exact = left_exact, 
      right_exact = right_exact, 
      merge_threshold = merge_threshold, 
      return_marginal = return_marginal, 
      marginal = marginal, 
      batch_size = batch_size, 
      crosswalk = crosswalk
    )
    if (return_marginal) {
      return(
        list(
          merged = out[[1]], 
          marginal = out[[2]]
        )
      )
    } 
    return(
      out
    )
  }, 


  #' @description A method to left join two datasets by fuzzy name merging
  #' 
  #' @param df1,df2 datasets to merge
  #' @param left_name_col a string, the column containing the name for fuzzy merging in df1
  #' @param right_name_col a string, optional, default NULL. The column containing the name for fuzzy merging in df2. If right_name_col is missing, defaults to left_name_col
  #' @param by a (named) vector containing the column names on which to exact merge the data. Column names in df1 correspond to names of vector if applicable. Follows dplyr's joining convention
  #' @param merge_threshold a double between 0 and 1, default 0.5.T he desired probability threshold to declare a match between two names.
  #' @param return_marginal a boolean, default TRUE. Whether to return a list containing the "marginal" name matches
  #' @param marginal a vector of doubles, default c(0.1, 0.9). The lower and upper bounds for "marginal" matches if return_marginal is TRUE
  #' @param batch_size an integer, default 100L, the size of batches for prediction
  #' @param crosswalk a boolean, default FALSE. Whether to return a crosswalk dataset (i.e., only return name columns and columns in by argument)
  #' 
  #' @return if return_marginal is FALSE, a tibble with the merged data and the probability of a match in the 'prob' column. If return_marginal is TRUE, a list with items 'merged' (containing the merged data and match probability) and 'marginal' (a dataset containing the "marginal" matches and their probability of matching)
  #' 
  #' @rdname join_name
  #' 
  #' @examples
  #' matcher <- matchName$new()
  #' d1 <- tibble::tibble(
  #'      name1 = c("John Johnson", "Mark Alexander", "Cynthia Bastion")
  #' )
  #' d2 <- tibble::tibble(
  #'      name2 = c("Bastion, Cynthia J.", "Johnson, John Mark", "Alexander, Mark E.")
  #' )
  #' matcher$left_join_name(d1, d2, 'name1', 'name2')
  left_join_name = function(
    df1, 
    df2, 
    left_name_col, 
    right_name_col, 
    by = NULL, 
    merge_threshold = 0.5, 
    return_marginal = TRUE, 
    marginal = c(0.1, 0.9), 
    batch_size = 100L, 
    crosswalk = FALSE
  ) {
    self$join_name(
      df1 = df1, df2 = df2, left_name_col = left_name_col, right_name_col = right_name_col, 
      how = 'left', by = by, merge_threshold = merge_threshold, return_marginal = return_marginal, 
      marginal = marginal, batch_size = batch_size, crosswalk = crosswalk
    )
  }, 

  #' @description A method to right join two datasets by fuzzy name merging
  #' 
  #' @param df1,df2 datasets to merge
  #' @param left_name_col a string, the column containing the name for fuzzy merging in df1
  #' @param right_name_col a string, optional, default NULL. The column containing the name for fuzzy merging in df2. If right_name_col is missing, defaults to left_name_col
  #' @param how a string, one of c('inner', 'outer', 'left', 'right')
  #' @param by a (named) vector containing the column names on which to exact merge the data. Column names in df1 correspond to names of vector if applicable. Follows dplyr's joining convention
  #' @param merge_threshold a double between 0 and 1, default 0.5.T he desired probability threshold to declare a match between two names.
  #' @param return_marginal a boolean, default TRUE. Whether to return a list containing the "marginal" name matches
  #' @param marginal a vector of doubles, default c(0.1, 0.9). The lower and upper bounds for "marginal" matches if return_marginal is TRUE
  #' @param batch_size an integer, default 100L, the size of batches for prediction
  #' @param crosswalk a boolean, default FALSE. Whether to return a crosswalk dataset (i.e., only return name columns and columns in by argument)
  #' @return if return_marginal is FALSE, a tibble with the merged data and the probability of a match in the 'prob' column. If return_marginal is TRUE, a list with items 'merged' (containing the merged data and match probability) and 'marginal' (a dataset containing the "marginal" matches and their probability of matching)
  #' 
  #' @rdname join_name
  #' 
  #' @examples
  #' matcher <- matchName$new()
  #' d1 <- tibble::tibble(
  #'      name1 = c("John Johnson", "Mark Alexander", "Cynthia Bastion")
  #' )
  #' d2 <- tibble::tibble(
  #'      name2 = c("Bastion, Cynthia J.", "Johnson, John Mark", "Alexander, Mark E.")
  #' )
  #' matcher$right_join_name(d1, d2, 'name1', 'name2')
  right_join_name = function(
    df1, 
    df2, 
    left_name_col, 
    right_name_col, 
    by = NULL, 
    merge_threshold = 0.5, 
    return_marginal = TRUE, 
    marginal = c(0.1, 0.9), 
    batch_size = 100L, 
    crosswalk = FALSE
  ) {
    self$join_name(
      df1 = df1, df2 = df2, left_name_col = left_name_col, right_name_col = right_name_col, 
      how = 'right', by = by, merge_threshold = merge_threshold, return_marginal = return_marginal, 
      marginal = marginal, batch_size = batch_size, crosswalk = crosswalk
    )
  }, 

  #' @description A method to full join two datasets by fuzzy name merging
  #' 
  #' @param df1,df2 datasets to merge
  #' @param left_name_col a string, the column containing the name for fuzzy merging in df1
  #' @param right_name_col a string, optional, default NULL. The column containing the name for fuzzy merging in df2. If right_name_col is missing, defaults to left_name_col
  #' @param by a (named) vector containing the column names on which to exact merge the data. Column names in df1 correspond to names of vector if applicable. Follows dplyr's joining convention
  #' @param merge_threshold a double between 0 and 1, default 0.5.T he desired probability threshold to declare a match between two names.
  #' @param return_marginal a boolean, default TRUE. Whether to return a list containing the "marginal" name matches
  #' @param marginal a vector of doubles, default c(0.1, 0.9). The lower and upper bounds for "marginal" matches if return_marginal is TRUE
  #' @param batch_size an integer, default 100L, the size of batches for prediction
  #' @param crosswalk a boolean, default FALSE. Whether to return a crosswalk dataset (i.e., only return name columns and columns in by argument)
  #' 
  #' @return if return_marginal is FALSE, a tibble with the merged data and the probability of a match in the 'prob' column. If return_marginal is TRUE, a list with items 'merged' (containing the merged data and match probability) and 'marginal' (a dataset containing the "marginal" matches and their probability of matching)
  #' 
  #' @rdname join_name
  #' 
  #' @examples
  #' matcher <- matchName$new()
  #' d1 <- tibble::tibble(
  #'      name1 = c("John Johnson", "Mark Alexander", "Cynthia Bastion")
  #' )
  #' d2 <- tibble::tibble(
  #'      name2 = c("Bastion, Cynthia J.", "Johnson, John Mark", "Alexander, Mark E.")
  #' )
  #' matcher$full_join_name(d1, d2, 'name1', 'name2')
  full_join_name = function(
    df1, 
    df2, 
    left_name_col, 
    right_name_col, 
    by = NULL, 
    merge_threshold = 0.5, 
    return_marginal = TRUE, 
    marginal = c(0.1, 0.9), 
    batch_size = 100L, 
    crosswalk = FALSE
  ) {
    self$join_name(
      df1 = df1, df2 = df2, left_name_col = left_name_col, right_name_col = right_name_col, 
      how = 'outer', by = by, merge_threshold = merge_threshold, return_marginal = return_marginal, 
      marginal = marginal, batch_size = batch_size, crosswalk = crosswalk
    )
  }, 

  #' @description A method to inner join two datasets by fuzzy name merging
  #' 
  #' @param df1,df2 datasets to merge
  #' @param left_name_col a string, the column containing the name for fuzzy merging in df1
  #' @param right_name_col a string, optional, default NULL. The column containing the name for fuzzy merging in df2. If right_name_col is missing, defaults to left_name_col
  #' @param by a (named) vector containing the column names on which to exact merge the data. Column names in df1 correspond to names of vector if applicable. Follows dplyr's joining convention
  #' @param merge_threshold a double between 0 and 1, default 0.5.T he desired probability threshold to declare a match between two names.
  #' @param return_marginal a boolean, default TRUE. Whether to return a list containing the "marginal" name matches
  #' @param marginal a vector of doubles, default c(0.1, 0.9). The lower and upper bounds for "marginal" matches if return_marginal is TRUE
  #' @param batch_size an integer, default 100L, the size of batches for prediction
  #' @param crosswalk a boolean, default FALSE. Whether to return a crosswalk dataset (i.e., only return name columns and columns in by argument)
  #' 
  #' @return if return_marginal is FALSE, a tibble with the merged data and the probability of a match in the 'prob' column. If return_marginal is TRUE, a list with items 'merged' (containing the merged data and match probability) and 'marginal' (a dataset containing the "marginal" matches and their probability of matching)
  #' 
  #' @rdname join_name
  #' 
  #' @examples
  #' matcher <- matchName$new()
  #' d1 <- tibble::tibble(
  #'      name1 = c("John Johnson", "Mark Alexander", "Cynthia Bastion")
  #' )
  #' d2 <- tibble::tibble(
  #'      name2 = c("Bastion, Cynthia J.", "Johnson, John Mark", "Alexander, Mark E.")
  #' )
  #' matcher$inner_join_name(d1, d2, 'name1', 'name2')
  inner_join_name = function(
    df1, 
    df2, 
    left_name_col, 
    right_name_col, 
    by = NULL, 
    merge_threshold = 0.5, 
    return_marginal = TRUE, 
    marginal = c(0.1, 0.9), 
    batch_size = 100L, 
    crosswalk = FALSE
  ) {
    self$join_name(
      df1 = df1, df2 = df2, left_name_col = left_name_col, right_name_col = right_name_col, 
      how = 'inner', by = by, merge_threshold = merge_threshold, return_marginal = return_marginal, 
      marginal = marginal, batch_size = batch_size, crosswalk = crosswalk
    )
  }, 

  #' @description A method to deduplicate names in a dataset
  #' 
  #' @param df a dataframe to be deduplicated
  #' @param name_col a string, containing the column with the name
  #' @param exact a string vector, optional, default NULL. Names of columns identifying groups within which to search for name duplicates
  #' @param merge_threshold a double between 0 and 1, default 0.5. The desired probability threshold to declare a match between two names.
  #' @param return_marginal a boolean, default TRUE. Whether to return a list containing the "marginal" name matches
  #' @param marginal a vector of doubles, default c(0.1, 0.9). The lower and upper bounds for "marginal" matches if return_marginal is TRUE
  #' @param batch_size an integer, default 100L, the size of batches for prediction
  #' 
  #' @return if return_marginal is FALSE, a tibble with the merged data and the probability of a match in the 'prob' column. If return_marginal is TRUE, a list with items 'merged' (containing the merged data and match probability) and 'marginal' (a dataset containing the "marginal" matches and their probability of matching)
  #' 
  #' @examples
  #' matcher <- matchName$new()
  #' d1 <- tibble::tibble(
  #'          name1 = c("John Johnson", "Mark Alexander", "Cynthia Bastion", "Johnny Johnson")
  #' )
  #' matcher$dedupe_name(d1, 'name1')
  dedupe_name = function(
    df, 
    name_col, 
    exact = NULL, 
    merge_threshold = 0.5, 
    return_marginal = TRUE, 
    marginal = c(0.1, 0.9), 
    batch_size = 100L
  ) {
    batch_size <- as.integer(batch_size)
    df <- reticulate::r_to_py(df)
    out <- self$mn$dedupe_name(
      df = df, 
      name_col = name_col, 
      exact = as.list(exact), 
      merge_threshold = merge_threshold, 
      return_marginal = return_marginal, 
      batch_size = batch_size
    )
    if(return_marginal) {
      return(
        list(
          deduped = out[[1]], 
          marginal = out[[2]]
        )
      )
    } 
    return(out)

  }
)
)

ns <- NULL
.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
  reticulate::py_require(
    packages = c('git+https://github.com/SamuelFrederick/namespace.git')
  )
  ns <<- reticulate::import('namespace', delay_load = TRUE)
}