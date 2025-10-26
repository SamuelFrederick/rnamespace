#' filterName
#' 
#' @description Class for filtering names to closest matches
#' 
#' @param embedding_model string, default "SamFrederick/namematch1m". Embedding model to use for embedding names
#' @param return_sim boolean, default FALSE Whether to return cosine similarities between name pairs
#' @param device string, default "cpu". Can be "cpu", "cuda", "mps"
#' @param tokenizer string, default "roberta-large". A tokenizer with which to tokenize names for the embedding_model
#' @param k integer, optional, default NULL. The number of nearest neighbors to filter to. Must supply either k or threshold. 
#' @param threshold double, optional, default NULL. Threshold of cosine similarity between names to filter to. Must supply either k or threshold.
#' 
#' @importFrom reticulate import
#' @importFrom tibble as_tibble
#' 
#' @return filterName object with method for filtering all pairs of names to closest matches
#' 
#' @export
#' 
#' @examples
#' filterer <- filterName$new(embedding_model = 'SamFrederick/namespace1m', k = 3)
#' filterer$filter_names(c('John Smith', 'Smith, Jonathan'), c('Smith, Johnny', 'Alex Marcus'))
filterName <- R6::R6Class("filterName", list(
  embedding_model = 'SamFrederick/namespace1m', 
  return_sim = FALSE,
  device = 'cpu', 
  tokenizer = 'roberta-large',
  k = NULL, 
  threshold = NULL, 
  fn = NULL,
  initialize = function(
    embedding_model = 'SamFrederick/namespace1m', 
    return_sim = FALSE, 
    device = 'cpu', 
    tokenizer = 'roberta-large', 
    k = NULL, 
    threshold = NULL
  ){
    self$return_sim <- return_sim
    self$embedding_model <- embedding_model
    self$device <- device
    if(!is.null(k)) {
      self$k <- as.integer(k)
    }else {
      self$k <- k
    }
    self$threshold <- threshold
    self$tokenizer <- tokenizer
    self$fn <- ns$FilterName(
      embedding_model = self$embedding_model, 
      return_sim = self$return_sim,
      tokenizer = self$tokenizer,
      device = self$device, 
      k = self$k, 
      threshold = self$threshold
    )
  }, 
#' @description A method to filter all pairs of names between two vectors by cosine similarity
#' @param names1,names2 string vectors containing the names for which to calculate the cosine similarity. Constructs all possible pairs of names1 and names2 and filters based on cosine similarity
#' 
#' @return A tibble with columns name1, name2, and sim (if return_sim = TRUE)
#' 
#' @export
#' 
#' @examples
#' filterer <- filterName$new(embedding_model = 'SamFrederick/namespace1m', k = 3)
#' filterer$filter_names(c('John Smith', 'Smith, Jonathan'), c('Smith, Johnny', 'Alex Marcus'))
  filter_names = function(names1, names2) {
    out <- self$fn$filter_names(names1, names2)
    cnames <- c('name1', 'name2')
    if(self$return_sim) cnames <- c(cnames, 'sim')
    do.call(rbind, lapply(out, function(x) x|> setNames(cnames)|> tibble::as_tibble()))
  }
))