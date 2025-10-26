#' embedName
#' 
#' @description Class for generating name embeddings
#' 
#' @param embedding_model string, default "SamFrederick/namematch1m". Embedding model to use for embedding names
#' @param tokenizer string, default "roberta-large". A tokenizer with which to tokenize names for the embedding_model
#' @param device string, default "cpu". Can be "cpu", "cuda", "mps"
#' 
#' @importFrom reticulate import
#' @importFrom tibble as_tibble
#' 
#' @return embedName object with methods for embedding names and calculating cosine similarity between names
#' 
#' @export
#' 
#' @examples
#' embedder <- embedName$new(embedding_model = 'SamFrederick/namespace1m',)
#' embedder$embed_name('John Smith')
embedName <- R6::R6Class("embedName", list(
  embedding_model = 'SamFrederick/namespace1m', 
  device = 'cpu', 
  tokenizer = 'roberta-large',
  en = NULL,
  initialize = function(
    embedding_model = 'SamFrederick/namespace1m', 
    device = 'cpu', 
    tokenizer = 'roberta-large'
  ){
    self$embedding_model <- embedding_model
    self$device <- device
    self$en <- ns$EmbedName(
      embedding_model = self$embedding_model, 
      tokenizer = self$tokenizer,
      device = self$device
    )
  }, 
#' @description A method to generate embeddings for name(s)
#' 
#' @param name a string or list of strings containing names for which to generate embeddings
#' 
#' @return a torch tensor containing the name embeddings
#' 
#' @export
#' 
#' @examples
#' embedder <- embedName$new(embedding_model = 'SamFrederick/namespace1m',)
#' embedder$embed_name('John Smith')
  embed_name = function(name) {
    self$en$embed_name(name)
  }, 
#' @description A method to calculate the cosine similarity between names
#' @param name1,name2 a string or list of strings containing names for which to calculate cosine similarity
#' 
#' @return a torch tensor containing the cosine similarities between names
#' 
#' @export
#' 
#' @examples
#' embedder <- embedName$new(embedding_model = 'SamFrederick/namespace1m',)
#' embedder$cosine_similarity('John Smith', 'Alex Johnson')
  cosine_similarity = function(name1, name2) {
    self$en$cosine_similarity(name1, name2)
  }
))
