##' Train a model by word2vec.
##'
##' The word2vec tool takes a text corpus as input and produces the
##' word vectors as output. It first constructs a vocabulary from the
##' training text data and then learns vector representation of words.
##' The resulting word vector file can be used as features in many
##' natural language processing and machine learning applications.
##'
##'
##'
##' @title Train a model by leftword2vec.
##' @param train_file Path of a single .txt file for training. Tokens are split on spaces.
##' @param output_file_in Path of the output file for input vector representations.
##' @param output_file_out Path of the output file for the output vector representations.
##' @param vectors The number of vectors to output. Defaults to 100.
##' More vectors usually means more precision, but also more random error, higher memory usage, and slower operations.
##' Sensible choices are probably in the range 100-500.
##' @param threads Number of threads to run training process on.
##' Defaults to 1; up to the number of (virtual) cores on your machine may speed things up.
##' @param window The size of the window (in words) to use in training.
##' @param classes Number of classes for k-means clustering. Not documented/tested.
##' @param cbow If 1, use a continuous-bag-of-words model instead of skip-grams.
##' Defaults to false (recommended for newcomers).
##' @param min_count Minimum times a word must appear to be included in the samples.
##' High values help reduce model size.
##' @param iter Number of passes to make over the corpus in training.
##' @param force Whether to overwrite existing model files.
##' @param negative_samples Number of negative samples to take in skip-gram training. 0 means full sampling, while lower numbers
##' give faster training. For large corpora 2-5 may work; for smaller corpora, 5-15 is reasonable.
##' @return A VectorSpaceModel object.
##' @author Jian Li <\email{rweibo@@sina.com}>, Ben Schmidt <\email{bmchmidt@@gmail.com}>
##' @references \url{https://code.google.com/p/word2vec/}
##' @export
##'
##' @import wordVectors
##' @useDynLib uniword2vec
##'
##' @examples \dontrun{
##' model = train_word2vec(system.file("examples", "rfaq.txt", package = "wordVectors"))
##' }
train_word2vec <- function(train_file, output_file_in = "vectors_in.bin",
                           output_file_out = "vectors_out.bin",
                           vectors=100,threads=1,window=12,
                           classes=0,cbow=0,min_count=5,iter=5,force=F, negative_samples=5)
{
  if (!file.exists(train_file)) stop("Can't find the training file!")
  if (file.exists(output_file_in) && !force) stop("The output file '",
                                     output_file_in ,
                                     "' already exists: give a new destination or run with 'force=TRUE'.")
  if (file.exists(output_file_out) && !force) stop("The output file '",
                                                  output_file_out ,
                                                  "' already exists: give a new destination or run with 'force=TRUE'.")

  train_dir <- dirname(train_file)

  # cat HDA15/data/Dickens/* | perl -pe 'print "1\t"' | egrep "[a-z]" | bookworm tokenize token_stream > ~/test.txt

  if(missing(output_file_in)) {
    output_file_in <- gsub(gsub("^.*\\.", "", basename(train_file)), "bin", basename(train_file))
    output_file_in <- file.path(train_dir, output_file_in)
  }
  if(missing(output_file_out)) {
    output_file_out <- gsub(gsub("^.*\\.", "", basename(train_file)), "bin", basename(train_file))
    output_file_out <- file.path(train_dir, output_file_out)
  }
  outfile_in_dir <- dirname(output_file_in)
  if (!file.exists(outfile_in_dir)) dir.create(outfile_in_dir, recursive = TRUE)
  outfile_out_dir <- dirname(output_file_out)
  if (!file.exists(outfile_out_dir)) dir.create(outfile_out_dir, recursive = TRUE)

  train_file <- normalizePath(train_file, winslash = "/", mustWork = FALSE)
  output_file_in <- normalizePath(output_file_in, winslash = "/", mustWork = FALSE)
  output_file_out <- normalizePath(output_file_out, winslash = "/", mustWork = FALSE)
  # Whether to output binary, default is 1 means binary.
  binary = 1

  OUT <- .C("CWrapper_word2vec",
            train_file = as.character(train_file),
            output_file_in = as.character(output_file_in),
            output_file_out = as.character(output_file_out),
            binary = as.character(binary),
            dims=as.character(vectors),
            threads=as.character(threads),
            window=as.character(window),
            classes=as.character(classes),
            cbow=as.character(cbow),
            min_count=as.character(min_count),
            iter=as.character(iter),
            neg_samples=as.character(negative_samples)
  )

  ll <- list();
  ll[["input"]] <-  wordVectors::read.vectors(output_file_in)
  ll[["output"]] <- wordVectors::read.vectors(output_file_out)
  return(ll);
}
