#' @include utils.R
#' @importFrom HTSeqGenie sclapply
#' @importFrom ShortRead FastqStreamer yield
NULL

.blastReportStreamer <- setRefClass(
  'blastReportStreamer',
  fields=list(
    .con = "blastReportDB",
    cs = "integer",
    q = "numeric",
    nq = "numeric",
    i = "numeric"
  ),
  methods=list(
    initialize=function(db, chunksize) {
      .con <<- db
      cs <<- as.integer(chunksize)
      q <<- as.numeric(db_query(.con, "select query_id from query", 1))
      nq <<- length(q)
      i <<- 1
    },
    yield=function(..., log = NULL) {
      lower_idx <- cs*(i - 1) + 1
      upper_idx <- cs*i
      upper_idx <- ifelse(upper_idx > nq, nq, upper_idx)
      if (lower_idx <= nq) {
        blastr:::do_log(log, "Yield chunk ", i, ":\n")
        db_path <- ":memory:"
        db <- blastReportDB(db_path=db_path, verbose=FALSE)
        WHERE <- paste("where query_id >=", q[lower_idx], "AND query_id <=", q[upper_idx])
        db_bulk_insert(db, "query", db_query(.con, paste("select query_id, query_def, query_len from query", WHERE), log=log), log=log)
        db_bulk_insert(db, "hit", db_query(.con, paste("select * from hit", WHERE), log=log), log=log)
        db_bulk_insert(db, "hsp", db_query(.con, paste("select * from hsp", WHERE), log=log), log=log)
        i <<- i + 1
        db
      } else {
        NULL
      }
    },
    show = function() {
      cat("class:", class(.self), "\n")
      showme <- sprintf("Number of queries: %s | Size of chunks: %s",
                        db_count(.con, "query"), cs)
      cat(showme, "\n")
    }
  )
)


#' Streaming records from a \code{\linkS4class{blastReportDB}}.
#' 
#' @usage blastReportStreamer(con, n = 100)
#' @param con A \code{blastReportDB} connection.
#' @param n Number of queries to stream
#' @export
blastReportStreamer <- function(con, n = 100) {
  .blastReportStreamer(db = con, chunksize = n)
}


setMethod("yield", "blastReportStreamer", function(x, ...) {
  x$yield(...)
})


blastReportStream.generator <- function(blastdb, chunksize = 100, log = NULL, verbose = TRUE) {
  streamer <- blastReportStreamer(blastdb, chunksize)
  MIN <- function(a, b) if (a <= b) a else b  
  n <- db_count(blastdb, "query")
  i <- 1
  function() {
    res <- yield(streamer, log = log)
    if (length(res) > 0) {
      if (verbose)
        message(paste0("Processing ", MIN(i*chunksize, n), "/", n, " queries:"))
      i <<- i + 1
      res
    } else NULL
  }
}


fastqStream.generator <- function(fastq, chunksize=100, ...) {
  streamer <- FastqStreamer(con=fastq, n=chunksize, ...)
  function() {
    res <- yield(streamer)
    if (length(res) > 0) res else NULL
  }
}


processChunks <- function(inext, fun, nb.parallel.jobs) {
  .fun <- function(..., chunkid) {
    fun(..., chunkid = chunkid)
  }
  slapply <- function(inext, fun) {
    inextdata <- NULL
    i <- 1
    repeat {
      if (is.null(inextdata)) {
        inextdata <- inext()
        chunkid <- i
        i <- i + 1
      }
      if (is.null(inextdata)) {
        break
      }
      fun(inextdata, chunkid = chunkid)
      inextdata <- NULL
    }
  }
  if (nb.parallel.jobs == 1) {
    slapply(inext, .fun)
  } else {
    stopifnot(require(parallel))
    HTSeqGenie::sclapply(inext, .fun, max.parallel.jobs=nb.parallel.jobs)
  }
}


