#' @include blastReportStreamer.R
NULL

blast2sqlite <- function(xml.out, db.out, max_hit = 20, max_hsp = 20, reset_at=NULL) {
  if (is.null(reset_at) && max_hit < 0) {
    stop("Provide 'reset_at' or 'max_hit' to a positive number")
  }
  if (is.null(reset_at)) {
    reset_at <- floor(8000/(2*max_hit+1))
  }
  append <- file.exists(db.out)
  args <- list(append = append, out = db.out, max_hit = max_hit,
               max_hsp = max_hsp, reset_at = reset_at)
  SysCall("bigBlastParser", args=args, stdin=xml.out, redirection=FALSE,
          style="gnu", show_cmd=FALSE, intern=FALSE)
  return(db.out)
}

#' Generate a blast reporter
#'
#' @usage blastReportDB.generator(db = "16SMicrobial", max_hits = 25, evalue = 1e-6,
#' perc_identity = 90, n.threads = floor(detectCores()*0.25))
#' @param db
#' @param max_hits
#' @param evalue
#' @param perc_identity
#' @param num_threads Number of threads to use in the BLAST search
#' @return A handler function that can be passed to \code{processChunks}.
#' @importFrom ShortRead sread id
#' @keywords internal
blastReportDB.generator <- function(
  db.out,
  db = "16SMicrobial",
  max_hits = 25,
  evalue = 1e-6,
  perc_identity = 90,
  num_threads = 1
) {
  if (missing(db.out)) {
    stop("Path to output DB is missing.")
  }
  function(..., chunkid) {
    fq_reads <- list(...)[[1]]
    xml.out <- tempfile(fileext=".xml")
    on.exit(unlink(xml.out))
    reads <- setNames(ShortRead::sread(fq_reads), ShortRead::id(fq_reads))
    blastn(reads, db=db, max_hits=max_hits, evalue=evalue, show_gis=TRUE,
           outfmt="xml", perc_identity=perc_identity, num_threads=num_threads, out=xml.out)
    blast2sqlite(xml.out, db.out, max_hit=max_hits, max_hsp=-1)
  }
}

#' @importFrom parallel detectCores
#' @importFrom ShortRead yield
#' @importFrom Rsamtools path
#' @rdname taxonomyReportDB-class
#' @export
generate.BlastReport <- function(fastq,
                                 chunksize = 1000,
                                 db = "16SMicrobial",
                                 max_hits = 25,
                                 evalue = 1e-6,
                                 perc_identity = 90,
                                 num_blast_threads = NULL,
                                 num_parallel_jobs = NULL) {
  max_threads <- detectCores()
  if (max_threads > 1) {
    if (is.null(num_parallel_jobs)) {
      num_parallel_jobs <- floor(detectCores()*0.25)
    }
    if (is.null(num_blast_threads)) {
      num_blast_threads <- max_threads %/% num_parallel_jobs
    }
  } else {
    num_parallel_jobs <- num_blast_threads <- 1
  }
  assert_that(num_blast_threads <= max_threads %/% num_parallel_jobs)
  
  ## initialise fastqStream
  streamer <- fastqStream.generator(fastq, chunksize)
  on.exit(close(get("streamer", environment(streamer))))
  fastq_path <- path(get("streamer", environment(streamer)))
  db.out <- normalizePath(file.path(dirname(fastq_path), replace_ext(basename(fastq_path), '.blastdb')), mustWork=FALSE)
  
  ## initialise blast handler
  blast_handler <- blastReportDB.generator(db.out=db.out, db=db, max_hits=max_hits, evalue=evalue,
                                           perc_identity=perc_identity, num_threads=num_blast_threads)
  processChunks(streamer, blast_handler, num_parallel_jobs)
  blastReportDBConnect(db.out)
}
