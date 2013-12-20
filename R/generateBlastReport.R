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
#' @param n.threads
#' @return A handler function that can be passed to \code{processChunks}.
#' @keywords internal
blastReportDB.generator <- function(
  db = "16SMicrobial",
  max_hits = 25,
  evalue = 1e-6,
  perc_identity = 90,
  n.threads = floor(detectCores()*0.25)
) {
  function(..., chunkid) {
    fq_reads <- list(...)[[1]]
    xml.out <- tempfile(fileext=".xml")
    on.exit(unlink(xml.out))
    reads <- setNames(sread(fq_reads), ShortRead::id(fq_reads))
    blastn(reads, db=db, max_hits=max_hits, evalue=evalue, show_gis=TRUE,
           outfmt="xml", perc_identity=perc_identity, num_threads=n.threads, out=xml.out)
    blast2sqlite(xml.out, db.out, max_hit=max_hits, max_hsp=-1)
  }
}

#' @rdname taxonomyReportDB-class
#' @export
generate.BlastReport <- function(fastq,
                                 chunksize = 1000,
                                 db = "16SMicrobial",
                                 max_hits = 25,
                                 evalue = 1e-6,
                                 perc_identity = 90,
                                 n.threads = NULL) {
  if (is.null(n.threads)) {
    n.threads <- floor(detectCores()*0.25)
  }
  max.threads <- detectCores()
  nb.parallel.jobs <- floor(max.threads/n.threads)
  
  ## initialise fastqStream
  streamer <- fastqStream.generator(fastq, chunksize)
  on.exit(close(streamer))
  db.out <- normalizePath(file.path(dirname(fastq), paste0(strip_ext(fastq), "_blast.db"), mustWork=FALSE))
  
  ## initialise blast handler
  blast.handler <- blastReportDB.generator(db=db, max_hits=max_hits, evalue=evalue,
                                           perc_identity=perc_identity, n.threads=n.threads)
  processChunks(streamer, blast.handler, nb.parallel.jobs)
  blastReportDBConnect(db.out)
}
