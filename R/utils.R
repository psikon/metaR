#' @include all-generics.R
#' @importFrom plyr arrange desc
#' @importFrom assertthat "on_failure<-" is.string
#' @useDynLib metaR
NULL

is.empty <- function(x) {
  is.null(x) || length(x) == 0L || (length(x) == 1L && !nzchar(x))
}
on_failure(is.empty) <- function(call, env) {
  paste0(deparse(call$x), " is not empty.")
}

"%||%" <- function(a, b) {
  if (is.empty(a)) force(b) else a
}

"%|na|%" <- function(a, b) {
  if (is.null(a) || all(is.na(a))) force(b) else a
}

## Vectorized default operators
"%|%" <- function(a, b) ifelse(nzchar(a), a, b)

"%|NA|%" <- function(a, b) ifelse(is.na(a), b, a)

"%ni%" <- Negate(`%in%`)

compact <- function(x) {
  x[!vapply(x, is.null, FALSE, USE.NAMES=FALSE)]
}

compactChar <- function(x) {
  x[vapply(x, nzchar, FALSE, USE.NAMES=FALSE)]
}

compactEmpty <- function(x) {
  x[!vapply(x, function(x) length(x)==0, FALSE, USE.NAMES=FALSE)]
}

trim <- function(x, trim = '\\s+') {
  assert_that(is.vector(x))
  gsub(paste0("^", trim, "|", trim, "$"), '', x)
}

Call <- function(fn, ...) {
  fn <- match.fun(fn)
  fn(...)
}

#' @keywords internal
Compose <- function(...) {
  fns <- lapply(compact(list(...)), match.fun)
  len <- length(fns)
  function(...) {
    res <- Call(fns[[len]], ...)
    for (fn in rev(fns[-len]))
      res <- fn(res)
    res
  }
}

#' @keywords internal
nunique <- function(x, ...) {
  if (is.factor(x)) {
    length(levels(x))
  } else {
    length(unique(x, ...))
  }
}

#' @keywords internal
bindList <- function(L) {
  n_col <- length(L[[1L]])
  col_classes <- vapply(L[[1L]], class, "", USE.NAMES=FALSE)
  res <- bind_list(L, n_col, col_classes)
  attr(res, "row.names")  <- seq_len(length(res[[1L]]))
  res
}

#' @keywords internal
strip_ext <- function (file, sep="\\.", level=0) {
  assert_that(!missing(file), is.character(file))
  if (level == 0L) {
    # level 0 ditches everything that comes after a dot
    vapply(file, function(x) usplit(x, sep)[1L], "", USE.NAMES = FALSE)
  } else if (level > 0L) {
    # level 1 removes the very last extension: file.xyz.abc > file.xyz
    # level 2: file.xyz.abc > file
    # and so on
    count <- count_re(file, sep) + 1L - level
    # to always grab at least the first element after the split
    # reset zero counts to 1
    count <- ifelse(count < 1, 1, count)
    unlist(Map(function(x, lvl) {
      paste0(usplit(x, sep)[seq_len(lvl)], collapse=gsub('\\', '', sep, fixed=TRUE))
    }, x=file, lvl=count, USE.NAMES=FALSE))
  } else {
    stop(sprintf("Level %s is invalid. Must be 0, 1, 2, ...", sQuote(level)))
  }
}

#' @keywords internal
replace_ext <- function(file, replacement="", sep="\\.", level=0) {
  if (nchar(replacement) == 0L)
    sep=""
  # strip a leading "." from replacement
  if (grepl("^\\.", replacement)) {
    replacement <- usplit(replacement, split="^\\.")[2L]
  }
  paste(strip_ext(file=file, sep=sep, level=level), replacement,
        sep=gsub("\\", "", sep, fixed=TRUE))  
}

#' @keywords internal
count_re <- function(x, re, ...) {
  vapply(gregexpr(re, x, ...), function(x) sum(x > 0L), 0, USE.NAMES=FALSE)
}

usplit <- Compose("unlist", "strsplit")


is.fastq <- function(x) {
  fq <- readLines(x, n=4)
  if (!grepl("^@", fq[1]) || !grepl("^\\+", fq[3])) FALSE else TRUE
}
on_failure(is.fastq) <- function(call, env) {
  paste0(deparse(call$x), " is not a FASTQ file.")
}

merge_list <- function(x, y) {
  if (length(x) == 0) return(y)
  if (length(y) == 0) return(x) 
  i <- is.na(match(names(y), names(x)))
  if (any(i)) {
    x[names(y)[which(i)]] <- y[which(i)]
  }
  x
}

are_null <- function (x) {
  vapply(x, is.null, FALSE, USE.NAMES=FALSE)
}

are_true <- function (x) {
  vapply(x, isTRUE, FALSE, USE.NAMES=FALSE)
}

are_false <- function(x) {
  vapply(x, function(x) identical(x, FALSE), FALSE, USE.NAMES=FALSE)
}

#' Test if an external executable is available
#' 
#' Uses \code{\link{Sys.which}} internally, so it should work
#' on Windows and Unix.alikes.
#' 
#' @param cmd The exececutable to test for.
#' @param msg Additional message if the test fails.
#' @keywords internal
has_command <- function(cmd, msg = "") {
  assert_that(is.string(cmd))
  unname(Sys.which(cmd) != "")
}
on_failure(has_command) <- function(call, env) {
  paste0("Dependency ", sQuote(eval(call$cmd, env)), " is not installed\n",
         eval(call$msg, env))
}

#' Wrapper for system commands
#' 
#' @param exec The system command to be invoked.
#' @param ... Arguments passed on to the \code{system} command as name-value or 
#' name=\code{TRUE} pairs.
#' @param args Named list of arguments passed on to the \code{system} command.
#' Is merged with \code{...}.
#' @param stdin Input.
#' @param stdout Output.
#' @param redirection Redirection.
#' @param style One of \sQuote{unix} or \sQuote{gnu}.
#' @param sep Seperator of option and option argument.
#' @param show_cmd Have a look what the final command looks like.
#' @param intern Passed on to \code{\link{system}}'s \code{intern} argument.
#' @param input Passed on to \code{\link{system}}'s \code{input} argument.
#' @keywords internal
SysCall <- function(exec, ..., args = list(), stdin = NULL, stdout = NULL,
                    redirection = TRUE, style = c("unix", "gnu"), sep = " ",
                    show_cmd = FALSE, intern = FALSE, input = NULL) {  
  assert_that(has_command(exec))
  args <- merge_list(list(...), args)
  style <- match.arg(style)
  if (is.null(stdin)) {
    stdin <- ""
  } else if (!is.null(stdin) && redirection) {
    stdin <- paste("<", stdin)
  }
  if (is.null(stdout)) {
    stdout <- ""
  } else {
    stdout <- paste(">", stdout)
  }
  args[are_true(args)] <- ""
  args[are_false(args) | are_null(args)] <- NULL
  args <- switch(style,
                 unix=paste0(trim(sprintf("-%s%s%s", names(args), sep, args)), collapse=" "),
                 gnu=paste0(trim(sprintf("--%s%s%s", names(args), sep, args)), collapse=" "))
  
  if (show_cmd) {
    print(trim(paste(exec, args, stdin, stdout)))
  } else{
    system(trim(paste(exec, args, stdin, stdout)), intern = intern, input = input)
  }
}


setMethod("has_ranks", "Taxon", function(x, ranks) {
  any(getRank(getLineage(x)) %in% ranks)
})
setMethod("has_ranks", "TaxonList", function(x, ranks) {
  vapply(x, has_ranks, ranks=ranks, FUN.VALUE=FALSE, USE.NAMES=FALSE)
})


# returns all hsp(s) matching a specific query_id and hit_id or NA
.getSelectedHsps <- function (blastReportDB, qid, hid) {
  stmts <- paste("SELECT * from hsp WHERE query_id=", qid, "AND hit_id=", hid)
  as.data.frame(do.call(rbind, lapply(stmts, FUN = function(stmt) {
    db_query(blastReportDB, stmt) %||% NA_character_ 
  })))
}

#'@keywords internal
.getHit <- function(blast_db, id, what = "*") {
  db_query(blast_db , paste("SELECT", what, "FROM hit WHERE query_id=", id))
}

#'@keywords internal
.getHsp <- function (blast_db, id, what = "*") {
  db_query(blast_db, paste("SELECT", what, "FROM hsp WHERE query_id=", id))
}

#'@keywords internal
.filterHsp <- function(hsps, tolerance) { 
  # get all hsp(s) bit_score >= tolerance threshold
  hsps <- hsps[which(hsps$bit_score >= max(hsps$bit_score)*tolerance), ]  
  # sort them descending by bit_score
  hsps <- arrange(hsps, desc(hsps$bit_score))
  # remove duplicates
  hsps <- hsps[!duplicated(hsps$hit_id), ]
  hsps
}

#'@keywords internal
# after filtering of the hsp(s) the hit(s) have to be adjusted to prevent hit(s) without hsp(s)
.discardEmptyHits <- function(hits, hsps) {
  hits[hits$hit_id %in% hsps$hit_id, ]
}

setMethod('.resolveNoRank', 'Taxon',
          function(taxon) {
            if (ncbi::getRank(taxon) != 'no rank') {
              return(taxon)
            } else {
              Recall(taxonDB(ncbi::getParentTaxID(taxon)))
            } 
          })
setMethod('.resolveNoRank', 'TaxonList',
          function (taxon, taxon_db) {
            ncbi:::TaxonList(lapply(taxon, .resolveNoRank))
          })

# extend the setAs frunction to convert taxon in data.frame
setAs("Taxon", "data.frame", function (from) {
  data.frame(tax_id = from@TaxId, scientific_name = from@ScientificName, rank = from@Rank,
             check.names = FALSE, stringsAsFactors = FALSE)
})

setAs("TaxonList", "data.frame", function (from) {
  do.call('rbind', lapply(from, as, Class = 'data.frame'))
})

#' @keywords internal
.getterConstructor <- function(SELECT, FROM, ..., as = 'character') {
  function (x, id, type) {
    args <- list(...)
    type <- match.arg(type, c("tax_id", "query_id", "hit_id"))
    stmts <- trim(paste("SELECT", SELECT, 'FROM', FROM,
                        if (is.null(args$WHERE)) {
                          paste('WHERE', type, '=', id)
                        } else {
                          paste('WHERE', args$WHERE, '=')
                        },
                        if (!is.null(args$VAL) && !is.null(args$TABLE)) {
                          paste('(SELECT', args$VAL, 'FROM', args$TABLE,
                                'WHERE', type, '=', id,')')
                        }))
    AS <- match.fun(paste0('as.', as))
    lapply(stmts, function(stmt) {
      AS(db_query(x, stmt, 1L) %||% NA_character_)
    })
  }
}
# copied from rmisc
rBind <- function (x) {
  n_col <- length(x[[1L]])
  col_classes <- vapply(x[[1L]], class, character(1L), USE.NAMES=FALSE)
  res <- .Call("rmisc_bind_list", x, n_col, col_classes, PACKAGE = "rmisc")
  attr(res, "row.names") <- seq_len(length(res[[1L]]))
  res
}

dots <- function(...) {
  eval(substitute(alist(...)))
}

pkg_checker <- function(pkg) {
  assert_that(is.string(pkg))
  function()
    if (!require(eval(substitute(pkg)), character.only=TRUE)) {
      stop("Please install the ", pkg, " package", call. = FALSE)
    }
}

check_parallel <- pkg_checker("parallel")

check_HTSeqGenie <- pkg_checker("HTSeqGenie")

#' Filter NA entries from a list.
#'
#' @param x A vector.
#' @export
compactNA <- function(x) {
  filterNA <- function(x) suppressWarnings(is.na(x)) %||% FALSE
  x[!vapply(x, filterNA, FALSE, USE.NAMES=FALSE)]
}
