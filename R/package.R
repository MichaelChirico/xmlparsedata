#' Convert R parse data to XML
#'
#' In recent R versions the parser can attach source code location
#' information to the parsed expressions. This information is often
#' useful for static analysis, e.g. code linting. It can be accessed
#' via the [utils::getParseData()] function.
#'
#' `xml_parse_data()` converts this information to an XML tree.
#' The R parser's token names are preserved in the XML as much as
#' possible, but some of them are not valid XML tag names, so they are
#' renamed, see the [xml_parse_token_map] vector for the
#' mapping.
#'
#' The top XML tag is `<exprlist>`, which is a list of
#' expressions, each expression is an `<expr>` tag. Each tag
#' has attributes that define the location: `line1`, `col1`,
#' `line2`, `col2`. These are from the [getParseData()]
#' data frame column names. Next, there are two attributes,
#' `start` and `end`, which can be used as an ordering of
#' expressions in the document. Note that while the values
#' are correlated with (and in some cases may match exactly)
#' positions in the document, this cannot be relied upon.
#'
#' See an example below. See also the README at
#' <https://github.com/r-lib/xmlparsedata#readme>
#' for examples on how to search the XML tree with the `xml2` package
#' and XPath expressions.
#'
#' Note that `xml_parse_data()` silently drops all control characters
#' (0x01-0x1f) from the input, except horizontal tab (0x09) and newline
#' (0x0a), because they are invalid in XML 1.0.
#'
#' @param pretty Whether to pretty-indent the XML output. It has a small
#'   overhead which probably only matters for very large source files.
#' @inheritParams utils::getParseData
#' @return An XML string representing the parse data. See details below.
#'
#' @export
#' @importFrom utils getParseData
#' @seealso [xml_parse_token_map] for the token names.
#' <https://github.com/r-lib/xmlparsedata#readme> for more
#' information and use cases.
#' @examples
#' code <- "function(a = 1, b = 2) {\n  a + b\n}\n"
#' expr <- parse(text = code, keep.source = TRUE)
#'
#' # The base R way:
#' getParseData(expr)
#'
#' cat(xml_parse_data(expr, pretty = TRUE))
xml_parse_data <- function(x, includeText = NA, pretty = FALSE) {
  xml_header <- paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" ",
    "standalone=\"yes\" ?>\n<exprlist>\n"
  )
  xml_footer <- "\n</exprlist>\n"

  ## Maybe it is already a data frame, e.g. when used in lintr
  if (is.data.frame(x)) {
    pd <- copy(x)
  } else {
    pd <- getParseData(x, includeText = includeText)
    if (is.null(pd)) {
      tmp_source <- tempfile()
      on.exit(unlink(tmp_source))
      dput(x, file = tmp_source)

      x <- parse(tmp_source, keep.source = TRUE)
      pd <- getParseData(x, includeText = includeText)
      pd$line1 <- pd$line2 <- pd$col1 <- pd$col2 <- NA_integer_
    }
  }

  if (!nrow(pd)) {
    return(paste0(xml_header, xml_footer))
  }

  setDT(pd)
  fix_comments_inplace(pd)
  
  has_text <- "text" %in% names(pd)
  if (!is.data.frame(x) && has_text) {
    pd[
      # workaround for R parser bug #18323; see #25
      # skip if there are tabs, which would require complicating the logic a lot
      token == "STR_CONST" & col2 - col1 != nchar(text) - 1L & !grepl("\t", text, fixed = TRUE),
      text := reparse_octal(.SD, attr(x, "srcfile")$lines)
    ]
  }

  if (!is.null(pd$text)) {
    pd[, text := enc2utf8(text)]
  }

  ## Tags for all nodes, teminal nodes have end tags as well
  pd[, token := map_token(token)]

  ## Positions, to make it easy to compare what comes first
  pd[, c("start", "end") := {
    maxcol = max(col1, col2) + 1L
    list(line1 * maxcol + col1, line2 * maxcol + col2)
  }]

  pd[, terminal_tag := character(.N)]
  pd[(terminal), terminal_tag := paste0("</", token, ">")]
  if (anyNA(pd$line1)) {
    pd[, tag := paste0(
      "<", token, ">",
      if (has_text) xml_encode(text) else "",
      terminal_tag
    )]
  } else {
    pd[, tag := paste0(
      "<", token,
      " line1=\"", line1,
      "\" col1=\"", col1,
      "\" line2=\"", line2,
      "\" col2=\"", col2,
      "\" start=\"", start,
      "\" end=\"", end,
      "\">",
      if (has_text) xml_encode(text) else "",
      terminal_tag
    )]
  }
  pd[, "terminal_tag" := NULL]

  ## Add an extra terminal tag for each non-terminal one
  pd2 <- pd[(!terminal), ]
  if (nrow(pd2)) {
    pd2[, `:=`(
      terminal = TRUE,
      parent = -1,
      line1 = line2,
      col1 = col2,
      line2 = line2 - 1L,
      col2 = col2 - 1L,
      tag = paste0("</", token, ">")
    )]
    pd <- rbind(pd, pd2)
  }

  ## Order the nodes properly
  ## - the terminal nodes from pd2 may be nested inside each other, when
  ##   this happens they will have the same line1, col1, line2, col2 and
  ##   terminal status; and 'start' is used to break ties
  setorder(pd, line1, col1, -line2, -col2, terminal, -start)

  if (pretty) {
    xml <- pd[, {
      str <- !terminal
      end <- parent == -1
      ind <- 2L + cumsum(str * 2L + end * (-2L)) - str * 2L
      paste0(strrep(" ", ind), tag, collapse = "\n")
    }]
  } else {
    xml <- paste(pd$tag, collapse = "\n")
  }

  paste0(xml_header, xml, xml_footer)
}

fix_comments_inplace <- function(pd) {
  pd[parent < 0, parent := 0]
}

map_token <- function(token) {
  needs_translation <- token %in% names(xml_parse_token_map)
  token[needs_translation] <- xml_parse_token_map[token[needs_translation]]
  token
}

#' Map token names of the R parser to token names in
#' [xml_parse_data()]
#'
#' Some of the R token names are not valid XML tag names,
#' so [xml_parse_data()] needs to replace them to create a
#' valid XML file.
#'
#' @export
#' @seealso [xml_parse_data()]

xml_parse_token_map <- c(
  "'?'" = "OP-QUESTION",
  "'~'" = "OP-TILDE",
  "'+'" = "OP-PLUS",
  "'-'" = "OP-MINUS",
  "'*'" = "OP-STAR",
  "'/'" = "OP-SLASH",
  "':'" = "OP-COLON",
  "'^'" = "OP-CARET",
  "'$'" = "OP-DOLLAR",
  "'@'" = "OP-AT",
  "'('" = "OP-LEFT-PAREN",
  "'['" = "OP-LEFT-BRACKET",
  "';'" = "OP-SEMICOLON",
  "'{'" = "OP-LEFT-BRACE",
  "'}'" = "OP-RIGHT-BRACE",
  "')'" = "OP-RIGHT-PAREN",
  "'!'" = "OP-EXCLAMATION",
  "']'" = "OP-RIGHT-BRACKET",
  "','" = "OP-COMMA",
  "'\\\\'" = "OP-LAMBDA"
)

xml_encode <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  # most control characters are not allowed in XML, except tab and nl
  x <- gsub("[\x01-\x08\x0b-\x1f]", "", x, useBytes = TRUE)
  x
}
