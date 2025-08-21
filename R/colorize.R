#' @title Add color to result if terminal accepts it
#' 
#' @description Add ANSI color to a string using the 
#' \code{crayon} package, if the R environment accepts color. 
#' This function is needed because some  
#' output cannot be colorized.  Color determination 
#' is made by \code{crayon::has_color()}. 
#' 
#' Rdistance results often include units, e.g., "25 [m]". 
#' Only colorize the numbers, not the units.  Everything between the 
#' first set of square brackets (\code{[...]}) is NOT colorized. 
#' Subsequent sets of brackets (e.g., "25 [m] + 30 [ft]") ARE colorized
#' (i.e., "[ft]" is color). 
#' 
#' @param STR A vector of strings to colorize. 
#' 
#' @param col A string specifying the desired foreground color.  
#' This is passed straight to \code{crayon::style} and so must be 
#' recognized as one of the 8 base crayon colors. i.e., 
#' "black", "red", "green", "yellow", "blue", "magenta", "cyan", 
#' "white", and "silver" (silver = gray). By default, numbers 
#' are styled in "green".
#' 
#' @param bg A string specifying the desired background color. 
#' Must be one of "bgBlack", "bgRed", "bgGreen", "bgYellow", "bgBlue"
#' "bgMagenta", "bgCyan", or "bgWhite".  By default, no background 
#' is applied. 
#' 
#' @return If color is not allowed in the terminal, the input 
#' string is returned unperturbed.  If color is allowed, the input 
#' string is returned with color and background ANSI code surrounding 
#' the initial part of the string from character 1 to the character 
#' before the [ in the first pair of []. 
#' 
#' @seealso \code{crayon::style}
#' 
#' 
# Do not export. In Rdistance namespace only
#
colorize <- function(STR, col=NULL, bg = NULL){
  if(crayon::has_color()){
    if( is.null(col) ){
      col <- "green"
    }
    uLoc <- regexpr("\\[(?:.(?!\\[))+\\]", STR, perl = TRUE)  
    u <- substring(STR, uLoc, uLoc + attr(uLoc, "match.length"))
    u[is.na(u)] <- ""
    uLoc <- ifelse(uLoc < 0, nchar(STR)+1, uLoc)
    STR1 <- substring(STR, 1, uLoc - 1)  # before [*]
    STR2 <- substring(STR, uLoc + nchar(u)) # after [*]
    STR2[is.na(STR2)] <- ""
    STR1 <- crayon::style(STR1, as = col, bg = bg)
    STR2 <- crayon::style(STR2, as = col, bg = bg)
    STR <- paste0(STR1, u, STR2)
  } 
  STR
}
