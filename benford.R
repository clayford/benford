compareBenford <- function(d){
  if(!"ggplot2" %in% rownames(installed.packages())) stop("This function requires the ggplot2 package.")
  digits <- d[!is.na(d)]
  digits <- substr(stringr::str_extract(as.character(abs(digits)), pattern = "[^0\\.]"),1,1)
  digits <- factor(digits, levels = 1:9) # ensure all digits represented
  depth <- prop.table(table(digits))
  ben <- log10(1 + (1/(1:9)))
  dat2 <- data.frame(ben, depth)
  names(dat2) <- c("Benford","Digit",deparse(substitute(d)))
  dat2L <- reshape2::melt(dat2,id.vars="Digit", variable.name = "Type", value.name = "Frequency")
  ggplot2::ggplot(dat2L, aes(x=Digit, y=Frequency, fill=Type)) +
    geom_bar(stat = "identity", position = "dodge")
}
