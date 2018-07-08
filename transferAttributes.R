## Attributes of variables are lost when subsetting. 
## Use this to restore them.
transferAttributes <- function (dsub, d) {
  ds <- dsub
  ## ds and dsub are the new variables; 
  ## d is the original with attributes
  A <- attributes (d)
  A$Dimensions$Time$len <- nrow (ds)
  A$row.names <- 1:nrow (ds)
  A$names <- names (ds)
  attributes (ds) <- A
  for (nm in names (ds)) {
    if ((nm != 'Time') && exists ('specialData') &&
        (nm %in% names (specialData))) {next}
    var <- sprintf ("d$%s", nm)
    A <- attributes (eval (parse (text=var)))
    if (!grepl ('Time', nm)) {
      A$dim[1] <- nrow (ds)
      A$class <- NULL
    } else {
      A$dim <- nrow (ds)
    }
    # print (sprintf ('tA: nm=%s, A=%s', nm, A))
    attributes (ds[,nm]) <- A
  }
  return(ds)
}