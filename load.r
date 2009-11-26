library(ggplot2)
library(plyr, warn.conflicts = FALSE)

frame_files <- compact(llply(sys.frames(), function(x) x$ofile))
PATH <- dirname(frame_files[[length(frame_files)]])

lapply(dir(file.path(PATH, "R"), full.name=TRUE), source)


lapply(dir(file.path(PATH, "code"), full.name=T, pattern = "\\.r$"), source)
if (!exists("h")) {
  
}