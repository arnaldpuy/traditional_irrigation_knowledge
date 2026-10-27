normalize_practice_fun <- function(x) {

  canon <- c(
    plowing           = "tillage",
    cover_cropping    = "cover_crop",
    irrigation_timing = "irrigation_scheduling",
    shade_cover       = "shade_management"
  )

  x <- trimws(as.character(x))
  hit <- x %in% names(canon)
  x[hit] <- canon[x[hit]]
  x
}
