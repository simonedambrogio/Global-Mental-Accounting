library(rsvg)
library(stringi)
library(stringr)

imgs <- list.files("index_files/figure-html/")

for( img_i in imgs){
  cat( which(img_i==imgs), "/", length(imgs), "\r" )
  if( str_detect(img_i, ".svg") ){
    rsvg_png(
      str_c("index_files/figure-html/", img_i), 
      str_c("index_files/figure-html/", str_replace(img_i, ".svg", ".png")), 
      height = 1024
    )
  }
}


