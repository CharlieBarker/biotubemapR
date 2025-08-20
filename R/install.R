#remove
rm(list = ls())
#detach
if ("biotubemapR" %in% loadedNamespaces()) {
  detach("package:biotubemapR", unload = TRUE)
}
#rebuild from scratch
devtools::build()
#reinstall from tar
install.packages("/home/charlie/Desktop/packages/biotubemapR_0.0.0.9004.tar.gz", repos = NULL, type = "source")
