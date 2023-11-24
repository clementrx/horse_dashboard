
shinylive::export(appdir = "myapp", destdir = "docs")

# test
httpuv::runStaticServer("docs/", port=8008)
