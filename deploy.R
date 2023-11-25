
shinylive::export("myapp", "docs")

# test
httpuv::runStaticServer("docs/", port=8008)


