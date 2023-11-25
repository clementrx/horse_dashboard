
shinylive::export("myapp", "docs")

# test
httpuv::runStaticServer("docs/", port=8008)


# library(plumber)
# pr() %>% pr_static("/", "docs") %>% pr_run()
