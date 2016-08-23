# Setting the maximum file upload limit to 100 MB
options(shiny.maxRequestSize=100*1024^2)
shinyServer(function(input, output, session) {
  callModule(setting_server, "setting")
  callModule(preproc_server, "preproc")
  callModule(saved_data_server, "saved_data")
  callModule(meta_clust_server, "meta_clust")
#  callModule(meta_de_server, "meta_de")
  callModule(meta_pca_server,"meta_pca")
})
