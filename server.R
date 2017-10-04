# Setting the maximum file upload limit to 100 MB
options(shiny.maxRequestSize=100*1024^2)
shinyServer(function(input, output, session) {
  installed=installed.packages()[,"Package"]

  if (TOOLSET.de %in% installed ==F && TOOLSET.path %in% installed)
  installed <- installed[-grep(TOOLSET.path,installed,fixed=T)]
       
  callModule(setting_server, "setting")
  callModule(preproc_server, "preproc")
  callModule(saved_data_server, "saved_data")

  if (TOOLSET.qc %in% installed) 
    callModule(meta_qc_server, "meta_qc")     
  if (TOOLSET.de %in% installed)
    callModule(meta_de_server, "meta_de")
  if (TOOLSET.path %in% installed)
    callModule(meta_path_server, "meta_path")    
  if (TOOLSET.dcn %in% installed)
    callModule(meta_dcn_server, "meta_dcn")
  if (TOOLSET.ktsp %in% installed)
      callModule(meta_ktsp_server, "meta_ktsp")        
  if (TOOLSET.clust %in% installed)
    callModule(meta_clust_server, "meta_clust")
  if (TOOLSET.pca %in% installed)
      callModule(meta_pca_server, "meta_pca")

})
