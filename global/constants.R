dataset.dir <- "data/.dataset"

study.dtype <- list(
  "microarray"="microarray",
  "RNA sequence"="RNAseq",
  "RNA sequence FPKM"="RNAseqFPKM"
)

study.stype <- list(
  "multiple studies"="multiple",
  "single study"="single"
)

study.ntype <- list(
  "continuous"="continuous",
  "discrete"="discrete"
)

ID.type <- list(
  "Gene Symbol"="GeneSymbol",
  "Probe ID"="ProbeID",
  "Reference Sequence ID"="RefSeqID",
  "Entrez ID"="EntrezID"
)

species.option <- list(
  Mounse="mounse",
  Human="human",
  Rat="rat"
)

select.noDefault <- list(
  placeholder = 'Choose from below',
  onInitialize = I('function() { this.setValue(""); }')
)
