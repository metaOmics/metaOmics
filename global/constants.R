DB.dir <- "data"

TOOLSET.de    <- "MetaDE"
TOOLSET.clust <- "MetaSparseKmeans"
TOOLSET.path  <- "MetaPath"
TOOLSET.pca   <- "MetaPCA"
TOOLSET.ktsp  <- "MetaKTSP"
TOOLSET.all <- c(TOOLSET.de, TOOLSET.clust, TOOLSET.path, TOOLSET.pca, TOOLSET.ktsp) 

select.noDefault <- list(
  placeholder = 'Choose from below',
  onInitialize = I('function() { this.setValue(""); }')
)

STUDY.expression.upload = "uploadExpression"
STUDY.clinical.upload   = "uploadClinical"
STUDY.select.from.db    = "selectStudy"

META.TYPE.p <- "p"
META.TYPE.effect <- "effect"
META.TYPE.other <- "other"
META.TYPE.all <- c(META.TYPE.p, META.TYPE.effect, META.TYPE.other)
names(META.TYPE.all) <- c("Combine P-Value", "Combine Effect Size", "Others")

META.maxP        <- "maxP"
META.maxP.OC     <- "maxP.OC"
META.minP        <- "minP"
META.minP.OC     <- "minP.OC"
META.Fisher      <- "Fisher"
META.Fisher.OC   <- "Fisher.OC"
META.AW          <- "AW"
META.roP         <- "roP"
META.roP.OC      <- "roP.OC"
META.Stouffer    <- "Stouffer"
META.Stouffer.OC <- "Stouffer.OC"
META.SR          <- "SR"
META.PR          <- "PR"
META.minMCC      <- "minMCC"
META.FEM         <- "FEM"
META.REM         <- "REM"
META.rankProd    <- "rankProd"
META.all <- c(META.maxP, META.maxP.OC, META.minP, META.minP.OC, META.Fisher, META.Fisher.OC, META.AW, META.roP, META.roP.OC, META.Stouffer, META.Stouffer.OC, META.SR, META.PR, META.minMCC, META.FEM, META.REM, META.rankProd)
names(META.all) <- c(
  "maxP",
  "maxP with one sided correction",
  "minP",
  "minP with one sided correction",
  "Fisher",
  "Fisher with one sided correction",
  "AW Fisher",
  "roP",
  "roP with one sided correction",
  "Stouffer",
  "Stouffer one sided correction",
  "Sum of Rank",
  "Product of Rank",
  "minMCC (Multi-class Correlation)",
  "FEM (Fixed Effect Model)",
  "REM (Random Effect Model)",
  "Rank Product"
)

META.p.core <- META.all[c(5, 7)]
META.p.all <- META.all[c(5, 7, 6, 1, 2, 3, 4, 8, 9, 10, 11)]
META.effect.all <- META.all[c(15, 16)]
META.other.all <- META.all[c(12, 13, 14, 17)]

IND.limma     <- "limma"
IND.sam       <- "sam"
IND.limmaVoom <- "limmaVoom"
IND.edgeR     <- "edgeR"
IND.DESeq2    <- "DESeq2"
IND.pearsonr  <- "pearsonr"
IND.spearmanr <- "spearmanr"
IND.all <- c(IND.limma, IND.sam, IND.limmaVoom, IND.edgeR, IND.DESeq2, IND.pearsonr, IND.spearmanr)
names(IND.all) <- c("LIMMA (Linear Model for Microarrray data)", "SAM (Significance Analysis of Microarrays)", "voom method in limma", "edgeR", "DESeq2", "pearson correlation", "spearman correlation")

TAIL.abs  <- "abs"
TAIL.high <- "high"
TAIL.low  <- "low"
TAIL.all <- c(TAIL.abs, TAIL.high, TAIL.low)
names(TAIL.all) <- TAIL.all


RESP.two.class   <- "twoclass"
RESP.multi.class <- "multiclass"
RESP.continuous  <- "continuous"
RESP.survival    <- "survival"
RESP.all <- c(RESP.two.class, RESP.multi.class, RESP.continuous, RESP.survival)
names(RESP.all) <- c("Two Class Comparison", "Multi-Class Comparison", "Continuous Outcome", "Survival Outcome")

AW.original      <- "original"
AW.unconditional <- "uncond"
AW.conditional   <- "cond"
AW.all <- c(AW.original, AW.unconditional, AW.conditional)
names(AW.all) <- c("Original", "Conditional", "Un-conditional")

REM.type = c("HS","HO", "DL", "SJ", "EB", "RML" )
REM.HO  <- "HO"
REM.HS  <- "HS"
REM.DL  <- "DL"
REM.SJ  <- "SJ"
REM.EB  <- "EB"
REM.RML <- "RML"
REM.all <- c(REM.HO, REM.HS, REM.DL, REM.SJ, REM.EB, REM.RML)
names(REM.all) <- c("HO (Hedges-Olkin)", "HS (Hunter-Schmidt)", "DL (DerSimonian-Laird)", "SJ (Sidik-Jonkman)", "Empirical Bayes", "Restricted Maximum Likelihood")

GENESET.KEGG        <- "KEGG.genesets"
GENESET.GOBP        <- "GOBP.genesets"
GENESET.GOCC        <- "GOCC.genesets"
GENESET.GOMF        <- "GOMF.genesets"
GENESET.Reactome    <- "Reactome.genesets"
GENESET.BioCarta    <- "Biocarta.genesets"
GENESET.Phenocarta  <- "Phenocarta.genesets"
GENESET.Hallmark    <- "Hallmark.genesets"
GENESET.Positional  <- "Positional.genesets"
GENESET.CMAP_up     <- "CMAP_up.genesets"
GENESET.CMAP_down   <- "CMAP_down.genesets"
GENESET.CGP         <- "CGP.genesets"
GENESET.Canonical   <- "Canonical.genesets"
GENESET.CGN         <- "CGN.genesets"
GENESET.CM          <- "CM.genesets"
GENESET.Oncogenic   <- "Oncogenic.genesets"
GENESET.Immunologic <- "Immunologic.genesets"
GENESET.PID         <- "PID.genesets"
GENESET.PPI         <- "PPI.genesets"
GENESET.MIR         <- "MIR.genesets"
GENESET.TFT         <- "TFT.genesets"
GENESET.TargetScan  <- "TargetScan.genesets"
GENESET.miRanda     <- "miRanda.genesets"
GENESET.JASPAR      <- "JASPAR.genesets"
GENESET.PITA        <- "PITA.genesets"
GENESET.all <- c(GENESET.KEGG, GENESET.GOBP, GENESET.GOCC, GENESET.GOMF, GENESET.Reactome, GENESET.BioCarta, GENESET.Phenocarta, GENESET.Hallmark, GENESET.Positional, GENESET.CMAP_up, GENESET.CMAP_down, GENESET.CGP, GENESET.Canonical, GENESET.CGN, GENESET.CM, GENESET.Oncogenic, GENESET.Immunologic, GENESET.PID, GENESET.PPI, GENESET.MIR, GENESET.TFT, GENESET.TargetScan, GENESET.miRanda, GENESET.JASPAR, GENESET.PITA)
names(GENESET.all) <- c(
"KEGG",
"GO Biological Process",
"GO Cellular Component",
"GO Molecular Function",
"Reactome",
"BioCarta",
"Phenocarta",
"Hallmark",
"Positional",
"Complement Map up",
"Complement Map down",
"Chemical and Genetic Perturbation",
"Canonical",
"Cancer Gene Neighborhoods",
"Cancer Modules",
"Oncogenic Signatures",
"Immunologic Signatures",
"Pathway Interaction Database",
"Protein-protein Interaction",
"microRNA targets",
"Transcription Factor Targets",
"TargetScan",
"miRanda",
"JASPAR",
"PITA"
)

MAPE.CPI <- "CPI"
MAPE.MAPE <- "MAPE"
MAPE.all <- c(MAPE.CPI, MAPE.MAPE)
names(MAPE.all) <- c("CPI (Comparative Pathway Integrator)", "MAPE (Meta Analysis Pathway Enrichment)")

ENRICHMENT.KS <- "KS"
ENRICHMENT.fisher <- "Fisher's exact"
ENRICHMENT.all <- c(ENRICHMENT.KS, ENRICHMENT.fisher)
names(ENRICHMENT.all) <- c("Kolmogorov–Smirnov test", "Fisher's exact test")

MAPE.STAT.fisher <- "Fisher"
MAPE.STAT.maxP <- "maxP"
MAPE.STAT.minP <- "minP"
MAPE.STAT.rth <- "rth"
MAPE.STAT.aw.fisher <- "AW Fisher"
MAPE.STAT.all <- c(MAPE.STAT.fisher, MAPE.STAT.maxP, MAPE.STAT.minP, MAPE.STAT.rth, MAPE.STAT.aw.fisher)
names(MAPE.STAT.all) <- c("Fisher's method", "maxP", "minP", "roP", "Adaptively Weighted Fisher's Method")

PERMUTE.sample <- "sameple"
PERMUTE.gene <- "gene"
PERMUTE.all <- c(PERMUTE.sample, PERMUTE.gene)
names(PERMUTE.all) <- c("Permute Sample", "Permute Gene")

QVALUE.estimate <- "estimate"
QVALUE.permutation <- "permutation"
QVALUE.all <- c(QVALUE.estimate, QVALUE.permutation)
names(QVALUE.all) <- c("Estimate", "Permutation")

