DB.dir <- "data"

select.noDefault <- list(
  placeholder = 'Choose from below',
  onInitialize = I('function() { this.setValue(""); }')
)

STUDY.expression.upload = "uploadExpression"
STUDY.clinical.upload   = "uploadClinical"
STUDY.select.from.db    = "selectStudy"

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

IND.limma     <- "limma"
IND.sam       <- "sam"
IND.limmaVoom <- "limmaVoom"
IND.edgeR     <- "edgeR"
IND.DESeq2    <- "DESeq2"
IND.pearsonr  <- "pearsonr"
IND.all <- c(IND.limma, IND.sam, IND.limmaVoom, IND.edgeR, IND.DESeq2, IND.pearsonr)
names(IND.all) <- IND.all

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
names(RESP.all) <- c("Two Class", "Multi-Class", "Continuous", "Survival")

AW.original      <- "original"
AW.unconditional <- "uncond"
AW.conditional   <- "cond"
AW.all <- c(AW.original, AW.unconditional, AW.conditional)
names(AW.all) <- c("Original", "Conditional", "Un-conditional")

REM.type = c("HS","HO", "DL", "SJ", "EB", "RML" )
REM.HS  <- "HS"
REM.HO  <- "HO"
REM.DL  <- "DL"
REM.SJ  <- "SJ"
REM.EB  <- "EB"
REM.RML <- "RML"
REM.all <- c(REM.HS, REM.HO, REM.DL, REM.SJ, REM.EB, REM.RML)
names(REM.all) <- c("HS", "HO", "DL", "SJ", "EB", "RML")
