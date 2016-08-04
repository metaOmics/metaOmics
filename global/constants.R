DB.dir <- "data"

select.noDefault <- list(
  placeholder = 'Choose from below',
  onInitialize = I('function() { this.setValue(""); }')
)

STUDY.expression.upload = "uploadExpression"
STUDY.clinical.upload   = "uploadClinical"
STUDY.select.from.db    = "selectStudy"
