
context("Testing loading IndustryClassification")

expect_false("wiod.indclass" %in% ls())
data(WIOD_IndustryClassification)
expect_true("wiod.indclass" %in% ls())

rm(wiod.indclass)
expect_false("wiod.indclass" %in% ls())
wiod.indclass
expect_false("wiod.indclass" %in% ls())
wiod.IndClass <- wiod.indclass
expect_true("wiod.IndClass" %in% ls())
