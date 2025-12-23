library(data.table)

df <- read.delim("H:/Documents/cbioportal p53 muts in pancancer studies 1.27.23.tsv", sep = "\t")
print(df)

dcast(setDT(df)[, ], Protein.Change ~ Cancer.Type.Detailed, fun.aggregate = length)
