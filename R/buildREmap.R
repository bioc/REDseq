buildREmap <-
function(REpatternFilePath,format="fasta", BSgenomeName, outfile)
{
		if(missing(REpatternFilePath))
		{
			stop("missing required parameter REpatternFilePath!")
		}
		if (!file.exists(REpatternFilePath))
		{
			stop("REpatternFilePath specified as ", REpatternFilePath, " does not exsist!")
		}
		if (format != "fasta" && format != "fastq")
		{
			stop("format needs to be either fasta or fastq!")
		}
		if (missing(BSgenomeName) || class(BSgenomeName) != "BSgenome")
		{
			stop("BSgenomeName is required as BSgenome object!")
		}
		if (file.exists(outfile))
		{
			stop("outfile specified as ", outfile, " already exists! Please rename the outfile!")
		}
		dict = read.DNAStringSet(REpatternFilePath, format, use.names=FALSE) 
		searchPattern(dict, BSgenomeName=BSgenomeName,outfile=outfile)
		hits <- read.table(outfile, sep="\t", header=TRUE)
		REmap <- BED2RangedData(hits)
		file.remove(outfile)
		REmap	
}

