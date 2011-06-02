searchPattern <-
function(dict0, BSgenomeName, outfile="")
 { 	
 	
		if(missing(dict0) || class(dict0) != "DNAStringSet" )
		{
			stop("REpatternFilePath is required as a DNAstringSet object!")
		}
		if (missing(BSgenomeName) || class(BSgenomeName) != "BSgenome")
		{
			stop("BSgenomeName is required as a BSgenome object!")
		}
		if (file.exists(outfile))
		{
			stop("outfile specified as ", outfile, " already exists! Please rename the outfile!")
		}

	seqnames <- seqnames(BSgenomeName)
	seqnames_in1string <- paste(seqnames, collapse=", ")
  append <- FALSE
 for (seqname in seqnames) {
		subject <- BSgenomeName[[seqname]]
 		cat(">>> Finding all hits in chromosome", seqname, "...\n")
 		for (i in seq_len(length(dict0))) {
 			patternID <- names(dict0)[i]
 			pattern <- dict0[[i]]
 			plus_matches <- matchPattern(pattern, subject, fixed=FALSE)
 			names(plus_matches) <- rep.int(patternID, length(plus_matches))
 			writeHits(seqname, plus_matches, "+", file=outfile, append=append)
 			append <- TRUE
 			rcpattern <- reverseComplement(pattern)
			if (rcpattern != pattern)
			{
 				minus_matches <- matchPattern(rcpattern, subject, fixed=FALSE)
 				names(minus_matches) <- rep.int(patternID, length(minus_matches))
 				writeHits(seqname, minus_matches, "-", file=outfile, append=append)
			}
 		}
 	cat(">>> DONE searching\n")
 	}
 }

