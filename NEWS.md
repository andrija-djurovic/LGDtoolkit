# LGDtoolkit 0.1.0
1. ```stepFWD``` and ```stepRPC``` help page improved
2. ```stepFWD``` - bug fixed (p-value indexing in the stepwise iterations)

# LGDtoolkit 0.2.0
1. Internal function ```summary.tbl``` renamed as ```summ.tbl``` to avoid S3 conflict
2. New functions:
  + ```heterogeneity``` - validation of heterogeneity of the rating scale
  + ```homogeneity``` - validation of homogeneity of the rating scale

# LGDtoolkit 0.2.1
1. Heterogeneity testing: in the test results p2 and p1 were swapped. 
This:
tr <- ifelse(p.val >= alpha, 
				 paste0("H0: LOSS(", p1, ") ", h0, " LOSS(", p2, ")"),
				 paste0("H1: LOSS(", p1, ") ", h1, " LOSS(", p2, ")"))
is replaced with this:
tr <- ifelse(p.val >= alpha, 
				 paste0("H0: LOSS(", p2, ") ", h0, " LOSS(", p1, ")"),
				 paste0("H1: LOSS(", p2, ") ", h1, " LOSS(", p1, ")"))
