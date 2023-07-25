# # tests for summary_actor
# ### all weighted version
# # 1-a) cross_sec & symmetric
# # 1-b) cross_sec $ asymmetric
# # 2-a) longit_list $ symmetric
# # 2-b) longit_list & asymmetric
#
# ############################################
# test_that('summary_actor: cross-sec & symmetric', {
#
#   # create symmetric dat
#   actor1 <- c('a','a','b', 'b', 'c','c')
#   actor2 <- c('b', 'c','a','c','b','a')
#   value <- c(2,4,2,7,7,4)
#   sym_weight_df <- data.frame(actor1=actor1,actor2=actor2, value = value)
#   sym_weight_df <- sym_weight_df[sym_weight_df$actor1!=sym_weight_df$actor2,]
#
#   # create matrix
#   sym_weight_matrix <- matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3],letters[1:3]))
#   for(ii in 1:nrow(sym_weight_df)){
#     sym_weight_matrix[sym_weight_df$actor1[ii],sym_weight_df$actor2[ii]] = sym_weight_df$value[ii] }
#
#   # calculate centrality scores
#
#   # degree centrality
#   average_degree_in <- rowMeans(sym_weight_matrix, na.rm=TRUE)
#   average_degree_out <- colMeans(sym_weight_matrix, na.rm=TRUE)
#   average_degree_total <- rowMeans(sym_weight_matrix, na.rm=TRUE) + colMeans(sym_weight_matrix, na.rm=TRUE)
#
#   # eigenvector centrality
#   decomp = eigen(sym_weight_matrix)
#   decompVectors = decomp$vectors[,1]
#
#   if(any(is.complex(decompVectors))){eigenvec <- NA}
#   else{eigenvec <- decompVectors/max(decompVectors)}
#
#   # authority score
#   decomp <- svd(sym_weight_matrix)
#   decompV <- decomp$v[,1]
#   authority <- scale(decompV)
#
#   # hub score
#   decompU <- decomp$u[,1]
#   hub <- scale(decompU)
#
#   # actor
#   actor = rownames(sym_weight_matrix)
#
#   summary_result = data.frame(average_degree_in = average_degree_in,
#                               average_degree_out = average_degree_out,
#                               average_degree_total = average_degree_total,
#                               eigenvec = eigenvec,
#                               authority=authority,
#                               hub=hub,
#                               actor = actor)
#
#   rownames(summary_result) <- NULL
#   summary_result <- summary_result[,c('actor', setdiff(names(summary_result), 'actor'))]
#
#   # convert the output into conflictNet object
#   class(summary_result) <- 'conflictNet'
#
#   # convert into conflictNet object
#   class(sym_weight_matrix) <- 'conflictNet'
#   attr(sym_weight_matrix, 'conflictNetType') <- 'cross_sec'
#   attr(sym_weight_matrix, 'symmetric') <- TRUE
#
#   summary_actor_result <- summary_actor(sym_weight_matrix)
#
#   # convert the output into conflictNet object
#   class(summary_result) <- 'conflictNet'
#   # the test
#   expect_identical(summary_actor_result, summary_result)
#
# })
#
# ############################################
# test_that('summary_actor: cross-sec & asymmetric', {
#
#   # create data that is cross-sectional, asymmetric, and non-weighted
#   asym_non_weight_df <- expand.grid(actor1=letters[1:3],actor2=letters[1:3],stringsAsFactors=FALSE)
#   asym_non_weight_df$value <- rbinom(nrow(asym_non_weight_df), 1, .5)
#   asym_non_weight_df <- asym_non_weight_df[asym_non_weight_df$actor1!=asym_non_weight_df$actor2,]
#
#   # filter out zero responses, equivalent to an event dataset
#   asym_non_weight_df <- asym_non_weight_df[asym_non_weight_df$value>0,]
#
#   # create matrix
#   asym_non_weight_matrix <- matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3],letters[1:3]))
#   for(ii in 1:nrow(asym_non_weight_df)){
#     asym_non_weight_matrix[asym_non_weight_df$actor1[ii],asym_non_weight_df$actor2[ii]] = asym_non_weight_df$value[ii] }
#   # calculate centrality scores
#
#   # degree centrality
#   average_degree_in <- rowMeans(asym_non_weight_matrix, na.rm=TRUE)
#   average_degree_out <- colMeans(asym_non_weight_matrix, na.rm=TRUE)
#   average_degree_total <- rowMeans(asym_non_weight_matrix, na.rm=TRUE) + colMeans(asym_non_weight_matrix, na.rm=TRUE)
#
#   # eigenvector centrality
#   decomp = eigen(asym_non_weight_matrix)
#   decompVectors = decomp$vectors[,1]
#
#   if(any(is.complex(decompVectors))){eigenvec <- NA}
#   else{eigenvec <- decompVectors/max(decompVectors)}
#
#   # authority score
#   decomp <- svd(asym_non_weight_matrix)
#   decompV <- decomp$v[,1]
#   authority <- scale(decompV)
#
#   # hub score
#   decompU <- decomp$u[,1]
#   hub <- scale(decompU)
#
#
#   # actor
#   actor = rownames(asym_non_weight_matrix)
#
#   summary_result = data.frame(average_degree_in = average_degree_in,
#                               average_degree_out = average_degree_out,
#                               average_degree_total = average_degree_total,
#                               eigenvec = eigenvec,
#                               authority = authority,
#                               hub = hub,
#                               actor = actor)
#
#   rownames(summary_result) <- NULL
#   summary_result <- summary_result[,c('actor', setdiff(names(summary_result), 'actor'))]
#
#
#   # convert into conflictNet object
#   class(asym_non_weight_matrix) <- 'conflictNet'
#   attr(asym_non_weight_matrix, 'conflictNetType') <- 'cross_sec'
#   attr(asym_non_weight_matrix, 'symmetric') <- FALSE
#
#   summary_actor_result <- summary_actor(asym_non_weight_matrix)
#
#   # convert the output into conflictNet object
#   class(summary_result) <- 'conflictNet'
#
#   # the test
#   expect_identical(summary_actor_result, summary_result)
#
# })
#
# ############################################
# test_that('summary_actor: longitudinal list & symmetric', {
#
#   # create data
#   actor1 <- rep(c('a','a','b', 'b', 'c','c'), 3)
#   actor2 <- rep(c('b', 'c','a','c','b','a'),3)
#   value <- c(c(2,4,2,7,7,4), c(4,5,4,6,6,5), c(11,2,11,3,3,2))
#   time <- rep(1:3, each = 6)
#   long_sym_weight_df <- data.frame(actor1=actor1,actor2=actor2, value = value, time = time)
#
#   # create matrix
#   long_sym_weight_matrix <- matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3],letters[1:3]))
#
#   # create an empty list object to store matrices
#   result <- list()
#   times = sort(unique(long_sym_weight_df[,"time"]))
#   for(i in times) {
#     slice <- long_sym_weight_df[long_sym_weight_df[,"time"] == i,]
#
#     for(ii in 1:nrow(slice)){
#       long_sym_weight_matrix[slice$actor1[ii],slice$actor2[ii]] = slice$value[ii]
#
#       result[[i]] <- long_sym_weight_matrix} }
#
#   names(result) <- times
#
#   # calculate centrality scores
#
#   # degree centrality
#
#   out <- list()
#   for(i in 1:length(result)){
#     average_degree_in <- rowMeans(result[[i]], na.rm=TRUE)
#     average_degree_out <- colMeans(result[[i]], na.rm=TRUE)
#     average_degree_total <- rowMeans(result[[i]], na.rm=TRUE) + colMeans(result[[i]], na.rm=TRUE)
#
#
#     # eigenvector centrality
#     decomp = eigen(result[[i]])
#     decompVectors = decomp$vectors[,1]
#
#     if(any(is.complex(decompVectors))){eigenvec <- NA}
#     else{eigenvec <- decompVectors/max(decompVectors)}
#     print("eigenvec is not well-defined for directed network")
#
#     # authority score
#     decomp <- svd(result[[i]])
#     decompV <- decomp$v[,1]
#     authority <- scale(decompV)
#
#     # hub score
#     decompU <- decomp$u[,1]
#     hub <- scale(decompU)
#
#
#     out[[i]] <- data.frame(
#       average_degree_in=average_degree_in, average_degree_out=average_degree_out, average_degree_total=average_degree_total,
#       eigenvec = eigenvec, authority = authority, hub = hub
#     )
#   }
#
#   out <- do.call('rbind', out)
#   out <- data.frame(out, stringsAsFactors=FALSE)
#
#   out$actor <- rep(c('a', 'b', 'c'), 3)
#   out$time <- as.double(rep(1:3, each=3))
#   rownames(out) <- NULL
#
#   vars <- c('actor', "time")
#   out <- out[,c(vars, setdiff(names(out), vars))]
#
#   summary_result <- out
#
#   # convert into conflictNet object
#   class(result) <- 'conflictNet'
#   attr(result, 'conflictNetType') <- 'longit_list'
#   attr(result, 'symmetric') <- TRUE
#   summary_actor_result <- summary_actor(result)
#
#   # convert the output into conflictNet object
#   class(summary_result) <- 'conflictNet'
#   # the test
#   expect_identical(summary_actor_result, summary_result)
#
# })
#
# ############################################
# test_that('summary_actor: longitudinal list & asymmetric', {
#
#   long_asym_weight_df <- expand.grid(actor1=letters[1:3],actor2=letters[1:3], time = 1:10, stringsAsFactors=FALSE)
#   long_asym_weight_df$value <- rnorm(nrow(long_asym_weight_df))
#   long_asym_weight_df <- long_asym_weight_df[long_asym_weight_df$actor1!=long_asym_weight_df$actor2,]
#
#   # create matrix
#   long_asym_weight_matrix <- matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3],letters[1:3]))
#
#   # create an empty list object to store matrices
#   times <- sort(unique(long_asym_weight_df[,"time"]))
#   result <- lapply(times, function(t){
#
#     # slice to relevant time period
#     slice <- long_asym_weight_df[long_asym_weight_df[,"time"] == t,]
#
#     # fill in
#     for(ii in 1:nrow(slice)){
#       long_asym_weight_matrix[slice$actor1[ii],slice$actor2[ii]] = slice$value[ii] }
#
#     #
#     return(long_asym_weight_matrix) })
#   names(result) <- times
#   # calculate centrality scores
#
#   # degree centrality
#
#   out <- list()
#   for(i in 1:length(result)){
#     average_degree_in <- rowMeans(result[[i]], na.rm=TRUE)
#     average_degree_out <- colMeans(result[[i]], na.rm=TRUE)
#     average_degree_total <- rowMeans(result[[i]], na.rm=TRUE) + colMeans(result[[i]], na.rm=TRUE)
#
#
#
#     # eigenvector centrality
#     decomp = eigen(result[[i]])
#     decompVectors = decomp$vectors[,1]
#
#     if(any(is.complex(decompVectors))){eigenvec <- NA}
#     else{eigenvec <- decompVectors/max(decompVectors)}
#
#     # authority score
#     decomp <- svd(result[[i]])
#     decompV <- decomp$v[,1]
#     authority <- scale(decompV)
#
#     # hub score
#     decompU <- decomp$u[,1]
#     hub <- scale(decompU)
#
#
#     out[[i]] <- data.frame(
#       average_degree_in=average_degree_in, average_degree_out=average_degree_out, average_degree_total=average_degree_total,
#       eigenvec = eigenvec, authority = authority, hub = hub
#     )
#   }
#
#   out <- do.call('rbind', out)
#   out <- data.frame(out, stringsAsFactors=FALSE)
#
#   out$actor <- rep(c('a', 'b', 'c'), 10)
#   out$time <- as.double(rep(1:10, each=3))
#   rownames(out) <- NULL
#
#   vars <- c('actor', "time")
#   out <- out[,c(vars, setdiff(names(out), vars))]
#
#   summary_result <- out
#
#   # convert into conflictNet object
#   class(result) <- 'conflictNet'
#   attr(result, 'conflictNetType') <- 'longit_list'
#   attr(result, 'symmetric') <- FALSE
#
#   summary_actor_result <- summary_actor(result)
#
#   # convert the output into conflictNet object
#   class(summary_result) <- 'conflictNet'
#
#   # the test
#   expect_identical(summary_actor_result, summary_result)
#
# })
#
#
#
