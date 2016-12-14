library(RMySQL)
library(MASS)
library(xgboost)
library(dplyr)
library(tidyr)
library(bulletr)
library(parallel)
library(randomForest)

## TODO
# DONE Determine amount of overlap and use as feature in analysis
# DONE RF/SVM/Logistic/xgboost
# DONE put password in filename thats hidden

dbname <- "bullets"
user <- "buser"
password <- readLines("code/buser_pass.txt")
host <- "127.0.0.1"

con <- dbConnect(MySQL(), user = user, password = password,
                 dbname = dbname, host = host)

runs <- dbReadTable(con, "runs")

###
### Parallel Infrastructure
###
no_cores <- detectCores()
cl <- makeCluster(no_cores, outfile = "", renice = 0)

###
### Bullet Loading
###
all_bullets_metadata <- dbReadTable(con, "metadata")
all_bullets <- parLapply(cl, all_bullets_metadata$name, function(x) {
    library(bulletr)
    
    result <- read_x3p(x)
    result$path <- x
    
    return(result)
})

###
### Ideal Crosscut
###
crosscut_xmin <- runs$crosscut_xmin[1]
crosscut_xmax <- runs$crosscut_xmax[1]
crosscut_distance <- runs$crosscut_distance[1]

clusterExport(cl, varlist = c("crosscut_xmin", "crosscut_xmax", "crosscut_distance"), envir = environment())
crosscuts <- parLapply(cl, all_bullets, function(x) {
    library(bulletr)
    
    cat(x$path, " ")
    crosscut <- bulletCheckCrossCut(x$path, bullet = x, xlimits = c(crosscut_xmin, crosscut_xmax), distance = crosscut_distance)
    cat(crosscut)
    cat("\n")
    names(crosscut) <- x$path

    crosscut
})

###
### Groove Detection
###
groove_smoothfactor <- runs$groove_smoothfactor[1]

clusterExport(cl, varlist = c("groove_smoothfactor"), envir = environment())
groove_locations <- parLapply(cl, all_bullets, function(bul) {
    cat(bul$path, "\n")
    
    fortified <- fortify_x3p(bul)
    
    all_grooves <- lapply(unique(fortified$x), function(x) {
        cat("Processing", x, "\n")
        cc <- get_crosscut(bullet = bul, x = x)
        result <- get_grooves(cc, smoothfactor = groove_smoothfactor)
        
        return(result$groove)
    })
    
    result <- cbind(data.frame(bullet = bul$path, x = unique(fortified$x)), do.call(rbind, all_grooves))
    names(result) <- c("bullet", "x", "groove_left", "groove_right")
    
    return(result)
})

groove.locs <- do.call(rbind, groove_locations)
maxid <- dbGetQuery(con, "SELECT MAX(id) FROM profiles")[1,1]
if (is.na(maxid)) maxid <- 0
groove.locs <- groove.locs %>% 
    left_join(select(all_bullets_metadata, id, name), by = c("bullet" = "name")) %>%
    mutate(id = (maxid + 1):(maxid + nrow(.)),
           run_id = 1) %>%
    select(id, land_id = bullet, run_id, x, groove_left, groove_right)

###
### Robust Grooves
###
res <- groove.locs %>%  tidyr::nest(-land_id)

minquant <- runs$groove_minquant[1]
maxquant <- runs$groove_maxquant[1]
res$grooves <- res$data %>% purrr::map(
    .f = function(d) {
        groove_right_pred = d$groove_right
        groove_left_pred = d$groove_left
        right_twist = NA
        left_twist = NA
        
        #    d <- d %>% mutate(
        #      groove_right = replace(groove_right, groove_right == max(groove_right), NA),
        #      groove_left = replace(groove_left, groove_left == min(groove_left), NA)
        #    )
        d <- d %>% mutate(
            groove_right = replace(groove_right, groove_right < quantile(groove_right, minquant, na.rm=TRUE), NA),
            groove_right = replace(groove_right, groove_right > quantile(groove_right, maxquant, na.rm=TRUE), NA),
            groove_left = replace(groove_left, groove_left < quantile(groove_left, minquant, na.rm=TRUE), NA),
            groove_left = replace(groove_left, groove_left > quantile(groove_left, maxquant, na.rm=TRUE), NA),
            groove_right = replace(groove_right, groove_right == max(groove_right, na.rm = TRUE), NA),
            groove_left = replace(groove_left, groove_left == min(groove_left, na.rm = TRUE), NA)
        )
        
        right_sample = sum(!is.na(d$groove_right))
        if (!all(is.na(d$groove_right))) {
            rr <- try({rlm(data=d, groove_right~x)})
            if (!inherits(rr, "try-error")) {
                groove_right_pred=predict(rr, d)
                right_twist = rr$coefficients[2]
            }
        }
        left_sample = sum(!is.na(d$groove_right))
        if (!all(is.na(d$groove_left))) {
            rl <- try({rlm(data=d, groove_left~x)})
            if (!inherits(rl, "try-error")) {
                groove_left_pred= predict(rl,d)
                left_twist = rl$coefficients[2]
            }
        }
        data.frame(groove_left_pred, groove_right_pred, 
                   right_twist=right_twist, left_twist= left_twist, 
                   right_sample = right_sample, left_sample = left_sample)
    }
)
grooves_robust <- res %>% unnest() %>% as.data.frame()
profiles <- grooves_robust %>% 
    left_join(select(all_bullets_metadata, id, name), by = c("land_id" = "name")) %>%
    select(id = id.x, land_id = id.y, run_id, x, groove_left, groove_right, groove_left_pred, groove_right_pred)
derived_metadata <- grooves_robust %>% 
    left_join(select(all_bullets_metadata, id, name), by = c("land_id" = "name")) %>%
    group_by(land_id) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(id.y) %>%
    mutate(ideal_crosscut = unlist(crosscuts)) %>%
    select(id = id.y, run_id, ideal_crosscut, left_twist, right_twist, left_sample, right_sample)

dbWriteTable(con, "metadata_derived", derived_metadata, row.names = FALSE, append = TRUE)
dbWriteTable(con, "profiles", profiles, row.names = FALSE, append = TRUE)

###
### Signatures
###
new_derived <- derived_metadata %>%
    left_join(select(all_bullets_metadata, id, name))
new_grooves <- grooves %>%
    left_join(select(all_bullets_metadata, id, name), by = c("land_id" = "id"))
clusterExport(cl, varlist = c("new_derived", "new_grooves"), envir = environment())

all_bullets_included <- all_bullets[which(!is.na(new_derived$ideal_crosscut))]
bullets_processed <- parLapply(cl, all_bullets_included, function(bul) {
    library(bulletr)
    library(dplyr)
    
    cat("Computing processed bullet", basename(bul$path), "\n")
    
    xval <- new_derived$ideal_crosscut[which(new_derived$name == bul$path)]
    
    grooves_sub <- filter(new_grooves, name == bul$path)
    
    left <- grooves_sub$groove_left_pred[which.min(abs(xval - grooves_sub$x))]
    right <- grooves_sub$groove_right_pred[which.min(abs(xval - grooves_sub$x))]
    
    processBullets(bullet = bul, name = bul$path, x = grooves_sub$x[which.min(abs(xval - grooves_sub$x))], grooves = c(left, right))
})

bullet_span <- runs$bullet_span[1]
bullets_smoothed <- bullets_processed %>% 
    bind_rows %>%
    bulletSmooth(span = bullet_span) %>%
    left_join(select(all_bullets_metadata, id, name), by = c("bullet" = "name")) %>%
    left_join(select(grooves, id, land_id, x), by = c("id" = "land_id", "x" = "x")) %>%
    ungroup() %>%
    select(-bullet, -id, -x, -abs_resid, -chop) %>%
    select(profile_id = id.y, everything()) %>%
    mutate(id = 1:nrow(.), run_id = 1) %>%
    select(id, profile_id, run_id, everything())
dbWriteTable(con, "signatures", bullets_smoothed, row.names = FALSE, append = TRUE)

###
### Comparisons
###
compares <- dbReadTable(con, "compares")

relevant_ids1 <- filter(bullets_smoothed, run_id == compares$run1_id)
relevant_ids2 <- filter(bullets_smoothed, run_id == compares$run2_id)
all_combinations <- combn(unique(c(relevant_ids1$profile_id, relevant_ids2$profile_id)), 2, simplify = FALSE)

peaks_smoothfactor <- compares$peaks_smoothfactor[1]
clusterExport(cl, varlist = c("peaks_smoothfactor", "bullets_smoothed"), envir = environment())
all_comparisons <- parLapply(cl, all_combinations, function(x) {
    library(dplyr)
    library(bulletr)
    
    cat(x, "\n")
    
    br1 <- filter(bullets_smoothed, profile_id == x[1]) %>%
        select(-id) %>%
        rename(bullet = profile_id)
    br2 <- filter(bullets_smoothed, profile_id == x[2]) %>%
        select(-id) %>%
        rename(bullet = profile_id)
    
    bulletGetMaxCMS(br1, br2, column = "l30", span = peaks_smoothfactor)
})

###
### Random Forest
###
ccf_temp <- parLapply(cl, all_comparisons, function(res) {
    if (is.null(res)) return(NULL)
    lofX <- res$bullets
    b12 <- unique(lofX$bullet)
    
    subLOFx1 <- subset(lofX, bullet==b12[1])
    subLOFx2 <- subset(lofX, bullet==b12[2]) 
    
    ys <- intersect(subLOFx1$y, subLOFx2$y)
    idx1 <- which(subLOFx1$y %in% ys)
    idx2 <- which(subLOFx2$y %in% ys)
    distr.dist <- mean((subLOFx1$val[idx1] - subLOFx2$val[idx2])^2, na.rm=TRUE)
    distr.sd <- sd(subLOFx1$val, na.rm=TRUE) + sd(subLOFx2$val, na.rm=TRUE)
    km <- which(res$lines$match)
    knm <- which(!res$lines$match)
    if (length(km) == 0) km <- c(length(knm)+1,0)
    if (length(knm) == 0) knm <- c(length(km)+1,0)
    #browser()    
    # feature extraction
    signature.length <- min(nrow(subLOFx1), nrow(subLOFx2))
    
    c(ccf=res$ccf, lag=res$lag, 
               D=distr.dist, 
               sd.D = distr.sd,
               b1=b12[1], b2=b12[2],
               signature.length = signature.length,
               overlap = length(ys),
               matches.per.y = sum(res$lines$match) / signature.length,
               mismatches.per.y = sum(!res$lines$match) / signature.length,
               cms.per.y = res$maxCMS / signature.length,
               cms2.per.y = bulletr::maxCMS(subset(res$lines, type==1 | is.na(type))$match) / signature.length,
               non_cms.per.y = bulletr::maxCMS(!res$lines$match) / signature.length,
               left_cms.per.y = max(knm[1] - km[1], 0) / signature.length,
               right_cms.per.y = max(km[length(km)] - knm[length(knm)],0) / signature.length,
               left_noncms.per.y = max(km[1] - knm[1], 0) / signature.length,
               right_noncms.per.y = max(knm[length(knm)]-km[length(km)],0) / signature.length,
               sumpeaks.per.y = sum(abs(res$lines$heights[res$lines$match])) / signature.length
    )
})
ccf <- as.data.frame(do.call(rbind, ccf_temp)) %>%
    mutate(id = 1:nrow(.), compare_id = compares$id[1]) %>%
    select(id, compare_id, profile1_id = b1, profile2_id = b2, ccf, lag, D, sd.D, signature.length, overlap,
           matches.per.y, mismatches.per.y, cms.per.y, non_cms.per.y, sumpeaks.per.y)
dbWriteTable(con, "ccf", ccf, row.names = FALSE, append = TRUE)

my_matches <- dbReadTable(con, "matches")
CCFs <- ccf %>%
    left_join(select(profiles, id, land_id), by = c("profile1_id" = "id")) %>%
    left_join(select(profiles, id, land_id), by = c("profile2_id" = "id")) %>%
    left_join(my_matches, by = c("land_id.x" = "land1_id", "land_id.y" = "land2_id")) %>%
    mutate(match = as.logical(match)) %>%
    select(-land_id.x, -land_id.y)

includes <- setdiff(names(CCFs), c("id", "compare_id", "profile1_id", "profile2_id"))

rtrees <- randomForest(factor(match) ~ ., data = CCFs[,includes], ntree = 300)
CCFs$forest <- predict(rtrees, type = "prob")[,2]
imp <- data.frame(importance(rtrees))
xtabs(~(forest > 0.5) + match, data = CCFs)

###
### XGBoost
###
mymat <- as.matrix(CCFs[,setdiff(includes, "match")])
mylab <-  as.numeric(CCFs$match)
xgmodel <- xgboost(data = mymat, label = mylab, nrounds = 2)
CCFs$xgboost <- predict(xgmodel, mymat)
xtabs(~(xgboost > 0.5) + match, data = CCFs)
