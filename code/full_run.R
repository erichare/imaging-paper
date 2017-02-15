library(RMySQL)
library(MASS)
library(xgboost)
library(dplyr)
library(tidyr)
library(bulletr)
library(parallel)
library(randomForest)
library(ggplot2)
library(stringr)

###
### DB Connection
###
dbname <- "bullets"
user <- "buser"
password <- readLines("buser_pass.txt")
host <- "10.25.122.176"

con <- dbConnect(MySQL(), user = user, password = password,
                 dbname = dbname, host = host)

runs <- dbReadTable(con, "runs") %>% filter(run_id == max(run_id))

###
### Parallel Infrastructure
###
no_cores <- detectCores()
cl <- makeCluster(no_cores, outfile = "", renice = 0)

###
### File Name Back Transform
###
get_filename <- function(study, barrel, bullet, land) {
    if (study == "Hamby44") {
        return(file.path("images", "Hamby (2009) Barrel", "bullets", 
                         paste0("br", barrel, "_", bullet, "_land", land, ".x3p")))
    } else if (study == "Hamby252") {
        if (barrel %in% LETTERS) {
            return(file.path("images", "Hamby (2009) Barrel", "bullets", 
                             paste0("Ukn Bullet ", barrel, "-", land, ".x3p")))
        } else {
            return(file.path("images", "Hamby (2009) Barrel", "bullets", 
                            paste0("Br", barrel, " Bullet ", bullet, "-", land, ".x3p")))
        }
    } else if (study == "Cary") {
        return(file.path("images", "Cary Persistence", "bullets", 
                         paste0("CWBLT", str_pad(bullet, 4, pad = "0"), "-1.x3p")))
    } else {
        return("Not Found")
    }
}

###
### Bullet Loading
###
all_bullets_metadata <- dbReadTable(con, "metadata")
all_bullets_filenames <- character(nrow(all_bullets_metadata))
for (i in 1:nrow(all_bullets_metadata)) {
    x <- all_bullets_metadata[i,]
    all_bullets_filenames[i] <- get_filename(x[2], x[3], x[4], x[5])
}
all_bullets_metadata$name <- all_bullets_filenames
all_bullets <- parLapply(cl, all_bullets_filenames, function(x) {
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
crosscut_window <- runs$crosscut_window[1]

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
new_derived <- dbReadTable(con, "metadata_derived") %>%
    left_join(select(all_bullets_metadata, land_id, name))
new_grooves <- dbReadTable(con, "profiles") %>%
    left_join(select(all_bullets_metadata, land_id, name))
clusterExport(cl, varlist = c("new_derived", "new_grooves", "crosscut_window"), envir = environment())

all_bullets_included <- all_bullets[which(!is.na(new_derived$ideal_crosscut))]
bullets_processed <- parLapply(cl, all_bullets_included, function(bul) {
    library(bulletr)
    library(dplyr)
    
    cat("Computing processed bullet", basename(bul$path), "with window", crosscut_window, "\n")
    
    xval <- new_derived$ideal_crosscut[which(new_derived$name == bul$path)]
    
    grooves_sub <- filter(new_grooves, name == bul$path)
    
    left <- grooves_sub$groove_left_pred[which.min(abs(xval - grooves_sub$x))]
    right <- grooves_sub$groove_right_pred[which.min(abs(xval - grooves_sub$x))]
    
    processBullets(bullet = bul, name = bul$path, x = grooves_sub$x[which.min(abs(xval - grooves_sub$x))], grooves = c(left, right), window = crosscut_window)
})

bullet_span <- runs$bullet_span[1]
maxsigid <- max(dbReadTable(con, "signatures")$signature_id)
bullets_smoothed <- bullets_processed %>% 
    bind_rows %>%
    bulletSmooth(span = bullet_span) %>%
    left_join(select(all_bullets_metadata, land_id, name), by = c("bullet" = "name")) %>%
    left_join(select(new_grooves, profile_id, land_id, x), by = c("land_id" = "land_id", "x" = "x")) %>%
    ungroup() %>%
    select(-bullet, -land_id, -x, -abs_resid, -chop) %>%
    mutate(signature_id = (maxsigid + 1):(maxsigid + nrow(.)), run_id = runs$run_id[1]) %>%
    select(signature_id, profile_id, run_id, everything())
dbWriteTable(con, "signatures", bullets_smoothed, row.names = FALSE, append = TRUE)

###
### Comparisons
###
compares <- dbReadTable(con, "compares") %>% filter(compare_id == max(compare_id))

bullets_smoothed <- dbReadTable(con, "signatures") %>% filter(run_id %in% c(compares$run1_id[1], compares$run2_id[1]))

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
        select(-signature_id, -run_id) %>%
        rename(bullet = profile_id) %>%
        filter(!is.na(l30))
    br2 <- filter(bullets_smoothed, profile_id == x[2]) %>%
        select(-signature_id, -run_id) %>%
        rename(bullet = profile_id) %>%
        filter(!is.na(l30))
    
    bulletGetMaxCMS(br1, br2, column = "l30", span = peaks_smoothfactor)
})

compare_doublesmooth <- compares$peaks_doublesmooth[1]
clusterExport(cl, varlist = c("compare_doublesmooth"), envir = environment())

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
    
    doublesmoothed <- lofX %>%
        group_by(y) %>%
        mutate(avgl30 = mean(l30, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(smoothavgl30 = smoothloess(x = y, y = avgl30, span = compare_doublesmooth),
               l50 = l30 - smoothavgl30)
    
    final_doublesmoothed <- doublesmoothed %>%
        filter(y %in% ys)
    
    double_cor <- cor(na.omit(final_doublesmoothed$l50[final_doublesmoothed$bullet == b12[1]]), 
                      na.omit(final_doublesmoothed$l50[final_doublesmoothed$bullet == b12[2]]),
                      use = "pairwise.complete.obs")
    
    c(ccf=res$ccf, double_cor = double_cor, lag=res$lag / 1000, 
               D=distr.dist, 
               sd_D = distr.sd,
               b1=b12[1], b2=b12[2],
               signature_length = signature.length * 1.5625 / 1000,
               overlap = length(ys) * 1.5625 / signature.length / 1000,
               matches = sum(res$lines$match) * (1000 / 1.5625) / length(ys),
               mismatches = sum(!res$lines$match) * (1000 / 1.5625) / length(ys),
               cms = res$maxCMS * (1000 / 1.5625) / length(ys),
               cms2 = bulletr::maxCMS(subset(res$lines, type==1 | is.na(type))$match) * (1000 / 1.5625) / length(ys),
               non_cms = bulletr::maxCMS(!res$lines$match) * (1000 / 1.5625) / length(ys),
               left_cms = max(knm[1] - km[1], 0) * (1000 / 1.5625) / length(ys),
               right_cms = max(km[length(km)] - knm[length(knm)],0) * (1000 / 1.5625) / length(ys),
               left_noncms = max(km[1] - knm[1], 0) * (1000 / 1.5625) / length(ys),
               right_noncms = max(knm[length(knm)]-km[length(km)],0) * (1000 / 1.5625) / length(ys),
               sumpeaks = sum(abs(res$lines$heights[res$lines$match])) * (1000 / 1.5625) / length(ys)
    )
})
ccf <- as.data.frame(do.call(rbind, ccf_temp)) %>%
    mutate(compare_id = compares$compare_id[1]) %>%
    select(compare_id, profile1_id = b1, profile2_id = b2, ccf, double_cor, lag, D, sd_D, signature_length, overlap,
           matches, mismatches, cms, non_cms, sumpeaks)
save(ccf, file = "ccf_compare2.RData")
dbWriteTable(con, "ccf", ccf, row.names = FALSE, append = TRUE)

my_matches <- dbReadTable(con, "matches")
profiles <- dbReadTable(con, "profiles")

CCFs_withlands <- ccf %>%
    left_join(select(profiles, profile_id, land_id), by = c("profile1_id" = "profile_id")) %>%
    left_join(select(profiles, profile_id, land_id), by = c("profile2_id" = "profile_id")) %>%
    left_join(my_matches, by = c("land_id.x" = "land1_id", "land_id.y" = "land2_id")) %>%
    mutate(match = as.logical(match)) 

CCFs <- CCFs_withlands %>%
    select(-land_id.x, -land_id.y)

includes <- setdiff(names(CCFs), c("compare_id", "profile1_id", "profile2_id"))

rtrees <- randomForest(factor(match) ~ ., data = CCFs[,includes], ntree = 300)
CCFs$forest <- predict(rtrees, type = "prob")[,2]
imp <- data.frame(importance(rtrees))
xtabs(~(forest > 0.5) + match, data = CCFs)

CCFs_set252 <- CCFs_withlands %>%
    left_join(select(all_bullets_metadata, land_id, study, barrel, bullet, land), by = c("land_id.x" = "land_id")) %>%
    left_join(select(all_bullets_metadata, land_id, study, barrel, bullet, land), by = c("land_id.y" = "land_id")) %>%
    filter(study.x == "Hamby252", study.y == "Hamby252")

rtrees_252 <- randomForest(factor(match) ~ ., data = CCFs_set252[,includes], ntree = 300)
CCFs_set252$forest <- predict(rtrees_252, type = "prob")[,2]
imp <- data.frame(importance(rtrees_252))
xtabs(~(forest > 0.5) + match, data = CCFs_set252)

CCFs_set44 <- CCFs_withlands %>%
    left_join(select(all_bullets_metadata, land_id, study, barrel, bullet, land), by = c("land_id.x" = "land_id")) %>%
    left_join(select(all_bullets_metadata, land_id, study, barrel, bullet, land), by = c("land_id.y" = "land_id")) %>%
    filter(study.x == "Hamby44", study.y == "Hamby44")

rtrees_44 <- randomForest(factor(match) ~ ., data = CCFs_set44[,includes], ntree = 300)
CCFs_set44$forest <- predict(rtrees_44, type = "prob")[,2]
imp <- data.frame(importance(rtrees_44))
xtabs(~(forest > 0.5) + match, data = CCFs_set44)

CCFs_cary <- CCFs_withlands %>%
    left_join(select(all_bullets_metadata, land_id, study, barrel, bullet, land), by = c("land_id.x" = "land_id")) %>%
    left_join(select(all_bullets_metadata, land_id, study, barrel, bullet, land), by = c("land_id.y" = "land_id")) %>%
    filter(study.x == "Cary", study.y == "Cary")

CCFs_cary$forest <- predict(rtrees_252, newdata = CCFs_cary, type = "prob")[,2]
imp <- data.frame(importance(rtrees))
xtabs(~(forest > 0.5) + match, data = CCFs_cary)

CCFs_set252 %>%
    filter(!match) %>%
    arrange(desc(forest)) %>%
    head

CCFs_set252 %>%
    filter(match) %>%
    arrange(forest) %>%
    head
