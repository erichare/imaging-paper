library(RMySQL)
library(dplyr)
library(tidyr)
library(bulletr)

dbname <- "bullets"
user <- "buser"
password <- "TihrcdTJn#7pWv"
host <- "127.0.0.1"

con <- dbConnect(MySQL(), user = user, password = password,
                 dbname = dbname, host = host)

datadir1 <- file.path("images/Hamby (2009) Barrel/bullets", dir("images/Hamby (2009) Barrel/bullets"))
datadir2 <- file.path("images/Cary Persistence/bullets", dir("images/Cary Persistence//bullets"))

all_bullets <- lapply(c(datadir1, datadir2), function(x) {
    result <- read_x3p(x)
    result[[3]] <- x
    names(result)[3] <- "path"
    
    return(result)
})
names(all_bullets) <- c(datadir1, datadir2)

metadata <- lapply(all_bullets, `[[`, 1)
metadata_df <- data.frame(matrix(unlist(metadata), nrow = length(metadata), byrow = TRUE))
names(metadata_df) <- names(all_bullets[[1]][[1]])

metadata_df <- cbind(id = 1:nrow(metadata_df), name = c(datadir1, datadir2), metadata_df)
# dbWriteTable(con, "metadata", metadata_df, row.names = FALSE)


alldata_fort <- lapply(all_bullets, fortify_x3p)
test <- alldata_fort %>% bind_rows
bullid <- rep(metadata_df$id, times = metadata_df$num_profiles * metadata_df$num_obs_per_profile)

bullet_df <- cbind(id = 1:nrow(test), bullet_id = bullid, test)


qry <- sqlCreateTable(con, "data", head(bullet_df, n = 1000), row.names = FALSE)
res <- dbSendStatement(con, qry)

for (i in seq(1, nrow(bullet_df) - 100000, by = 100000)) {
    cat(i, "\n")
    
    endid <- min(nrow(bullet_df), (i + 99999))
    
    x <- bullet_df[i:endid,]
    x$value[is.nan(x$value)] <- NA
    qry <- sqlAppendTable(con, "data", x, row.names = FALSE)
    res <- dbSendStatement(con, qry)
    ids <- dbFetch(res)
    dbClearResult(res)
}
dbWriteTable(con, "metadata_derived", mydf, row.names = FALSE)
# dbWriteTable(con, "data", bullet_df, row.names = FALSE)

metadata <- dbReadTable(con, "metadata_bullet")
metadata_derived <- dbReadTable(con, "metadata_bullet_derived")
mygrooves <- dbReadTable(con, "metadata_profile_derived")

myjoin <- left_join(metadata, metadata_derived)
myjoin2 <- left_join(mygrooves, metadata)

bullets_processed <- lapply(all_bullets, function(bul) {
    cat("Computing processed bullet", basename(bul$path), "\n")
    
    xval <- metadata_derived$crosscut[which(myjoin$name == bul$path)]
    if (is.na(xval)) return(NA)
    
    grooves_sub <- filter(myjoin2, name == bul$path)
    
    left <- grooves_sub$groove_left_pred[which.min(abs(xval - grooves_sub$x))]
    right <- grooves_sub$groove_right_pred[which.min(abs(xval - grooves_sub$x))]
    #if (length(left) == 0 || is.na(left)) left <- grooves_sub$groove_left[which.min(abs(xval - grooves_sub$x))]
    #if (length(right) == 0 || is.na(right)) right <- grooves_sub$groove_right[which.min(abs(xval - grooves_sub$x))]
    
    processBullets(bullet = bul, name = bul$path, x = grooves_sub$x[which.min(abs(xval - grooves_sub$x))], grooves = c(left, right))
})
names(bullets_processed) <- names(all_bullets)

to_include <- sapply(bullets_processed, is.data.frame) 

bullets_smoothed <- bullets_processed[to_include] %>% 
    bind_rows %>%
    bulletSmooth %>%
    select(-x, -abs_resid, -chop) %>%
    left_join(select(metadata, id, name), by = c("bullet" = "name")) %>%
    ungroup() %>%
    select(-bullet) %>%
    select(bullet_id = id, everything()) %>%
    mutate(id = 1:nrow(.)) %>%
    select(id, everything())

dbWriteTable(con, "signatures", bullets_smoothed, row.names = FALSE)
