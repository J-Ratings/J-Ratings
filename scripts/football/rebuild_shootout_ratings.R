# Run manually: full historical Elo rebuild for both football sports.
# No website downloads, Git commands, or publication.
repo_dir <- normalizePath(Sys.getenv("J_RATINGS_REPO", getwd()), winslash="/", mustWork=TRUE)
if (!file.exists(file.path(repo_dir,"scripts/football/elo_decisions.R"))) stop("Run this from the J-Ratings repository root.")
Sys.setenv(J_RATINGS_REPO=repo_dir)
backup_dir <- file.path(repo_dir,"EuropeanFootball/pipeline_data/Elo",paste0("before_shootout_wins_",format(Sys.time(),"%Y%m%d_%H%M%S")))
dir.create(backup_dir, recursive=TRUE, showWarnings=FALSE)
for (name in c("checkpoint_2024_12_31.rds","json_export_checkpoint_2024_12_31.rds")) {
  original <- file.path(repo_dir,"EuropeanFootball/pipeline_data/Elo",name)
  if (file.exists(original) && !file.copy(original,file.path(backup_dir,name),overwrite=FALSE)) stop("Checkpoint backup failed: ",name)
}
cat("Original Club Football checkpoints backed up to:\n",backup_dir,"\n")
# Marker forces a full rebuild even if an earlier attempt produced a new checkpoint.
writeLines("Full history rebuild requested for shootout wins.",file.path(repo_dir,"EuropeanFootball/pipeline_data/Elo/rebuild_required.txt"))
run_stage <- function(path) {
  cat("\nRunning ",path,"\n",sep="")
  source(file.path(repo_dir,path),local=new.env(parent=globalenv()))
}
run_stage("scripts/EuropeanFootball/02_calculate_elo.R")
run_stage("scripts/EuropeanFootball/03_write_json.R")
run_stage("scripts/world_football/02_calculate_elo.R")
run_stage("scripts/world_football/03_write_json.R")
cat("\nBoth football histories and JSON exports rebuilt with shootout wins. Nothing pushed.\n")
