source("global.R")
source("playersDataFunctions.R")
source("courseOverview.R")
source("utils.R")
source("basics.R")

curr_course <- "Torrey Pines Golf Course (South Course)"
playersInTournament <- unique(salaries$player)
playersInTournamentTourneyNameConv <- nameFanduelToTournament(playersInTournament)
playersInTournamentPgaNames <- nameFanduelToPga(playersInTournament)

favorite_players <- c()

# Make Course Fit Data for Current Course
course_fit_cache <- list()
course_fit_cache[[curr_course]] <- makeProjFitData(curr_course, favorite_players, playersInTournament)
saveRDS(course_fit_cache, "course_fit_cache.rds")

# Make Ott Data for Current Course
ott_cache <- list()
ott_cache[[curr_course]] <- makeCourseOttData(curr_course, favorite_players, playersInTournament)
saveRDS(ott_cache, "ott_cache.rds")

# Make Ovr Course Data for Current Course
ovr_cache <- list()
ovr_cache[[curr_course]] <- makeCourseOvrData(curr_course, favorite_players, playersInTournament)
saveRDS(ovr_cache, "ovr_cache.rds")