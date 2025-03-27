# library(terra)
# library(Rcpp)
#
# # ========== USER CONFIG ==========
# flow_path <- "C:/Thesis/desertHydro/Results/log-test/flow_direction.tif"
# output_path <- "flow_stack_Cpp_All.tif"
# # =================================
#
# # ---- Load flow direction raster ----
# flow_d8 <- rast(flow_path)
#
# # ---- Create discharge raster (1 where flow direction exists) ----
# discharge <- classify(flow_d8, cbind(NA, NA, NA), others = 1)
# values(discharge)[is.na(values(flow_d8))] <- NA
# names(discharge) <- "discharge"
#
# # ---- Define D8 flow key ----
# flowKey <- list(
#   "0" = list("no-flow", c(0, 0), 1),
#   "1" = list("east", c(1, 0), 1),
#   "2" = list("south-east", c(1, -1), sqrt(2)),
#   "4" = list("south", c(0, -1), 1),
#   "8" = list("south-west", c(-1, -1), sqrt(2)),
#   "16" = list("west", c(-1, 0), 1),
#   "32" = list("north-west", c(-1, 1), sqrt(2)),
#   "64" = list("north", c(0, 1), 1),
#   "128" = list("north-east", c(1, 1), sqrt(2))
# )
#
# # ---- Embed and compile C++ multi-direction flow routing ----
# cppFunction('
# #include <Rcpp.h>
# using namespace Rcpp;
#
# const int d8_codes[8] = {1, 2, 4, 8, 16, 32, 64, 128};
# const int dxs[8] = {1, 1, 0, -1, -1, -1, 0, 1};
# const int dys[8] = {0, -1, -1, -1, 0, 1, 1, 1};
# const double distances[8] = {1.0, 1.4142, 1.0, 1.4142, 1.0, 1.4142, 1.0, 1.4142};
#
# // [[Rcpp::export]]
# List shiftDischargeAllCpp(NumericMatrix discharge, IntegerMatrix flow_d8) {
#   int nrow = discharge.nrow();
#   int ncol = discharge.ncol();
#
#   List result(8);
#   for (int d = 0; d < 8; ++d) {
#     NumericMatrix layer(nrow, ncol);
#     int code = d8_codes[d];
#     int dx = dxs[d];
#     int dy = dys[d];
#     double dist = distances[d];
#
#     for (int i = 0; i < nrow; ++i) {
#       for (int j = 0; j < ncol; ++j) {
#         if (flow_d8(i, j) == code) {
#           int ni = i + dy;
#           int nj = j + dx;
#           if (ni >= 0 && ni < nrow && nj >= 0 && nj < ncol) {
#             layer(ni, nj) += discharge(i, j) / dist;
#           }
#         }
#       }
#     }
#     result[d] = layer;
#   }
#
#   return result;
# }
# ')
#
# # ---- Run all D8 directions with correct matrix orientation ----
# run_all_cpp_directions <- function(discharge, flow_d8, flowKey) {
#   d8_codes <- c(1, 2, 4, 8, 16, 32, 64, 128)
#   dir_names <- sapply(as.character(d8_codes), function(k) flowKey[[k]][[1]])
#
#   # ---- Clean flow direction matrix (correct orientation) ----
#   flow_vals <- as.numeric(values(flow_d8))
#   flow_vals[is.na(flow_vals)] <- 0
#   flow_vals <- as.integer(flow_vals)
#   flow_vals[!flow_vals %in% d8_codes] <- 0
#   flow_mat <- matrix(flow_vals, nrow = nrow(flow_d8))  # no byrow
#
#   # ---- Clean discharge matrix ----
#   discharge_vals <- as.numeric(values(discharge))
#   discharge_vals[is.na(discharge_vals)] <- 0
#   discharge_mat <- matrix(discharge_vals, nrow = nrow(discharge))  # no byrow
#
#   # ---- Run C++ routing ----
#   cpp_layers <- shiftDischargeAllCpp(discharge_mat, flow_mat)
#
#   # ---- Convert matrices to rasters ----
#   cpp_rasters <- list()
#   for (i in seq_along(cpp_layers)) {
#     m <- cpp_layers[[i]]
#
#     if (all(m == 0 | is.na(m))) {
#       m[,] <- NA
#     }
#
#     r <- rast(m)
#     ext(r) <- ext(discharge)
#     crs(r) <- crs(discharge)
#     res(r) <- res(discharge)
#     names(r) <- dir_names[i]
#     cpp_rasters[[length(cpp_rasters) + 1]] <- r
#   }
#
#   # ---- Fallback layer if all are empty ----
#   if (length(cpp_rasters) == 0 || all(sapply(cpp_rasters, function(r) all(is.na(values(r)))))) {
#     dummy <- setValues(rast(discharge), NA)
#     names(dummy) <- "no_flow"
#     cpp_rasters <- list(dummy)
#     cat("⚠️ All layers empty — added fallback.\n")
#   }
#
#   return(rast(cpp_rasters))
# }
#
# # ---- Execute and save output ----
# flow_stack_cpp <- run_all_cpp_directions(discharge, flow_d8, flowKey)
#
# # Show routed flow stats per direction
# cat("🔍 Non-empty layer totals:\n")
# print(global(flow_stack_cpp, "sum", na.rm = TRUE))
#
# # Export
# writeRaster(flow_stack_cpp, output_path, overwrite = TRUE)
#
# cat("✅ Export complete:", output_path, "\n")
