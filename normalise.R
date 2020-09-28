normalise <- function(df) {
df$CoreSize <- log(df$CoreSize) 
df$HydroSize <- log(df$HydroSize)
df$SurfArea <- log(df$SurfArea)
df$SurfCharge <- scale(df$SurfCharge, center =TRUE, scale =TRUE)
df$Ec <- log(6.17+df$Ec)
df$Dosage <- log10(df$Dosage) 
df$Time <- log((df$Time)/3,2)
df$eNeg <- log(df$eNeg) 
df
}
