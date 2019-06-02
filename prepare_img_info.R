library("data.table")
library("stringr")

metadata.raw <- fread("data/metadata-v1.csv")

metadata <-
    metadata.raw[, .(
        turbine_number,
        blade_number_abc,
        blade_side,
        path_to_file,
        cam_to_blade_rbt
    )]

good <- metadata$blade_number_abc == "A" & metadata$turbine_number == "BW01"
metadata <- metadata[good,]

metadata <-
    metadata[, .(
        blade_side,
        path_to_file,
        cam_to_blade_rbt
    )]

parseCoordinatesString <- function(coordString) {
    matrix(as.double(unlist(str_extract_all(coordString, "-?\\d+.(\\d+|)"))), nrow = 4, ncol = 4, byrow = T)
}

extractFileName <- function(path) {
    str_extract(path, "snapshot\\d.+")
}

metadata[, cam_to_blade_rbt := lapply(cam_to_blade_rbt, parseCoordinatesString)]
metadata[, file := sapply(path_to_file, extractFileName)]
metadata[, path_to_file := NULL]

normalize <- function(vec) {
    vec/sqrt(sum(vec^2))
}

xprod <- function(a, b) {
    matrix(c(0,-a[3],a[2],
             a[3],0,-a[1],
            -a[2],a[1],0),
        nrow = 3, ncol = 3, byrow = T) %*% b
}

getAngle <- function(a, b, n) {
    angle = acos(normalize(a) %*% normalize(b))
    cross = xprod(a, b)
    if (n %*% cross < 0) { angle = -angle }
    angle
}

getPitchRollAngles <- function(mat) {
    x <- mat[1:3,1]
    y <- mat[1:3,2]
    z <- mat[1:3,3]
    k <- c(0,0,-1)
    f <- c(z[1]*z[3], z[2]*z[3], -(z[1]^2 + z[2]^2))
    roll <- getAngle(y, f, z)
    x <- x %*% matrix(c(cos(roll),-sin(roll),0,
                    sin(roll),cos(roll),0,
                    0,0,1),
                  nrow = 3, ncol = 3, byrow = T)
    pitch <- getAngle(f, k, x)
    c(pitch, roll)
}

sides <- c("leading-edge", "suction-side", "trailing-edge", "pressure-side")
directions <- list(c(-1,0), c(0,-1), c(1,0), c(0,1))
names(directions) <- sides


getAzimuthSin <- function(zCamVector, mainDirection) {
    zCamVector <- zCamVector[1:2]
    zCamVector <- zCamVector/sum(zCamVector^2)^0.5
    zCamVector[1]*mainDirection[2] - zCamVector[2]*mainDirection[1]
}

object.center = c(1.0, 0.5)

getDistance <- function(xyPoint) {
    sqrt(sum((xyPoint-object.center)^2))
}

# imageSize <- c(5456, 3632)

prepareSideData <- function(side, direction) {
    data <- metadata[blade_side == side]
    data <- data[order(file)]
    coords <- data[, .(file, cam_to_blade_rbt)]
    coords[,
            c("x", "y", "z", "pitch", "roll", "azimuth", "distance") := .(
                sapply(cam_to_blade_rbt, function(mat) {mat[1, 4]}),
                sapply(cam_to_blade_rbt, function(mat) {mat[2, 4]}),
                sapply(cam_to_blade_rbt, function(mat) {mat[3, 4]}),
                sapply(cam_to_blade_rbt, function(mat) {getPitchRollAngles(mat)[1]}),
                sapply(cam_to_blade_rbt, function(mat) {getPitchRollAngles(mat)[2]}),
                sapply(cam_to_blade_rbt, function(mat) {getAzimuthSin(mat[1:3, 3], direction)}),
                sapply(cam_to_blade_rbt, function(mat) {getDistance(mat[1:2, 4])})
            )
          ]
    coords$z <- coords$z - tan(coords$pitch)*coords$distance
    plot(coords$x, coords$y, type = "b", asp = 1)
    coords
}

###### leading-edge
coords.leading.edge <- prepareSideData("leading-edge", directions$`leading-edge`)
distanceToBlade <- 7.0 # 20 and 21 snapshot of leading edge
snapshot20_21_z_delta <- coords.leading.edge$z[21] - coords.leading.edge$z[20] # in meters
snapshot20_21_Ydelta <- 3565 - 407 # in pixels

R <- snapshot20_21_Ydelta / (cos(coords.leading.edge$pitch[21])^2 * snapshot20_21_z_delta/distanceToBlade) # const for camera (pixels)
# R <- 0.035 # in meters (23.2 x 15.4)

scaleFactor <- cos(coords.leading.edge$pitch[21]) * snapshot20_21_z_delta / snapshot20_21_Ydelta # meter (in object plane) per pixel on corrected image

ProjectionZ <- matrix(c(1,0,0,0,
                        0,1,0,0,
                        0,0,0,0,
                        0,0,0,1),
                      nrow = 4, ncol = 4, byrow = T)

getTransformation <- function(roll, pitch, R, xShift, azSin, distance) {
    ReconstructionY <- matrix(c(1,0,0,0,
                                0,1,tan(pitch),-tan(pitch)/R,
                                0,0,0,0,
                                0,0,0,1),
                              nrow = 4, ncol = 4, byrow = T)
    RotateZ <- matrix(c(cos(roll),-sin(roll),0,0,
                        sin(roll),cos(roll),0,0,
                        0,0,1,0,
                        0,0,0,1),
                      nrow = 4, ncol = 4, byrow = T)
    RotateX <- matrix(c(1,0,0,0,
                        0,cos(pitch),-sin(pitch),0,
                        0,sin(pitch),cos(pitch),0,
                        0,0,0,1),
                      nrow = 4, ncol = 4, byrow = T)
    Unshift <- matrix(c(1,0,0,0,
                        0,1,0,0,
                        0,0,1,0,
                        (azSin*distance - xShift * 0)/scaleFactor,0,0,1),
                      nrow = 4, ncol = 4, byrow = T)
    distanceScaleFactor = distance/distanceToBlade
    AlignDistance <- matrix(c(distanceScaleFactor,0,0,0,
                              0,distanceScaleFactor,0,0,
                              0,0,1,0,
                              0,0,0,1),
                            nrow = 4, ncol = 4, byrow = T)

    Transformation <-   RotateZ %*%
                        ReconstructionY %*%
                        RotateX %*%
                        Unshift %*%
                        AlignDistance %*%
                        ProjectionZ

    Transformation <- (Transformation/Transformation[4,4])[-3,-3]
    Transformation
}

saveAndShowStitchData <- function(data, side) {
    data[,
        transformation := mapply(
            function(rAngle, pAngle, ySh, azSin, dist) {getTransformation(rAngle, pAngle, R, ySh, azSin, dist)},
            roll,
            pitch,
            x,
            azimuth,
            distance,
            SIMPLIFY = FALSE)
        ]

    debugData <- data.table(data)
    data[, c("cam_to_blade_rbt", "roll", "azimuth", "distance", "x", "y") := NULL]
    fwrite(data, file = paste0("data/", side, "/metadata.csv"))
    debugData
}

le <- saveAndShowStitchData(coords.leading.edge, "leading-edge")

###### suction-side
coords.suction.side <- prepareSideData("suction-side", directions$`suction-side`)
ss <- saveAndShowStitchData(coords.suction.side, "suction-side")

###### trailing-edge
coords.trailing.edge <- prepareSideData("trailing-edge", directions$`trailing-edge`)
te <- saveAndShowStitchData(coords.trailing.edge, "trailing-edge")

###### pressure-side
coords.pressure.side <- prepareSideData("pressure-side", directions$`pressure-side`)
ps <- saveAndShowStitchData(coords.pressure.side, "pressure-side")

###### make and run blade_viewer
system2("bash", args = c("run_blade_viewer.sh", toString(scaleFactor)))



