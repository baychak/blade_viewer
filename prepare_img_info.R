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

getPolarAngle <- function(yVect) {
    z <- c(0,0,1)
    yVect <- -yVect/sum(yVect^2)^0.5
    acos(z%*%yVect)
}

sides <- c("leading-edge", "suction-side", "trailing-edge", "pressure-side")

getAzimuthSin <- function(zCamVector, mainDirection) {
    zCamVector <- zCamVector[1:2]
    zCamVector <- zCamVector/sum(zCamVector^2)^0.5
    zCamVector[1]*mainDirection[2] - zCamVector[2]*mainDirection[1]
}

object.center = c(1.0, 0.5)

getDistance <- function(xyPoint) {
    sqrt(sum((xyPoint-object.center)^2))
}

###### leading-edge
data.leading.edge <- metadata[blade_side == "leading-edge"]
projection.direction = c(-1,0)
data.leading.edge <- data.leading.edge[order(file)]
coords.leading.edge <- data.leading.edge[, .(file, cam_to_blade_rbt)]
coords.leading.edge[,
       c("x", "y", "z", "alpha", "azimuth", "distance") := .(
           sapply(cam_to_blade_rbt, function(mat) {mat[1, 4]}),
           sapply(cam_to_blade_rbt, function(mat) {mat[2, 4]}),
           sapply(cam_to_blade_rbt, function(mat) {mat[3, 4]}),
           sapply(cam_to_blade_rbt, function(mat) {getPolarAngle(mat[1:3, 2])}),
           sapply(cam_to_blade_rbt, function(mat) {getAzimuthSin(mat[1:3, 3], projection.direction)}),
           sapply(cam_to_blade_rbt, function(mat) {getDistance(mat[1:2, 4])})
       )
    ]

plot(coords.leading.edge$x, coords.leading.edge$y, type = "b", asp = 1)

coords.leading.edge$z <- coords.leading.edge$z - tan(coords.leading.edge$alpha)*coords.leading.edge$distance

trans <- c(2728, 1816)

distanceToBlade <- 7.0 # 20 and 21 snapshot of leading edge
snapshot20_21_z_delta <- coords.leading.edge$z[21] - coords.leading.edge$z[20] # in meters
snapshot20_21_Ydelta <- 3565 - 407 # in pixels

R <- snapshot20_21_Ydelta / (cos(coords.leading.edge$alpha[21])^2 * snapshot20_21_z_delta/distanceToBlade) # const for camera

scaleFactor <- cos(coords.leading.edge$alpha[21]) * snapshot20_21_z_delta / snapshot20_21_Ydelta # meter per pixel on corrected image
# coords.leading.edge$z <- coords.leading.edge$z - tan(coords.leading.edge$alpha)*distanceToBlade

# le.projection.center = c(8.5, 0.0)

# le.projection.direction = c(-1,0,0)

# FlipY <- matrix(c(1,0,0,0,
#                   0,1,0,0,
#                   0,0,1,0,
#                   0,0,0,1),
#                 nrow = 4, ncol = 4, byrow = T)
ShiftToImgCenter <- matrix(c(1,0,0,0,
                        0,1,0,0,
                        0,0,1,0,
                        -trans[1],-trans[2],0,1),
                      nrow = 4, ncol = 4, byrow = T)
ShiftToImgTopLeft <- solve(ShiftToImgCenter)
ProjectionZ <- matrix(c(1,0,0,0,
                        0,1,0,0,
                        0,0,0,0,
                        0,0,0,1),
                      nrow = 4, ncol = 4, byrow = T)

getTransformation <- function(alpha, R, xShift, azSin, distance) {
    ReconstructionY <- matrix(c(1,0,0,0,
                                0,1,tan(alpha),-tan(alpha)/R,
                                0,0,0,0,
                                0,0,0,1),
                              nrow = 4, ncol = 4, byrow = T)
    RotateX <- matrix(c(1,0,0,0,
                        0,cos(alpha),-sin(alpha),0,
                        0,sin(alpha),cos(alpha),0,
                        0,0,0,1),
                      nrow = 4, ncol = 4, byrow = T)
    Unshift <- matrix(c(1,0,0,0,
                        0,1,0,0,
                        0,0,1,0,
                        (azSin*distance)/scaleFactor,0,0,1),
                      nrow = 4, ncol = 4, byrow = T)
    distanceScaleFactor = distance/distanceToBlade
    AlignDistance <- matrix(c(distanceScaleFactor,0,0,0,
                              0,distanceScaleFactor,0,0,
                              0,0,1,0,
                              0,0,0,1),
                            nrow = 4, ncol = 4, byrow = T)

    Transformation <- ShiftToImgCenter %*%
                        # FlipY %*%
                        ReconstructionY %*%
                        RotateX %*%
                        Unshift %*%
                        AlignDistance %*%
                        ProjectionZ %*%
                        # FlipY %*%
                        ShiftToImgTopLeft
    (Transformation/Transformation[4,4])[-3,-3]
}

coords.leading.edge[,  transformation := mapply(
        function(angle, ySh, azSin, dist) {getTransformation(angle, R, ySh, azSin, dist)},
        alpha,
        y,
        azimuth,
        distance,
        SIMPLIFY = FALSE
    )]
coords.leading.edge[,  cam_to_blade_rbt := NULL]
coords.leading.edge[,  azimuth := NULL]
coords.leading.edge[,  distance := NULL]
coords.leading.edge[,  x := NULL]
coords.leading.edge[,  y := NULL]
coords.leading.edge[, top := sapply(
    transformation,
    function(mat) {
        t = t(c(2728, 0, 1)) %*% mat
        (t/t[1,3])[1,2]
    }
)]
coords.leading.edge[, bottom := sapply(
    transformation,
    function(mat) {
        t = t(c(2728, 3632, 1)) %*% mat
        (t/t[1,3])[1,2]
    }
)]

colOrder <- c("file", "top", "bottom", "z", "alpha", "transformation")
setcolorder(coords.leading.edge, colOrder)

# coords.leading.edge$transformation[10]

fwrite(coords.leading.edge, file = "data/leading-edge/metadata.csv")

###### suction-side
data.suction.side <- metadata[blade_side == "suction-side"]
projection.direction = c(0,-1)
data.suction.side <- data.suction.side[order(file)]
coords.suction.side <- data.suction.side[, .(file, cam_to_blade_rbt)]
coords.suction.side[,
                    c("x", "y", "z", "alpha", "azimuth", "distance") := .(
                        sapply(cam_to_blade_rbt, function(mat) {mat[1, 4]}),
                        sapply(cam_to_blade_rbt, function(mat) {mat[2, 4]}),
                        sapply(cam_to_blade_rbt, function(mat) {mat[3, 4]}),
                        sapply(cam_to_blade_rbt, function(mat) {getPolarAngle(mat[1:3, 2])}),
                        sapply(cam_to_blade_rbt, function(mat) {getAzimuthSin(mat[1:3, 3], projection.direction)}),
                        sapply(cam_to_blade_rbt, function(mat) {getDistance(mat[1:2, 4])})
                    )
                   ]

plot(coords.suction.side$x, coords.suction.side$y, type = "b", asp = 1)

# coords.suction.side$z <- coords.suction.side$z - tan(coords.suction.side$alpha)*distanceToBlade
coords.suction.side$z <- coords.suction.side$z - tan(coords.suction.side$alpha)*coords.suction.side$distance
# ((coords.suction.side$x - 1)^2 + (coords.suction.side$y - 0.5)^2)^0.5
coords.suction.side[,
                    transformation := mapply(
                        function(angle, ySh, azSin, dist) {getTransformation(angle, R, ySh, azSin, dist)},
                        alpha,
                        y,
                        azimuth,
                        distance,
                        SIMPLIFY = FALSE
                    )]
coords.suction.side[,  cam_to_blade_rbt := NULL]
coords.suction.side[,  azimuth := NULL]
coords.suction.side[,  distance := NULL]
coords.suction.side[,  x := NULL]
coords.suction.side[,  y := NULL]
coords.suction.side[, top := sapply(
    transformation,
    function(mat) {
        t = t(c(2728, 0, 1)) %*% mat
        (t/t[1,3])[1,2]
    }
)]
coords.suction.side[, bottom := sapply(
    transformation,
    function(mat) {
        t = t(c(2728, 3632, 1)) %*% mat
        (t/t[1,3])[1,2]
    }
)]

setcolorder(coords.suction.side, colOrder)

fwrite(coords.suction.side, file = "data/suction-side/metadata.csv")

###### trailing-edge
data.trailing.edge <- metadata[blade_side == "trailing-edge"]
projection.direction = c(1,0)
data.trailing.edge <- data.trailing.edge[order(file)]
coords.trailing.edge <- data.trailing.edge[, .(file, cam_to_blade_rbt)]
coords.trailing.edge[,
                    c("x", "y", "z", "alpha", "azimuth") := .(
                        sapply(cam_to_blade_rbt, function(mat) {mat[1, 4]}),
                        sapply(cam_to_blade_rbt, function(mat) {mat[2, 4]}),
                        sapply(cam_to_blade_rbt, function(mat) {mat[3, 4]}),
                        sapply(cam_to_blade_rbt, function(mat) {getPolarAngle(mat[1:3, 2])}),
                        sapply(cam_to_blade_rbt, function(mat) {getAzimuthSin(mat[1:3, 3], projection.direction)})
                    )
                    ]

plot(coords.trailing.edge$x, coords.trailing.edge$y, type = "b", asp = 1)

# coords.trailing.edge$z <- coords.trailing.edge$z - tan(coords.trailing.edge$alpha)*distanceToBlade
coords.trailing.edge[,
                    transformation := mapply(
                        function(angle, xSh, azSin) {getTransformation(angle, R, ySh, azSin)},
                        alpha,
                        x,
                        azimuth,
                        SIMPLIFY = FALSE
                    )]
coords.trailing.edge[,  cam_to_blade_rbt := NULL]
coords.trailing.edge[,  azimuth := NULL]

fwrite(coords.trailing.edge, file = "data/trailing-edge/metadata.csv")

###### pressure-side
data.pressure.side <- metadata[blade_side == "pressure-side"]
projection.direction = c(0,1)
data.pressure.side <- data.pressure.side[order(file)]
coords.pressure.side <- data.pressure.side[, .(file, cam_to_blade_rbt)]
coords.pressure.side[,
                     c("x", "y", "z", "alpha", "azimuth") := .(
                         sapply(cam_to_blade_rbt, function(mat) {mat[1, 4]}),
                         sapply(cam_to_blade_rbt, function(mat) {mat[2, 4]}),
                         sapply(cam_to_blade_rbt, function(mat) {mat[3, 4]}),
                         sapply(cam_to_blade_rbt, function(mat) {getPolarAngle(mat[1:3, 2])}),
                         sapply(cam_to_blade_rbt, function(mat) {getAzimuthSin(mat[1:3, 3], projection.direction)})
                     )
                     ]

plot(coords.pressure.side$x, coords.pressure.side$y, type = "b", asp = 1)

# coords.pressure.side$z <- coords.pressure.side$z - tan(coords.pressure.side$alpha)*distanceToBlade
coords.pressure.side[,
                     transformation := mapply(
                         function(angle, xSh, azSin) {getTransformation(angle, R, ySh, azSin)},
                         alpha,
                         x,
                         azimuth,
                         SIMPLIFY = FALSE
                     )]
coords.pressure.side[,  cam_to_blade_rbt := NULL]
coords.pressure.side[,  azimuth := NULL]

fwrite(coords.pressure.side, file = "data/pressure-side/metadata.csv")



##########################################################################
mapply(function(angle, ySh) {getTransformation(angle, R, ySh)}, coords.leading.edge$alpha, coords.leading.edge$y)

Trans <- coords.leading.edge$transformation[[10]]
output <- Trans %*% c(0,0,0,1)
(output <- (output/output[4])[1:2])
output <- Trans %*% c(5456,0,0,1)
(output <- (output/output[4])[1:2])
output <- Trans %*% c(5456,3632,0,1)
(output <- (output/output[4])[1:2])
output <- Trans %*% c(0,3632,0,1)
(output <- (output/output[4])[1:2])

Trans

#############################
getProjCoord <- function(X, angle, R, Translate) {
    X <- (X-Translate)/R
    X[2] <- -X[2]
    d <- 1 + X[2]*tan(angle)
    newX <- c(X[1]/d, X[2]*cos(angle)*(1 + tan(angle)^2)/d) * R
    newX[2] <- -newX[2]
    newX + Translate
}

getProjCoord(c(0,0), alpha, 10000, trans)
getProjCoord(c(5456,0), alpha, 10000, trans)
getProjCoord(c(5456,3632), alpha, 10000, trans)
getProjCoord(c(0,3632), alpha, 10000, trans)

alpha <- 0.2081214

# endTrans <- matrix(c(1,0,0,0,
#                      0,cos(alpha)+tan(alpha)*sin(alpha),0,tan(alpha)/R,
#                      0,0,0,0,
#                      0,0,0,1),
#                    nrow = 4, ncol = 4, byrow = T)

Transformation <- ShiftToImgCenter %*% FlipY %*% ReconstructionY %*% RotateX %*% ProjectionZ %*% FlipY %*% ShiftToImgTopLeft

input <- t(c(0,3632,0,1))
output <- input %*% Transformation
output <- output/output[1,4]
t(Transformation/Transformation[4,4])


