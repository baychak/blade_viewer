library("data.table")
library("stringr")

metadata.raw <- fread("data/metadata-v1.csv")

metadata <-
    metadata.raw[, .(
        turbine_number,
        blade_number_abc,
        blade_side,
        path_to_file,
        cam_to_blade_rbt,
        plane_to_blade_rbt
    )]



good <- metadata$blade_number_abc == "A" & metadata$turbine_number == "BW01"
metadata <- metadata[good,]

metadata <-
    metadata[, .(
        blade_side,
        path_to_file,
        cam_to_blade_rbt,
        plane_to_blade_rbt
    )]

parseCoordinatesString <- function(coordString) {
    matrix(as.double(unlist(str_extract_all(coordString, "-?\\d+.(\\d+|)"))), nrow = 4, ncol = 4, byrow = T)
}

metadata[, cam_to_blade_rbt := lapply(cam_to_blade_rbt, parseCoordinatesString)]
metadata[, plane_to_blade_rbt := lapply(plane_to_blade_rbt, parseCoordinatesString)]

getUpAngle <- function(yVect) {
    z <- c(0,0,1)
    yVect <- -yVect/sum(yVect^2)^0.5
    acos(sum(z*yVect))
}

data.leading.edge <- metadata[blade_side == "leading-edge"]
data.leading.edge <- data.leading.edge[order(path_to_file)]
coords.leading.edge <- data.leading.edge[, .(cam_to_blade_rbt, plane_to_blade_rbt)]
coords.leading.edge[,
       c("x", "y", "z", "alpha") := .(
       sapply(cam_to_blade_rbt, function(mat) {mat[1, 4]}),
       sapply(cam_to_blade_rbt, function(mat) {mat[2, 4]}),
       sapply(cam_to_blade_rbt, function(mat) {mat[3, 4]}),
       sapply(cam_to_blade_rbt, function(mat) {getUpAngle(mat[1:3, 2])})
       )
    ]

plot(coords.leading.edge$x, coords.leading.edge$y, type = "l")

trans <- c(2728, 1816)

R <- 5000
FlipY <- matrix(c(1,0,0,0,
                  0,-1,0,0,
                  0,0,1,0,
                  0,0,0,1),
                nrow = 4, ncol = 4, byrow = T)
Translation <- matrix(c(1,0,0,0,
                        0,1,0,0,
                        0,0,1,0,
                        -trans[1],-trans[2],0,1),
                      nrow = 4, ncol = 4, byrow = T)
InvTranslation <- solve(Translation)
ProjectionZ <- matrix(c(1,0,0,0,
                        0,1,0,0,
                        0,0,0,0,
                        0,0,0,1),
                      nrow = 4, ncol = 4, byrow = T)

getTransformation <- function(alpha, R) {
    ReconstructionY <- matrix(c(1,0,0,0,
                                0,1,-tan(alpha),tan(alpha)/R,
                                0,0,0,0,
                                0,0,0,1),
                              nrow = 4, ncol = 4, byrow = T)
    RotateX <- matrix(c(1,0,0,0,
                        0,cos(alpha),sin(alpha),0,
                        0,-sin(alpha),cos(alpha),0,
                        0,0,0,1),
                      nrow = 4, ncol = 4, byrow = T)

    Transformation <- Translation %*% FlipY %*% ReconstructionY %*% RotateX %*% ProjectionZ %*% FlipY %*% InvTranslation
    t(Transformation/Transformation[4,4])
}

coords.leading.edge[,  transformation := lapply(alpha, function(angle) {getTransformation(angle, R)})]

Trans <- coords.leading.edge$transformation[[10]]
output <- Trans %*% c(0,0,0,1)
(output <- (output/output[4])[1:2])
output <- Trans %*% c(5456,0,0,1)
(output <- (output/output[4])[1:2])
output <- Trans %*% c(5456,3632,0,1)
(output <- (output/output[4])[1:2])
output <- Trans %*% c(0,3632,0,1)
(output <- (output/output[4])[1:2])

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

Transformation <- Translation %*% FlipY %*% ReconstructionY %*% RotateX %*% ProjectionZ %*% FlipY %*% InvTranslation

input <- t(c(0,3632,0,1))
output <- input %*% Transformation
output <- output/output[1,4]
t(Transformation/Transformation[4,4])

#####################################

data.pressure.side <- metadata[blade_side == "pressure-side"]
data.pressure.side <- data.pressure.side[order(path_to_file)]
coords.pressure.side <- data.pressure.side[, .(cam_to_blade_rbt, plane_to_blade_rbt)]
coords.pressure.side[,
                    c("x", "y", "z") := .(
                        sapply(cam_to_blade_rbt, function(mat) {mat[1, 4]}),
                        sapply(cam_to_blade_rbt, function(mat) {mat[2, 4]}),
                        sapply(cam_to_blade_rbt, function(mat) {mat[3, 4]}))
                    ]

plot(coords.pressure.side$x, coords.pressure.side$y, type = "l")

data.trailing.edge <- metadata[blade_side == "trailing-edge"]
data.trailing.edge <- data.trailing.edge[order(path_to_file)]
coords.trailing.edge <- data.trailing.edge[, .(cam_to_blade_rbt, plane_to_blade_rbt)]
coords.trailing.edge[,
                     c("x", "y", "z") := .(
                         sapply(cam_to_blade_rbt, function(mat) {mat[1, 4]}),
                         sapply(cam_to_blade_rbt, function(mat) {mat[2, 4]}),
                         sapply(cam_to_blade_rbt, function(mat) {mat[3, 4]}))
                     ]

plot(coords.trailing.edge$x, coords.trailing.edge$y, type = "l")

data.suction.side <- metadata[blade_side == "suction-side"]
data.suction.side <- data.suction.side[order(path_to_file)]
coords.suction.side <- data.suction.side[, .(cam_to_blade_rbt, plane_to_blade_rbt)]
coords.suction.side[,
                     c("x", "y", "z") := .(
                         sapply(cam_to_blade_rbt, function(mat) {mat[1, 4]}),
                         sapply(cam_to_blade_rbt, function(mat) {mat[2, 4]}),
                         sapply(cam_to_blade_rbt, function(mat) {mat[3, 4]}))
                     ]

plot(coords.suction.side$x, coords.suction.side$y, type = "l")



