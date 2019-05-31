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
    print(x)
    print(y)
    print(z)
    k <- c(0,0,-1)
    print(xprod(z,k))
    f <- c(z[1]*z[2], z[2]*z[3], -(z[1]^2 + z[2]^2))
    # f <- normalize(c(z[1]/z[2], 1, -(z[1]^2 + z[2]^2)/(z[2]*z[3])))
    print(f)
    roll <- getAngle(y, f, z)
    print(roll)
    rolMat <- matrix(c(cos(roll),-sin(roll),0,
                    sin(roll),cos(roll),0,
                    0,0,1),
                  nrow = 3, ncol = 3, byrow = T)
    str(rolMat)
    str(x)
    x <- x %*% rolMat
    # print(mat)
    pitch <- getAngle(f, k, x)
    c(pitch, roll)
}

m = coords.leading.edge$cam_to_blade_rbt[[10]]

getPitchRollAngles(m)

getPolarAngle <- function(yVect) {
    z <- c(0,0,1)
    yVect <- -yVect/sum(yVect^2)^0.5
    acos(z%*%yVect)
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

imageSize <- c(5456, 3632)
trans <- imageSize/2

getWarpedImageParameters <- function(transformation, imageSize) {
    imgPoints <- matrix(c(-trans[1],-trans[2],1,
                          trans[1],-trans[2],1,
                          trans[1],trans[2],1,
                          -trans[1],trans[2],1,
                           0,0,1),
                         nrow = 5, ncol = 3, byrow = T)
    imgPoints <- imgPoints %*% transformation
    imgPoints <- imgPoints / imgPoints[,3]
    minX <- min(imgPoints[,1])
    maxX <- max(imgPoints[,1])
    minY <- min(imgPoints[,2])
    maxY <- max(imgPoints[,2])
    shift <- c(minX, minY)
    size <- c(maxX, maxY) - shift
    center <- imgPoints[5,1:2] - shift
    list(shift = shift, size = size, center = center)
}

prepareSideData <- function(side, direction) {
    data <- metadata[blade_side == side]
    data <- data[order(file)]
    coords <- data[, .(file, cam_to_blade_rbt)]
    coords[,
            c("x", "y", "z", "alpha", "azimuth", "distance") := .(
                sapply(cam_to_blade_rbt, function(mat) {mat[1, 4]}),
                sapply(cam_to_blade_rbt, function(mat) {mat[2, 4]}),
                sapply(cam_to_blade_rbt, function(mat) {mat[3, 4]}),
                sapply(cam_to_blade_rbt, function(mat) {getPolarAngle(mat[1:3, 2])}),
                sapply(cam_to_blade_rbt, function(mat) {getAzimuthSin(mat[1:3, 3], direction)}),
                sapply(cam_to_blade_rbt, function(mat) {getDistance(mat[1:2, 4])})
            )
          ]
    coords$z <- coords$z - tan(coords$alpha)*coords$distance
    plot(coords$x, coords$y, type = "b", asp = 1)
    coords
}

###### leading-edge
coords.leading.edge <- prepareSideData("leading-edge", directions$`leading-edge`)
distanceToBlade <- 7.0 # 20 and 21 snapshot of leading edge
snapshot20_21_z_delta <- coords.leading.edge$z[21] - coords.leading.edge$z[20] # in meters
snapshot20_21_Ydelta <- 3565 - 407 # in pixels

R <- snapshot20_21_Ydelta / (cos(coords.leading.edge$alpha[21])^2 * snapshot20_21_z_delta/distanceToBlade) # const for camera
# R <- 0.035

scaleFactor <- cos(coords.leading.edge$alpha[21]) * snapshot20_21_z_delta / snapshot20_21_Ydelta # meter (in object plane) per pixel on corrected image

ShiftToImgCenter <- matrix(c(1,0,0,
                        0,1,0,
                        -trans[1],-trans[2],1),
                      nrow = 3, ncol = 3, byrow = T)
ProjectionZ <- matrix(c(1,0,0,0,
                        0,1,0,0,
                        0,0,0,0,
                        0,0,0,1),
                      nrow = 4, ncol = 4, byrow = T)

getTransformationInfo <- function(alpha, R, xShift, azSin, distance) {
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
                        (azSin*distance * 0 - xShift * 0)/scaleFactor,0,0,1),
                      nrow = 4, ncol = 4, byrow = T)
    distanceScaleFactor = distance/distanceToBlade
    AlignDistance <- matrix(c(distanceScaleFactor,0,0,0,
                              0,distanceScaleFactor,0,0,
                              0,0,1,0,
                              0,0,0,1),
                            nrow = 4, ncol = 4, byrow = T)

    Transformation <-   ReconstructionY %*%
                        RotateX %*%
                        Unshift %*%
                        AlignDistance %*%
                        ProjectionZ

    Transformation <- (Transformation/Transformation[4,4])[-3,-3]
    imgParams <- getWarpedImageParameters(Transformation, imageSize)
    ShiftToImgTopLeft <- matrix(c(1,0,0,
                                  0,1,0,
                                  -imgParams$shift[1],-imgParams$shift[2],1),
                                nrow = 3, ncol = 3, byrow = T)

    FullTransformation <- ShiftToImgCenter %*%
                        Transformation %*%
                        ShiftToImgTopLeft
    list(shift = imgParams$shift, size = imgParams$size, center = imgParams$center, transformation = FullTransformation)
}

saveAndShowStitchData <- function(data, side) {
    data[,
        transformationInfo := mapply(
            function(angle, ySh, azSin, dist) {getTransformationInfo(angle, R, ySh, azSin, dist)},
            alpha,
            x,
            azimuth,
            distance,
            SIMPLIFY = FALSE)
        ]

    data[,
        c("shift", "size", "center", "transformation") := .(
            lapply(transformationInfo, function(el) {el$shift}),
            lapply(transformationInfo, function(el) {el$size}),
            lapply(transformationInfo, function(el) {el$center}),
            lapply(transformationInfo, function(el) {el$transformation})
        )
        ]
    debugData <- data.table(data)
    data[, c("transformationInfo", "cam_to_blade_rbt", "azimuth", "distance", "x", "y") := NULL]
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
# system2("bash", args = c("run_blade_viewer.sh", toString(scaleFactor)))



