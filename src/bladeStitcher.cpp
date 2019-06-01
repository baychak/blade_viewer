#include "bladeStitcher.h"

#include <opencv2/core.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/highgui.hpp>
#include "opencv2/imgproc.hpp"
#include "opencv2/imgcodecs.hpp"
#include <iostream>

std::string resultPath = "../result/";

BladeStitcher::BladeStitcher(double scaleFactor) : mScaleFactor(scaleFactor)
{
}

void BladeStitcher::stitch(const std::string side)
{
    std::vector<ImageMetadata> metaData = mCsvReader.readCSV("../data/" + side + "/metadata.csv");

    Mat img, img_warp;
    std::string inputPath = "../data/" + side + "/";
    std::string outputPath = inputPath + "warp/";
    
    int resultWidth = 0;
   
    for (ImageMetadata & imageMetadata: metaData) {
        img = imread( inputPath + imageMetadata.file, IMREAD_COLOR );
        findWarpedImageParameters(imageMetadata, img.size());
        // Size warpImgSize = {(int)imageMetadata.size.x, (int)imageMetadata.size.y};
        // std::cout << "warpImgSize=========== " << warpImgSize << std::endl;
        // warpPerspective(img, img_warp, imageMetadata.transformation, warpImgSize);
        // if (!imwrite( outputPath + imageMetadata.file, img_warp)) {
        //     std::cerr << "ERROR: Can't save to " << outputPath << imageMetadata.file << std::endl;
        // }
        // if (resultWidth < img_warp.cols && img_warp.cols < 9000) {
        //     resultWidth = img_warp.cols;
        // }
    }

    // auto itLastMetadata = metaData.rbegin();
    // double lastZ = itLastMetadata->z;
    // int resultHeight = (int)((lastZ - metaData.begin()->z)/mScaleFactor) + 5000;
    // Mat result = Mat::zeros(resultHeight, resultWidth, CV_8UC3);
    
    // Point2d resultShift(resultWidth/2.0, -itLastMetadata->shift.y);
    // Mat imgROI;
    // for (std::vector<ImageMetadata>::reverse_iterator it = metaData.rbegin(); it != metaData.rend(); ++it ) {
    //     int delta = (lastZ - it->z)/mScaleFactor;
    //     img = imread( outputPath + it->file, IMREAD_COLOR );
    //     Point2i imgShift = resultShift + it->shift;
    //     imgROI = img(findImageROI(img, result, imgShift));
    //     imgShift.x = std::min(std::max(imgShift.x, 0), result.cols);
    //     imgShift.y = std::min(std::max(imgShift.y, 0), result.rows);
    //     imgROI.copyTo(result(Rect(imgShift, imgROI.size())));
    //     resultShift.y += delta;
    //     lastZ = it->z;
    // }

    // Mat resultSmall;

    // resize(result, resultSmall, Size(), 0.1, 0.1, INTER_LINEAR);

    // if (!imwrite( resultPath + side + ".jpg", resultSmall)) {
    //         std::cerr << "ERROR: Can't save to " << outputPath << "result" + side + ".jpg" << std::endl;
    //     }
}

Rect BladeStitcher::findImageROI(const Mat &img, const Mat &dst, const Point2i &shift)
{
    Point2i roiXY;
    roiXY.x = (shift.x < 0) ? -shift.x : 0;
    roiXY.y = (shift.y < 0) ? -shift.y : 0;
    if (shift.x < -img.cols) roiXY.x = img.cols;
    if (shift.y < -img.rows) roiXY.y = img.rows;

    Size size(
            std::max(std::min(dst.cols - shift.x - roiXY.x, img.cols - roiXY.x), 0), 
            std::max(std::min(dst.rows - shift.y - roiXY.y, img.rows - roiXY.y), 0)
        );

    std::cout << "shift ------- " << shift << std::endl;
    std::cout << "size ------------ " << size << std::endl;
    return Rect(roiXY, size);
}

void BladeStitcher::findWarpedImageParameters(ImageMetadata &imageMetadata, const Size &size)
{
    std::cout << "imageMetadata.transformation 1" << std::endl << imageMetadata.transformation << std::endl;

    imageMetadata.shift = {size.width / 2.0, size.height / 2.0};

    Mat shiftToImgCenter = Mat::eye(3, 3, CV_64F);
    shiftToImgCenter.at<double>(0,2) = imageMetadata.shift.x;
    shiftToImgCenter.at<double>(1,2) = imageMetadata.shift.y;
    std::cout << "shiftToImgCenter" << std::endl << shiftToImgCenter << std::endl;

    imageMetadata.transformation = imageMetadata.transformation * shiftToImgCenter;
    std::cout << "imageMetadata.transformation 2" << std::endl << imageMetadata.transformation << std::endl;

    imageMetadata.transformation = imageMetadata.transformation / imageMetadata.transformation.at<double>(2,2);
    std::cout << "imageMetadata.transformation 3" << std::endl << imageMetadata.transformation << std::endl;

    std::vector<Point3d> contour;
    contour.push_back({0, 0, 1});
    contour.push_back({size.width, 0, 1});
    contour.push_back({size.width, size.height, 1});
    contour.push_back({0, size.height, 1});

    std::cout << "imageMetadata.transformation * contour[0]" << std::endl << imageMetadata.transformation * contour[0] << std::endl << std::endl;
   

}

