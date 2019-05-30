#include "bladeStitcher.h"

#include <opencv2/core.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/highgui.hpp>
#include "opencv2/imgproc.hpp"
#include "opencv2/imgcodecs.hpp"
#include <iostream>

double scaleFactor = 0.0007120899;
std::string resultPath = "../result/";

void BladeStitcher::stitch(const std::string side)
{
    std::vector<ImageMetadata> metaData = mCsvReader.readCSV("../data/" + side + "/metadata.csv");

    Mat img, img_warp;
    std::string inputPath = "../data/" + side + "/";
    std::string outputPath = inputPath + "warp/";
    
    int resultWidth = 0;
   
    for (const ImageMetadata & imageMetadata: metaData) {
        img = imread( inputPath + imageMetadata.file, IMREAD_COLOR );
        Size warpImgSize = {(int)imageMetadata.size.x, (int)imageMetadata.size.y};
        std::cout << "warpImgSize=========== " << warpImgSize << std::endl;
        warpPerspective(img, img_warp, imageMetadata.transformation, warpImgSize);
        if (!imwrite( outputPath + imageMetadata.file, img_warp)) {
            std::cerr << "ERROR: Can't save to " << outputPath << imageMetadata.file << std::endl;
        }
        if (resultWidth < img_warp.cols && img_warp.cols < 6000) {
            resultWidth = img_warp.cols;
        }
    }

    auto itLastMetadata = metaData.rbegin();
    double lastZ = itLastMetadata->z;
    int resultHeight = (int)((lastZ - metaData.begin()->z)/scaleFactor) + 5000;
    Mat result = Mat::zeros(resultHeight, resultWidth, CV_8UC3);
    
    Point2d resultShift(resultWidth/2.0, -itLastMetadata->shift.y);
    Mat imgROI;
    for (std::vector<ImageMetadata>::reverse_iterator it = metaData.rbegin(); it != metaData.rend(); ++it ) {
        int delta = (lastZ - it->z)/scaleFactor;
        img = imread( outputPath + it->file, IMREAD_COLOR );
        Point2i imgShift = resultShift + it->shift;
        imgROI = img(findImageROI(img, result, imgShift));
        imgShift.x = std::min(std::max(imgShift.x, 0), result.cols);
        imgShift.y = std::min(std::max(imgShift.y, 0), result.rows);
        imgROI.copyTo(result(Rect(imgShift, imgROI.size())));
        resultShift.y += delta;
        lastZ = it->z;
    }

    Mat resultSmall;

    resize(result, resultSmall, Size(), 0.1, 0.1, INTER_LINEAR);

    if (!imwrite( resultPath + side + ".jpg", resultSmall)) {
            std::cerr << "ERROR: Can't save to " << outputPath << "result" + side + ".jpg" << std::endl;
        }
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

    std::cout << "imgShift ------------ " << shift << std::endl;
    std::cout << "size ------------ " << size << std::endl;
    return Rect(roiXY, size);
}

