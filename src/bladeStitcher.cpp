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
        Size warpImgSize = {(int)imageMetadata.size.x, (int)imageMetadata.size.y};
        std::cout << "warpImgSize ====== " << warpImgSize << "  " << imageMetadata.file << std::endl;
        warpPerspective(img, img_warp, imageMetadata.transformation, warpImgSize);
        if (!imwrite( outputPath + imageMetadata.file, img_warp)) {
            std::cerr << "ERROR: Can't save to " << outputPath << imageMetadata.file << std::endl;
        }
        if (resultWidth < img_warp.cols && img_warp.cols < 9000) {
            resultWidth = img_warp.cols;
        }
    }

    auto itLastMetadata = metaData.rbegin();
    double lastZ = itLastMetadata->z;
    int resultHeight = (int)((lastZ - metaData.begin()->z)/mScaleFactor) + 5000;
    Mat result = Mat::zeros(resultHeight, resultWidth, CV_8UC3);
    
    Point2d resultShift(resultWidth/2.0, itLastMetadata->shift.y);
    Mat imgROI;
    for (std::vector<ImageMetadata>::reverse_iterator it = metaData.rbegin(); it != metaData.rend(); ++it ) {
        int delta = (lastZ - it->z)/mScaleFactor;
        img = imread( outputPath + it->file, IMREAD_COLOR );
        Point2i imgShift = resultShift - it->shift;
        Rect roi = findROIRect(img, result, imgShift);
        imgROI = img(roi);
        Mat mask(img.rows, img.cols, CV_8UC1, cv::Scalar(0));
        std::vector<std::vector<Point2i>> contours({it->contour});
        drawContours(mask, contours, 0, Scalar(255), FILLED, 8);
        Mat maskROI = mask(roi);

        imgShift.x = std::min(std::max(imgShift.x, 0), result.cols);
        imgShift.y = std::min(std::max(imgShift.y, 0), result.rows);

        imgROI.copyTo(result(Rect(imgShift, imgROI.size())), maskROI);
        resultShift.y += delta;
        lastZ = it->z;
    }

    Mat resultSmall;

    resize(result, resultSmall, Size(), 0.1, 0.1, INTER_LINEAR);

    if (!imwrite( resultPath + side + ".jpg", resultSmall)) {
            std::cerr << "ERROR: Can't save to " << outputPath << "result" + side + ".jpg" << std::endl;
        }
}

Rect BladeStitcher::findROIRect(const Mat &img, const Mat &dst, const Point2i &shift)
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

    std::cout << "------ shift: " << shift << "    size: " << size << std::endl;
    return Rect(roiXY, size);
}

void BladeStitcher::findWarpedImageParameters(ImageMetadata &imageMetadata, const Size &size)
{
    Point2d corner = {size.width / 2.0, size.height / 2.0};

    Mat contour = (Mat_<double>(4,3) << -corner.x , -corner.y, 1,
                                        corner.x , -corner.y, 1,
                                        corner.x , corner.y, 1,
                                        -corner.x , corner.y, 1);
    contour = imageMetadata.transformation * contour.t();
    contour = contour / repeat(contour.row(2), 3, 1);

    double minX, maxX, minY, maxY;
    minMaxLoc(contour.row(0), &minX, &maxX);
    minMaxLoc(contour.row(1), &minY, &maxY);

    imageMetadata.shift = {-minX, -minY};
    imageMetadata.size = {maxX - minX, maxY - minY};

    Mat shiftImgToCenter = Mat::eye(3, 3, CV_64F);
    shiftImgToCenter.at<double>(0,2) = -corner.x;
    shiftImgToCenter.at<double>(1,2) = -corner.y;

    Mat shiftImgFromCenter = Mat::eye(3, 3, CV_64F);
    shiftImgFromCenter.at<double>(0,2) = imageMetadata.shift.x;
    shiftImgFromCenter.at<double>(1,2) = imageMetadata.shift.y;

    imageMetadata.transformation = shiftImgFromCenter * imageMetadata.transformation * shiftImgToCenter;
    imageMetadata.transformation = imageMetadata.transformation / imageMetadata.transformation.at<double>(2,2);

    contour = shiftImgFromCenter * contour;
    for (size_t i = 0; i < 4; i++)
    {
        imageMetadata.contour.emplace_back(Point2i(contour.at<double>(0,i), contour.at<double>(1,i)));
    }
    for (size_t i = 0; i < 4; i++)
    {
        std::cout << imageMetadata.contour[i] << std::endl;
    }

}


