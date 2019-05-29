#include <opencv2/core.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/highgui.hpp>
#include "opencv2/imgproc.hpp"
#include "opencv2/imgcodecs.hpp"
#include <iostream>
#include <fstream>
#include <string>
#include <math.h>       /* sin */

#include "csvReader.h"

using namespace cv;

#define PI 3.14159265
// #define CV_IO_MAX_IMAGE_PIXELS ((uint64_t)1<<32)

int main()
{
    CsvReader csvReader;
    
    std::string side = "leading-edge";
    std::ifstream file("../data/" + side + "/metadata.csv");
    double scaleFactor = 0.0007120899;

    std::vector<ImageMetadata> metaData = csvReader.readCSV(file);
    file.close();

    Mat img, img_warp;
    std::string inputPath = "../data/" + side + "/";
    std::string outputPath = inputPath + "warp/";
    Size warpImgSize(6900, 4743);

    int resultHeight = 0;
   
    for (const ImageMetadata & imageMetadata: metaData) {
        img = imread( inputPath + imageMetadata.file, IMREAD_COLOR );
        warpImgSize.height = (int)imageMetadata.bottom;
        warpPerspective(img, img_warp, imageMetadata.transformation, warpImgSize);
        if (!imwrite( outputPath + imageMetadata.file, img_warp)) {
            std::cerr << "ERROR: Can't save to " << outputPath << imageMetadata.file << std::endl;
        }
        resultHeight += img_warp.rows;
    }

    Mat result = Mat::zeros(resultHeight, warpImgSize.width, CV_8UC3);
    
    int currentHeight = 0;
    double lastZ = metaData.rbegin()->z;
    Mat imgROI;
    for (std::vector<ImageMetadata>::reverse_iterator it = metaData.rbegin(); it != metaData.rend(); ++it ) {
        int delta = (int)((lastZ - it->z)/scaleFactor);
        img = imread( outputPath + it->file, IMREAD_COLOR );
        int top = (int)(it->top + 1);
        top = (top >= 0) ? top : 0;
        imgROI = img(Rect(0, top, img.cols, img.rows - top));
        imgROI.copyTo(result(Rect(0, currentHeight + top, imgROI.cols, imgROI.rows)));
        currentHeight += delta;
        lastZ = it->z;
    }

    Mat resultSmall;

    resize(result, resultSmall, Size(), 0.1, 0.1, INTER_LINEAR);

    if (!imwrite( outputPath + "result" + side + ".jpg", resultSmall)) {
            std::cerr << "ERROR: Can't save to " << outputPath << "result" + side + ".jpg" << std::endl;
        }

///////////////////////////////////////////// suction-side
    side = "suction-side";
    file.open("../data/" + side + "/metadata.csv");
    metaData = csvReader.readCSV(file);
    file.close();

    inputPath = "../data/" + side + "/";
    outputPath = inputPath + "warp/";
    
    resultHeight = 0;
   
    for (const ImageMetadata & imageMetadata: metaData) {
        img = imread( inputPath + imageMetadata.file, IMREAD_COLOR );
        warpImgSize.height = (int)imageMetadata.bottom;
        warpPerspective(img, img_warp, imageMetadata.transformation, warpImgSize);
        if (!imwrite( outputPath + imageMetadata.file, img_warp)) {
            std::cerr << "ERROR: Can't save to " << outputPath << imageMetadata.file << std::endl;
        }
        resultHeight += img_warp.rows;
    }

    result = Mat::zeros(resultHeight, warpImgSize.width, CV_8UC3);
    
    currentHeight = 0;
    lastZ = metaData.rbegin()->z;
    for (std::vector<ImageMetadata>::reverse_iterator it = metaData.rbegin(); it != metaData.rend(); ++it ) {
        int delta = (int)((lastZ - it->z)/scaleFactor);
        img = imread( outputPath + it->file, IMREAD_COLOR );
        int top = (int)(it->top + 1);
        top = (top >= 0) ? top : 0;
        imgROI = img(Rect(0, top, img.cols, img.rows - top));
        imgROI.copyTo(result(Rect(0, currentHeight + top, imgROI.cols, imgROI.rows)));
        currentHeight += delta;
        lastZ = it->z;
    }

    resize(result, resultSmall, Size(), 0.1, 0.1, INTER_LINEAR);

    if (!imwrite( outputPath + "result" + side + ".jpg", resultSmall)) {
            std::cerr << "ERROR: Can't save to " << outputPath << "result" + side + ".jpg" << std::endl;
        }

// /////////////////////////////////////////////// trailing-edge
//     side = "trailing-edge";
//     file.open("../data/" + side + "/metadata.csv");
//     metaData = csvReader.readCSV(file);
//     file.close();

//     inputPath = "../data/" + side + "/";
//     outputPath = inputPath + "warp/";
    
//     resultHeight = 0;
   
//     for (const ImageMetadata & imageMetadata: metaData) {
//         img = imread( inputPath + imageMetadata.file, IMREAD_COLOR );
//         warpPerspective(img, img_warp, imageMetadata.transformation, warpImgSize);
//         if (!imwrite( outputPath + imageMetadata.file, img_warp)) {
//             std::cerr << "ERROR: Can't save to " << outputPath << imageMetadata.file << std::endl;
//         }
//         resultHeight += img_warp.rows;
//     }

//     result = Mat::zeros(resultHeight, warpImgSize.width, CV_8UC3);
    
//     currentHeight = 0;
//     lastZ = metaData.rbegin()->z;
//     for (std::vector<ImageMetadata>::reverse_iterator it = metaData.rbegin(); it != metaData.rend(); ++it ) {
//         int delta = (int)((lastZ - it->z)/scaleFactor);
//         img = imread( outputPath + it->file, IMREAD_COLOR );
//         imgROI = img(Rect(0, 28, img.cols, img.rows - 28));
//         imgROI.copyTo(result(Rect(0, currentHeight + 28, imgROI.cols, imgROI.rows)));
//         currentHeight += delta;
//         lastZ = it->z;
//     }

//     resize(result, resultSmall, Size(), 0.1, 0.1, INTER_LINEAR);

//     if (!imwrite( outputPath + "result" + side + ".jpg", resultSmall)) {
//             std::cerr << "ERROR: Can't save to " << outputPath << "result" + side + ".jpg" << std::endl;
//         }

// /////////////////////////////////////////////// pressure-side
//     side = "pressure-side";
//     file.open("../data/" + side + "/metadata.csv");
//     metaData = csvReader.readCSV(file);
//     file.close();

//     inputPath = "../data/" + side + "/";
//     outputPath = inputPath + "warp/";
    
//     resultHeight = 0;
   
//     for (const ImageMetadata & imageMetadata: metaData) {
//         img = imread( inputPath + imageMetadata.file, IMREAD_COLOR );
//         warpPerspective(img, img_warp, imageMetadata.transformation, warpImgSize);
//         if (!imwrite( outputPath + imageMetadata.file, img_warp)) {
//             std::cerr << "ERROR: Can't save to " << outputPath << imageMetadata.file << std::endl;
//         }
//         resultHeight += img_warp.rows;
//     }

//     result = Mat::zeros(resultHeight, warpImgSize.width, CV_8UC3);
    
//     currentHeight = 0;
//     lastZ = metaData.rbegin()->z;
//     for (std::vector<ImageMetadata>::reverse_iterator it = metaData.rbegin(); it != metaData.rend(); ++it ) {
//         int delta = (int)((lastZ - it->z)/scaleFactor);
//         img = imread( outputPath + it->file, IMREAD_COLOR );
//         imgROI = img(Rect(0, 28, img.cols, img.rows - 28));
//         imgROI.copyTo(result(Rect(0, currentHeight + 28, imgROI.cols, imgROI.rows)));
//         currentHeight += delta;
//         lastZ = it->z;
//     }

//     resize(result, resultSmall, Size(), 0.1, 0.1, INTER_LINEAR);

//     if (!imwrite( outputPath + "result" + side + ".jpg", resultSmall)) {
//             std::cerr << "ERROR: Can't save to " << outputPath << "result" + side + ".jpg" << std::endl;
//         }

//     // const String window_name = "Transformed image";
//     // namedWindow( window_name, WINDOW_NORMAL );
//     // imshow( window_name, img20_warp );
//     //  waitKey(0);

    return 0;
}
