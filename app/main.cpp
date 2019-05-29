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
    Size warpImgSize(5456, 3743);

    int resultHeight = 0;
   
    for (const ImageMetadata & imageMetadata: metaData) {
        img = imread( inputPath + imageMetadata.file, IMREAD_COLOR );
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
        imgROI = img(Rect(0, 28, img.cols, img.rows - 28));
        imgROI.copyTo(result(Rect(0, currentHeight + 28, imgROI.cols, imgROI.rows)));
        currentHeight += delta;
        lastZ = it->z;
    }

    Mat resultSmall;

    resize(result, resultSmall, Size(), 0.1, 0.1, INTER_LINEAR);

    if (!imwrite( outputPath + "result" + side + ".jpg", resultSmall)) {
            //std::cerr << "resultHeight * warpImgSize.width * 3 " << (uint64_t)resultHeight * warpImgSize.width * 3 << std::endl;
            //std::cerr << "CV_IO_MAX_IMAGE_PIXELS " << OPENCV_IO_MAX_IMAGE_PIXELS << std::endl;
            std::cerr << "ERROR: Can't save to " << outputPath << "result" + side + ".jpg" << std::endl;
        }

/////////////////////////////////////////////// suction-side
    side = "suction-side";
    file.open("../data/" + side + "/metadata.csv");
    metaData = csvReader.readCSV(file);
    file.close();

    inputPath = "../data/" + side + "/";
    outputPath = inputPath + "warp/";
    
    resultHeight = 0;
   
    for (const ImageMetadata & imageMetadata: metaData) {
        img = imread( inputPath + imageMetadata.file, IMREAD_COLOR );
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
        imgROI = img(Rect(0, 28, img.cols, img.rows - 28));
        imgROI.copyTo(result(Rect(0, currentHeight + 28, imgROI.cols, imgROI.rows)));
        currentHeight += delta;
        lastZ = it->z;
    }

    resize(result, resultSmall, Size(), 0.1, 0.1, INTER_LINEAR);

    if (!imwrite( outputPath + "result" + side + ".jpg", resultSmall)) {
            //std::cerr << "resultHeight * warpImgSize.width * 3 " << (uint64_t)resultHeight * warpImgSize.width * 3 << std::endl;
            //std::cerr << "CV_IO_MAX_IMAGE_PIXELS " << OPENCV_IO_MAX_IMAGE_PIXELS << std::endl;
            std::cerr << "ERROR: Can't save to " << outputPath << "result" + side + ".jpg" << std::endl;
        }

/////////////////////////////////////////////// trailing-edge
    side = "trailing-edge";
    file.open("../data/" + side + "/metadata.csv");
    metaData = csvReader.readCSV(file);
    file.close();

    inputPath = "../data/" + side + "/";
    outputPath = inputPath + "warp/";
    
    resultHeight = 0;
   
    for (const ImageMetadata & imageMetadata: metaData) {
        img = imread( inputPath + imageMetadata.file, IMREAD_COLOR );
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
        imgROI = img(Rect(0, 28, img.cols, img.rows - 28));
        imgROI.copyTo(result(Rect(0, currentHeight + 28, imgROI.cols, imgROI.rows)));
        currentHeight += delta;
        lastZ = it->z;
    }

    resize(result, resultSmall, Size(), 0.1, 0.1, INTER_LINEAR);

    if (!imwrite( outputPath + "result" + side + ".jpg", resultSmall)) {
            //std::cerr << "resultHeight * warpImgSize.width * 3 " << (uint64_t)resultHeight * warpImgSize.width * 3 << std::endl;
            //std::cerr << "CV_IO_MAX_IMAGE_PIXELS " << OPENCV_IO_MAX_IMAGE_PIXELS << std::endl;
            std::cerr << "ERROR: Can't save to " << outputPath << "result" + side + ".jpg" << std::endl;
        }

/////////////////////////////////////////////// pressure-side
    side = "pressure-side";
    file.open("../data/" + side + "/metadata.csv");
    metaData = csvReader.readCSV(file);
    file.close();

    inputPath = "../data/" + side + "/";
    outputPath = inputPath + "warp/";
    
    resultHeight = 0;
   
    for (const ImageMetadata & imageMetadata: metaData) {
        img = imread( inputPath + imageMetadata.file, IMREAD_COLOR );
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
        imgROI = img(Rect(0, 28, img.cols, img.rows - 28));
        imgROI.copyTo(result(Rect(0, currentHeight + 28, imgROI.cols, imgROI.rows)));
        currentHeight += delta;
        lastZ = it->z;
    }

    resize(result, resultSmall, Size(), 0.1, 0.1, INTER_LINEAR);

    if (!imwrite( outputPath + "result" + side + ".jpg", resultSmall)) {
            //std::cerr << "resultHeight * warpImgSize.width * 3 " << (uint64_t)resultHeight * warpImgSize.width * 3 << std::endl;
            //std::cerr << "CV_IO_MAX_IMAGE_PIXELS " << OPENCV_IO_MAX_IMAGE_PIXELS << std::endl;
            std::cerr << "ERROR: Can't save to " << outputPath << "result" + side + ".jpg" << std::endl;
        }

    // const String window_name = "Transformed image";
    // namedWindow( window_name, WINDOW_NORMAL );
    // imshow( window_name, img20_warp );
    //  waitKey(0);

    // Mat img20, img21, img20_warp, img21_warp;

    // ImageMetadata imageMetadata = metaData[19];
    // img20 = imread( inputPath + imageMetadata.file, IMREAD_COLOR );
    // Size img_size = img20.size();
    // img_size.height += 150;
    // warpPerspective(img20, img20_warp, imageMetadata.transformation, img_size);

    // imageMetadata = metaData[20];
    // img21 = imread( inputPath + imageMetadata.file, IMREAD_COLOR );
    // warpPerspective(img21, img21_warp, imageMetadata.transformation, img_size);

    // Mat result = Mat::zeros(2*img20_warp.rows, img20_warp.cols, CV_8UC3);
    // img20_warp.copyTo(result(Rect(0, img20_warp.rows, img20_warp.cols, img20_warp.rows)));
    // img21_warp.copyTo(result(Rect(0, 0, img20_warp.cols, img20_warp.rows)));

    // std::cout << "img20 Size: " << img20.size() << std::endl;
    // std::cout << "img20_warp Size: " << img20_warp.size() << std::endl;
    // std::cout << "result Size: " << result.size() << std::endl;
    // const String window_name = "Transformed image";
    // namedWindow( window_name, WINDOW_NORMAL );
    // imshow( window_name, img20_warp );
    // waitKey(0);

    // std::cout << "TTTTT Size: " << metaData.size() << std::endl;
    // std::cout << "TTTTT:\n" << metaData[25].transformation << std::endl;
    
    // String imageName( "../data/test/snapshot00010.jpg" ); // by default
    // Mat src;
    // const String window_name = "Input image";
    // const String window_name2 = "Transformed image";

    // src = imread( imageName, IMREAD_COLOR );
    // if( src.empty() )
    // {
    //     std::cout << "Could not open or find the image" << std::endl ;
    //     return -1;
    // }
    // namedWindow( window_name, WINDOW_NORMAL );
    // imshow( window_name, src );
    // // waitKey(0);

    // // double alpha = 0;

    // // double sinA = sin(alpha * PI / 180);
    // // double cosA = cos(alpha * PI / 180);

    // // Mat H = (Mat_<double>(3,3) << 
    // //                             cosA, sinA, 0, 
    // //                             -sinA, cosA, 0, 
    // //                             -10, 10, 1);
    // // cout << "H:\n" << H << endl;
    // Size s = src.size();
    
    // std::cout << "X size: " << s.width << std::endl;
    // std::cout << "Y size: " << s.height << std::endl;

    // const Point2f points1a[4] = {{0,0},{5456,0},{5456,3632},{0,3632}};
    // const Point2f points1b[4] = {{194.33306, 92.16646},{5261.66694, 92.16646},{5682.620, 3826.238},{-226.6203, 3826.2380}};
    // Mat H = getPerspectiveTransform(points1a, points1b);
    // std::cout << "H:\n" << H << std::endl;
    
    // Mat img1_warp;
    // warpPerspective(src, img1_warp, H, src.size());
    // namedWindow( window_name2 + "_1", WINDOW_NORMAL );
    // imshow( window_name2 + "_1", img1_warp );

    // const Point2f points2a[4] = {{0,0},{5456,0},{5456,3632},{0,3632}};
    // const Point2f points2b[4] = {{100.75525, 28.49883},{5355.24475, 28.49883},{5564.791, 3746.071},{-108.7914, 3746.0706}};
    // H = getPerspectiveTransform(points2a, points2b);
    // std::cout << "H:\n" << H << std::endl;
    
    // Mat img2_warp;
    // warpPerspective(src, img2_warp, H, src.size());
    // namedWindow( window_name2 + "_2", WINDOW_NORMAL );
    // imshow( window_name2 + "_2", img2_warp );
    // waitKey(0);

    return 0;
}
