#include <opencv2/core.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/highgui.hpp>
#include "opencv2/imgproc.hpp"
#include "opencv2/imgcodecs.hpp"
#include <opencv2/features2d.hpp>
#include "opencv2/xfeatures2d/nonfree.hpp"
#include <opencv2/calib3d.hpp>
#include <iostream>
#include <string>
#include <math.h>       /* sin */

#define PI 3.14159265

class RobustMatcher {
private:
    // pointer to the feature point detector object
    cv::Ptr<cv::FeatureDetector> detector;
    // pointer to the feature descriptor extractor object
    cv::Ptr<cv::DescriptorExtractor> descriptor;
    int normType;
    float ratio;
    // max ratio between 1st and 2nd NN
    bool refineF;
    // if true will refine the F matrix
    bool refineM;
    // if true will refine the matches
    double confidence;
    double distance;
    // min distance to epipolar
    // confidence level (probability)
public:
    RobustMatcher(const cv::Ptr<cv::FeatureDetector> &detector, const cv::Ptr<cv::DescriptorExtractor> &descriptor = cv::Ptr<cv::DescriptorExtractor>()):
        detector(detector), descriptor(descriptor),
        normType(cv::NORM_L2), ratio(0.8f),
        refineF(true), refineM(true),
        confidence(0.98), distance(1.0) 
    {
        // in this case use the associated descriptor
        if (!this->descriptor) {
            this->descriptor = this->detector;
        }
    };


    // Match feature points using RANSAC
    // returns fundamental matrix and output match set
    cv::Mat match(cv::Mat& image1, cv::Mat& image2, 
        std::vector<cv::DMatch>& matches, 
        std::vector<cv::KeyPoint>& keypoints1, std::vector<cv::KeyPoint>& keypoints2) 
    {
        detector->detect(image1,keypoints1);
        detector->detect(image2,keypoints2);

        cv::Mat descriptors1, descriptors2;
        descriptor->compute(image1,keypoints1,descriptors1);
        descriptor->compute(image2,keypoints2,descriptors2);
        // 3. Match the two image descriptors
        // (optionally apply some checking method)
        // Construction of the matcher with crosscheck
        cv::BFMatcher matcher(normType, true);
        //crosscheck flag
        // match descriptors
        std::vector<cv::DMatch> outputMatches;
        matcher.match(descriptors1,descriptors2,outputMatches);
        // 4. Validate matches using RANSAC
        cv::Mat fundamental= ransacTest(outputMatches, keypoints1, keypoints2, matches);
        // return the found fundamental matrix
        return fundamental;
    };

        // Identify good matches using RANSAC
    // Return fundamental matrix and output matches
    cv::Mat ransacTest(const std::vector<cv::DMatch>& matches, 
        std::vector<cv::KeyPoint>& keypoints1,
        std::vector<cv::KeyPoint>& keypoints2,
        std::vector<cv::DMatch>& outMatches) 
    {
        // Convert keypoints into Point2f
        std::vector<cv::Point2f> points1, points2;
        for (std::vector<cv::DMatch>::const_iterator it = matches.begin(); it!= matches.end(); ++it) {
            // Get the position of left keypoints
            points1.push_back(keypoints1[it->queryIdx].pt);
            // Get the position of right keypoints
            points2.push_back(keypoints2[it->trainIdx].pt);
        }
        // Compute F matrix using RANSAC
        std::vector<uchar> inliers(points1.size(),0);
        cv::Mat fundamental = cv::findFundamentalMat( points1, points2, cv::FM_RANSAC, distance, confidence);
        
        // extract the surviving (inliers) matches
        std::vector<uchar>::const_iterator itIn = inliers.begin();
        std::vector<cv::DMatch>::const_iterator itM = matches.begin();
        // for all matches
        for ( ;itIn != inliers.end(); ++itIn, ++itM) {
            if (*itIn) { // it is a valid match
                outMatches.push_back(*itM);
            }
        }
        return fundamental;
    }
};


//using namespace cv;
//using namespace std;

int main()
{
    cv::String imageName1( "../data/test/snapshot96.jpg" );
    cv::String imageName2( "../data/test/snapshot97.jpg" );
    cv::Mat src1, src2;
    const cv::String window_name1 = "Input image 1";
    const cv::String window_name2 = "Input image 2";
    const cv::String window_name3 = "Transformed image";

    src1 = cv::imread( imageName1, cv::IMREAD_COLOR );
    if( src1.empty() )
    {
        std::cout << "Could not open or find the image" << std::endl ;
        return -1;
    }
    cv::namedWindow( window_name1, cv::WINDOW_NORMAL );
    cv::imshow( window_name1, src1 );

    src2 = cv::imread( imageName2, cv::IMREAD_COLOR );
    if( src2.empty() )
    {
        std::cout << "Could not open or find the image" << std::endl ;
        return -1;
    }
    cv::namedWindow( window_name2, cv::WINDOW_NORMAL );
    cv::imshow( window_name2, src2 );

    // Prepare the matcher (with default parameters)
    // SIFT detector and descriptor
    RobustMatcher rmatcher(cv::xfeatures2d::SIFT::create(250));// Match the two images
    std::vector<cv::DMatch> matches;
    std::vector<cv::KeyPoint> keypoints1, keypoints2;
    cv::Mat fundamental = rmatcher.match(src1, src2, matches, keypoints1, keypoints2);

    
    // const Point2f points1[4] = {{0,0},{5000,0},{5000,5000},{0,5000}};
    // const Point2f points2[4] = {{1000,1000},{5000,0},{5000,5000},{0,5000}};

    // Mat H = getPerspectiveTransform(points1, points2);
    // cout << "H:\n" << H << endl;
    
    // Mat img1_warp;
    // warpPerspective(src, img1_warp, H, src.size());
    // namedWindow( window_name2, WINDOW_NORMAL );
    // imshow( window_name2, img1_warp );
    cv::waitKey(0);

    return 0;
}
