#include <opencv2/core.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/highgui.hpp>
#include "opencv2/imgproc.hpp"
#include "opencv2/imgcodecs.hpp"
#include <iostream>
#include <string>
#include <math.h>       /* sin */

#define PI 3.14159265

using namespace cv;
using namespace std;
int main()
{
    String imageName( "../data/test/snapshot00010.jpg" ); // by default
    Mat src;
    const String window_name = "Input image";
    const String window_name2 = "Transformed image";

    src = imread( imageName, IMREAD_COLOR );
    if( src.empty() )
    {
        cout << "Could not open or find the image" << std::endl ;
        return -1;
    }
    namedWindow( window_name, WINDOW_NORMAL );
    imshow( window_name, src );
    // waitKey(0);

    // double alpha = 0;

    // double sinA = sin(alpha * PI / 180);
    // double cosA = cos(alpha * PI / 180);

    // Mat H = (Mat_<double>(3,3) << 
    //                             cosA, sinA, 0, 
    //                             -sinA, cosA, 0, 
    //                             -10, 10, 1);
    // cout << "H:\n" << H << endl;
    cv::Size s = src.size();
    
    cout << "X size: " << s.width << endl;
    cout << "Y size: " << s.height << endl;

    const Point2f points1a[4] = {{0,0},{5456,0},{5456,3632},{0,3632}};
    const Point2f points1b[4] = {{194.33306, 92.16646},{5261.66694, 92.16646},{5682.620, 3826.238},{-226.6203, 3826.2380}};
    Mat H = getPerspectiveTransform(points1a, points1b);
    cout << "H:\n" << H << endl;
    
    Mat img1_warp;
    warpPerspective(src, img1_warp, H, src.size());
    namedWindow( window_name2 + "_1", WINDOW_NORMAL );
    imshow( window_name2 + "_1", img1_warp );

    const Point2f points2a[4] = {{0,0},{5456,0},{5456,3632},{0,3632}};
    const Point2f points2b[4] = {{100.75525, 28.49883},{5355.24475, 28.49883},{5564.791, 3746.071},{-108.7914, 3746.0706}};
    H = getPerspectiveTransform(points2a, points2b);
    cout << "H:\n" << H << endl;
    
    Mat img2_warp;
    warpPerspective(src, img2_warp, H, src.size());
    namedWindow( window_name2 + "_2", WINDOW_NORMAL );
    imshow( window_name2 + "_2", img2_warp );
    waitKey(0);

    return 0;
}
