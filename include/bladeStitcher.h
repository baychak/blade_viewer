#pragma once

#include <string>
#include <opencv2/core.hpp>

#include "csvReader.h"

using namespace cv;

class BladeStitcher
{
public:
    BladeStitcher(double scaleFactor);
    void stitch(const std::string side); 

private:
    Rect findROIRect(const Mat &img, const Mat &dst, const Point2i &shift);
    void findWarpedImageParameters(ImageMetadata &imageMetadata, const Size &size);
    
    CsvReader mCsvReader;
    double mScaleFactor;
};
