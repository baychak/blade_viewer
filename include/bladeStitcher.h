#pragma once

#include <string>
#include <opencv2/core.hpp>

#include "csvReader.h"

using namespace cv;

class BladeStitcher
{
public:
    void stitch(const std::string side); 

private:
    Rect findImageROI(const Mat &img, const Mat &dst, const Point2i &shift);

    CsvReader mCsvReader;
};
