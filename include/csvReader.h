#pragma once

#include <fstream>
#include <string>
#include <vector>

#include <opencv2/core.hpp>

struct ImageMetadata
{
    std::string file;
    double z;
    double pitch;
    cv::Point2d shift;
    cv::Point2d size;
    cv::Mat transformation;
    std::vector<cv::Point2i> contour;
};

class CsvReader
{
public:
    std::vector<ImageMetadata> readCSV(const std::string &file);

private:
    ImageMetadata readCSVRow(std::string &row);
    std::string getToken(std::string &row, const std::string &delimiter);
};
