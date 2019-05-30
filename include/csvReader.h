#pragma once

#include <fstream>
#include <string>
#include <vector>

#include <opencv2/core.hpp>

struct ImageMetadata
{
    std::string file;
    double z;
    double alpha;
    cv::Point2d shift;
    cv::Point2d size;
    cv::Point2d center;
    cv::Mat transformation;
};

class CsvReader
{
public:
    std::vector<ImageMetadata> readCSV(const std::string &file);

private:
    ImageMetadata readCSVRow(std::string &row);
    std::string getToken(std::string &row, const std::string &delimiter);
};
