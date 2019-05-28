#pragma once

#include <fstream>
#include <string>
#include <vector>

#include <opencv2/core.hpp>

struct ImageMetadata
{
    std::string file;
    double x;
    double y;
    double z;
    double alpha;
    cv::Mat transformation;
};

class CsvReader
{
public:
    CsvReader();

    std::vector<ImageMetadata> readCSV(std::ifstream &in);
private:
    ImageMetadata readCSVRow(std::string &row);
    std::string getToken(std::string &row, std::string delimiter);
};
