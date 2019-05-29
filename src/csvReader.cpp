#include "csvReader.h"

#include <iostream>

CsvReader::CsvReader() 
{}

std::vector<ImageMetadata> CsvReader::readCSV(std::ifstream &in)
{
    std::vector<ImageMetadata> table;
    std::string row;
    std::getline(in, row);
    while (!in.eof()) {
        std::getline(in, row);
        if (in.bad() || in.fail()) {
            break;
        }
        ImageMetadata imageMetadata = readCSVRow(row);
        table.push_back(imageMetadata);
    }
    return table;
}

std::string CsvReader::getToken(std::string &row, std::string delimiter) {
    size_t pos = 0;
    std::string token;
    if ((pos = row.find(delimiter)) != std::string::npos) {
        token = row.substr(0, pos);
        row.erase(0, pos + delimiter.length());
    } else if (row.length() != 0) {
        token = row.substr(0, pos);
    }
    return token;
}

ImageMetadata CsvReader::readCSVRow(std::string &row)
{
    ImageMetadata metaData;

    metaData.file = getToken(row, ",");
    metaData.top = std::stod(getToken(row, ","));
    metaData.bottom = std::stod(getToken(row, ","));
    metaData.z = std::stod(getToken(row, ","));
    metaData.alpha = std::stod(getToken(row, ","));

    std::string matrix = getToken(row, ",");
    cv::Mat transformMatrix = cv::Mat_<double>(3,3);
    cv::MatIterator_<double> it;
    for (it = transformMatrix.begin<double>(); it != transformMatrix.end<double>(); ++it) {
        *it = std::stod(getToken(matrix, "|"));
    }
    metaData.transformation = transformMatrix;

    return metaData;
}
