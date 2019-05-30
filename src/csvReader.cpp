#include "csvReader.h"

#include <iostream>

std::vector<ImageMetadata> CsvReader::readCSV(const std::string &file)
{
    std::ifstream in(file);
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
    in.close();
    return table;
}

std::string CsvReader::getToken(std::string &row, const std::string &delimiter) {
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
    metaData.z = std::stod(getToken(row, ","));
    metaData.alpha = std::stod(getToken(row, ","));

    std::string point = getToken(row, ",");
    metaData.shift = {std::stod(getToken(point, "|")), std::stod(getToken(point, "|"))};

    point = getToken(row, ",");
    metaData.size = {std::stod(getToken(point, "|")), std::stod(getToken(point, "|"))};

    point = getToken(row, ",");
    metaData.center = {std::stod(getToken(point, "|")), std::stod(getToken(point, "|"))};

    std::string matrix = getToken(row, ",");
    cv::Mat transformMatrix = cv::Mat_<double>(3,3);
    cv::MatIterator_<double> it;
    for (it = transformMatrix.begin<double>(); it != transformMatrix.end<double>(); ++it) {
        *it = std::stod(getToken(matrix, "|"));
    }
    metaData.transformation = transformMatrix;
    // std::cout << "file " << metaData.file << std::endl;
    // std::cout << "z " << metaData.z << std::endl;
    // std::cout << "alpha " << metaData.alpha << std::endl;
    // std::cout << "shift " << metaData.shift << std::endl;
    // std::cout << "size " << metaData.size << std::endl;
    // std::cout << "center " << metaData.center << std::endl;
    // std::cout << "transformation " << metaData.transformation << std::endl;

    return metaData;
}
