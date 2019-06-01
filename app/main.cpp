#include <string>
#include <iostream>

#include "bladeStitcher.h"

int main(int argc, char* argv[])
{
    double scaleFactor;
    if (argc == 1)
    {
        std::cout << "Use default scale factor" << std::endl;
        scaleFactor = 0.0007120899;
    } else {
        scaleFactor = std::stod(argv[1]);
    }

    BladeStitcher stitcher(scaleFactor);

    stitcher.stitch("leading-edge");
    //stitcher.stitch("suction-side");
    //stitcher.stitch("trailing-edge");
    //stitcher.stitch("pressure-side");

    return 0;
}
