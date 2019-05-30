#include <string>

#include "bladeStitcher.h"

int main()
{

    BladeStitcher stitcher;

    stitcher.stitch("leading-edge");
    stitcher.stitch("suction-side");
    stitcher.stitch("trailing-edge");
    stitcher.stitch("pressure-side");

    return 0;
}
