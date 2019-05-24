cmake_minimum_required(VERSION 3.10)
project(blade_viewer)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -Wall -Wextra")
set(EXECUTABLE_OUTPUT_PATH bin)

include_directories("/usr/local/include/opencv4/")
link_directories("/usr/local/lib/")

set(SOURCE_FILES app/main.cpp)

add_executable(${PROJECT_NAME} ${SOURCE_FILES})

target_link_libraries(${PROJECT_NAME} 
                        opencv_core 
                        opencv_features2d
                        opencv_stitching 
                        opencv_imgcodecs 
                        opencv_highgui 
                        opencv_imgproc
                        opencv_calib3d
                    )