cmake_minimum_required(VERSION 3.17)
project(wamc C)

set(CMAKE_C_STANDARD 11)

add_executable(wamc main.c wam.h wam.c)