cmake_minimum_required(VERSION 2.8.11)
project(Kaleidoscope)

add_executable(toy.exe toy.cpp)

if(${CMAKE_CXX_COMPILER_ID} STREQUAL "GNU")
	set(CMAKE_CXX_FLAGS "-std=c++14 -pedantic -Wall -Wextra -Ofast -march=native -D NDEBUG -pipe")
	set(CMAKE_EXE_LINKER_FLAGS "-flto")
endif()

if(${CMAKE_CXX_COMPILER_ID} STREQUAL "Clang")
	set(CMAKE_CXX_FLAGS "-std=c++11 -pedantic -Wall -Wextra -Ofast -march=native -D NDEBUG -pipe")
	set(CMAKE_EXE_LINKER_FLAGS "-flto")
endif()

