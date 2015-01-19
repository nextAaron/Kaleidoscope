cmake_minimum_required(VERSION 2.8.11)
project(Kaleidoscope)

#set(llvm /usr/local/Cellar/llvm/3.5.0_2/)
#set(CMAKE_CXX_COMPILER /usr/local/bin/g++-4.9)
#${llvm}/bin/clang++)

add_definitions(-D __STDC_CONSTANT_MACROS -D __STDC_LIMIT_MACROS -D NDEBUG)
add_compile_options(-std=c++14 -pedantic -Wall -Wextra -Ofast -march=native)
#include_directories(${llvm}include/)
#link_directories(${llvm}lib/)

add_executable(toy.exe toy.cpp)
target_link_libraries(toy.exe LLVMX86Disassembler LLVMX86AsmParser LLVMX86CodeGen LLVMSelectionDAG LLVMAsmPrinter
  LLVMCodeGen LLVMScalarOpts LLVMInstCombine LLVMTransformUtils LLVMipa LLVMAnalysis LLVMX86Desc LLVMX86Info
  LLVMX86AsmPrinter LLVMX86Utils LLVMMCJIT LLVMTarget LLVMRuntimeDyld LLVMObject LLVMMCParser LLVMBitReader
  LLVMExecutionEngine LLVMMC LLVMCore LLVMSupport curses z)


