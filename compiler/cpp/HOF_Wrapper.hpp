#ifndef HOF_WRAPPER_H
#define HOF_WRAPPER_H

// Defines the default block and grid size
#ifndef BLOCK_SIZE
#define BLOCK_SIZE 1024

#ifndef GRID_SIZE
#define GRID_SIZE 32

/ Include statements
#include <stdlib.h>
#include <stdarg.h>
#include <iostream>
#include "VLC_Array.hpp"
using namespace std;

// Useful Macros for CUDA
#define min(a, b) (((a) < (b)) ? (a) : (b))
#define max(a, b) (((a) > (b)) ? (a) : (b))

// CUDA Error checking function

void checkCudaErrors(CUresult err) {
	assert(err == CUDA_SUCCESS);
}

class HOF_Wrapper(){
	VLC_Array *input_arrays;
	
}