#ifndef VLC_ARRAY_H
#define VLC_ARRAY_H

// Defines the default block and grid size
#ifndef BLOCK_SIZE
#define BLOCK_SIZE 1024

#ifndef GRID_SIZE
#define GRID_SIZE 32

// Include statements
#include <stdlib.h>
#include <stdarg.h>
#include <iostream>
using namespace std;

// Useful Macros for CUDA
#define min(a, b) (((a) < (b)) ? (a) : (b))
#define max(a, b) (((a) > (b)) ? (a) : (b))

// CUDA Error checking function

void checkCudaErrors(CUresult err) {
	assert(err == CUDA_SUCCESS);
}

// VLC Array class
template <class T>
class VLC_Array {
	private:
		int number_of_dimensions;
		// Dimensions are listed in order
		int *dimensions_list;
		
		int current_length;
		T*  values;
	public:
		VLC_Array();
		VLC_Array(int number_of_dimensions, ...);
		VLC_Array(const VLC_Array<T> &vlcarray);
		// Assignment
		VLC_Array& operator=(const VLC_Array& vlcarray);
		// Arithmetic Operators
		VLC_Array& operator+(const VLC_Array& vlcarray);
		VLC_Array& operator-(const VLC_Array& vlcarray);
		VLC_Array& operator*(const VLC_Array& vlcarray);
		VLC_Array& operator/(const VLC_Array& vlcarray);
		// Matrix multiplication
		VLC_Array& operator**(const VLC_Array& vlcarray);
		// Bitshift
		VLC_Array& operator<<(int shift_level);
		VLC_Array& operator>>(int shift_level);
		// Copy from host to kernel
		void copyToKernel();
		// Copy from kernel to host
		void copyToHost();

};

template <class T>
VLC_Array::VLC_Array(){
	number_of_dimensions = 0;
	dimensions_list = NULL;
	current_length = 0;
	values = NULL; 
}


template <class T>
VLC_Array::VLC_Array(const VLC_Array<T> &vlcarray){
	number_of_dimensions = vlcarray.number_of_dimensions;
	dimensions_list = vlcarray.dimensions_list;
	current_length = vlcarray.current_length;
	values = vlcarray.values;

}