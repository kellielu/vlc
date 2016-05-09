#ifndef VLC_ARRAY_H
#define VLC_ARRAY_H

// Defines the default block and grid size
#ifndef BLOCK_SIZE
#define BLOCK_SIZE 1024

#ifndef GRID_SIZE
#define GRID_SIZE 32

// Include statements
#include <stdlib.h>
#include <iostream>
#include <stdarg.h>
using namespace std;

// Useful Macros for CUDA
// #define min(a, b) (((a) < (b)) ? (a) : (b))
// #define max(a, b) (((a) > (b)) ? (a) : (b))

// CUDA Error checking function
// void checkCudaErrors(CUresult err) {
// 	assert(err == CUDA_SUCCESS);
// }

/* Why this class exists:
	- For codegenning array operations such as a1 + a2
	- For ensuring that we don't have any arrays allocated on the stack and all are allocated on the heap
	( can get messy with memory otherwise ) 
	- Allows easy copy to and from CPU and GPU
	- To bypass C/C++ not being able to do things like the following assignment
			int a[5];
			int b[5] = {1,2,3,4,5};
			a=b;

			!!int[5] not assignable error!!
*/

// VLC Array class
template <class T>
class VLC_Array {
	private:
		int num_values; //Tells us how many values are in the array in total. Ex. would be 4 if [2][2] array
		T*  values; // Pointer to values in array
		
		int num_dimensions; // Integer that tells us how many dimensions the array contains
		int *dimensions; // Integer array of the dimensions of the VLC_Array
	public:
		// Constructors and Destructors
		VLC_Array(int num_dimensions,...); 			// For declarations and initializations like int a[5] = {1,2,3,4,5}
		VLC_Array(const VLC_Array<T> &vlcarray); 	// For assignments like int a[1] = {5}, int b[1]={7},  a=b
		~VLC_Array();
		
		/* Class Accessors and Getters */
		T* 	 get_values(); // Returns the pointer to VLC's internal array
		int* get_dimensions(); // Returns the pointer to VLC's dimensions
		int  size(); // Returns length of first dimension
		int  total_elements(); // Returns total elements in the array 

		/* Element Accessors and Getters */
		T operator[](int i); // Accesses ith element of the array
		void assign(T,int number_accessing_dims,...); // Assigns ith element of the array with value T
		


		// // Assignment
		// VLC_Array& operator=(const VLC_Array& vlcarray);
		// // Arithmetic Operators
		// VLC_Array& operator+(const VLC_Array& vlcarray);
		// VLC_Array& operator-(const VLC_Array& vlcarray);
		// VLC_Array& operator*(const VLC_Array& vlcarray);
		// VLC_Array& operator/(const VLC_Array& vlcarray);
		// // Matrix multiplication
		// VLC_Array& operator**(const VLC_Array& vlcarray);
		// // Bitshift
		// VLC_Array& operator<<(int shift_level);
		// VLC_Array& operator>>(int shift_level);
		// // Copy from host to kernel
		// void copyToKernel();
		// // Copy from kernel to host
		// void copyToHost();
};

/*---------------------------------- Regular constructors ----------------------------------*/
// Declarations, Assignments by value
template <class T>
VLC_Array<T>::VLC_Array(int num_values, int num_dimensions,...){ 
	/* Assign the dimensions and values */
	this.num_dimensions = num_dimensions;
	this.num_values = num_values;

	this.dimensions = malloc(sizeof(int) * num_dimensions);
	this.values = malloc(sizeof(T) * num_values);

	/* Now access the values that are passed in */
	va_list args;
	va_start(args,num_dimensions + num_values);
	for(int i = 0; i < num_dimensions)	{ 	this.dimensions[i] = va_arg(args,int); 	}
	for(int j = 0; j < num_values; j++)	{	this.values[j] = va_arg(args,T);		}
	va_end(args);
}

// Assignments to other arrays
template <class T>
VLC_Array<T>::VLC_Array(const VLC_Array<T> &vlcarray){
	/* For now, make a deep copy every time. Can optimize later */
	this.num_values = vlcarray.num_values;
	this.num_dimensions = vlcarray.num_dimensions;

	this.values = malloc(sizeof(T) * this.num_values);
	this.dimensions = malloc(sizeof(int) * this.num_dimensions);

	/* Now access the values that are passed in */
	for(int j = 0; j < this.num_values; j++){ 		this.values[j] 		= vlcarray.get_values()[j]; 			}
	for(int i = 0; i < num_dimensions;  i++){ 		this.dimensions[i] 	= vlcarray.get_dimensions()[i]; 		}
}

// Destructor
template <class T>
VLC_Array<T>::~VLC_Array(){
	free(this.values);
	free(this.dimensions);
}

/*---------------------------------- Accessing Functions ----------------------------------*/
// Accesses ith element of the array
template <class T,int n>
T VLC_Array<T,n>::operator[](int i){
	return this.values[i];
}

// Assigns ith element of the array with value T
template <class T,int n>
void VLC_Array<T,n>::assign(int i,T){
	this.values[i] = T;
}

// Returns the pointer to VLC's internal array
template <class T,int n>
T* VLC_Array<T,n>::get_array_pointer(){
	return this.values;
}

//Gets size of array
template <class T,int n>
int VLC_Array<T,n>::size(){
	return this.length;
}
