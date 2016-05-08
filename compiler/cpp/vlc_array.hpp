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
template <class T,int n>
class VLC_Array {
	private:
		int length; // Size of current array
		T*  values; // Pointer to values in array

		int ndimensions; // Integer that tells us how many dimensions the array contains
		int *dimensions; // Integer array of the dimensions of the VLC_Array
		
	public:
		// Constructors and Destructors
		VLC_Array(); // For declarations like int a[5]
		VLC_Array(...); // For initializations like int a[5] = {1,2,3,4,5}
		VLC_Array(T* values);
		VLC_Array(const VLC_Array<T> &vlcarray); // For assignments like int a[1] = {5}, int b[1]={7},  a=b
		~VLC_Array();
		
		T operator[](int i); // Accesses ith element of the array
		void assign(int i,T); // Assigns ith element of the array with value T
		T* get_array_pointer(); // Returns the pointer to VLC's internal array
		int size();

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
// Declarations
template <class T,int n>
VLC_Array<T, n>::VLC_Array(){ this.length = n; this.values = malloc(sizeof(T) * n); }

// Assignments direct
template <class T,int n>
VLC_Array<T, n>::VLC_Array(...){

	va_list array_elements;// Retrieve arguments
	va_start(array_elements,n);
	T* values = malloc(sizeof(T)* n);// Set values to the what is given
	for(int i = 0; i < n; i++){
		values[i] = va_arg(array_elements,T);
	}
	va_end(array_elements);
	this.length = n;// Set private values
	this.values = values;
}

// Assignments to other arrays
template <class T,int n>
VLC_Array<T, n>::VLC_Array(const VLC_Array<T> &vlcarray){
	this.length = vlcarray.length;
	this.values = vlcarray.values;
}

template <class T,int n>
VLC_Array<T,n>::VLC_Array(T*values){

	this.values = values;
}
// Destructor
template <class T,int n>
VLC_Array<T, n>::~VLC_Array(){
	free(this.values);
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
