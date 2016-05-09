#ifndef VLC_ARRAY_H
#define VLC_ARRAY_H
#endif

// Defines the default block and grid size
#ifndef BLOCK_SIZE
#define BLOCK_SIZE 1024
#endif

#ifndef GRID_SIZE
#define GRID_SIZE 32
#endif
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
		VLC_Array(int num_values, T*values, int num_dimensions, int *dimensions);
		VLC_Array(int num_values, int num_dimensions,...); 			// For declarations and initializations like int a[5] = {1,2,3,4,5}
		VLC_Array(const VLC_Array<T> &vlcarray); 	// For assignments like int a[1] = {5}, int b[1]={7},  a=b
		~VLC_Array();
		
		/* Class Accessors and Getters */
		T* 	 get_values(); // Returns the pointer to VLC's internal array
		int* get_dimensions(); // Returns the pointer to VLC's dimensions
		int  size(); // Returns length of first dimension
		int  total_elements(); // Returns total elements in the array 

		/* Element Accessors and Getters */
		T get_element_value(int number_accessing_dims,...); 
		VLC_Array<T> get_array_value(int number_accessing_dims,...);
		void set_element_value(int number_accessing_dims, T new_value,...);
		void set_array_value(int number_accessing_dims, VLC_Array<T> array,...);
	
};

/*---------------------------------- Regular constructors ----------------------------------*/
template <class T>
VLC_Array<T>::VLC_Array(int num_values, T*values, int num_dimensions, int *dimensions){
	this->num_values = num_values;
	this->num_dimensions = num_dimensions;

	T *values_copy = (T*)malloc(sizeof(T) * num_values);
	for(int i = 0; i < num_values; i++){
		values_copy[i] = values[i];
	}

	int *dims_copy = (int*)malloc(sizeof(int) * num_dimensions);
	for(int j = 0; j < num_dimensions; j++){
		dims_copy[j] = dimensions[j];
	}

	this->values = values_copy;
	this->dimensions = dims_copy;
}

// Declarations, Assignments by value
template <class T>
VLC_Array<T>::VLC_Array(int num_values, int num_dimensions,...){ 
	/* Assign the dimensions and values */
	this->num_dimensions = num_dimensions;
	this->num_values = num_values;

	this->dimensions = (int *)malloc(sizeof(int) * num_dimensions);
	this->values = (T*)malloc(sizeof(T) * num_values);

	/* Now access the values that are passed in */
	int total = num_dimensions + num_values;
	va_list args;
	va_start(args,total);
	for(int i = 0; i < num_dimensions; 	i++)	{ 	this->dimensions[i] = va_arg(args,int); 	}
	for(int j = 0; j < num_values; 		j++)	{	this->values[j] = va_arg(args,T);			}
	va_end(args);
}

// Assignments to other arrays
template <class T>
VLC_Array<T>::VLC_Array(const VLC_Array<T> &vlcarray){
	/* For now, make a deep copy every time. Can optimize later */
	this->num_values = vlcarray.num_values;
	this->num_dimensions = vlcarray.num_dimensions;

	this->values = malloc(sizeof(T) * this->num_values);
	this->dimensions = malloc(sizeof(int) * this->num_dimensions);

	/* Now access the values that are passed in */
	for(int j = 0; j < this->num_values; 	j++)	{ 		this->values[j] 		= vlcarray.get_values()[j]; 			}
	for(int i = 0; i < this->num_dimensions;i++)	{ 		this->dimensions[i] 	= vlcarray.get_dimensions()[i]; 		}
}

// Destructor
template <class T>
VLC_Array<T>::~VLC_Array(){
	free(this->values);
	free(this->dimensions);
}

/*---------------------------------- Accessing Functions ----------------------------------*/
// Get Values
// Returns the pointer to VLC's internal array
template <class T>
T* VLC_Array<T>::get_values()		{ return this->values; }


// Get Dimensions
// Returns the pointer to VLC's dimension list
template <class T>
int* VLC_Array<T>::get_dimensions()	{ return this->dimensions; }


// Get Element Value
// Accesses element of the array - must check num_accessing = num_dims in semant
template <class T>
T VLC_Array<T>::get_element_value(int number_accessing_dims,...){
	int index = 1;

	va_list dims;
	va_start(dims,number_accessing_dims);
	for(int i=0; i < number_accessing_dims;i ++){
		index = va_arg(dims,int) * index;
	}
	va_end(dims);
	return this->values[index];
}

// Get Array Value
// Accesses an array of the array - must check num_accessing < num_dims in semant
template <class T>
VLC_Array<T> VLC_Array<T>::get_array_value(int number_accessing_dims,...){
	// Get where new array starts
	int index = 1;
	va_list dims;
	va_start(dims,number_accessing_dims);
	for(int i=0; i < number_accessing_dims; i++){
		index = va_arg(dims,int) * index;
	}
	va_end(dims);

	// Get all the elements in this new array
	int num_elements = 1;
	for(int i = this->num_dimensions - num_accessing_dims; i < this->num_dimensions;i++){
		num_elements = num_elements * this->dimensions[i];
	}

	// Set values
	int num_dimensions = this->num_dimensions - number_accessing_dims;
	int *new_dimensions = this->dimensions[this->num_dimensions - number_accessing_dims];
	int num_values = num_elements;
	int *new_values = this->values[index];

	// Return a VLC_Array
	return VLC_Array(num_values,new_values,num_dimensions,new_dimensions);

}

// Set Element Value
// Sets value for element of an array
template <class T>
void VLC_Array<T>::set_element_value(int number_accessing_dims,T new_value,...){
	// Get where new array starts
	int index = 1;
	va_list dims;
	va_start(dims,number_accessing_dims);
	for(int i=0; i < number_accessing_dims; i++){
		index = va_arg(dims,int) * index;
	}
	va_end(dims);

	this->values[index] = new_value;
}

// Set Array Value
// Sets value for an array of an array 
template <class T>
void VLC_Array<T>::set_array_value(int number_accessing_dims,VLC_Array<T> array,...){
	// Get where new array starts
	int index = 1;
	va_list dims;
	va_start(dims,number_accessing_dims);
	for(int i=0; i < number_accessing_dims; i++){
		index = va_arg(dims,int) * index;
	}
	va_end(dims);

	//Get number of elements to replace
	int num_elements = 1;
	for(int i = this->num_dimensions - number_accessing_dims; i < this->num_dimensions;i++){
		num_elements = num_elements * this->dimensions[i];
	}
	// Copy values
	for(int i =0; i < num_elements; i++){
		this->values[index + i] = array[i];
	}
}


