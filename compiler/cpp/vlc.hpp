#ifndef VLC_H
#define VLC_H


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

// Useful Macros for CUDA
// #define min(a, b) (((a) < (b)) ? (a) : (b))
// #define max(a, b) (((a) > (b)) ? (a) : (b))

// CUDA Error checking function
// void checkCudaErrors(CUresult err) {
// 	assert(err == CUDA_SUCCESS);
// }

/* Why this class exists:
	- For ensuring that we don't have any arrays allocated on the stack and all are allocated on the heap
	( can get messy with memory otherwise ) 
	- To bypass C/C++ not being able to do things like the following assignment
			size_t a[5];
			size_t b[5] = {1,2,3,4,5};
			a=b;

			!!size_t[5] not assignable error!!
*/

// VLC Array class
template <class T>
class VLC_Array {
	private:
		size_t num_values; //Tells us how many values are in the array in total. Ex. would be 4 if [2][2] array
		T*  values; // Posize_ter to values in array
		
		size_t num_dimensions; // Integer that tells us how many dimensions the array contains
		size_t *dimensions; // Integer array of the dimensions of the VLC_Array
	public:
		// Constructors and Destructors
		VLC_Array();
		VLC_Array(size_t num_values, size_t num_dimensions,...); 
		VLC_Array(size_t num_values, T*values, size_t num_dimensions, size_t *dimensions);																		// For declarations
		VLC_Array(size_t num_values, size_t num_dimensions,size_t total_args...); 			// For declarations and initializations like size_t a[5] = {1,2,3,4,5}
		VLC_Array(const VLC_Array<T> &vlcarray); 	// For assignments like size_t a[1] = {5}, size_t b[1]={7},  a=b
		~VLC_Array();
		
		/* Class Accessors and Getters */
		T* 	 	get_values() const ; // Returns the posize_ter to VLC's size_ternal array
		size_t* get_dimensions() const; // Returns the posize_ter to VLC's dimensions
		size_t  get_num_dimensions () const; // Returns number of dimensions
		size_t  size()const; // Returns length of first dimension
		size_t  total_elements() const; // Returns total elements in the array 

		/* Element Accessors and Getters */
		T get_element_value(size_t number_accessing_dims,...) const; 
		VLC_Array<T> get_array_value_host(size_t number_accessing_dims,...) const;
		T* get_array_value_kernel(size_t number_accessing_dims,...) const;
		void set_element_value(T new_value,size_t number_accessing_dims, ...);
		void set_array_value(const VLC_Array<T> &array,size_t number_accessing_dims, ...);
		VLC_Array<T> operator=(const VLC_Array<T> &vlcarray);
	
};

/*---------------------------------- Regular constructors ----------------------------------*/
template <class T>
VLC_Array<T>::VLC_Array(){
	this->num_values = 0;
	this->values = NULL;
	this->num_dimensions = 0;
	this->dimensions = NULL;
}


template <class T>
VLC_Array<T>::VLC_Array(size_t num_values, T*values, size_t num_dimensions, size_t *dimensions){
	this->num_values = num_values;
	this->num_dimensions = num_dimensions;

	T *values_copy = (T*)calloc(num_values,sizeof(T));
	for(size_t i = 0; i < num_values; i++){
		values_copy[i] = values[i];
	}

	size_t *dims_copy = (size_t*)calloc( num_dimensions,sizeof(size_t));
	for(size_t j = 0; j < num_dimensions; j++){
		dims_copy[j] = dimensions[j];
	}

	this->values = values_copy;
	this->dimensions = dims_copy;
}

//Declarations
template <class T>
VLC_Array<T>::VLC_Array(size_t num_values, size_t num_dimensions,...){ 
	/* Assign the dimensions and values */
	this->num_dimensions = num_dimensions;
	this->num_values = num_values;

	this->dimensions = (size_t *)calloc( num_dimensions,sizeof(size_t));
	this->values = (T*)calloc( num_values,sizeof(T));

	/* Now access the values that are passed in */
	std::cout<<num_dimensions<<std::endl;
	va_list args;
	va_start(args,num_dimensions);
	for(size_t i = 0; i < num_dimensions; 	i++)	{ 	this->dimensions[i] = va_arg(args,size_t); 	}
	va_end(args);
}

// Declarations, Assignments by value
template <class T>
VLC_Array<T>::VLC_Array(size_t num_values, size_t num_dimensions,size_t total_args...){ 
	/* Assign the dimensions and values */
	this->num_dimensions = num_dimensions;
	this->num_values = num_values;

	this->dimensions = (size_t *)calloc(num_dimensions,sizeof(size_t));
	this->values = (T*)calloc( num_values,sizeof(T));

	/* Now access the values that are passed in */
	va_list args;
	va_start(args,total_args);
	for(size_t i = 0; i < num_dimensions; 	i++)	{ 	this->dimensions[i] = va_arg(args,size_t); 	}
	for(size_t j = 0; j < num_values; 		j++)	{	this->values[j] = va_arg(args,T);			}
	va_end(args);
}

// Assignments to other arrays
template <class T>
VLC_Array<T>::VLC_Array(const VLC_Array<T> &vlcarray){
	/* For now, make a deep copy every time. Can optimize later */
	this->num_values = vlcarray.total_elements();
	this->num_dimensions = vlcarray.get_num_dimensions();

	this->values = (T *)calloc(this->num_values,sizeof(T));
	this->dimensions = (size_t *)calloc( this->num_dimensions,sizeof(size_t));

	/* Now access the values that are passed in */
	for(size_t j = 0; j < this->num_values; 	j++)	{ 		this->values[j] 		= vlcarray.get_values()[j]; 			}
	for(size_t i = 0; i < this->num_dimensions;i++)	{ 		this->dimensions[i] 	= vlcarray.get_dimensions()[i]; 		}
}

// Destructor
template <class T>
VLC_Array<T>::~VLC_Array(){
	free(this->values);
	free(this->dimensions);
}

/*---------------------------------- Accessing Functions ----------------------------------*/
// Get Element Value
// Accesses element of the array - must check num_accessing = num_dims in semant
template <class T>
T VLC_Array<T>::get_element_value(size_t number_accessing_dims,...) const{
	size_t index = 1;
	size_t corr_dim;
	printf("num_access_dim%zu\n",number_accessing_dims );
	va_list dims;
	va_start(dims,number_accessing_dims);
	for(size_t i=0; i < number_accessing_dims;i ++){
		index = va_arg(dims,size_t) * index;
		printf("index right now is %zu\n",index);
		corr_dim = this-> dimensions[i];
		printf("dim right now is%zu\n",corr_dim);
		index = i * corr_dim + index;
	}
	printf("%zu\n",index);
	va_end(dims);
	return this->values[index];
}

// Get Array Value In Host
// Accesses an array of the array - must check num_accessing < num_dims in semant
template <class T>
VLC_Array<T> VLC_Array<T>::get_array_value_host(size_t number_accessing_dims,...) const{ 
	// Get where new array starts
	size_t index = 1;
	size_t corr_dim;
	va_list dims;
	va_start(dims,number_accessing_dims);
	for(size_t i=0; i < number_accessing_dims; i++){
		index = va_arg(dims,size_t) * index;
		corr_dim = this-> dimensions[i];
		index = i * corr_dim + index;
	}
	va_end(dims);

	// Get all the elements in this new array
	size_t num_elements = 1;
	for(size_t i = this->num_dimensions - number_accessing_dims; i < this->num_dimensions;i++){
		num_elements = num_elements * this->dimensions[i];
	}

	// Set values
	size_t num_dimensions = this->num_dimensions - number_accessing_dims;
	size_t *new_dimensions = &(this->dimensions[this->num_dimensions - number_accessing_dims]);
	size_t num_values = num_elements;
	size_t *new_values = this->values[index];

	// Return a VLC_Array
	return VLC_Array(num_values,new_values,num_dimensions,new_dimensions);

}

// Get Array Value In Kernel
// Accesses an array of the array - must check num_accessing < num_dims in semant
template <class T>
T* VLC_Array<T>::get_array_value_kernel(size_t number_accessing_dims,...) const{
	// Get where new array starts
	size_t index = 1;
	size_t corr_dim;
	va_list dims;
	va_start(dims,number_accessing_dims);
	for(size_t i=0; i < number_accessing_dims; i++){
		index = va_arg(dims,size_t) * index;
		corr_dim = this-> dimensions[i];
		index = i * corr_dim + index;
	}
	va_end(dims);

	// Get all the elements in this new array
	size_t num_elements = 1;
	for(size_t i = this->num_dimensions - number_accessing_dims; i < this->num_dimensions;i++){
		num_elements = num_elements * this->dimensions[i];
	}

	// Set values
	size_t num_dimensions = this->num_dimensions - number_accessing_dims;
	size_t *new_dimensions = &(this->dimensions[this->num_dimensions - number_accessing_dims]);
	size_t num_values = num_elements;
	size_t *new_values = this->values[index];

	// Return a VLC_Array
	return VLC_Array(num_values,new_values,num_dimensions,new_dimensions);

}

// Set Element Value
// Sets value for element of an array
template <class T>
void VLC_Array<T>::set_element_value(T new_value,size_t number_accessing_dims,...){
	// Get where new array starts
	size_t index = 1;
	size_t corr_dim;
	va_list dims;
	va_start(dims,number_accessing_dims);
	for(size_t i=0; i < number_accessing_dims; i++){
		index = va_arg(dims,size_t) * index;
		corr_dim = this-> dimensions[i];
		index = i * corr_dim + index;
	}
	va_end(dims);
	printf("new value is %d\n", new_value);
	this->get_values()[index] = new_value;
}

// Set Array Value
// Sets value for an array of an array 
template <class T>
void VLC_Array<T>::set_array_value(const VLC_Array<T> &array,size_t number_accessing_dims,...){
	// Get where new array starts
	size_t index = 1;
	size_t corr_dim;
	va_list dims;
	va_start(dims,number_accessing_dims);
	for(size_t i=0; i < number_accessing_dims; i++){
		index = va_arg(dims,size_t) * index;
		corr_dim = this-> dimensions[i];
		index = i * corr_dim + index;
	}
	va_end(dims);

	//Get number of elements to replace
	size_t num_elements = 1;
	for(size_t i = this->num_dimensions - number_accessing_dims; i < this->num_dimensions;i++){
		num_elements = num_elements * this->dimensions[i];
	}
	// Copy values
	for(size_t i =0; i < num_elements; i++){
		this->values[int(index + i)] = array.get_element_value(1,i);
	}
}
//Operator =
template <class T>
VLC_Array<T> VLC_Array<T>::operator=(const VLC_Array<T> &vlcarray){
	if(this == &vlcarray){
		return *this;
	}
	/* For now, make a deep copy every time. Can optimize later */
	num_values = vlcarray.total_elements();
	num_dimensions = vlcarray.get_num_dimensions();

	values = (T*)calloc(sizeof(T) * vlcarray.total_elements());
	dimensions = (size_t *)calloc(sizeof(size_t) * vlcarray.get_num_dimensions());

	/* Now access the values that are passed in */
	for(size_t j = 0; j < this->num_values; 	j++)	{ 		this->values[int(j)] 			= vlcarray.get_values()[int(j)]; 			}
	for(size_t i = 0; i < this->num_dimensions; i++)	{ 			this->dimensions[int(i)] 		= vlcarray.get_dimensions()[int(i)]; 		}
	return *this;
}

template <class T> 
T* VLC_Array<T>::get_values() const{ return this->values;}

template <class T>
size_t* VLC_Array<T>::get_dimensions() const{ return this->dimensions;}

template <class T>
size_t VLC_Array<T>::get_num_dimensions() const{ return this->num_dimensions; }

template <class T>
size_t VLC_Array<T>::size()const{ return this->dimensions[0]; }

template <class T>
size_t VLC_Array<T>::total_elements() const{ return this->num_values; }

#endif