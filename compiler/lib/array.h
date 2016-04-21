#ifndef ARRAY_H
#define ARRAY_H

#define BLOCK_SIZE 1024
#define GRID_SIZE 32

// Useful Macros for CUDA
#define min(a,b)
template <class T>
class VLC_Array {
	int length;
	T*  array;
	VLC_Array();
	// Assignment
	VLC_Array& operator=(const VLC_Array& other_array);
	// Arithmetic Operators
	VLC_Array& operator+(const VLC_Array& other_array);
	VLC_Array& operator-(const VLC_Array& other_array);
	VLC_Array& operator*(const VLC_Array& other_array);
	VLC_Array& operator/(const VLC_Array& other_array);
	// Matrix multiplication
	VLC_Array& operator**(const VLC_Array& other_array);
	// Bitshift
	VLC_Array& operator<<(int shift_level);
	VLC_Array& operator>>(int shift_level);

	// Copy from host to kernel

	// Copy from kernel to host




}