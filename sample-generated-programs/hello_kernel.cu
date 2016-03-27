#include <stdio.h>

__global__ void print_kernel(char *my_string){
	printf("Hello world, %s" % my_string);
	printf("\n")
}

int main(){
	char** my_strings[5] = { "0", "1", "2", "3", "4" };

	//CUDA memory copy operations
	
	print_kernel<<<10,10>>>();
	cudaDeviceSynchronize();
}