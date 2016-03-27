#include <stdio.h>

__global__ void print_kernel(char * my_string){
	printf("Hello world, %s\n" % my_string);
}

int main(){
	char** my_strings[5] = { "0", "1", "2", "3", "4" };
	//memory copy operation
	print_kernel<<<10,10>>>();
	cudaDeviceSynchronize();
}