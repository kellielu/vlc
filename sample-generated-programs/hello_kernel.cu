
#include <stdio.h>
#include <stdlib.h>
#include "cuda.h"
#include <iostream>
#include <vlc>
#include <stdargs.h>
CUdevice    device;
CUmodule    cudaModule;
CUcontext   context;
CUfunction  function;
VLC_Array <VLC_Array<int>>map_c0(...){

checkCudaErrors(cuCtxCreate(&context, 0, device));

std::ifstream add("add.ptx");
std::ifstream map_ptx0("map_ptx0.ptx");

std::string add_str((std::istreambuf_iterator<char>(add)), std::istreambuf_iterator<char>());
std::string map_ptx0_str((std::istreambuf_iterator<char>(map_ptx0)), std::istreambuf_iterator<char>());

map_ptx0_str = add_str +"\n" + map_ptx0_str

checkCudaErrors(cuModuleLoadDataEx(&cudaModule,map_ptx0_str, 0, 0, 0));
checkCudaErrors(cuModuleGetFunction(&function, cudaModule, "map_ptx0"));

size_t num_constants = 1;
size_t num_input_arrays = 2;

int host_ptr3;
int* host_ptr1;
int* host_ptr2;
int* host_ptr0;

CUdeviceptr scale;
CUdeviceptr dev_ptr1;
CUdeviceptr dev_ptr2;
CUdeviceptr dev_ptr0;

va_list constants;
va_start(constants,num_constants);
for(int i = 0; i < num_constants; i++){

if(i ==0){
	host_ptr3 = va_args(constants,int);
	checkCudaErrors(cuMemAlloc(&scale, sizeof(int)*1));
	checkCudaErrors(cuMemcpyHtoD(scale, &host_ptr3, sizeof(int)*1));

}

}
va_end(constants);

checkCudaErrors(cuMemAlloc(&dev_ptr1, sizeof(int)*5));
checkCudaErrors(cuMemAlloc(&dev_ptr2, sizeof(int)*5));
checkCudaErrors(cuMemcpyHtoD(dev_ptr1, host_ptr1, sizeof(int)*5));
checkCudaErrors(cuMemcpyHtoD(dev_ptr2, host_ptr2, sizeof(int)*5));



checkCudaErrors(cuMemAlloc(&dev_ptr0, sizeof(int)*5));
checkCudaErrors(cuMemcpyHtoD(dev_ptr0, host_ptr0, sizeof(int)*5));


void *KernelParams[] = { &dev_ptr1, &dev_ptr2, &dev_ptr0};

unsigned int blockSizeX = 16;
unsigned int blockSizeY = 1;
unsigned int blockSizeZ = 1;
unsigned int gridSizeX = 1;
unsigned int gridSizeY = 1;
unsigned int gridSizeZ = 1;

checkCudaErrors(cuLaunchKernel(function, gridSizeX, gridSizeY, gridSizeZ, blockSizeX, blockSizeY, blockSizeZ,0, NULL, KernelParams, NULL));

checkCudaErrors(cuMemcpyDtoH(host_ptr0,dev_ptr0, sizeof(VLC_Array<int>)*5));

checkCudaErrors(cuMemFree(dev_ptr1));
checkCudaErrors(cuMemFree(dev_ptr2));
checkCudaErrors(cuMemFree(dev_ptr0));
checkCudaErrors(cuMemFree(scale));

checkCudaErrors(cuModuleUnload(cudaModule));
checkCudaErrors(cuCtxDestroy(context));
}


int add(int b,int a){

return a + b;

}




int vlc(){

VLC_Array<int> a=VLC_Array(5,1,5,1,2,3,4,5);
VLC_Array<int> b=VLC_Array(5,1,5,1,2,3,4,5);
VLC_Array<int> c=map_c0(a,b);
int d=a.get_element_value(1,2);
a.set_array_value(5,1);a=b;
return 0;

}


int main(void) { return vlc(); }