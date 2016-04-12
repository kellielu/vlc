#include <stdio.h>
#include <stdlib.h>
#include "cuda.h"
#include <iostream>
#include <assert.h>

CUdevice    device;
CUmodule    cudaModule;
CUcontext   context;
CUfunction  function;
int         devCount;

void checkCudaErrors(CUresult err) {
assert(err == CUDA_SUCCESS);
}
int cudaInit(){
// CUDA initialization
checkCudaErrors(cuInit(0));
checkCudaErrors(cuDeviceGetCount(&devCount));
checkCudaErrors(cuDeviceGet(&device, 0));

char name[128];
checkCudaErrors(cuDeviceGetName(name, 128, device));
printf("Using CUDA Device [0]:%s",name);

int devMajor, devMinor;
checkCudaErrors(cuDeviceComputeCapability(&devMajor, &devMinor, device));
printf("Device Compute Capability:%d.%d",devMajor,devMinor);

if (devMajor < 2) {
printf("ERROR: Device 0 is not SM 2.0 or greater");
return 1;
}
return 0;
}

char* vector_add = ".version 3.1\
.target sm_20\
.address_size 64\
.visible .entry vector_add(\
.param .u64 kernel_param_0,\
.param .u64 kernel_param_1,\
.param .u64 kernel_param_2\
)\
{\
.reg .f32   %%f<4>;\
.reg .s32   %%r<2>;\
.reg .s64   %%rl<8>;\
ld.param.u64    %%rl1, [kernel_param_0];\
mov.u32         %%r1, %%tid.x;\
mul.wide.s32    %%rl2, %%r1, 4;\
add.s64         %%rl3, %%rl1, %%rl2;\
ld.param.u64    %%rl4, [kernel_param_1];\
add.s64         %%rl5, %%rl4, %%rl2;\
ld.param.u64    %%rl6, [kernel_param_2];\
add.s64         %%rl7, %%rl6, %%rl2;\
ld.global.f32   %%f1, [%%rl3];\
ld.global.f32   %%f2, [%%rl5];\
add.f32         %%f3, %%f1, %%f2;\
st.global.f32   [%%rl7], %%f3;\
ret;\
}";
int vlc(){
int a[5] = {1, 2, 3, 4, 5};
int b[5] = {1, 2, 3, 4, 5};
int c[5];
//Create context for device
checkCudaErrors(cuCtxCreate(&context, 0, device));
// Create module for object
checkCudaErrors(cuModuleLoadDataEx(&cudaModule, vector_add, 0, 0, 0));
// Get kernel function
checkCudaErrors(cuModuleGetFunction(&function, cudaModule, "vector_add"));
//Allocate device pointers and memory
CUdeviceptr dev_a;
CUdeviceptr dev_b;
CUdeviceptr dev_c;
checkCudaErrors(cuMemAlloc(&dev_a, sizeof(int)*5));
checkCudaErrors(cuMemAlloc(&dev_b, sizeof(int)*5));
checkCudaErrors(cuMemAlloc(&dev_c, sizeof(int)*5));
//Copy data from host to GPU
checkCudaErrors(cuMemcpyHtoD(dev_a, a, sizeof(int)*5));
checkCudaErrors(cuMemcpyHtoD(dev_b, b, sizeof(int)*5));
//Set kernel parameters
void *KernelParams[] = { &dev_a, &dev_b, &dev_c };
unsigned int blockSizeX = 16;
unsigned int blockSizeY = 1;
unsigned int blockSizeZ = 1;
unsigned int gridSizeX  = 1;
unsigned int gridSizeY  = 1;
unsigned int gridSizeZ  = 1;
//Launching Kernel
printf("Launching kernel...\n");
checkCudaErrors(cuLaunchKernel(function, gridSizeX, gridSizeY, gridSizeZ,
                             blockSizeX, blockSizeY, blockSizeZ,
                             0, NULL, KernelParams, NULL));
//Copy data from GPU to host
checkCudaErrors(cuMemcpyDtoH(c, dev_c, sizeof(int)*5));
//Do cleanup
checkCudaErrors(cuMemFree(dev_a));
checkCudaErrors(cuMemFree(dev_b));
checkCudaErrors(cuMemFree(dev_c));
checkCudaErrors(cuModuleUnload(cudaModule));
checkCudaErrors(cuCtxDestroy(context));
//Check that c has correct values
printf("c should be [2, 4, 6, 8, 10]\n");
printf("c is actually [");
for(int i =0; i < 5; i ++){
	printf("%d",c[i]);
	if(i!=4){ printf(", ");}
}
printf("]\n");
//Return from vlc function
return 0;
}

int main(void) {
int init_error = cudaInit();
if( init_error != 0 ) { return 1; }
else { return vlc(); }

}
