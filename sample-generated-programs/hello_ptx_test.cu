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
fprintf(stderr, "0");
checkCudaErrors(cuInit(0));
fprintf(stderr, "1");
checkCudaErrors(cuDeviceGetCount(&devCount));
fprintf(stderr, "2");
checkCudaErrors(cuDeviceGet(&device, 0));
fprintf(stderr, "3");

char name[128];
checkCudaErrors(cuDeviceGetName(name, 128, device));
fprintf(stderr, "4");
printf("Using CUDA Device [0]:%s",name);

int devMajor, devMinor;
checkCudaErrors(cuDeviceComputeCapability(&devMajor, &devMinor, device));
fprintf(stderr, "5");
printf("Device Compute Capability:%d.%d",devMajor,devMinor);

if (devMajor < 2) {
printf("ERROR: Device 0 is not SM 2.0 or greater");
return 1;
}
return 0;
} 

void cudaCleanup(){
	checkCudaErrors(cuCtxDestroy(context));
}

char* vector_add = ".version 3.1\n\
.target sm_20\n\
.address_size 64\n\
\n\
  // .globl kernel\n\
                                        // @kernel\n\
.visible .entry kernel(\n\
  .param .u64 kernel_param_0,\n\
  .param .u64 kernel_param_1,\n\
  .param .u64 kernel_param_2\n\
)\n\
{\n\
  .reg .f32   %f<4>;\n\
  .reg .s32   %r<2>;\n\
  .reg .s64   %rl<8>;\n\
\n\
// BB#0:                                // %entry\n\
  ld.param.u64    %rl1, [kernel_param_0];\n\
  mov.u32         %r1, %tid.x;\n\
  mul.wide.s32    %rl2, %r1, 4;\n\
  add.s64         %rl3, %rl1, %rl2;\n\
  ld.param.u64    %rl4, [kernel_param_1];\n\
  add.s64         %rl5, %rl4, %rl2;\n\
  ld.param.u64    %rl6, [kernel_param_2];\n\
  add.s64         %rl7, %rl6, %rl2;\n\
  ld.global.f32   %f1, [%rl3];\n\
  ld.global.f32   %f2, [%rl5];\n\
  add.f32         %f3, %f1, %f2;\n\
  st.global.f32   [%rl7], %f3;\n\
  ret;\n\
}";

int vlc(){
fprintf(stderr, "%s", vector_add);
int a[5] = {1, 2, 3, 4, 5};
int b[5] = {1, 2, 3, 4, 5};
int c[5];
//Create context for device
checkCudaErrors(cuCtxCreate(&context, 0, device));
fprintf(stderr, "6");
// Create module for objectd
checkCudaErrors(cuModuleLoadDataEx(&cudaModule, vector_add, 0, 0, 0));
fprintf(stderr, "7");
// Get kernel function
checkCudaErrors(cuModuleGetFunction(&function, cudaModule, "kernel"));
fprintf(stderr, "8");
//Allocate device pointers and memory
CUdeviceptr dev_a;
CUdeviceptr dev_b;
CUdeviceptr dev_c;
checkCudaErrors(cuMemAlloc(&dev_a, sizeof(int)*5));
fprintf(stderr, "9");
checkCudaErrors(cuMemAlloc(&dev_b, sizeof(int)*5));
fprintf(stderr, "10");
checkCudaErrors(cuMemAlloc(&dev_c, sizeof(int)*5));
fprintf(stderr, "11");
//Copy data from host to GPU
checkCudaErrors(cuMemcpyHtoD(dev_a, a, sizeof(int)*5));
fprintf(stderr, "12");
checkCudaErrors(cuMemcpyHtoD(dev_b, b, sizeof(int)*5));
fprintf(stderr, "13");
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
fprintf(stderr, "14");
//Copy data from GPU to host
checkCudaErrors(cuMemcpyDtoH(c, dev_c, sizeof(int)*5));
fprintf(stderr, "15");
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
printf("main 0");
int init_error = cudaInit();
printf("main 1");
if( init_error != 0 ) { return 1; }
else { return vlc(); }

}
