#include <stdio.h>
#include <stdlib.h>
#include <cuda.h>
#include <math.h>

#define imin(a,b) (a<b?a:b)

__global__ void vecAdd(int *x, int *y,int *z, int *scale){
	int tid = threadIdx.x + blockIdx.x * blockDim.x;
	z[tid] = *scale *( x[tid] + y[tid]);
}

int main(){
	int a[5] = {1,2,3,4,5};
	int b[5] = {1,2,3,4,5};
	int c[5];

	int constant_scale = 4;
	int *dev_a;
	int *dev_b;
	int *dev_c;
	int *scale;

	/* Upon hitting map, this code should generate*/
	int threadsPerBlock = 512;
	int blocksPerGrid = ceil(5/512);

	cuInit(0);
	cuDeviceGetCount(&devCount);
	cuDeviceGet(&device, 0);

	int devMajor, devMinor;
  cuDeviceComputeCapability(&devMajor, &devMinor, device);
  std::cout << "Device Compute Capability: "<< devMajor << \".\" << devMinor << \"\n\";
  if (devMajor < 2) {
    std::cerr << \"ERROR: Device 0 is not SM 2.0 or greater\n\";
    return 1;
  }
	cudaMalloc( (void**) &dev_a, 5*sizeof(int) );
   	cudaMalloc( (void**) &dev_b, 5*sizeof(int) );
   	cudaMalloc( (void**) &dev_c, 5*sizeof(int) );
   	cudaMalloc( (void**) &scale, sizeof(int) );


   	cudaMemcpy( dev_a, a, 5*sizeof(int), cudaMemcpyHostToDevice ) ;
   	cudaMemcpy( dev_b, b, 5*sizeof(int), cudaMemcpyHostToDevice ) ;
   	cudaMemcpy( scale, &constant_scale, sizeof(int), cudaMemcpyHostToDevice );

   	vecAdd<<<blocksPerGrid,threadsPerBlock>>>( dev_a, dev_b, dev_c, scale);
 	cudaMemcpy( c, dev_c, 5*sizeof(int),cudaMemcpyDeviceToHost ) ;
 
 	printf("[");
 	for(int i = 0; i < 5; i ++){
 		if(i == 5 - 1){
 			printf("%d",c[i]);
 		}
 		else{
 			printf("%d,",c[i]);
 		}
 	}
 	printf("]");
	cudaDeviceSynchronize();
	cudaFree( dev_a ) ;
   	cudaFree( dev_b ) ;
   	cudaFree( dev_c ) ;
   	cudaFree( scale ) ;
}