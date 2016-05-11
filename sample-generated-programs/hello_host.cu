
#include <stdio.h>
#include <stdlib.h>
#include "cuda.h"
#include <iostream>
#include "../compiler/cpp/vlc.hpp"
#include <stdargs.h>
CUdevice    device;
CUmodule    cudaModule;
CUcontext   context;
CUfunction  function;
int BLOOM_SIZE=32;
int KILO=1024;
int PASSES=50;
VLC_Array<int> bloom_filt_seq;
VLC_Array<int> bloom_filt_gpu;
VLC_Array<int> test_vals;
int MAX=10000;
void random_fill(int max,int size,VLC_Array<int,32>filt){
int i;
for(i=0;
i < size;i=i + 1;
){
filt.set_array_value(random(),1,i);
}
}




int hash(int in){
int shifted=in >> 22;
return in ^ shifted;
}




void set_index(int i,VLC_Array<int,32>filt){
filt.set_array_value(1 << i % 32 | filt.get_element_value(1,i / 32),1,i / 32);
}




int test_index(int i,VLC_Array<int,32>filt){
return filt.get_element_value(1,i / 32) & 1 << i % 32 != 0;
}




int vlc(){
int test_size=PASSES * KILO * 2;
random_fill(test_vals,test_size,MAX);
int index;
int seq_counter;
int seq_def_not;
int result;
int i;
printf("Starting sequential...");
for(i=0;
i < test_size;i=i + 1;
){
bool divis=i % 2;
if(divis == true){
index=hash(test_vals.get_element_value(1,i));
set_index(bloom_filt_seq,index);
seq_counter++;
}
else{
index=hash(test_vals.get_element_value(1,i));
result=test_index(bloom_filt_seq,index);
if(result == 0){
seq_def_not++;
}
}
}
printf(seq_counter);
printf(seq_def_not);
return 0;
}


int main(void) { return vlc(); }