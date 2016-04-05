#include <stdio.h>
#include <stdlib.h>

char * helloworld;
int vlc(){
helloworld = "Hello world!";
printf(helloworld);
return 0;
}

int main(void) { return vlc(); }
