#include <stdio.h> 
#include <stdlib.h> 



void print_integer(int32_t i) {
    printf("%d\n", i);
}

int32_t read_integer() {
    int32_t i; 
    scanf("%d", &i); 
    return i;
}

void *allocate_record(int32_t size) {
    return malloc(size)
}
