/* a simple driver for scheme_entry */
#include <stdio.h>

int scheme_entry();

#define fixnum_mask   0x3
#define fixnum_tag    0x0
#define fixnum_shift  2

#define char_mask     0xff 
#define char_tag      0x0f
#define char_shift    8

#define bool_mask     0x7f 
#define bool_tag      0x3f
#define bool_shift    7  

#define empty_list    47

int main(int argc, char** argv){
  int val = scheme_entry();

  if((val & fixnum_mask) == fixnum_tag) {
    printf("%d\n", val >> fixnum_shift);
  }
  else if((val & char_mask) == char_tag) {
    printf("%c\n", val >> char_shift);
  }
  else if((val & bool_mask) == bool_tag) {
    if(val >> bool_shift) {
      printf("true");
    } else {
      printf("false");
    }
  }
  else if(val == empty_list) {
    printf("()\n");
  }
  else {
    printf("Can't interpret tag for value: %#010x\n", val);
  }
  return 0;
}
