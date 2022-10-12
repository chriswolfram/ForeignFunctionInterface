#include <stdio.h>
#include <ffi.h>

int main() {
  ffi_cif cif;
  ffi_type *args[1];
  void *values[1];
  char *s;
  ffi_arg rc;

  printf("ffi_cif size: %d\n", sizeof(ffi_cif));
  printf("ffi_type size: %d\n", sizeof(ffi_type));
  printf("ffi_arg size: %d\n", sizeof(ffi_arg));
  printf("ffi_status size: %d\n", sizeof(ffi_status));
  
  /* Initialize the argument info vectors */    
  args[0] = &ffi_type_pointer;
  values[0] = &s;
  
  /* Initialize the cif */
  if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1, 
           &ffi_type_sint, args) == FFI_OK)
    {
      s = "Hello World!";
      ffi_call(&cif, puts, &rc, values);
      /* rc now holds the result of the call to puts */
      
      /* values holds a pointer to the function's arg, so to 
         call puts() again all we need to do is change the 
         value of s */
      s = "This is cool!";
      ffi_call(&cif, puts, &rc, values);
    }
  
  return 0;
}

int main_old()
{
  ffi_cif cif;
  ffi_type *args[1];
  void *values[1];
  char *s;
  ffi_arg rc;
  
  /* Initialize the argument info vectors */    
  args[0] = &ffi_type_pointer;
  values[0] = &s;
  
  /* Initialize the cif */
  if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1, 
		       &ffi_type_sint, args) == FFI_OK)
    {
      s = "Hello World!";
      ffi_call(&cif, puts, &rc, values);
      /* rc now holds the result of the call to puts */
      
      /* values holds a pointer to the function's arg, so to 
         call puts() again all we need to do is change the 
         value of s */
      s = "This is cool!";
      ffi_call(&cif, puts, &rc, values);
    }
  
  return 0;
}
