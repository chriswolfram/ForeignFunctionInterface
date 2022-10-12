#include <stdlib.h>
#include <ffi.h>


//CIFs

ffi_cif* create_ffi_cif() {
	return (ffi_cif*)malloc(sizeof(ffi_cif));
}

void free_ffi_cif(ffi_cif* cif) {
	free(cif);
}

int prepare_cif(ffi_cif* cif, unsigned int nargs, ffi_type* rtype, ffi_type** argtypes) {
	return 2;
	// switch(ffi_prep_cif(cif, nargs, FFI_DEFAULT_ABI, rtype, argtypes)) {
	// 	case FFI_OK:
	// 		return 1;
	// 	case FFI_BAD_TYPEDEF:
	// 		return -1;
	// 	case FFI_BAD_ARGTYPE:
	// 		return -2;
	// 	case FFI_BAD_ABI:
	// 		return -3;
	// }
}


// Types

ffi_type* get_ffi_type_sint32() {
	return &ffi_type_sint32;
}


// Old experiments

// int main()
// {
//   ffi_cif cif;
//   ffi_type *args[1];
//   void *values[1];
//   char *s;
//   ffi_arg rc;
  
//   /* Initialize the argument info vectors */    
//   args[0] = &ffi_type_pointer;
//   values[0] = &s;
  
//   /* Initialize the cif */
//   if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1, 
// 		       &ffi_type_sint, args) == FFI_OK)
//     {
//       s = "Hello World!";
//       ffi_call(&cif, puts, &rc, values);
//       /* rc now holds the result of the call to puts */
      
//       /* values holds a pointer to the function's arg, so to 
//          call puts() again all we need to do is change the 
//          value of s */
//       s = "This is cool!";
//       ffi_call(&cif, puts, &rc, values);
//     }
  
//   return 0;
// }
