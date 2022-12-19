#include <stdio.h>
#include <ffi.h>

int main() {
	ffi_type tm_type;
	ffi_type* tm_type_elements[3];
	int i;

	tm_type.size = tm_type.alignment = 0;
	tm_type.type = FFI_TYPE_STRUCT;
	tm_type.elements = &tm_type_elements;

	tm_type_elements[0] = &ffi_type_uint32;
	tm_type_elements[1] = &ffi_type_uint32;
	tm_type_elements[2] = &ffi_type_uint64;
	tm_type_elements[3] = NULL;

	size_t offsets[3] = {99,88,77};

	int error_code = ffi_get_struct_offsets(FFI_DEFAULT_ABI, &tm_type, offsets);

	printf("Error code: %d\n", error_code);
	printf("Bad typedef: %d\n", FFI_BAD_TYPEDEF);

	for(int i = 0; i < 3; i++)
		printf("offset: %d\n", offsets[i]);

	return 0;
}

int main_old2() {
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
