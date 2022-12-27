#include <stdio.h>
#include <dlfcn.h>
#include <stdlib.h>
#include <ffi.h>

typedef struct {
	int a;
	long b;
	double c;
} my_struct;

int main() {
	ffi_cif* cif = (ffi_cif*)malloc(sizeof(ffi_cif));
	ffi_type** argTypes = (ffi_type**)malloc(sizeof(ffi_type*) * 1);
	void** argValues = (void**)malloc(sizeof(void*) * 1);
	double* rc = (double*)malloc(sizeof(double));

	ffi_type* struct_type = (ffi_type*)malloc(sizeof(ffi_type));
	ffi_type** struct_type_elements = (ffi_type*)malloc(sizeof(ffi_type) * 4);

	struct_type->size = struct_type->alignment = 0;
	struct_type->type = FFI_TYPE_STRUCT;
	struct_type->elements = struct_type_elements;

	struct_type_elements[0] = &ffi_type_sint;
	struct_type_elements[1] = &ffi_type_slong;
	struct_type_elements[2] = &ffi_type_double;
	struct_type_elements[3] = NULL;

	ffi_get_struct_offsets(FFI_DEFAULT_ABI, struct_type, NULL);

	printf("Type pointer: %d\n", struct_type);
	printf("Type size: %d\n", struct_type->size);
	printf("Type alignment: %d\n", struct_type->alignment);
	printf("Type type: %d\n", struct_type->type);

	printf("Type pointer: %d\n", struct_type_elements[0]);
	printf("Type size: %d\n", struct_type_elements[0]->size);
	printf("Type alignment: %d\n", struct_type_elements[0]->alignment);
	printf("Type type: %d\n", struct_type_elements[0]->type);

	printf("Type pointer: %d\n", struct_type_elements[1]);
	printf("Type size: %d\n", struct_type_elements[1]->size);
	printf("Type alignment: %d\n", struct_type_elements[1]->alignment);
	printf("Type type: %d\n", struct_type_elements[1]->type);

	printf("Type pointer: %d\n", struct_type_elements[2]);
	printf("Type size: %d\n", struct_type_elements[2]->size);
	printf("Type alignment: %d\n", struct_type_elements[2]->alignment);
	printf("Type type: %d\n", struct_type_elements[2]->type);

	argTypes[0] = struct_type;
	my_struct* struct_value = (my_struct*)malloc(sizeof(my_struct));
	struct_value->a = 4;
	struct_value->b = 6;
	struct_value->c = 5.6;
	argValues[0] = struct_value;

	ffi_prep_cif(cif, FFI_DEFAULT_ABI, 1, &ffi_type_double, argTypes);

	dlopen("/private/var/folders/tz/hpyqywfd5n13zsbg6btzb31r0000gn/T/m0000117541/structTaker2.dylib", RTLD_LAZY | RTLD_GLOBAL);
	printf("dlopen error: %s\n", dlerror());
	void* fun_ptr = (void*)dlsym(RTLD_DEFAULT, "accept_struct2");
	printf("fun_ptr: %d\n", (size_t)fun_ptr);

	printf("Arg pointer: %d\n", argValues[0]);

	ffi_call(cif, fun_ptr, rc, argValues);

	printf("Arg pointer: %d\n", argValues[0]);

	printf("Return value: %lf\n", *rc);

	ffi_call(cif, fun_ptr, rc, argValues);

	printf("Arg pointer: %d\n", argValues[0]);

	printf("Return value: %lf\n", *rc);

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
