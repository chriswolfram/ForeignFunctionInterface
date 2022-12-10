BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Callback`"]

GetCallbackPointer

Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`CreateForeignFunction`"]


DeclareCompiledComponent["ForeignFunctionInterface", {

	TypeDeclaration["Product", "CallbackObject",
		<|
			"CallInterface" -> "FFICallInterface",
			"Closure" -> "OpaqueRawPointer",
			"CodeLocation" -> "OpaqueRawPointer"
		|>,
		"AbstractTypes" -> {"DataStructures"}
	],

	FunctionDeclaration[CreateCallback,
		Typed[{"InertExpression", "ListVector"::["FFIType"], "FFIType"} -> "CallbackObject"]@
		Function[{expr, argTypes, outputType},
			Module[{codelocPtr, codeloc, closure, cif, fun},
				codelocPtr = TypeHint[ToRawPointer[], "RawPointer"::["OpaqueRawPointer"]];
				closure =
					LibraryFunction["ffi_closure_alloc"][
						LibraryFunction["get_ffi_closure_size"][],
						codelocPtr
					];
				codeloc = FromRawPointer[codelocPtr];
				
				cif = CreateFFICallInterface[argTypes, outputType];
				
				fun =
					Typed["RawFunction"::[{"FFICallInterface", "OpaqueRawPointer", "CArray"::["OpaqueRawPointer"], "OpaqueRawPointer"} -> "Null"(*should be "Void"*)]]@
					TypeFramework`MetaData[<|"FunctionCategory" -> "RawFunction"|>]@
					Function[{Typed[cif, "FFICallInterface"], Typed[ret, "OpaqueRawPointer"], Typed[args, "CArray"::["OpaqueRawPointer"]], Typed[userData, "OpaqueRawPointer"]},
						Echo[Cast[userData, "InertExpression", "BitCast"]];
					];
				
				(*TODO: Check error code*)
				LibraryFunction["ffi_prep_closure_loc"][
					closure,
					cif,
					Cast[fun, "OpaqueRawPointer", "BitCast"],
					Cast[expr, "OpaqueRawPointer", "BitCast"],
					codeloc
				];

				CreateTypeInstance["CallbackObject", <|
					"CallInterface" -> cif,
					"Closure" -> closure,
					"CodeLocation" -> codeloc
				|>]
				
			]
		]
	],

	FunctionDeclaration[FreeCallback,
		Typed[{"CallbackObject"} -> "Null"]@
		Function[callback,
			DeleteObject[callback["CallInterface"]];
			LibraryFunction["ffi_closure_free"][callback["Closure"]];
			(* TODO: Confirm that "CodeLocation" doesn't need to be freed in some way. *)
		]
	],

	FunctionDeclaration[GetCallbackPointer,
		Typed[{"InertExpression"} -> "InertExpression"]@
		Function[expr,
			If[Native`PrimitiveFunction["TestGet_ObjectInstanceByName"][expr, Typed["CallbackObject","CString"], ToRawPointer[]],
				PointerToExpression[Cast[expr,"CallbackObject"]["CodeLocation"]],
				expr
			]
		]
	]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	CreateCallback,
	FreeCallback
}];



End[] (* End `Private` *)

EndPackage[]
