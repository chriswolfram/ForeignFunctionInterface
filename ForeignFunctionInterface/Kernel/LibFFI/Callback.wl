BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Callback`"]

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
		Typed[{"InertExpression", "ListVector"::["FFIType"], "FFIType"} -> "InertExpression"]@
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
				|>];

				PointerToExpression[codeloc]
				
			]
		]
	]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	CreateCallback
}];



End[] (* End `Private` *)

EndPackage[]
