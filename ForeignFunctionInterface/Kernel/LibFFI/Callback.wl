BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Callback`"]

GetCallbackPointer

Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`CreateForeignFunction`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`CallForeignFunction`"]


DeclareCompiledComponent["ForeignFunctionInterface", {

	TypeDeclaration["Product", "CallbackObject",
		<|
			"CallInterface" -> "FFICallInterface",
			"Closure" -> "OpaqueRawPointer",
			"CodeLocation" -> "OpaqueRawPointer",
			"Function" -> "InertExpression",
			"ArgumentTypes" -> "ListVector"::["InertExpression"],
			"OutputType" -> "InertExpression"
		|>,
		"AbstractTypes" -> {"DataStructures"},
		"MemoryManaged" -> False
		(* TODO: Arguably this should be memory managed. If it ever had refcount=0 but wasn't manually freed, it would be 
		a memory leak, but it would be a smaller leak if this was memory managed. *)
	],

	FunctionDeclaration[CreateCallback,
		Typed[{"InertExpression", "InertExpression"} -> "CallbackObject"]@
		Function[{expr, funcTypeI},
			Module[{funcType, argTypes, outputType, codelocPtr, codeloc, closure, cif, fun, callbackObject},

				funcType = CanonicalizeFunctionType[funcTypeI];
				argTypes = FunctionTypeArguments[funcType];
				outputType = FunctionTypeOutput[funcType];

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
						Module[{argCount, callbackObject, head, expr},
							argCount = Cast[cif["ArgumentCount"], "MachineInteger", "CCast"];
							callbackObject = Cast[userData, "CallbackObject", "BitCast"];
							head = callbackObject["Function"];
							expr = Native`PrimitiveFunction["CreateHeaded_IE_E"][argCount, head];
							Do[
								Native`PrimitiveFunction["SetElement_EIE_Void"][
									expr,
									i,
									CToExpression[FromRawPointer[args,i-1], FromRawPointer[cif["ArgumentTypes"],i-1](*, callbackObject["ArgumentTypes"][[i]]*)]
								],
								{i, argCount}
							];
							ExpressionToC[ret, cif["OutputType"], InertEvaluate[expr]];
						];
					];

				callbackObject = CreateTypeInstance["CallbackObject", <|
					"CallInterface" -> cif,
					"Closure" -> closure,
					"CodeLocation" -> codeloc,
					"Function" -> expr,
					"ArgumentTypes" -> argTypes,
					"OutputType" -> outputType
				|>];
				
				(*TODO: Check error code*)
				LibraryFunction["ffi_prep_closure_loc"][
					closure,
					cif,
					Cast[fun, "OpaqueRawPointer", "BitCast"],
					Cast[callbackObject, "OpaqueRawPointer", "BitCast"],
					codeloc
				];

				callbackObject
				
			]
		]
	],

	FunctionDeclaration[DeleteCallback,
		Typed[{"CallbackObject"} -> "Null"]@
		Function[callback,
			DeleteFFICallInterface[callback["CallInterface"]];
			LibraryFunction["ffi_closure_free"][callback["Closure"]];
			DeleteObject[callback];
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
	DeleteCallback
}];



End[] (* End `Private` *)

EndPackage[]
