BeginPackage["ForeignFunctionInterface`LibFFI`Base`"]


Begin["`Private`"]


Needs["ForeignFunctionInterface`"]
Needs["ForeignFunctionInterface`LibFFI`"]
Needs["ForeignFunctionInterface`RawObject`"]


(* TEMPORARY DECLARATIONS *)
(*
	These declarations should probably be added to the C types that are included with the compiler by default
*)
DeclareCompiledComponent["ForeignFunctionInterface", {

	TypeDeclaration["Macro", "CUnsignedChar", "UnsignedInteger8"],
	TypeDeclaration["Macro", "CSignedChar", "Integer8"]

}];


DeclareCompiledComponent["ForeignFunctionInterface", {

	TypeDeclaration["Product", "ForeignFunctionObject",
		<|
			"ArgumentTypes" -> "Managed"::["CArray"::["FFIType"]],
			"OutputType" -> "FFIType",
			"CallInterface" -> "Managed"::["FFICallInterface"],
			"FunctionPointer" -> "OpaqueRawPointer"
		|>,
		"AbstractTypes" -> {"DataStructures"}
	],

	FunctionDeclaration[CreateForeignFunctionWithLibrary,
		Typed[{"ExternalLibraryHandle", "String", "ListVector"::["FFIType"], "FFIType"} -> "ForeignFunctionObject"]@
		Function[{lib, funName, argTypes, returnType},
			Module[{cif, argArray, fun},
				argArray = CreateTypeInstance["Managed"::["CArray"::["FFIType"]], argTypes];

				cif = CreateTypeInstance["Managed", LibraryFunction["create_ffi_cif"][]];
				LibraryFunction["prepare_ffi_cif"][cif, Cast[Length[argTypes],"CUnsignedInt","CCast"], returnType, argArray];

				fun = LibraryFunction["dlsym"][lib, Cast[funName, "Managed"::["CString"]]];
				If[fun === Cast[0, "OpaqueRawPointer", "BitCast"],
					Native`ThrowWolframExceptionCode["System"]
				];

				CreateTypeInstance["ForeignFunctionObject", <|
					"ArgumentTypes" -> argArray,
					"OutputType" -> returnType,
					"CallInterface" -> cif,
					"FunctionPointer" -> fun
				|>]
			]
		]
	],

	FunctionDeclaration[CreateForeignFunction,
		Typed[{"String", "ListVector"::["FFIType"], "FFIType"} -> "ForeignFunctionObject"]@
		Function[{funName, argTypes, returnType},
			(* TODO: The value for RTLD_DEFAULT might be platform specific *)
			CreateForeignFunctionWithLibrary[
				Cast[0,"ExternalLibraryHandle","BitCast"] (*RTLD_DEFAULT*),
				funName,
				argTypes,
				returnType
			]
		]
	],


	FunctionDeclaration[typeStackPointer,
		Typed[ForAllType[ty, {"TypeSpecifier"::[ty]} -> "OpaqueRawPointer"]]@
		Function[type,
			Cast[Typed[ToRawPointer[], "RawPointer"::[type]], "OpaqueRawPointer", "BitCast"]
		],
		"Inline" -> "Always"
	],

	FunctionDeclaration[typeStackPointer,
		Typed[{"TypeSpecifier"::["Void"]} -> "OpaqueRawPointer"]@
		Function[type,
			Cast[0, "OpaqueRawPointer", "BitCast"]
		],
		"Inline" -> "Always"
	],

	FunctionDeclaration[typeStackPointer,
		Typed[{"FFIType"} -> "OpaqueRawPointer"]@
		Function[type,
			Switch[type,

					FFIType[LiteralType["Void"]],								typeStackPointer[TypeSpecifier["Void"]],
					FFIType[LiteralType["UnsignedInteger8"]],		typeStackPointer[TypeSpecifier["UnsignedInteger8"]],
					FFIType[LiteralType["Integer8"]],						typeStackPointer[TypeSpecifier["Integer8"]],
					FFIType[LiteralType["UnsignedInteger16"]],	typeStackPointer[TypeSpecifier["UnsignedInteger16"]],
					FFIType[LiteralType["Integer16"]],					typeStackPointer[TypeSpecifier["Integer16"]],
					FFIType[LiteralType["UnsignedInteger32"]],	typeStackPointer[TypeSpecifier["UnsignedInteger32"]],
					FFIType[LiteralType["Integer32"]],					typeStackPointer[TypeSpecifier["Integer32"]],
					FFIType[LiteralType["UnsignedInteger64"]],	typeStackPointer[TypeSpecifier["UnsignedInteger64"]],
					FFIType[LiteralType["Integer64"]],					typeStackPointer[TypeSpecifier["Integer64"]],
					FFIType[LiteralType["CFloat"]],							typeStackPointer[TypeSpecifier["CFloat"]],
					FFIType[LiteralType["CDouble"]],						typeStackPointer[TypeSpecifier["CDouble"]],
					FFIType[LiteralType["CUnsignedChar"]],			typeStackPointer[TypeSpecifier["CUnsignedChar"]],
					FFIType[LiteralType["CSignedChar"]],				typeStackPointer[TypeSpecifier["CSignedChar"]],
					FFIType[LiteralType["CUnsignedShort"]],			typeStackPointer[TypeSpecifier["CUnsignedShort"]],
					FFIType[LiteralType["CShort"]],							typeStackPointer[TypeSpecifier["CShort"]],
					FFIType[LiteralType["CUnsignedInt"]],				typeStackPointer[TypeSpecifier["CUnsignedInt"]],
					FFIType[LiteralType["CInt"]],								typeStackPointer[TypeSpecifier["CInt"]],
					FFIType[LiteralType["CUnsignedLong"]],			typeStackPointer[TypeSpecifier["CUnsignedLong"]],
					FFIType[LiteralType["CLong"]],							typeStackPointer[TypeSpecifier["CLong"]],
					FFIType[LiteralType["OpaqueRawPointer"]],		typeStackPointer[TypeSpecifier["OpaqueRawPointer"]],
					_, 																					Native`ThrowWolframExceptionCode["Unimplemented"]

				]
		],
		"Inline" -> "Always"
	],

	FunctionDeclaration[typeStackPointer,
		Typed[ForAllType[ty, {"TypeSpecifier"::[ty], "InertExpression"} -> "OpaqueRawPointer"]]@
		Function[{type, init},
			Cast[ToRawPointer[Cast[init, type]], "OpaqueRawPointer", "BitCast"]
		],
		"Inline" -> "Always"
	],

	FunctionDeclaration[typeStackPointer,
		Typed[ForAllType[ty, {"TypeSpecifier"::["OpaqueRawPointer"], "InertExpression"} -> "OpaqueRawPointer"]]@
		Function[{type, init},
			Cast[ToRawPointer[UnwrapRawObject[Cast[init, "RawObject"::["OpaqueRawPointer"]]]], "OpaqueRawPointer", "BitCast"]
		],
		"Inline" -> "Always"
	],
							
	FunctionDeclaration[typeStackPointer,
		Typed[{"FFIType", "InertExpression"} -> "OpaqueRawPointer"]@
		Function[{type, init},
			Switch[type,

					(* "Void" is removed *)
					FFIType[LiteralType["UnsignedInteger8"]],		typeStackPointer[TypeSpecifier["UnsignedInteger8"], init],
					FFIType[LiteralType["Integer8"]],						typeStackPointer[TypeSpecifier["Integer8"], init],
					FFIType[LiteralType["UnsignedInteger16"]],	typeStackPointer[TypeSpecifier["UnsignedInteger16"], init],
					FFIType[LiteralType["Integer16"]],					typeStackPointer[TypeSpecifier["Integer16"], init],
					FFIType[LiteralType["UnsignedInteger32"]],	typeStackPointer[TypeSpecifier["UnsignedInteger32"], init],
					FFIType[LiteralType["Integer32"]],					typeStackPointer[TypeSpecifier["Integer32"], init],
					FFIType[LiteralType["UnsignedInteger64"]],	typeStackPointer[TypeSpecifier["UnsignedInteger64"], init],
					FFIType[LiteralType["Integer64"]],					typeStackPointer[TypeSpecifier["Integer64"], init],
					FFIType[LiteralType["CFloat"]],							typeStackPointer[TypeSpecifier["CFloat"], init],
					FFIType[LiteralType["CDouble"]],						typeStackPointer[TypeSpecifier["CDouble"], init],
					FFIType[LiteralType["CUnsignedChar"]],			typeStackPointer[TypeSpecifier["CUnsignedChar"], init],
					FFIType[LiteralType["CSignedChar"]],				typeStackPointer[TypeSpecifier["CSignedChar"], init],
					FFIType[LiteralType["CUnsignedShort"]],			typeStackPointer[TypeSpecifier["CUnsignedShort"], init],
					FFIType[LiteralType["CShort"]],							typeStackPointer[TypeSpecifier["CShort"], init],
					FFIType[LiteralType["CUnsignedInt"]],				typeStackPointer[TypeSpecifier["CUnsignedInt"], init],
					FFIType[LiteralType["CInt"]],								typeStackPointer[TypeSpecifier["CInt"], init],
					FFIType[LiteralType["CUnsignedLong"]],			typeStackPointer[TypeSpecifier["CUnsignedLong"], init],
					FFIType[LiteralType["CLong"]],							typeStackPointer[TypeSpecifier["CLong"], init],
					FFIType[LiteralType["OpaqueRawPointer"]],		typeStackPointer[TypeSpecifier["OpaqueRawPointer"], init],
					_, 																					Native`ThrowWolframExceptionCode["Unimplemented"]

				]
		],
		"Inline" -> "Always"
	],


	FunctionDeclaration[pointerExpression,
		Typed[ForAllType[ty, {"TypeSpecifier"::[ty], "OpaqueRawPointer"} -> "InertExpression"]]@
		Function[{type, ptr},
			Cast[FromRawPointer@Cast[ptr, "RawPointer"::[type], "BitCast"], "InertExpression"]
		],
		"Inline" -> "Always"
	],

	FunctionDeclaration[pointerExpression,
		Typed[{"TypeSpecifier"::["Void"], "OpaqueRawPointer"} -> "InertExpression"]@
		Function[{type, ptr},
			InertExpression[Null]
		],
		"Inline" -> "Always"
	],

	FunctionDeclaration[pointerExpression,
		Typed[{"TypeSpecifier"::["OpaqueRawPointer"], "OpaqueRawPointer"} -> "InertExpression"]@
		Function[{type, ptr},
			Cast[CreateRawObject@FromRawPointer@Cast[ptr, "RawPointer"::["OpaqueRawPointer"], "BitCast"], "InertExpression"]
		],
		"Inline" -> "Always"
	],

	FunctionDeclaration[pointerExpression,
		Typed[{"FFIType", "OpaqueRawPointer"} -> "InertExpression"]@
		Function[{type, ptr},
			Switch[type,

					FFIType[LiteralType["Void"]],								pointerExpression[TypeSpecifier["Void"], ptr],
					FFIType[LiteralType["UnsignedInteger8"]],		pointerExpression[TypeSpecifier["UnsignedInteger8"], ptr],
					FFIType[LiteralType["Integer8"]],						pointerExpression[TypeSpecifier["Integer8"], ptr],
					FFIType[LiteralType["UnsignedInteger16"]],	pointerExpression[TypeSpecifier["UnsignedInteger16"], ptr],
					FFIType[LiteralType["Integer16"]],					pointerExpression[TypeSpecifier["Integer16"], ptr],
					FFIType[LiteralType["UnsignedInteger32"]],	pointerExpression[TypeSpecifier["UnsignedInteger32"], ptr],
					FFIType[LiteralType["Integer32"]],					pointerExpression[TypeSpecifier["Integer32"], ptr],
					FFIType[LiteralType["UnsignedInteger64"]],	pointerExpression[TypeSpecifier["UnsignedInteger64"], ptr],
					FFIType[LiteralType["Integer64"]],					pointerExpression[TypeSpecifier["Integer64"], ptr],
					FFIType[LiteralType["CFloat"]],							pointerExpression[TypeSpecifier["CFloat"], ptr],
					FFIType[LiteralType["CDouble"]],						pointerExpression[TypeSpecifier["CDouble"], ptr],
					FFIType[LiteralType["CUnsignedChar"]],			pointerExpression[TypeSpecifier["CUnsignedChar"], ptr],
					FFIType[LiteralType["CSignedChar"]],				pointerExpression[TypeSpecifier["CSignedChar"], ptr],
					FFIType[LiteralType["CUnsignedShort"]],			pointerExpression[TypeSpecifier["CUnsignedShort"], ptr],
					FFIType[LiteralType["CShort"]],							pointerExpression[TypeSpecifier["CShort"], ptr],
					FFIType[LiteralType["CUnsignedInt"]],				pointerExpression[TypeSpecifier["CUnsignedInt"], ptr],
					FFIType[LiteralType["CInt"]],								pointerExpression[TypeSpecifier["CInt"], ptr],
					FFIType[LiteralType["CUnsignedLong"]],			pointerExpression[TypeSpecifier["CUnsignedLong"], ptr],
					FFIType[LiteralType["CLong"]],							pointerExpression[TypeSpecifier["CLong"], ptr],
					FFIType[LiteralType["OpaqueRawPointer"]],		pointerExpression[TypeSpecifier["OpaqueRawPointer"], ptr],
					_, 																					Native`ThrowWolframExceptionCode["Unimplemented"]

				]
		],
		"Inline" -> "Always"
	],


	FunctionDeclaration[CallForeignFunction,
		Typed[{"ForeignFunctionObject", "ListVector"::["InertExpression"]} -> "InertExpression"]@
		Function[{ff, args},
			Module[{out, argArray},

				out = typeStackPointer[ff["OutputType"]];

				argArray = CreateTypeInstance["Managed"::["CArray"::["OpaqueRawPointer"]], Length[args]];
				Do[
					ToRawPointer[argArray, i-1,
						typeStackPointer[
							FromRawPointer[ff["ArgumentTypes"],i-1],
							args[[i]]
						]
					],
					{i, Length[args]}
				];

				LibraryFunction["ffi_call"][ff["CallInterface"], ff["FunctionPointer"], Cast[out,"OpaqueRawPointer","BitCast"], argArray];

				pointerExpression[ff["OutputType"], out]
			]
		]
	]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	CreateForeignFunctionWithLibrary,
	CreateForeignFunction,
	CallForeignFunction
}];



End[] (* End `Private` *)

EndPackage[]
