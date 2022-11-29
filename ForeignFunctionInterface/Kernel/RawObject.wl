BeginPackage["ForeignFunctionInterface`RawObject`"]


CreateRawObject
UnwrapRawObject


Begin["`Private`"]

Needs["ForeignFunctionInterface`"]
Needs["ForeignFunctionInterface`LibFFI`"]



DeclareCompiledComponent["ForeignFunctionInterface", {

	(******* Base RawObject declarations *******)

	TypeDeclaration["Product", "RawObject"::[t],
			<|
				"Object" -> t
			|>,
			"AbstractTypes" -> {"DataStructures"}
		],

		FunctionDeclaration[CreateRawObject,
			Typed[ForAllType[t, {t} -> "RawObject"::[t]]]@
			Function[obj,
				CreateTypeInstance["RawObject", <|"Object" -> obj|>]
			]
		],

		FunctionDeclaration[UnwrapRawObject,
			Typed[ForAllType[t, {"RawObject"::[t]}->t]]@
			Function[obj,
				obj["Object"]
			]
		],


		(******* CArray *******)

		FunctionDeclaration[CreateCArray,
			Typed[ForAllType[elemType, {"TypeSpecifier"::[elemType], "MachineInteger"} -> "RawObject"::["OpaqueRawPointer"]]]@
			Function[{type, len},
				CreateRawObject@Cast[CreateTypeInstance["CArray"::[type],len], "OpaqueRawPointer", "BitCast"]
			]
		],

		FunctionDeclaration[CreateCArray,
			Typed[{"FFIType", "MachineInteger"} -> "RawObject"::["OpaqueRawPointer"]]@
			Function[{type, len},

				Switch[type,

					(* "Void" is removed *)
					FFIType["UnsignedInteger8"],	CreateCArray[TypeSpecifier["UnsignedInteger8"], len],
					FFIType["Integer8"],					CreateCArray[TypeSpecifier["Integer8"], len],
					FFIType["UnsignedInteger16"],	CreateCArray[TypeSpecifier["UnsignedInteger16"], len],
					FFIType["Integer16"],					CreateCArray[TypeSpecifier["Integer16"], len],
					FFIType["UnsignedInteger32"],	CreateCArray[TypeSpecifier["UnsignedInteger32"], len],
					FFIType["Integer32"],					CreateCArray[TypeSpecifier["Integer32"], len],
					FFIType["UnsignedInteger64"],	CreateCArray[TypeSpecifier["UnsignedInteger64"], len],
					FFIType["Integer64"],					CreateCArray[TypeSpecifier["Integer64"], len],
					FFIType["CFloat"],						CreateCArray[TypeSpecifier["CFloat"], len],
					FFIType["CDouble"],						CreateCArray[TypeSpecifier["CDouble"], len],
					FFIType["CUnsignedChar"],			CreateCArray[TypeSpecifier["CUnsignedChar"], len],
					FFIType["CSignedChar"],				CreateCArray[TypeSpecifier["CSignedChar"], len],
					FFIType["CUnsignedShort"],		CreateCArray[TypeSpecifier["CUnsignedShort"], len],
					FFIType["CShort"],						CreateCArray[TypeSpecifier["CShort"], len],
					FFIType["CUnsignedInt"],			CreateCArray[TypeSpecifier["CUnsignedInt"], len],
					FFIType["CInt"],							CreateCArray[TypeSpecifier["CInt"], len],
					FFIType["CUnsignedLong"],			CreateCArray[TypeSpecifier["CUnsignedLong"], len],
					FFIType["CLong"],							CreateCArray[TypeSpecifier["CLong"], len],
					FFIType["OpaqueRawPointer"],	CreateCArray[TypeSpecifier["OpaqueRawPointer"], len],
					_, 														Native`ThrowWolframExceptionCode["Unimplemented"]

				]

			]
		],

		FunctionDeclaration[CArrayToNumericArray,
			Typed[ForAllType[elemType, {"RawObject"::["OpaqueRawPointer"], "TypeSpecifier"::[elemType], "MachineInteger"} -> "InertExpression"]]@
			Function[{pointer, type, len},
				
				Cast[
					CreateTypeInstance[
						"NumericArray"::[type,1],
						Cast[pointer["Object"],"CArray"::[type],"BitCast"],
						len
					],
					"InertExpression"
				]

			]
		],

		FunctionDeclaration[CArrayToNumericArray,
			Typed[{"RawObject"::["OpaqueRawPointer"], "FFIType", "MachineInteger"} -> "InertExpression"]@
			Function[{pointer, type, len},
				
				Switch[type,

					(* "Void" is removed *)
					FFIType["UnsignedInteger8"],	CArrayToNumericArray[pointer, TypeSpecifier["UnsignedInteger8"], len],
					FFIType["Integer8"],					CArrayToNumericArray[pointer, TypeSpecifier["Integer8"], len],
					FFIType["UnsignedInteger16"],	CArrayToNumericArray[pointer, TypeSpecifier["UnsignedInteger16"], len],
					FFIType["Integer16"],					CArrayToNumericArray[pointer, TypeSpecifier["Integer16"], len],
					FFIType["UnsignedInteger32"],	CArrayToNumericArray[pointer, TypeSpecifier["UnsignedInteger32"], len],
					FFIType["Integer32"],					CArrayToNumericArray[pointer, TypeSpecifier["Integer32"], len],
					FFIType["UnsignedInteger64"],	CArrayToNumericArray[pointer, TypeSpecifier["UnsignedInteger64"], len],
					FFIType["Integer64"],					CArrayToNumericArray[pointer, TypeSpecifier["Integer64"], len],
					FFIType["CFloat"],						CArrayToNumericArray[pointer, TypeSpecifier["CFloat"], len],
					FFIType["CDouble"],						CArrayToNumericArray[pointer, TypeSpecifier["CDouble"], len],
					FFIType["CUnsignedChar"],			CArrayToNumericArray[pointer, TypeSpecifier["CUnsignedChar"], len],
					FFIType["CSignedChar"],				CArrayToNumericArray[pointer, TypeSpecifier["CSignedChar"], len],
					FFIType["CUnsignedShort"],		CArrayToNumericArray[pointer, TypeSpecifier["CUnsignedShort"], len],
					FFIType["CShort"],						CArrayToNumericArray[pointer, TypeSpecifier["CShort"], len],
					FFIType["CUnsignedInt"],			CArrayToNumericArray[pointer, TypeSpecifier["CUnsignedInt"], len],
					FFIType["CInt"],							CArrayToNumericArray[pointer, TypeSpecifier["CInt"], len],
					FFIType["CUnsignedLong"],			CArrayToNumericArray[pointer, TypeSpecifier["CUnsignedLong"], len],
					FFIType["CLong"],							CArrayToNumericArray[pointer, TypeSpecifier["CLong"], len],
					_, 														Native`ThrowWolframExceptionCode["Unimplemented"]

				]

			]
		],

		FunctionDeclaration[StringToCArray,
			Typed[{"String"} -> "RawObject"::["OpaqueRawPointer"]]@
			Function[str,
				CreateRawObject@Cast[CreateTypeInstance["CArray"::["UnsignedInteger8"], str], "OpaqueRawPointer", "BitCast"]
			]
		],

		FunctionDeclaration[CArrayToString,
			Typed[{"RawObject"::["OpaqueRawPointer"]} -> "String"]@
			Function[carr,
				CreateTypeInstance["String", Cast[UnwrapRawObject[carr], "CString", "BitCast"]]
			]
		],

		FunctionDeclaration[FreeCArray,
			Typed[{"RawObject"::["OpaqueRawPointer"]} -> "Null"]@
			Function[pointer,
				(* TODO: This ignores the element type of the array. Confirm that this isn't a problem. *)
				DeleteObject[Cast[pointer["Object"], "CArray"::["Integer8"], "BitCast"]]
			]
		]

}];


DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	CreateCArray,
	CArrayToNumericArray,
	StringToCArray,
	CArrayToString,
	FreeCArray
}];



End[] (* End `Private` *)

EndPackage[]
