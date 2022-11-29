BeginPackage["ForeignFunctionInterface`CArray`"]


Begin["`Private`"]

Needs["ForeignFunctionInterface`"]
Needs["ForeignFunctionInterface`OpaqueRawPointer`"]



DeclareCompiledComponent["ForeignFunctionInterface", {

		(******* Freeing CArrays *******)

		FunctionDeclaration[FreeCArray,
			Typed[{"InertExpression"} -> "Null"]@
			Function[ptr,
				(* TODO: This ignores the element type of the array. Confirm that this isn't a problem. *)
				DeleteObject[Cast[ExpressionToPointer[ptr], "CArray"::["Integer8"], "BitCast"]]
			]
		],

		(******* Creating uninitialized CArrays *******)

		FunctionDeclaration[CreateCArray,
			Typed[ForAllType[elemType, {"TypeSpecifier"::[elemType], "MachineInteger"} -> "InertExpression"]]@
			Function[{type, len},
				PointerToExpression@Cast[CreateTypeInstance["CArray"::[type],len], "OpaqueRawPointer", "BitCast"]
			]
		],

		FunctionDeclaration[CreateCArray,
			Typed[{"FFIType", "MachineInteger"} -> "InertExpression"]@
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

		(******* Conversion between CArrays and NumericArrays / Strings *******)

		FunctionDeclaration[CArrayToNumericArray,
			Typed[ForAllType[elemType, {"InertExpression", "TypeSpecifier"::[elemType], "MachineInteger"} -> "InertExpression"]]@
			Function[{ptr, type, len},
				
				Cast[
					CreateTypeInstance[
						"NumericArray"::[type,1],
						Cast[ExpressionToPointer[ptr],"CArray"::[type],"BitCast"],
						len
					],
					"InertExpression"
				]

			]
		],

		FunctionDeclaration[CArrayToNumericArray,
			Typed[{"InertExpression", "FFIType", "MachineInteger"} -> "InertExpression"]@
			Function[{ptr, type, len},
				
				Switch[type,

					(* "Void" is removed *)
					FFIType["UnsignedInteger8"],	CArrayToNumericArray[ptr, TypeSpecifier["UnsignedInteger8"], len],
					FFIType["Integer8"],					CArrayToNumericArray[ptr, TypeSpecifier["Integer8"], len],
					FFIType["UnsignedInteger16"],	CArrayToNumericArray[ptr, TypeSpecifier["UnsignedInteger16"], len],
					FFIType["Integer16"],					CArrayToNumericArray[ptr, TypeSpecifier["Integer16"], len],
					FFIType["UnsignedInteger32"],	CArrayToNumericArray[ptr, TypeSpecifier["UnsignedInteger32"], len],
					FFIType["Integer32"],					CArrayToNumericArray[ptr, TypeSpecifier["Integer32"], len],
					FFIType["UnsignedInteger64"],	CArrayToNumericArray[ptr, TypeSpecifier["UnsignedInteger64"], len],
					FFIType["Integer64"],					CArrayToNumericArray[ptr, TypeSpecifier["Integer64"], len],
					FFIType["CFloat"],						CArrayToNumericArray[ptr, TypeSpecifier["CFloat"], len],
					FFIType["CDouble"],						CArrayToNumericArray[ptr, TypeSpecifier["CDouble"], len],
					FFIType["CUnsignedChar"],			CArrayToNumericArray[ptr, TypeSpecifier["CUnsignedChar"], len],
					FFIType["CSignedChar"],				CArrayToNumericArray[ptr, TypeSpecifier["CSignedChar"], len],
					FFIType["CUnsignedShort"],		CArrayToNumericArray[ptr, TypeSpecifier["CUnsignedShort"], len],
					FFIType["CShort"],						CArrayToNumericArray[ptr, TypeSpecifier["CShort"], len],
					FFIType["CUnsignedInt"],			CArrayToNumericArray[ptr, TypeSpecifier["CUnsignedInt"], len],
					FFIType["CInt"],							CArrayToNumericArray[ptr, TypeSpecifier["CInt"], len],
					FFIType["CUnsignedLong"],			CArrayToNumericArray[ptr, TypeSpecifier["CUnsignedLong"], len],
					FFIType["CLong"],							CArrayToNumericArray[ptr, TypeSpecifier["CLong"], len],
					_, 														Native`ThrowWolframExceptionCode["Unimplemented"]

				]

			]
		],

		FunctionDeclaration[StringToCArray,
			Typed[{"String"} -> "InertExpression"]@
			Function[str,
				PointerToExpression@Cast[CreateTypeInstance["CString", str], "OpaqueRawPointer", "BitCast"]
			]
		],

		FunctionDeclaration[CArrayToString,
			Typed[{"InertExpression"} -> "String"]@
			Function[carr,
				CreateTypeInstance["String", Cast[ExpressionToPointer[carr], "CString", "BitCast"]]
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
