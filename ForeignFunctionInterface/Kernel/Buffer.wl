BeginPackage["ChristopherWolfram`ForeignFunctionInterface`Buffer`"]


Begin["`Private`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]



(***************************************************)
(********** Creating and freeing buffers ***********)
(***************************************************)


DeclareCompiledComponent["ForeignFunctionInterface", {

		(******* Freeing buffers *******)

		FunctionDeclaration[FreeBuffer,
			Typed[{"InertExpression"} -> "Null"]@
			Function[ptr,
				(* TODO: This ignores the element type of the array. Confirm that this isn't a problem. *)
				DeleteObject[Cast[ExpressionToPointer[ptr], "CArray"::["Integer8"], "BitCast"]]
			]
		],

		(******* Creating uninitialized buffers *******)

		FunctionDeclaration[CreateBuffer,
			Typed[{"FFIType", "MachineInteger"} -> "InertExpression"]@
			Function[{type, len},
				PointerToExpression@Cast[
					CreateTypeInstance["CArray"::["Integer8"], Cast[type["Size"]*len, "MachineInteger", "CCast"]],
					"OpaqueRawPointer", "BitCast"
				]
			]
		],

		FunctionDeclaration[CreateBuffer,
			Typed[{"FFIType"} -> "InertExpression"]@
			Function[type,
				CreateBuffer[type, 1]
			]
		],

		(******* Conversion between buffers and NumericArrays / Strings *******)

		FunctionDeclaration[BufferToNumericArray,
			Typed[ForAllType[elemType, {"InertExpression", "TypeSpecifier"::[elemType], "MachineInteger"} -> "InertExpression"]]@
			Function[{ptr, type, len},
				
				Cast[
					CreateTypeInstance[
						"NumericArray"::[type,1],
						Cast[ExpressionToPointer[GetManagedExpression[ptr]],"CArray"::[type],"BitCast"],
						len
					],
					"InertExpression"
				]

			]
		],

		FunctionDeclaration[BufferToNumericArray,
			Typed[{"InertExpression", "FFIType", "MachineInteger"} -> "InertExpression"]@
			Function[{ptr, type, len},
				
				Switch[type,

					(* "Void" is removed *)
					FFIType["UnsignedInteger8"],	BufferToNumericArray[ptr, TypeSpecifier["UnsignedInteger8"], len],
					FFIType["Integer8"],					BufferToNumericArray[ptr, TypeSpecifier["Integer8"], len],
					FFIType["UnsignedInteger16"],	BufferToNumericArray[ptr, TypeSpecifier["UnsignedInteger16"], len],
					FFIType["Integer16"],					BufferToNumericArray[ptr, TypeSpecifier["Integer16"], len],
					FFIType["UnsignedInteger32"],	BufferToNumericArray[ptr, TypeSpecifier["UnsignedInteger32"], len],
					FFIType["Integer32"],					BufferToNumericArray[ptr, TypeSpecifier["Integer32"], len],
					FFIType["UnsignedInteger64"],	BufferToNumericArray[ptr, TypeSpecifier["UnsignedInteger64"], len],
					FFIType["Integer64"],					BufferToNumericArray[ptr, TypeSpecifier["Integer64"], len],
					FFIType["CFloat"],						BufferToNumericArray[ptr, TypeSpecifier["CFloat"], len],
					FFIType["CDouble"],						BufferToNumericArray[ptr, TypeSpecifier["CDouble"], len],
					FFIType["CUnsignedChar"],			BufferToNumericArray[ptr, TypeSpecifier["CUnsignedChar"], len],
					FFIType["CSignedChar"],				BufferToNumericArray[ptr, TypeSpecifier["CSignedChar"], len],
					FFIType["CUnsignedShort"],		BufferToNumericArray[ptr, TypeSpecifier["CUnsignedShort"], len],
					FFIType["CShort"],						BufferToNumericArray[ptr, TypeSpecifier["CShort"], len],
					FFIType["CUnsignedInt"],			BufferToNumericArray[ptr, TypeSpecifier["CUnsignedInt"], len],
					FFIType["CInt"],							BufferToNumericArray[ptr, TypeSpecifier["CInt"], len],
					FFIType["CUnsignedLong"],			BufferToNumericArray[ptr, TypeSpecifier["CUnsignedLong"], len],
					FFIType["CLong"],							BufferToNumericArray[ptr, TypeSpecifier["CLong"], len],
					_, 														Native`ThrowWolframExceptionCode["Unimplemented"]

				]
			]
		],

		FunctionDeclaration[StringToBuffer,
			Typed[{"String"} -> "InertExpression"]@
			Function[str,
				PointerToExpression@Cast[CreateTypeInstance["CString", str], "OpaqueRawPointer", "BitCast"]
			]
		],

		FunctionDeclaration[BufferToString,
			Typed[{"InertExpression"} -> "String"]@
			Function[carr,
				CreateTypeInstance["String", Cast[ExpressionToPointer[GetManagedExpression[carr]], "CString", "BitCast"]]
			]
		]
}];


(* Installed functions *)

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	BufferToNumericArray,
	StringToBuffer,
	BufferToString,
	FreeBuffer
}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	iCreateBuffer -> Typed[CreateBuffer, {"FFIType", "MachineInteger"} -> "InertExpression"]
|>];


(* Down values *)

CreateBuffer[ty_, len_] :=
	iCreateBuffer[ty, len]

CreateBuffer[ty_] :=
	iCreateBuffer[ty, 1]


(***************************************************)
(******* Extracting expressions from buffers *******)
(***************************************************)


DeclareCompiledComponent["ForeignFunctionInterface", {

		(******* Dereferencing / indexing *******)

		FunctionDeclaration[DereferenceBuffer,
			Typed[ForAllType[ty, {"OpaqueRawPointer", "TypeSpecifier"::[ty], "MachineInteger"} -> "InertExpression"]]@
			Function[{ptr, type, offset},
				Cast[FromRawPointer[Cast[ptr, "CArray"::[type], "BitCast"], offset], "InertExpression"]
			]
		],

		FunctionDeclaration[DereferenceBuffer,
			Typed[{"OpaqueRawPointer", "TypeSpecifier"::["Void"], "MachineInteger"} -> "InertExpression"]@
			Function[{ptr, type, offset},
				InertExpression[Null]
			]
		],

		FunctionDeclaration[DereferenceBuffer,
			Typed[{"OpaqueRawPointer", "TypeSpecifier"::["OpaqueRawPointer"], "MachineInteger"} -> "InertExpression"]@
			Function[{ptr, type, offset},
				PointerToExpression[FromRawPointer[Cast[ptr, "CArray"::["OpaqueRawPointer"], "BitCast"], offset]]
			]
		],

		FunctionDeclaration[DereferenceBuffer,
			Typed[{"OpaqueRawPointer", "FFIType", "MachineInteger"} -> "InertExpression"]@
			Function[{ptr, type, offset},
				Switch[type,

					FFIType[LiteralType["Void"]],								DereferenceBuffer[ptr, TypeSpecifier["Void"], offset],
					FFIType[LiteralType["UnsignedInteger8"]],		DereferenceBuffer[ptr, TypeSpecifier["UnsignedInteger8"], offset],
					FFIType[LiteralType["Integer8"]],						DereferenceBuffer[ptr, TypeSpecifier["Integer8"], offset],
					FFIType[LiteralType["UnsignedInteger16"]],	DereferenceBuffer[ptr, TypeSpecifier["UnsignedInteger16"], offset],
					FFIType[LiteralType["Integer16"]],					DereferenceBuffer[ptr, TypeSpecifier["Integer16"], offset],
					FFIType[LiteralType["UnsignedInteger32"]],	DereferenceBuffer[ptr, TypeSpecifier["UnsignedInteger32"], offset],
					FFIType[LiteralType["Integer32"]],					DereferenceBuffer[ptr, TypeSpecifier["Integer32"], offset],
					FFIType[LiteralType["UnsignedInteger64"]],	DereferenceBuffer[ptr, TypeSpecifier["UnsignedInteger64"], offset],
					FFIType[LiteralType["Integer64"]],					DereferenceBuffer[ptr, TypeSpecifier["Integer64"], offset],
					FFIType[LiteralType["CFloat"]],							DereferenceBuffer[ptr, TypeSpecifier["CFloat"], offset],
					FFIType[LiteralType["CDouble"]],						DereferenceBuffer[ptr, TypeSpecifier["CDouble"], offset],
					FFIType[LiteralType["CUnsignedChar"]],			DereferenceBuffer[ptr, TypeSpecifier["CUnsignedChar"], offset],
					FFIType[LiteralType["CSignedChar"]],				DereferenceBuffer[ptr, TypeSpecifier["CSignedChar"], offset],
					FFIType[LiteralType["CUnsignedShort"]],			DereferenceBuffer[ptr, TypeSpecifier["CUnsignedShort"], offset],
					FFIType[LiteralType["CShort"]],							DereferenceBuffer[ptr, TypeSpecifier["CShort"], offset],
					FFIType[LiteralType["CUnsignedInt"]],				DereferenceBuffer[ptr, TypeSpecifier["CUnsignedInt"], offset],
					FFIType[LiteralType["CInt"]],								DereferenceBuffer[ptr, TypeSpecifier["CInt"], offset],
					FFIType[LiteralType["CUnsignedLong"]],			DereferenceBuffer[ptr, TypeSpecifier["CUnsignedLong"], offset],
					FFIType[LiteralType["CLong"]],							DereferenceBuffer[ptr, TypeSpecifier["CLong"], offset],
					FFIType[LiteralType["OpaqueRawPointer"]],		DereferenceBuffer[ptr, TypeSpecifier["OpaqueRawPointer"], offset],
					_, 																					Native`ThrowWolframExceptionCode["Unimplemented"]

				]
			]
		],

		(******* ManagedExpression case *******)
		
		FunctionDeclaration[DereferenceBuffer,
			Typed[ForAllType[ty, {"ManagedExpression", ty, "MachineInteger"} -> "InertExpression"]]@
			Function[{man, type, offset},
				DereferenceBuffer[GetManagedExpression[man], type, offset]
			]
		],

		(******* Expression case *******)

		FunctionDeclaration[DereferenceBuffer,
			Typed[{"InertExpression", "FFIType", "MachineInteger"} -> "InertExpression"]@
			Function[{ptr, type, offset},
				DereferenceBuffer[ExpressionToPointer[GetManagedExpression[ptr]], type, offset]
			]
		],

		(******* 2-argument form *******)

		FunctionDeclaration[DereferenceBuffer,
			Typed[ForAllType[{ptrTy, valTy}, {ptrTy, valTy} -> "InertExpression"]]@
			Function[{ptr, type},
				DereferenceBuffer[ptr, type, 0]
			]
		]

}];


(* Installed functions *)

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	iDereferenceBuffer -> Typed[DereferenceBuffer, {"InertExpression", "FFIType", "MachineInteger"} -> "InertExpression"]
|>];


(* Down values *)

DereferenceBuffer[ptr_, ty_, offset_] :=
	iDereferenceBuffer[ptr, ty, offset]

DereferenceBuffer[ptr_, ty_] :=
	iDereferenceBuffer[ptr, ty, 0]



End[] (* End `Private` *)

EndPackage[]
