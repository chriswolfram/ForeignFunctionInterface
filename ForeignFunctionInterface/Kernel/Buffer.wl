BeginPackage["ChristopherWolfram`ForeignFunctionInterface`Buffer`"]


Begin["`Private`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]



(***************************************************)
(********** Creating and freeing buffers ***********)
(***************************************************)


(******* FreeBuffer *******)
DeclareCompiledComponent["ForeignFunctionInterface", {

		FunctionDeclaration[FreeBuffer,
			Typed[{"InertExpression"} -> "Null"]@
			Function[ptr,
				(* TODO: This ignores the element type of the array. Confirm that this isn't a problem. *)
				DeleteObject[Cast[ExpressionToPointer[ptr], "CArray"::["Integer8"], "BitCast"]]
			]
		]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	FreeBuffer
}];



(******* CreateBuffer *******)
DeclareCompiledComponent["ForeignFunctionInterface", {

		FunctionDeclaration[CreateBuffer,
			Typed[{"InertExpression", "MachineInteger"} -> "InertExpression"]@
			Function[{type, len},
				PointerToExpression@Cast[
					CreateTypeInstance["CArray"::["Integer8"], Cast[FFITypeByteCount[type]*len, "MachineInteger", "CCast"]],
					"OpaqueRawPointer", "BitCast"
				]
			]
		]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	iCreateBuffer -> Typed[CreateBuffer, {"InertExpression", "MachineInteger"} -> "InertExpression"]
|>];


(* Down values *)

CreateBuffer[ty_, len_] :=
	iCreateBuffer[ty, len]

CreateBuffer[ty_] :=
	iCreateBuffer[ty, 1]



(******* BufferToNumericArray / NumericArrayToBuffer *******)
DeclareCompiledComponent["ForeignFunctionInterface", {

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
			Typed[{"InertExpression", "CUnsignedShort", "MachineInteger"} -> "InertExpression"]@
			Function[{ptr, typeID, len},

				Switch[typeID,

					NameFFITypeID["UINT8"][],		BufferToNumericArray[ptr, TypeSpecifier["UnsignedInteger8"], len],
					NameFFITypeID["SINT8"][],		BufferToNumericArray[ptr, TypeSpecifier["Integer8"], len],
					NameFFITypeID["UINT16"][],	BufferToNumericArray[ptr, TypeSpecifier["UnsignedInteger16"], len],
					NameFFITypeID["SINT16"][],	BufferToNumericArray[ptr, TypeSpecifier["Integer16"], len],
					NameFFITypeID["UINT32"][],	BufferToNumericArray[ptr, TypeSpecifier["UnsignedInteger32"], len],
					NameFFITypeID["SINT32"][],	BufferToNumericArray[ptr, TypeSpecifier["Integer32"], len],
					NameFFITypeID["UINT64"][],	BufferToNumericArray[ptr, TypeSpecifier["UnsignedInteger64"], len],
					NameFFITypeID["SINT64"][],	BufferToNumericArray[ptr, TypeSpecifier["Integer64"], len],
					NameFFITypeID["INT"][],			BufferToNumericArray[ptr, TypeSpecifier["CInt"], len],
					NameFFITypeID["FLOAT"][],		BufferToNumericArray[ptr, TypeSpecifier["CFloat"], len],
					NameFFITypeID["DOUBLE"][],	BufferToNumericArray[ptr, TypeSpecifier["CDouble"], len],
					_, 												Native`ThrowWolframExceptionCode["Unimplemented"]

				]
				
			]
		],


		FunctionDeclaration[NumericArrayToBuffer,
			Typed[ForAllType[ty, {"NumericArray"::[ty,1]} -> "InertExpression"]]@
			Function[arr,
				PointerToExpression@Cast[
					CreateTypeInstance["CArray", Array`GetData[arr], Array`NumberOfElements[arr]],
					"OpaqueRawPointer", "BitCast"
				]
			]
		],

		FunctionDeclaration[NumericArrayToBuffer,
			Typed[ForAllType[ty, {"InertExpression", "TypeSpecifier"::[ty]} -> "InertExpression"]]@
			Function[{expr, elemTy},
				NumericArrayToBuffer[Cast[expr, "NumericArray"::[elemTy,1]]]
			]
		],

		FunctionDeclaration[NumericArrayToBuffer,
			Typed[{"InertExpression", "CUnsignedShort"} -> "InertExpression"]@
			Function[{expr, typeID},

				Switch[typeID,

					NameFFITypeID["UINT8"][],		NumericArrayToBuffer[expr, TypeSpecifier["UnsignedInteger8"]],
					NameFFITypeID["SINT8"][],		NumericArrayToBuffer[expr, TypeSpecifier["Integer8"]],
					NameFFITypeID["UINT16"][],	NumericArrayToBuffer[expr, TypeSpecifier["UnsignedInteger16"]],
					NameFFITypeID["SINT16"][],	NumericArrayToBuffer[expr, TypeSpecifier["Integer16"]],
					NameFFITypeID["UINT32"][],	NumericArrayToBuffer[expr, TypeSpecifier["UnsignedInteger32"]],
					NameFFITypeID["SINT32"][],	NumericArrayToBuffer[expr, TypeSpecifier["Integer32"]],
					NameFFITypeID["UINT64"][],	NumericArrayToBuffer[expr, TypeSpecifier["UnsignedInteger64"]],
					NameFFITypeID["SINT64"][],	NumericArrayToBuffer[expr, TypeSpecifier["Integer64"]],
					NameFFITypeID["INT"][],			NumericArrayToBuffer[expr, TypeSpecifier["CInt"]],
					NameFFITypeID["FLOAT"][],		NumericArrayToBuffer[expr, TypeSpecifier["CFloat"]],
					NameFFITypeID["DOUBLE"][],	NumericArrayToBuffer[expr, TypeSpecifier["CDouble"]],
					_, 												Native`ThrowWolframExceptionCode["Unimplemented"]

				]
			]
		]

}];


DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	BufferToNumericArray -> Typed[BufferToNumericArray, {"InertExpression", "CUnsignedShort", "MachineInteger"} -> "InertExpression"],
	NumericArrayToBuffer -> Typed[NumericArrayToBuffer, {"InertExpression", "CUnsignedShort"} -> "InertExpression"]
|>];



(******* StringToBuffer / BufferToString *******)
DeclareCompiledComponent["ForeignFunctionInterface", {

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

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	StringToBuffer,
	BufferToString
}];



(***************************************************)
(******* Extracting expressions from buffers *******)
(***************************************************)


DeclareCompiledComponent["ForeignFunctionInterface", {

		(******* Dereferencing / indexing *******)

		FunctionDeclaration[DereferenceBuffer,
			Typed[ForAllType[ty, {"OpaqueRawPointer", "InertExpression", "MachineInteger"} -> "InertExpression"]]@
			Function[{ptr, type, offset},
				Module[{ffiType},
					ffiType = CreateTypeInstance["Managed", CreateFFIType[type], DeleteFFIType];
					CToExpression[
						Cast[Cast[ptr, "UnsignedInteger64", "BitCast"] + FFITypeByteCount[Compile`BorrowManagedObject[ffiType]] * offset, "OpaqueRawPointer", "BitCast"],
						Compile`BorrowManagedObject[ffiType]
					]
				]
			]
		],

		(******* Expression version *******)

		FunctionDeclaration[DereferenceBuffer,
			Typed[ForAllType[ty, {"InertExpression", "InertExpression", "MachineInteger"} -> "InertExpression"]]@
			Function[{expr, type, offset},
				DereferenceBuffer[ExpressionToPointer[expr], type, offset]
			]
		],

		(******* 2-argument form *******)

		FunctionDeclaration[DereferenceBuffer,
			Typed[ForAllType[p, (*Element[p, {"InertExpression", "OpaqueRawPointer"}],*) {p, "InertExpression"} -> "InertExpression"]]@
			Function[{ptr, type},
				DereferenceBuffer[ptr, type, 0]
			]
		]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	iDereferenceBuffer -> Typed[DereferenceBuffer, {"InertExpression", "InertExpression", "MachineInteger"} -> "InertExpression"]
|>];


(* Down values *)

DereferenceBuffer[ptr_, ty_, offset_] :=
	iDereferenceBuffer[ptr, ty, offset]

DereferenceBuffer[ptr_, ty_] :=
	iDereferenceBuffer[ptr, ty, 0]



End[] (* End `Private` *)

EndPackage[]
