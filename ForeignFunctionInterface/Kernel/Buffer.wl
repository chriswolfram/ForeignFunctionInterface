BeginPackage["ChristopherWolfram`ForeignFunctionInterface`Buffer`"]


Begin["`Private`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]



(***************************************************)
(********** Creating and freeing buffers ***********)
(***************************************************)


(******* DeleteBuffer *******)
DeclareCompiledComponent["ForeignFunctionInterface", {

		FunctionDeclaration[DeleteBuffer,
			Typed[{"InertExpression"} -> "Null"]@
			Function[ptr,
				(* TODO: This ignores the element type of the array. Confirm that this isn't a problem. *)
				DeleteObject[Cast[ExpressionToPointer[ptr], "CArray"::["Integer8"], "BitCast"]]
			]
		]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	DeleteBuffer
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
			Typed[{"InertExpression", "FFIType", "MachineInteger"} -> "InertExpression"]@
			Function[{ptr, type, len},

				Switch[type["Type"],

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

		FunctionDeclaration[BufferToNumericArray,
			Typed[{"InertExpression", "InertExpression", "MachineInteger"} -> "InertExpression"]@
			Function[{ptr, typeExpr, len},
				Module[{type = CreateTypeInstance["Managed", CreateFFIType[typeExpr], DeleteFFIType]},
					BufferToNumericArray[ptr, Compile`BorrowManagedObject[type], len]		
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
			Typed[{"InertExpression", "FFIType"} -> "InertExpression"]@
			Function[{expr, type},

				Switch[type["Type"],

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
		],

		FunctionDeclaration[NumericArrayToBuffer,
			Typed[{"InertExpression", "InertExpression"} -> "InertExpression"]@
			Function[{expr, typeExpr},
				Module[{type = CreateTypeInstance["Managed", CreateFFIType[typeExpr], DeleteFFIType]},
					NumericArrayToBuffer[expr, Compile`BorrowManagedObject[type]]		
				]		
			]
		]

}];


DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	BufferToNumericArray -> Typed[BufferToNumericArray, {"InertExpression", "InertExpression", "MachineInteger"} -> "InertExpression"],
	NumericArrayToBuffer -> Typed[NumericArrayToBuffer, {"InertExpression", "InertExpression"} -> "InertExpression"]
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

		(******* ManagedExpression case *******)

		FunctionDeclaration[DereferenceBuffer,
			Typed[ForAllType[ty, {"ManagedExpression", ty, "MachineInteger"} -> "InertExpression"]]@
			Function[{man, type, offset},
				DereferenceBuffer[GetManagedExpression[man], type, offset]
			]
		],

		(******* Expression case *******)

		FunctionDeclaration[DereferenceBuffer,
			Typed[{"InertExpression", "InertExpression", "MachineInteger"} -> "InertExpression"]@
			Function[{expr, type, offset},
				DereferenceBuffer[ExpressionToPointer[GetManagedExpression[expr]], type, offset]
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



(***************************************************)
(********* Writing expressions into buffers ********)
(***************************************************)


DeclareCompiledComponent["ForeignFunctionInterface", {

		(******* Dereferencing / indexing *******)

		FunctionDeclaration[PopulateBuffer,
			Typed[ForAllType[ty, {"OpaqueRawPointer", "InertExpression", "InertExpression", "MachineInteger"} -> "InertExpression"]]@
			Function[{ptr, type, val, offset},
				Module[{ffiType},
					ffiType = CreateTypeInstance["Managed", CreateFFIType[type], DeleteFFIType];
					ExpressionToC[
						Cast[Cast[ptr, "UnsignedInteger64", "BitCast"] + FFITypeByteCount[Compile`BorrowManagedObject[ffiType]] * offset, "OpaqueRawPointer", "BitCast"],
						Compile`BorrowManagedObject[ffiType],
						val
					];
					PointerToExpression[ptr]
				]
			]
		],

		(******* ManagedExpression case *******)

		FunctionDeclaration[PopulateBuffer,
			Typed[ForAllType[ty, {"ManagedExpression", ty, "InertExpression", "MachineInteger"} -> "InertExpression"]]@
			Function[{man, type, val, offset},
				PopulateBuffer[GetManagedExpression[man], type, val, offset]
			]
		],

		(******* Expression case *******)

		FunctionDeclaration[PopulateBuffer,
			Typed[{"InertExpression", "InertExpression", "InertExpression", "MachineInteger"} -> "InertExpression"]@
			Function[{expr, type, val, offset},
				PopulateBuffer[ExpressionToPointer[GetManagedExpression[expr]], type, val, offset]
			]
		],

		(******* 3-argument form *******)

		FunctionDeclaration[PopulateBuffer,
			Typed[ForAllType[p, (*Element[p, {"InertExpression", "OpaqueRawPointer"}],*) {p, "InertExpression", "InertExpression"} -> "InertExpression"]]@
			Function[{ptr, val, type},
				DereferenceBuffer[ptr, type, val, 0]
			]
		]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	iPopulateBuffer -> Typed[PopulateBuffer, {"InertExpression", "InertExpression", "InertExpression", "MachineInteger"} -> "InertExpression"]
|>];


(* Down values *)

PopulateBuffer[ptr_, ty_, val_, offset_] :=
	iPopulateBuffer[ptr, ty, val, offset]

PopulateBuffer[ptr_, ty_, val_] :=
	iPopulateBuffer[ptr, ty, val, 0]



End[] (* End `Private` *)

EndPackage[]
