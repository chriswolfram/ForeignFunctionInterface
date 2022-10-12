BeginPackage["ForeignFunctionInterface`RawObject`", {
	"ForeignFunctionInterface`",
	"Compile`Utilities`Components`"
}]


CreateRawObject
UnwrapRawObject


Begin["`Private`"]



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

		(* FunctionDeclaration[CreateRawObject,
			Typed[ForAllType[t, Element[t, "Numbers"], {t} -> t]]@
			Function[obj,
				obj
			]
		],

		FunctionDeclaration[CreateRawObject,
			Typed[ForAllType[t, Element[t, "DataStructures"], {t} -> t]]@
			Function[obj,
				obj
			]
		],

		FunctionDeclaration[CreateRawObject,
			Typed[{"String"} -> "String"]@
			Function[obj,
				obj
			]
		], *)


		FunctionDeclaration[UnwrapRawObject,
			Typed[ForAllType[t, {"RawObject"::[t]}->t]]@
			Function[obj,
				obj["Object"]
			]
		],

		(* FunctionDeclaration[UnwrapRawObject,
			Typed[ForAllType[t, {t}->t]]@
			Function[obj,
				obj
			]
		], *)


		(******* CArray *******)

		FunctionDeclaration[CreateCArray,
			Typed[{"MachineInteger"} -> "RawObject"::["OpaqueRawPointer"]]@
			Function[len,
				CreateRawObject@Cast[
					CreateTypeInstance["CArray"::["Integer8"], len],
					"OpaqueRawPointer", "BitCast"
				]
			]
		],

		FunctionDeclaration[FreeMemory,
			Typed[{"RawObject"::["OpaqueRawPointer"]} -> "Null"]@
			Function[pointer,
				DeleteObject[Cast[pointer["Object"], "CArray"::["Integer8"], "BitCast"]]
			]
		]

}];


DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	CreateCArray,
	FreeMemory
}];



End[] (* End `Private` *)

EndPackage[]
