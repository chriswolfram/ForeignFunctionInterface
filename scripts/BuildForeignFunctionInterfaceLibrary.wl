
SetAttributes[buildProgress, HoldAll];
buildProgress[label_, e_] := Check[
    Print["===== ", label];
    Print["===== Done in ", N[Round[First[AbsoluteTiming[e]], 1/100]], " seconds"];
    ,
    Print["===== BUILD FAILED"];
    Exit[1]
]

basedir = ParentDirectory[$CommandLine[[First[FirstPosition[$CommandLine, "-basedir"]] + 1]]];
If[Not[StringQ[basedir]],
    basedir = ParentDirectory[DirectoryName[$InputFileName], 2]
];


(*
  The directory that contains the "ffiConstantsDir" libraries should be
  added to $LibraryPath.
*)
libffiConstantsDir = $CommandLine[[First[FirstPosition[$CommandLine, "-outputdir"]] + 1]];
If[StringQ[libffiConstantsDir],
    PrependTo[$LibraryPath, libffiConstantsDir];
    PrependTo[$LibraryPath, FileNameJoin[{libffiConstantsDir, "Release"}]]
];

libffidir = DirectoryName[$CommandLine[[First[FirstPosition[$CommandLine, "-libffi"]] + 1]]];
If[StringQ[libffidir],
    PrependTo[$LibraryPath, libffidir]
];

outputdir = $CommandLine[[First[FirstPosition[$CommandLine, "-outputdir"]] + 1]];
If[Not[StringQ[outputdir]],
    outputdir = FileNameJoin[{basedir, "ForeignFunctionInterface",
        "ForeignFunctionInterface", "LibraryResources", $SystemID}]
];

Print["Mathematica build : ", {$Version, Internal`$LayoutCreationID}];
Print["Base directory    : ", basedir];
Print["Output directory  : ", outputdir];
Print["libffiConstants directory    : ", libffiConstantsDir];

libffiConstants = "ffiConstants";

If[Not[FileExistsQ[FindLibrary[libffiConstants]]],
    Print["Error: unable to locate the ", libffiConstants, " library"];
    Exit[1]
];


buildProgress["Source code directories setup",
    Scan[
        Function[
            PacletDirectoryLoad[Echo[FileNameJoin[{basedir, FileNameJoin[#]}]]]
        ]
        ,
        {
            {"Compile", "Compile"},
            {"Compile", "LLVMCompileTools"},
            {"CompileAST", "CompileAST"},
            {"CompileUtilities", "CompileUtilities"},
            {"LLVMTools", "LLVMTools"},
            {"Compile", "TypeFramework"},
            {"ForeignFunctionInterface", "ForeignFunctionInterface"}
        }
    ]
];

(*
    Handled separately, because these are not simple source
    checkouts, they contain prebuilt artifacts from TeamCity.
*)
buildProgress["Artifact directories setup",
    Scan[
        Function[
            PacletDirectoryLoad[Echo[FileNameJoin[{basedir, #}]]]
        ]
        ,
        {
            "CompiledCompiler",
            "CompiledLibrary",
            FileNameJoin[{"Layout", "LLVMLink"}]
        }
    ]
];

Needs["Compile`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`"]

buildProgress["Initializing Compiler",
    InitializeCompiler["LoadResources" -> False]
];


extraOpts = 
    If[
        $OperatingSystem === "MacOSX"
        ,
        {
            "CreateLibraryOptions" -> {"SystemLibraries" -> {}, 
                "CompileOptions" -> "-rpath @loader_path"}
        }
        ,
        {}]

buildProgress["Building ForeignFunctionInterface",
        BuildCompiledComponent["ForeignFunctionInterface", outputdir,
            CompilerEnvironment -> CreateCompilerEnvironment["TypeEnvironmentOptions" -> {"LoadResources" -> False}],
            CompilerOptions -> Join[{
                "TraceFunction" -> Print
            }, extraOpts]
        ]
    ]



successfile = Environment["SUCCESS_TXT"];
If[StringQ[successfile],
    Put[0, FileNameJoin[successfile]];
    Quit[]
];
