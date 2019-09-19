BeginPackage["AnsiTerminal`"];

AnsiDigitPattern::usage = "";
AnsiOperatorPattern::usage = "";
AnsiDigitColor::usage = "";
AnsiOperatorColor::usage = "";
AnsiSystemColor::usage = "";


Begin["`Private`"];

ansi[str_String]:=FromCharacterCode[27]<>"["<>str;
reset[]:=ansi["0m"];
fg[r_Integer,g_Integer,b_Integer]:=ansi["38;2;"<>ToString[r]<>";"<>ToString[g]<>";"<>ToString[b]<>"m"];
fg[color_]:=Module[{r,g,b},{r,g,b}=Round[255*List@@ColorConvert[color,RGBColor]];ansi["38;2;"<>ToString[r]<>";"<>ToString[g]<>";"<>ToString[b]<>"m"]];
bg[r_Integer,g_Integer,b_Integer]:=ansi["48;2;"<>ToString[r]<>";"<>ToString[g]<>";"<>ToString[b]<>"m"];
bg[color_]:=Module[{r,g,b},{r,g,b}=Round[255*List@@ColorConvert[color,RGBColor]];ansi["48;2;"<>ToString[r]<>";"<>ToString[g]<>";"<>ToString[b]<>"m"]];

AnsiDigitPattern = DigitCharacter;
AnsiOperatorPattern = Alternatives@@Characters["`~!@#$%^&*()-_=+[{]}\\|;:'\",<.>/?"];

AnsiDigitColor = GrayLevel[.8];
AnsiOperatorColor = RGBColor[0.8,0.6,0.4];
AnsiSystemColor = RGBColor[0.6,0.6,1];

SetOptions[$Output,PageWidth->Infinity];
$PrePrint=Function[{out},
	Module[{str=ToString[out],sys},
		sys = Alternatives @@ Cases[out,e_Symbol /; Context[e] === "System`" :> ToString[e], \[Infinity], Heads -> True];
		StringReplace[str,{
			d:AnsiDigitPattern :> (fg[AnsiDigitColor]<>d<>reset[]),
			d:AnsiOperatorPattern :> (fg[AnsiOperatorColor]<>d<>reset[]),
			d:sys :> (fg[AnsiSystemColor]<>d<>reset[])
			}
		]
	]
]

End[]

EndPackage[]
