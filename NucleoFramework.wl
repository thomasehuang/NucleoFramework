(* ::Package:: *)

(* ::Title:: *)
(*Nucleo Framework*)


BeginPackage["NucleoFramework`"]


ConnectNucleo::usage="Connect to Nucleo"
Init::usage="Initialize variables"
InitGraphs::usage="Initialize graphs"
CallFunc::usage="Call a function"
SetAngles::usage="Set angles of servos"


Begin["`Private`"]


ConnectNucleo[x_]:=
Return[$dev=DeviceOpen["Serial", x]]


Init[]:=
Module[{},
$gGXList:={};
$gGYList:={};
$gGZList:={};
$gAXList:={};
$gAYList:={};
$gAZList:={};
$gMXList:={};
$gMYList:={};
$gMZList:={};
$gMList:={};
$writeCode="";
]


InitGraphs[]:=
Module[{},
Dynamic[Labeled[ListLinePlot[{$gGXList}, PlotRange->All,PlotRangeClipping->False, ImageSize->Medium], "gx"],UpdateInterval->1]
Dynamic[Labeled[ListLinePlot[$gGYList, PlotRange->All,PlotRangeClipping->False, ImageSize->Medium], "gy"],UpdateInterval->1]
Dynamic[Labeled[ListLinePlot[$gGZList, PlotRange->All,PlotRangeClipping->False, ImageSize->Medium], "gz"],UpdateInterval->1]
Dynamic[Labeled[ListLinePlot[{$gGXList, $gGYList, $gGZList}, PlotRange->All,PlotRangeClipping->False, ImageSize->Medium, PlotLegends->{"gx","gy", "gz"}],"gyroscope"],UpdateInterval->1]
Dynamic[Labeled[ListLinePlot[$gAXList, PlotRange->All,PlotRangeClipping->False, ImageSize->Medium], "ax"],UpdateInterval->1]
Dynamic[Labeled[ListLinePlot[$gAYList, PlotRange->All,PlotRangeClipping->False, ImageSize->Medium], "ay"],UpdateInterval->1]
Dynamic[Labeled[ListLinePlot[$gAZList, PlotRange->All,PlotRangeClipping->False, ImageSize->Medium], "az"],UpdateInterval->1]
Dynamic[Labeled[ListLinePlot[{$gAXList, $gAYList, $gAZList}, PlotRange->All,PlotRangeClipping->False, ImageSize->Medium, PlotLegends->{"ax","ay", "az"}],"accelerometer"],UpdateInterval->1]
Dynamic[Labeled[ListLinePlot[$gMXList, PlotRange->All,PlotRangeClipping->False, ImageSize->Medium], "mx"],UpdateInterval->1]
Dynamic[Labeled[ListLinePlot[$gMYList, PlotRange->All,PlotRangeClipping->False, ImageSize->Medium], "my"],UpdateInterval->1]
Dynamic[Labeled[ListLinePlot[$gMZList, PlotRange->All,PlotRangeClipping->False, ImageSize->Medium], "mz"],UpdateInterval->1]
Dynamic[Labeled[ListLinePlot[{$gMXList, $gMYList, $gMZList}, PlotRange->All,PlotRangeClipping->False, ImageSize->Medium, PlotLegends->{"mx","my", "mz"}],"magnetometer"],UpdateInterval->1]
]


OFunc[]:=
Module[{},
While[1<2,
Pause[0.2];
DeviceWrite[$dev, "o"];
reader=FromCharacterCode[DeviceReadBuffer[$dev]];
Print[reader];
]
]


PFunc[]:=
Module[{},
While[1<2,
Pause[0.2];
DeviceWrite[$dev, "p"];
reader=FromCharacterCode[DeviceReadBuffer[$dev]];
roll=ToExpression[Part[StringSplit[reader],1]];
pitch=ToExpression[Part[StringSplit[reader],2]];
yaw=ToExpression[Part[StringSplit[reader],3]];
Print[reader];
]
]


GFunc[]:=
Module[{},
While[1<2,
Pause[0.2];
DeviceWrite[$dev, "g"];
reader=FromCharacterCode[DeviceReadBuffer[$dev]];
gString:=StringSplit[reader," "];
If[Length[gString]!=20,Continue[]];
gTime:=Part[gString, 2];
gGX:=ToExpression[Part[gString, 4]];
gGY:=ToExpression[Part[gString, 6]];
gGZ:=ToExpression[Part[gString, 8]];
AppendTo[$gGXList,gGX];
AppendTo[$gGYList,gGY];
AppendTo[$gGZList,gGZ];
gAX:=ToExpression[Part[gString, 10]];
gAY:=ToExpression[Part[gString, 12]];
gAZ:=ToExpression[Part[gString, 14]];
AppendTo[$gAXList,gAX];
AppendTo[$gAYList,gAY];
AppendTo[$gAZList,gAZ];
gMX:=ToExpression[Part[gString, 16]];
gMY:=ToExpression[Part[gString, 18]];
gMZ:=ToExpression[Part[gString, 20]];
AppendTo[$gMXList,gMX];
AppendTo[$gMYList,gMY];
AppendTo[$gMZList,gMZ];
]
]


TFunc[]:=
Module[{},
While[1<2,
Pause[0.2];
DeviceWrite[$dev, "g"];
reader=FromCharacterCode[DeviceReadBuffer[$dev]];
gString:=StringSplit[reader," "];
If[Length[gString]!=20,Continue[]];
gTime:=Part[gString, 2];
gMX:=ToExpression[Part[gString, 16]];
gMY:=ToExpression[Part[gString, 18]];
gMZ:=ToExpression[Part[gString, 20]];
gM:={gMX,gMY,gMZ};
AppendTo[$gMList,gM];
]
]


CallFunc[code_]:=
Module[{},
If[code=="o", OFunc[]]
If[code=="p", PFunc[]]
If[code=="g", GFunc[]]
If[code=="t", TFunc[]]
]


SetAngles[x_]:=
Module[{numservos=Length[x]},
str=ToString/@x;
Part[str,numservos]=Part[str,numservos]<>";";
DeviceWrite[$dev,StringJoin[Riffle[Join[{ToString[numservos]},str],","]]];
Pause[1];
Return[FromCharacterCode[DeviceReadBuffer[$dev]]];
]


End[]
EndPackage[]
