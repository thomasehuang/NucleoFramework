(* ::Package:: *)

(* ::Title:: *)
(*Nucleo Framework*)


BeginPackage["NucleoFramework`"]


ConnectNucleo::usage="Connect to Nucleo"
Init::usage="Initialize variables"
InitGraphs::usage="Initialize graphs"
CallFunc::usage="Call a function"
SetAngles::usage="Set angles of servos"
ServoOriGraph::usage="Draw servo graph"


Begin["`Private`"]


ConnectNucleo[x_]:={
Return[$dev=DeviceOpen["Serial", x]];}


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
Module[{},
(*Initialize values*)
$numservos=Length[x];
$angles=Array[0&,$numservos];
$servocubes=Array[0&,$numservos+1];
rectstats=Array[0&,$numservos+1];

(*Check if angles are valid*)
error=False;
Do[
If[x[[i]]>45||x[[i]]<-45,error=True,$angles[[i]]=x[[i]]];
,{i,$numservos}];

(*====================Servo orientation code start====================*)
Print["Orientation: "];
Do[
$servocubes[[i]] = Rectangle[{0, (i-1)*4}, {1, (i-1)*4+3}];,
{i,$numservos+1}];
Do[
rectstats[[i]]={0.5,3.5+(i-1)*4};,
{i,$numservos+1}];
Do[
 Do[
  $servocubes[[j]] = Rotate[$servocubes[[j]], $angles[[i-1]]Degree, rectstats[[i - 1]]];
  rectstats[[j]] = RotationTransform[$angles[[i-1]]Degree, rectstats[[i - 1]]][rectstats[[j]]];
  , {j, i, $numservos+1, 1}];
 , {i, 2, $numservos+1, 1}];

(*Eyes and tail for the snake*)
eye1 = Rectangle[{0.2, 0.25}, {0.4, 0.5}];
eye2 = Rectangle[{0.6, 0.25}, {0.8, 0.5}];
g2 = Graphics[{Yellow, eye1, Yellow, eye2}];
vec = Normalize[rectstats[[$numservos + 1]] - rectstats[[$numservos]]];
tail = Line[{rectstats[[$numservos + 1]], rectstats[[$numservos + 1]] + vec}];
g3 = Graphics[{White, Thick, tail}];
servograph = Graphics[{RGBColor[1, 1, 1], $servocubes},Background->Black,PlotRange->{{-1 * $numservos * 5, $numservos * 5}, {0, $numservos * 7}}];
Print[Show[servograph, g2, g3]];
(*====================Servo orientation code end====================*)

(*Set up string to send through serial
  Format: "numservos, angle1, angle2, anglen;"*)
str=ToString/@x;
Part[str,$numservos]=Part[str,$numservos]<>";";
If[error,Print["Invalid angles"],DeviceWrite[$dev,StringJoin[Riffle[Join[{ToString[$numservos]},str],","]]]];
Print["Sending: "];
Print[StringJoin[Riffle[Join[{ToString[$numservos]},str],","]]];
Print["Receiving: "];
Pause[1];
If[error,Return[], Return[FromCharacterCode[DeviceReadBuffer[$dev]]]];
]


End[]
EndPackage[]
