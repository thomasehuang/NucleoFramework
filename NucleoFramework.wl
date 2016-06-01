(* ::Package:: *)

(* ::Title:: *)
(*Nucleo Framework*)


BeginPackage["NucleoFramework`"];


Init::usage="Initializes values";
ConstructIMUGUI::usage="Constructs user interface for IMU";
ConstructServoGUI::usage="Constructs user interface for Servo";
ReadSerialData::usage="Reads data through serial";
EnableGyr::usage="Enables gyroscope graph";
EnableAcc::usage="Enables accelerometer graph";
EnableMag::usage="Enables magnetometer graph";
EnableIMU::usage="Enables IMU graph";
EnableAngles::usage="Enable angles graph";
SetAngles::usage="Sets angles for servos";


Begin["`Private`"];


ConnectNucleo[x_]:=Return[$dev=DeviceOpen["Serial", x]];


Init[]:=
Module[{},
$gGXList={};
$gGYList={};
$gGZList={};
$gAXList={};
$gAYList={};
$gAZList={};
$gMXList={};
$gMYList={};
$gMZList={};
$gMList={};
$roll=0.0;
$pitch=0.0;
$yaw=0.0;
$pov={0,0,Infinity};
$writeCode="";
$numservos = 1;
$angles = {0};
$servograph = Graphics[{RGBColor[0, 0, 0], Rectangle[{0, 0}, {1, 3}]},Background -> White,PlotRange -> {{-5, 5}, {0, 6}}];
eye1 = Rectangle[{0.2, 0.25}, {0.4, 0.5}];
eye2 = Rectangle[{0.6, 0.25}, {0.8, 0.5}];
$g2 = Graphics[{Yellow, eye1, Yellow, eye2}];
$g4 = Graphics[Rectangle[{0,0},{0,0}]];
]


EnableGyr[]:=Dynamic[Labeled[ListLinePlot[{$gGXList, $gGYList, $gGZList}, PlotRange->All,PlotRangeClipping->False, ImageSize->Medium, PlotLegends->{"gx","gy", "gz"}],"gyroscope"],UpdateInterval->1]


EnableAcc[]:=Dynamic[Labeled[ListLinePlot[{$gAXList, $gAYList, $gAZList}, PlotRange->All,PlotRangeClipping->False, ImageSize->Medium, PlotLegends->{"ax","ay", "az"}],"accelerometer"],UpdateInterval->1]


EnableMag[]:=Dynamic[Labeled[ListLinePlot[{$gMXList, $gMYList, $gMZList}, PlotRange->All,PlotRangeClipping->False, ImageSize->Medium, PlotLegends->{"mx","my", "mz"}],"magnetometer"],UpdateInterval->1]


EnableAngles[]:=Dynamic[Show[$servograph, $g2, $g3, $g4]]


SetAngles[]:=
Module[{},
(*Initialize values*)
$servocubes = Array[0&, $numservos + 1];
rectstats = Array[0&, $numservos + 1];
angletext = Array[0&, $numservos];

Do[
 $servocubes[[i]] = Rectangle[{0, (i - 1) * 4}, {1, (i - 1) * 4 + 3}];
, {i, $numservos + 1}];
Do[
 rectstats[[i]] = {0.5, 3.5 + (i - 1) * 4};
, {i, $numservos + 1}];

Do[
 Do[
  $servocubes[[j]] = Rotate[$servocubes[[j]], -$angles[[i - 1]]Degree, rectstats[[i - 1]]];
  rectstats[[j]] = RotationTransform[-$angles[[i - 1]]Degree, rectstats[[i - 1]]][rectstats[[j]]];
  , {j, i, $numservos + 1, 1}];
 , {i, 2, $numservos + 1, 1}];
$servograph = Graphics[{RGBColor[0, 0, 0], $servocubes},Background -> White,PlotRange -> {{-1 * $numservos * 5, $numservos * 5}, {0, $numservos * 6}}];

(*Eyes and tail for the snake*)
eye1 = Rectangle[{0.2, 0.25}, {0.4, 0.5}];
eye2 = Rectangle[{0.6, 0.25}, {0.8, 0.5}];
$g2 = Graphics[{Yellow, eye1, Yellow, eye2}];
vec = Normalize[rectstats[[$numservos + 1]] - rectstats[[$numservos]]];
tail = Line[{rectstats[[$numservos + 1]], rectstats[[$numservos + 1]] + vec}];
$g3 = Graphics[{Black, Thick, tail}];

(*Angles display with text*)
Do[
 If[$angles[[i]] < 0, angletext[[i]] = Text[$angles[[i]], rectstats[[i]], {0, 0}], angletext[[i]] = Text[$angles[[i]], rectstats[[i]], {0, 0}]];
, {i, $numservos}];
$g4 = Graphics[{Red, angletext}];
]


EnableIMU[]:=
Module[{},
imu:=Graphics3D[Dynamic[GeometricTransformation[Cuboid[{-2,-2,-2},{2,2,2}],RotationTransform[$roll Degree,{1,0,0}].RotationTransform[$pitch Degree,{0,1,0}].RotationTransform[$yaw Degree,{0,0,1}]]],ImageSize->Medium,Axes->True,AxesOrigin->{0,0,0},Boxed->False,PlotRange->{{-5,5},{-5,5},{-5,5}},Ticks->None,ViewPoint->Dynamic[$pov]];
axeslabel=Graphics3D[{Text[Style["x",Large], {7, 0, 0}], Text[Style["y",Large], {0, 7, 0}], Text[Style["z",Large], {0, 0, 7}]}];
Labeled[Show[imu,axeslabel],"IMU"]
RadioButtonBar[Dynamic[$pov],{{0,0,Infinity}->"Above",{0,0,-Infinity}->"Below",{0,-Infinity,0}->Front,{0,Infinity,0}->Back,{Infinity,0,0}->Right,{-Infinity,0,0}->Left}]
]


ConstructIMUGUI[]:=
Module[{},
Print[Column[{
Button["Enable Gyr",
 Print[EnableGyr[]];
 $IMUflag = True;
 $gyrflag = True;
],
Button["Enable Acc",
 Print[EnableAcc[]];
 $IMUflag = True;
 $accflag = True;
],
Button["Enable Mag",
 Print[EnableMag[]];
 $IMUflag = True;
 $magflag = True;
],
Button["Enable IMU",
 Print[EnableIMU[]];
 $IMUflag = True;
 $IMUgraphflag = True;
],
Button["Stop", 
 $runprogram = False;
 $IMUflag = False;
 $gyrflag = False;
 $accflag = False;
 $magflag = False;
 $IMUgraphflag = False;
]}]];
]


ConstructServoGUI[]:=
Module[{},
Print[Column[{
Button["Enable Angles",
 Print[EnableAngles[]];
],
Panel[Grid[{{Style["Plot",Bold],SpanFromLeft},{"Number of Servos:",InputField[Dynamic[numservosstring],String]},{"Angles:",InputField[Dynamic[anglestring],String]},{Button["Send",
 $numservos = ToExpression[numservosstring];
 $sendstring = "s"<>ToString[$numservos]<>","<>anglestring<>";";
 $angles = ToExpression/@StringSplit[anglestring, ","];
 SetAngles[];
 $anglesflag = True;]}}]]}]];
]


ReadSerialData[]:=
Module[{},
	$runprogram = True;
	While[$runprogram,
		If[$anglesflag,Module[{},
        	DeviceWrite[$dev, $sendstring];
	        $anglesflag=False;
	    ]];
        If[$IMUflag,Module[{},
        	DeviceReadBuffer[$dev];
        	DeviceWrite[$dev, "g"];
			Pause[0.2];
            reader=FromCharacterCode[DeviceReadBuffer[$dev]];
            readerarr=StringSplit[reader];
            If[Length[readerarr]!=26,Continue[]];
        	If[$IMUgraphflag,Module[{},
				$roll=ToExpression[Part[readerarr,22]];
				$pitch=ToExpression[Part[readerarr,24]];
				$yaw=ToExpression[Part[readerarr,26]];
        	]];
            If[$gyrflag,Module[{},
            	gGX=ToExpression[Part[readerarr, 4]];
				gGY=ToExpression[Part[readerarr, 6]];
                gGZ=ToExpression[Part[readerarr, 8]];
                AppendTo[$gGXList,gGX];
                AppendTo[$gGYList,gGY];
                AppendTo[$gGZList,gGZ];
            ]];
            If[$accflag,Module[{},
            	gAX=ToExpression[Part[readerarr, 10]];
                gAY=ToExpression[Part[readerarr, 12]];
                gAZ=ToExpression[Part[readerarr, 14]];
                AppendTo[$gAXList,gAX];
                AppendTo[$gAYList,gAY];
                AppendTo[$gAZList,gAZ];
        	]];
        	If[$magflag,Module[{},
            	gMX=ToExpression[Part[readerarr, 16]];
                gMY=ToExpression[Part[readerarr, 18]];
                gMZ=ToExpression[Part[readerarr, 20]];
                AppendTo[$gMXList,gMX];
                AppendTo[$gMYList,gMY];
                AppendTo[$gMZList,gMZ];
        	]];
        ]];
	]
]


End[];
EndPackage[];
