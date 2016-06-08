(* ::Package:: *)

(* ::Title:: *)
(*Serial Framework*)


BeginPackage["SerialFramework`"];


ConnectDevice::usage = "Connects to Nucleo through Serial";
WriteMessage::usage = "Writes a message to Nucleo";
ReadMessage::usage = "Reads a message from Nucleo";
DisconnectDevice::usage = "Disconnects device";


Begin["`Private`"];


ConnectDevice[dev_, baud_: 9600]:=
Module[{},
$dev = DeviceOpen["Serial", {dev, "BaudRate" -> baud}];
$startbit = 124;
Return[$dev]];


WriteMessage[msg_]:=
Module[{},
 id = 1;
 len = Length[msg] + 2;
 func = 0;

 DeviceWrite[$dev, $startbit];
 DeviceWrite[$dev, id];
 DeviceWrite[$dev, len];
 DeviceWrite[$dev, func];

 sum = $startbit + id + len + func;
 Do[
  DeviceWrite[$dev, msg[[i]]];
  sum = sum + msg[[i]];
 , {i, 1, Length[msg]}]

 While[sum > 255, sum = sum - 256;];
 DeviceWrite[$dev, sum];
]


ReadMessage[arg_: 0]:=
Module[{},
 DeviceReadBuffer[$dev];

 id = 1;
 len = 3;
 func = 1;

 DeviceWrite[$dev, $startbit];
 DeviceWrite[$dev, id];
 DeviceWrite[$dev, len];
 DeviceWrite[$dev, func];
 DeviceWrite[$dev, arg];

 sum = $startbit + id + len + func + arg;
 While[sum > 255, sum = sum - 256;];
 DeviceWrite[$dev, sum];

 Pause[0.015];
 reader = FromCharacterCode[DeviceReadBuffer[$dev]];
 Return[reader];
]


DisconnectDevice[]:=DeviceClose[$dev];


End[];
EndPackage[];
