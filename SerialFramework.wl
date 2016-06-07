(* ::Package:: *)

(* ::Title:: *)
(*Serial Framework*)


BeginPackage["SerialFramework`"];


ConnectDevice::usage = "Connects to Nucleo through Serial";
WriteMessage::usage = "Writes a message to Nucleo";
ReadMessage::usage = "Reads a message from Nucleo";
DisconnectDevice::usage = "Disconnects device";


Begin["`Private`"];


ConnectDevice[dev_]:=
Module[{},
$dev = DeviceOpen["Serial", {dev, "BaudRate" -> 230400}];
Return[$dev]];


WriteMessage[msg_]:=
Module[{},
 id = 1;
 sendstring = ToString[id] <> ".0." <> ToString[msg] <> ";";
 DeviceWrite[$dev, sendstring];
]


ReadMessage[arg_: 0]:=
Module[{},
 DeviceReadBuffer[$dev];
 sendstring = "1.1." <> ToString[arg] <> ";";
 DeviceWrite[$dev, "1.1;"];
 Pause[0.2];
 reader=FromCharacterCode[DeviceReadBuffer[$dev]];
 Return[reader];
]


DisconnectDevice[]:=DeviceClose[$dev];


End[];
EndPackage[];
