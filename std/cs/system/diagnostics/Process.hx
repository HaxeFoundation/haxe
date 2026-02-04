package cs.system.diagnostics;

@:native("System.Diagnostics.Process")
extern class Process {
	function new():Void;
	var StartInfo(default, never):ProcessStartInfo;
	var StandardOutput(default, never):cs.system.io.StreamReader;
	var StandardError(default, never):cs.system.io.StreamReader;
	var StandardInput(default, never):cs.system.io.StreamWriter;
	var Id(default, never):Int;
	var HasExited(default, never):Bool;
	var ExitCode(default, never):Int;
	function Start():Bool;
	function WaitForExit():Void;
	function Close():Void;
	function Kill():Void;
}
