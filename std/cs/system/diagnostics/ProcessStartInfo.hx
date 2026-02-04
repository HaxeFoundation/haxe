package cs.system.diagnostics;

@:native("System.Diagnostics.ProcessStartInfo")
extern class ProcessStartInfo {
	var FileName:String;
	var Arguments:String;
	var UseShellExecute:Bool;
	var CreateNoWindow:Bool;
	var RedirectStandardInput:Bool;
	var RedirectStandardOutput:Bool;
	var RedirectStandardError:Bool;
}
