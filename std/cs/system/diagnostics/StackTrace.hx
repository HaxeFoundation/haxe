package cs.system.diagnostics;

@:native("System.Diagnostics.StackTrace")
extern class StackTrace {
	@:overload(function():Void {})
	@:overload(function(fNeedFileInfo:Bool):Void {})
	@:overload(function(e:cs.system.Exception):Void {})
	function new(e:cs.system.Exception, fNeedFileInfo:Bool):Void;

	var FrameCount(default, never):Int;
	function GetFrame(index:Int):StackFrame;
	function GetFrames():cs.NativeArray<StackFrame>;
	override function ToString():String;
}

@:native("System.Diagnostics.StackFrame")
extern class StackFrame {
	function new():Void;
	function GetMethod():cs.system.reflection.MethodBase;
	function GetFileName():String;
	function GetFileLineNumber():Int;
	function GetFileColumnNumber():Int;
	function GetILOffset():Int;
	override function ToString():String;
}
