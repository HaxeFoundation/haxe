package cs.system.diagnostics;

@:native("System.Diagnostics.StackTrace")
extern class StackTrace {
	@:overload function new():Void;
	@:overload function new(fNeedFileInfo:Bool):Void;
	@:overload function new(e:cs.system.Exception):Void;
	@:overload function new(e:cs.system.Exception, fNeedFileInfo:Bool):Void;

	var FrameCount(default, never):Int;
	function GetFrame(index:Int):StackFrame;
	function GetFrames():cs.NativeArray<StackFrame>;
	function ToString():String;
}

@:native("System.Diagnostics.StackFrame")
extern class StackFrame {
	function new():Void;
	function GetMethod():cs.system.reflection.MethodBase;
	function GetFileName():String;
	function GetFileLineNumber():Int;
	function GetFileColumnNumber():Int;
	function GetILOffset():Int;
	function ToString():String;
}
