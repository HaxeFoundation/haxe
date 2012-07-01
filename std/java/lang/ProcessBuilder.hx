package java.lang;
import java.NativeArray;

extern class ProcessBuilder
{
	function new(command:NativeArray<String>):Void;
	
	@:overload(function (dir:java.io.File):ProcessBuilder {})
	function directory():java.io.File;
	
	//function environment //TODO
	function redirectErrorStream():Bool;
	function start():Process;
}