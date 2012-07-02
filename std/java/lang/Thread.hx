package java.lang;
import haxe.Int64;

extern class Thread 
{
	static function activeCount():Int;
	static function currentThread():Thread;
	static function dumpStack():Void;
	static function enumerate(tarray:java.NativeArray<Thread>):Int;
	static function interrupted():Bool;
	@:overload(function(ms:Int64, nano:Int):Void {})
	static function sleep(ms:Int64):Void;
	static function yield():Void;
	
	function new(target:Runnable):Void;
	
	function destroy():Void;
	function getName():String;
	function getPriority():Int;
	function interrupt():Void;
	function isAlive():Bool;
	function isDaemon():Bool;
	function isInterrupted():Bool;
	
	@:overload(function(ms:Int64):Void {})
	@:overload(function(ms:Int64, nanos:Int):Void {})
	function join():Void;
	
	function run():Void; //should call start(), not run()
	function setPriority(newPriority:Int):Void;
	function start():Void;
}