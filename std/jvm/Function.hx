package jvm;

import java.NativeArray;

@:native("haxe.jvm.Function")
@:nativeGen
extern class Function implements java.lang.Runnable {
	function new():Void;
	function invokeDynamic(args:NativeArray<Dynamic>):Dynamic;
	function equals(other:java.lang.Object):Bool;
	function invoke(arg1:java.lang.Object):java.lang.Object;
	function run():Void;
}
