package cs.system.io;
import cs.NativeArray;
import cs.StdTypes;
import haxe.Int64;
import haxe.io.BytesData;

@:native('System.IO.MemoryStream') extern class MemoryStream extends Stream 
{
	function new():Void;
	function GetBuffer():NativeArray<UInt8>;
	
}