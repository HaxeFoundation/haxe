package cs.system.threading;
import cs.system.LocalDataStoreSlot;

@:native("System.Threading.ThreadStart") @:delegate typedef ThreadStart = Void->Void;

@:native("System.Threading.Thread") extern class Thread
{
	static function AllocateDataStoreSlot():LocalDataStoreSlot;
	static function GetData(slot:LocalDataStoreSlot):Dynamic;
	static function SetData(slot:LocalDataStoreSlot, data:Dynamic):Void;
	static function Sleep(ms:Int):Void;
	
	@:overload(function(s:ThreadStart, maxStack:Int):Void {})
	function new(s:ThreadStart):Void;
	
	@:overload(function(obj:Dynamic):Void {})
	function Abort():Void;
	
	@:overload(function(msTimeout:Int):Void {})
	function Join():Void;
	
	function Start():Void;
}