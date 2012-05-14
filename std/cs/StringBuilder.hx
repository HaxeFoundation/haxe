package cs;

//FIXME this class is here due to seemingly a bug in type resolution inside _std packages.
//Once this bug is fixed, it will be moved back to the system.text package
@:native('System.Text.StringBuilder') extern class StringBuilder 
{

	function new():Void;
	
	@:overload(function(obj:Dynamic):cs.StringBuilder {})
	@:overload(function(str:String, startIndex:Int, len:Int):cs.StringBuilder {})
	function Append(char:cs.StdTypes.Char16):StringBuilder;
	
	function ToString():String;
}