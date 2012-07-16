package cs.system;
 
@:native("System.Array")
extern class Array 
{

	public static function Copy(sourceArray:Array, sourceIndex:Int, destinationArray:Array, destinationIndex:Int, length:Int):Void;
}