extern class Stage
{
	/* haXe specific */
	static var _global : Dynamic;
	static var _root : MovieClip;
	static var current : MovieClip;
	/* end */

	static var width:Float;
	static var height:Float;
	static var scaleMode:String;
	static var align:String;
	static var showMenu:Bool;
	static function addListener(listener:Dynamic):Void;
	static function removeListener(listener:Dynamic):Void;
}


