package flash;

extern class TextFormat
{
	var font:String;
	var size:Float;
	var color:Int;
	var url:String;
	var target:String;
	var bold:Bool;
	var italic:Bool;
	var underline:Bool;
	var align:String;
	var leftMargin:Float;
	var rightMargin:Float;
	var indent:Float;
	var leading:Float;
	var blockIndent:Float;
	var tabStops:Array<Int>;
	var bullet:Bool;

	function new( ?font:String, ?size:Float, ?textColor:Int,
                  ?bold:Bool, ?italic:Bool, ?underline:Bool,
                  ?url:String, ?window:String, ?align:String,
                  ?leftMargin:Float, ?rightMargin:Float, ?indent:Float,
                  ?leading:Float ) : Void;

	function getTextExtent(text:String, ?width : Float) : Dynamic;

	#if flash8
	var kerning : Bool;
	var letterSpacing : Float;
	var display : String;
	#end

	private static function __init__() : Void untyped {
		flash.TextFormat = _global["TextFormat"];
	}

}
