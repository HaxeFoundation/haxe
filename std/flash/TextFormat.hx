extern class TextFormat
{
	var font:String;
	var size:Float;
	var color:Float;
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
	var tabStops:Array<Dynamic>;
	var bullet:Bool;
	function new(font:String,size:Float,textColor:Float,
                    	bold:Bool,italic:Bool,underline:Bool,
                    	url:String,window:String,align:String,
                    	leftMargin:Float,rightMargin:Float,indent:Float,leading:Float) : Void;
	function getTextExtent(text:String):Dynamic;
}


