package flash;

extern class TextField
{
	var _x:Float;
	var _y:Float;
	var _xmouse:Float;
	var _ymouse:Float;
	var _xscale:Float;
	var _yscale:Float;
	var _width:Float;
	var _height:Float;
	var _alpha:Float;
	var _visible:Bool;
	var _target:String;
	var _rotation:Float;
	var _name:String;
	var _framesloaded:Float;
	var _droptarget:String;
	var _currentframe:Float;
	var _totalframes:Float;
	var _quality:String;
	var _focusrect:Bool;
	var _soundbuftime:Float;
	var _url:String;
	var _parent:MovieClip;

	var autoSize:Dynamic;
	var background:Bool;
	var backgroundColor:Float;
	var border:Bool;
	var borderColor:Float;
	var bottomScroll:Float;
	var condenseWhite:Bool;
	var embedFonts:Bool;
	var hscroll:Float;
	var html:Bool;
	var htmlText:String;
	var length:Float;
	var maxChars:Float;
	var maxhscroll:Float;
	var maxscroll:Float;
	var multiline:Bool;
	var password:Bool;
	var restrict:String;
	var scroll:Float;
	var selectable:Bool;
	var tabEnabled:Bool;
	var tabIndex:Float;
	var text:String;
	var textColor:Float;
	var textHeight:Float;
	var textWidth:Float;
	var type:String;
	var variable:String;
	var wordWrap:Bool;
	var mouseWheelEnabled:Bool;

#if flash8
	var antiAliasType:String;
	var gridFitType:String;
	var sharpness:Float;
#end

	var styleSheet:flash.text.StyleSheet;

	function replaceText(beginIndex:Float,endIndex:Float,newText:String):Void;
	function replaceSel(newText:String):Void;
	function getTextFormat(beginIndex:Float,endIndex:Float):TextFormat;
	function setTextFormat( begin : Dynamic, end : Dynamic, tf : TextFormat ):Void;
	function removeTextField():Void;
	function getNewTextFormat():TextFormat;
	function setNewTextFormat(tf:TextFormat):Void;
	function getDepth():Float;
	function addListener(listener:Dynamic):Bool;
	function removeListener(listener:Dynamic):Bool;
	static function getFontList():Array<Dynamic>;

	function onChanged(changedField:TextField):Void;
	function onKillFocus(newFocus:Dynamic):Void;
	function onScroller(scrolledField:TextField):Void;
	function onSetFocus(oldFocus:Dynamic):Void;

}
