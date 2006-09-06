package flash;

extern class Button
#if flash_strict
#else true
implements Dynamic
#end
{
	var _alpha : Float;
	var enabled : Bool;
	var _focusrect : Bool;
	var _height : Float;
	var _highquality : Int; //Deprecated
	var menu : ContextMenu;
	var _name : String;
	var _parent : MovieClip;
	var _quality : String;
	var _rotation : Float;
	var _soundbuftime : Float;
	var tabEnabled : Bool;
	var tabIndex : Int;
	var _target : String;
	var trackAsMenu : Bool;
	var _url : String;
	var useHandCursor : Bool;
	var _visible : Bool;
	var _width : Float;
	var _x : Float;
	var _xmouse : Float;
	var _xscale : Float;
	var _y : Float;
	var _ymouse : Float;
	var _yscale : Float;
	
	function onDragOut() : Void;
	function onDragOver() : Void;
	function onKeyDown() : Void;
	function onKeyUp() : Void;
	function onKillFocus(newFocus : Dynamic) : Void;
	function onPress() : Void;
	function onRelease() : Void;
	function onReleaseOutside() : Void;
	function onRollOut() : Void;
	function onRollOver() : Void;
	function onSetFocus(oldFocus : Dynamic) : Void;
	
	function getDepth() : Int;

#if flash8
	var filters : Array<+flash.filters.BitmapFilter>;
	var blendMode : Dynamic;
	var cacheAsBitmap : Bool;
	var scale9Grid : flash.geom.Rectangle<Float>;
#end

	private static function __init__() : Void untyped {
		flash.Button = _global["Button"];
	}

}


