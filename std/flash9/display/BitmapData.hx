package flash.display;

extern class BitmapData implements IBitmapDrawable {
	function new(width : Int, height : Int, ?transparent : Bool, ?fillColor : UInt) : Void;
	function applyFilter(sourceBitmapData : flash.display.BitmapData, sourceRect : flash.geom.Rectangle, destPoint : flash.geom.Point, filter : flash.filters.BitmapFilter) : Void;
	function clone() : flash.display.BitmapData;
	function colorTransform(rect : flash.geom.Rectangle, colorTransform : flash.geom.ColorTransform) : Void;
	function compare(otherBitmapData : flash.display.BitmapData) : Dynamic;
	function copyChannel(sourceBitmapData : flash.display.BitmapData, sourceRect : flash.geom.Rectangle, destPoint : flash.geom.Point, sourceChannel : UInt, destChannel : UInt) : Void;
	function copyPixels(sourceBitmapData : flash.display.BitmapData, sourceRect : flash.geom.Rectangle, destPoint : flash.geom.Point, ?alphaBitmapData : flash.display.BitmapData, ?alphaPoint : flash.geom.Point, ?mergeAlpha : Bool) : Void;
	function dispose() : Void;
	function draw(source : flash.display.IBitmapDrawable, ?matrix : flash.geom.Matrix, ?colorTransform : flash.geom.ColorTransform, ?blendMode : String, ?clipRect : flash.geom.Rectangle, ?smoothing : Bool) : Void;
	function fillRect(rect : flash.geom.Rectangle, color : UInt) : Void;
	function floodFill(x : Int, y : Int, color : UInt) : Void;
	function generateFilterRect(sourceRect : flash.geom.Rectangle, filter : flash.filters.BitmapFilter) : flash.geom.Rectangle;
	function getColorBoundsRect(mask : UInt, color : UInt, ?findColor : Bool) : flash.geom.Rectangle;
	function getPixel(x : Int, y : Int) : UInt;
	function getPixel32(x : Int, y : Int) : UInt;
	function getPixels(rect : flash.geom.Rectangle) : flash.utils.ByteArray;
	var height(default,null) : Int;
	function hitTest(firstPoint : flash.geom.Point, firstAlphaThreshold : UInt, secondObject : Dynamic, ?secondBitmapDataPoint : flash.geom.Point, ?secondAlphaThreshold : UInt) : Bool;
	function lock() : Void;
	function merge(sourceBitmapData : flash.display.BitmapData, sourceRect : flash.geom.Rectangle, destPoint : flash.geom.Point, redMultiplier : UInt, greenMultiplier : UInt, blueMultiplier : UInt, alphaMultiplier : UInt) : Void;
	function noise(randomSeed : Int, ?low : UInt, ?high : UInt, ?channelOptions : UInt, ?grayScale : Bool) : Void;
	function paletteMap(sourceBitmapData : flash.display.BitmapData, sourceRect : flash.geom.Rectangle, destPoint : flash.geom.Point, ?redArray : Array<Dynamic>, ?greenArray : Array<Dynamic>, ?blueArray : Array<Dynamic>, ?alphaArray : Array<Dynamic>) : Void;
	function perlinNoise(baseX : Float, baseY : Float, numOctaves : UInt, randomSeed : Int, stitch : Bool, fractalNoise : Bool, ?channelOptions : UInt, ?grayScale : Bool, ?offsets : Array<Dynamic>) : Void;
	function pixelDissolve(sourceBitmapData : flash.display.BitmapData, sourceRect : flash.geom.Rectangle, destPoint : flash.geom.Point, ?randomSeed : Int, ?numPixels : Int, ?fillColor : UInt) : Int;
	var rect(default,null) : flash.geom.Rectangle;
	function scroll(x : Int, y : Int) : Void;
	function setPixel(x : Int, y : Int, color : UInt) : Void;
	function setPixel32(x : Int, y : Int, color : UInt) : Void;
	function setPixels(rect : flash.geom.Rectangle, inputByteArray : flash.utils.ByteArray) : Void;
	function threshold(sourceBitmapData : flash.display.BitmapData, sourceRect : flash.geom.Rectangle, destPoint : flash.geom.Point, operation : String, threshold : UInt, ?color : UInt, ?mask : UInt, ?copySource : Bool) : UInt;
	var transparent(default,null) : Bool;
	function unlock(?changeRect : flash.geom.Rectangle) : Void;
	var width(default,null) : Int;
}
