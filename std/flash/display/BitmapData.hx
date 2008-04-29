package flash.display;

#if !flash8
"This class is only accesible in Flash8"
#end

import flash.geom.Rectangle;
import flash.geom.Point;

extern class BitmapData {

	static function loadBitmap( id : String ) : BitmapData;

	var width : Int;
	var height : Int;
	var rectangle : Rectangle<Int>;
	var transparent : Bool;

	function new( width : Int, height : Int, ?transparent : Bool, ?fillcolor : Int ) : Void;

	function getPixel( x : Int, y : Int ) : Int;
	function setPixel( x : Int, y : Int, color : Int ) : Void;
	function getPixel32( x : Int, y : Int ) : Int;
	function setPixel32( x : Int, y : Int, color : Int ) : Void;

	function fillRect( r : Rectangle<Int>, color : Int ) : Void;
	function copyPixels( src : BitmapData, srcRect : Rectangle<Int>, dst : Point<Int>, ?alpha : BitmapData, ?alphaPos : Point<Int>, ?mergeAlpha : Bool ) : Void;
	function applyFilter( source : BitmapData, sourceRect : Rectangle<Int>, dest : Point<Int>, filter : flash.filters.BitmapFilter ) : Int;
	function scroll( dx : Int, dy : Int ) : Void;
	function threshold( src : BitmapData , srcRect : Rectangle<Int>, dstPoint : Point<Int>, op : String, threshold : Int, ?color : Int, ?mask : Int, ?copy : Bool ) : Int;
	function draw( source : Dynamic, ?matrix : flash.geom.Matrix, ?colortrans : flash.geom.ColorTransform, ?blendMode : Dynamic, ?clipRect : Rectangle<Int>, ?smooth : Bool) : Void;
	function pixelDissolve( src : BitmapData, srcRect : Rectangle<Int>, dst : Point<Int>, ?seed : Int, ?npixels : Int, ?fillColor : Int ) : Int;
	function floodFill( x : Int, y : Int, color : Int ) : Void;
	function getColorBoundsRect( mask : Int, color : Int, ?fillColor : Bool ) : Rectangle<Int>;
	function perlinNoise( x : Int, y : Int, num : Int, seed : Int, stitch : Bool, noise : Bool, ?channels : Int, ?gray : Bool, ?offsets : Array<Point<Float>> ) : Void;
	function colorTransform( r : Rectangle<Int>, trans : flash.geom.ColorTransform ) : Void;
	function hitTest( firstPoint : Point<Int>, firstAlpha : Int, object : Dynamic, ?secondPoint : Point<Int>, ?secondAlpha : Int ) : Bool;
	function paletteMap( source : BitmapData, srcRect : Rectangle<Int>, dst : Point<Int>, ?reds : Array<Dynamic>, ?greens : Array<Dynamic>, ?blues : Array<Dynamic>, ?alphas : Array<Dynamic> ) : Void;
	function merge( src : BitmapData, srcRect : Rectangle<Int>, dst : Point<Int>, redMult : Int, greenMult : Int, blueMult : Int, alphaMult : Int ) : Void;
	function noise( seed : Int, ?low : Int, ?high : Int, ?channels : Int, ?gray : Bool ) : Void;
	function copyChannel( source : BitmapData, sourceRect : Rectangle<Int>, dest : Point<Int>, sourceChannel : Int, destChannel : Int ) : Void;
	function clone() : BitmapData;
	function dispose() : Void;
	function generateFilterRect(sourceRect : Rectangle<Int>, filter : flash.filters.BitmapFilter ) : Rectangle<Int>;

#if flash_v9
	function compare( b : BitmapData ) : BitmapData; // WTF ?
#end

}

