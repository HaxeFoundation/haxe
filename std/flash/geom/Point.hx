package flash.geom;

#if !flash8
"This class is only accesible in Flash8"
#end

extern class Point<T> {

	var x : T;
	var y : T;
	var length : Float;

	function new( x : T, y : T ) : Void;

	function normalize( length : T ) : Void;
	function add( p : Point<T> ) : Point<T>;
	function subtract( p : Point<T> ) : Point<T>;
	function equals( p : Point<T> ) : Bool;
	function offset( dx : T, dy : T ) : Void;
	function clone() : Point<T>;
	function toString() : String;

	static function distance( p1 : Point<T>, p2 : Point<T> ) : T;
	static function interpolate( p1 : Point<T>, p2 : Point<T>, f : T ) : Point<T>;
	static function polar( dist : T, angle : T ) : Point<T>;

}