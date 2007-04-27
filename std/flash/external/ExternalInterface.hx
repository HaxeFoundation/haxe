package flash.external;

#if !flash8
"This class is only accesible in Flash8"
#end

extern class ExternalInterface {

	static var available : Bool;
	static function addCallback( methodName : String, instance : Dynamic, method : Dynamic ) : Bool;
	static function call( methodName : String, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic, ?p6 : Dynamic, ?p7 : Dynamic, ?p8 : Dynamic, ?p9 : Dynamic ) : Dynamic;

}