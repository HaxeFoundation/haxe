package cpp;

@:unreflective @:coreType
extern class Pointer<T> implements ArrayAccess<T>
{
   // ptr actually returns the pointer - not strictly a 'T' - for pointers to smaert pointers
   // Use value or [0] for a reference to the item
	public var ptr:T;
	public var value(get,set):T;

	public static function arrayElem<T>(array:Array<T>, inElem:Int):Pointer<T>;

	public function inc():Pointer<T>;
	public function dec():Void;
	public function postIncBy(inT:Int):Pointer<T>;
	public function incBy(inT:Int):Pointer<T>;
	public function add(inT:Int):Pointer<T>;

	public function destroy():Void;
	public function destroyArray():Void;

	public function lt(inOther:Pointer<T>):Bool;
	public function leq(inOther:Pointer<T>):Bool;
	public function gt(inOther:Pointer<T>):Bool;
	public function geq(inOther:Pointer<T>):Bool;

}

