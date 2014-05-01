package cpp;


extern class Pointer<T> implements ArrayAccess<T>
{
   // ptr actually returns the pointer - not strictly a 'T'
   // Use value or [0] for a reference to the item
	public var ptr:T;
	public var value(get,set):T;

	public static function addressOf<T>(value:T):Pointer<T>;

	public function inc():Void;
	public function dec():Void;
	public function add(inT:Int):Void;
	public function deref():T;

}

