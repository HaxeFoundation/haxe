package cpp;


extern class Pointer<T> implements ArrayAccess<T>
{
	public static function fromArray<T>(array:Array<T>, inIdx:Int):Pointer<T>;

	public function inc():Void;
	public function dec():Void;
	public function add(inT:Int):Void;

   // ptr actually returns the pointer, so the ->member sysntax will work
   // Use [0] for a reference to the item
	public function ptr():T;

}

