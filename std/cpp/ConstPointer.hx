package cpp;

@:coreType @:include("cpp/Pointer.h") @:native("cpp.Pointer")
extern class ConstPointer<T>
{
   // ptr actually returns the pointer - not strictly a 'T' - for pointers to smart pointers
   // Use value or ref to get dereferenced value
	private var ptr:T;

	public var value(get,never):T;
	public var raw(get,never):RawConstPointer<T>;

   public function get_raw() : RawConstPointer<T>;

	public function lt(inOther:Pointer<T>):Bool;
	public function leq(inOther:Pointer<T>):Bool;
	public function gt(inOther:Pointer<T>):Bool;
	public function geq(inOther:Pointer<T>):Bool;



   public static function fromPointer<T>(inNativePointer:Dynamic) : ConstPointer<T>;

	public function reinterpret<Other>():Pointer<Other>;

	public function at(inIndex:Int):T;

	public function inc():ConstPointer<T>;
	public function dec():ConstPointer<T>;
	public function postIncVal():T;
	public function incBy(inT:Int):ConstPointer<T>;
	public function add(inT:Int):ConstPointer<T>;

}

