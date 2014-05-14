package cpp;

@:coreType @:include("cpp/Pointer.h") @:native("cpp.Pointer")
extern class ConstPointer<T> extends BasePointer<T>
{
   public static function fromPointer<T>(inNativePointer:Dynamic) : ConstPointer<T>;

	public function reinterpret<Other>():Pointer<Other>;

	public function at(inIndex:Int):T;

	public function inc():ConstPointer<T>;
	public function dec():ConstPointer<T>;
	public function postIncVal():T;
	public function incBy(inT:Int):ConstPointer<T>;
	public function add(inT:Int):ConstPointer<T>;

}

