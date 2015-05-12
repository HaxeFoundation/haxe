package cpp;

@:coreType
@:analyzer(no_simplification)
extern class Pointer<T> extends ConstPointer<T> implements ArrayAccess<T>
{
   @:analyzer(no_simplification)
	public var ref(get,set):T;

   @:analyzer(no_simplification)
   public function get_ref() : T;
   @:analyzer(no_simplification)
   public function set_ref(t:T) : T;


   public static function fromRaw<T>(ptr:RawPointer<T>) : Pointer<T>;

   public static function fromHandle<T>(inHandle:Dynamic,?inKind:String) : Pointer<T>;

   public static function fromPointer<T>(inNativePointer:Dynamic) : Pointer<T>;

   public static function addressOf<T>(inVariable:T) : Pointer<T>;

	public static function arrayElem<T>(array:Array<T>, inElem:Int):Pointer<T>;

   public function get_raw() : RawPointer<T>;

	override public function inc():Pointer<T>;
	override public function dec():Pointer<T>;
	override public function incBy(inT:Int):Pointer<T>;
	override public function add(inT:Int):Pointer<T>;

   @:analyzer(no_simplification)
	public function postIncRef():T;

	public function destroy():Void;
	public function destroyArray():Void;
}

