package cpp;

@:coreType
extern class Pointer<T> extends ConstPointer<T> implements ArrayAccess<T>
{
	public var ref(get,set):T;

	public static function arrayElem<T>(array:Array<T>, inElem:Int):Pointer<T>;

	override public function inc():Pointer<T>;
	override public function dec():Pointer<T>;
	override public function postIncBy(inT:Int):Pointer<T>;
	override public function incBy(inT:Int):Pointer<T>;
	override public function add(inT:Int):Pointer<T>;

	public function destroy():Void;
	public function destroyArray():Void;
}

