package cpp;

@:coreType @:include("cpp/Pointer.h")
extern class ConstPointer<T> extends BasePointer<T>
{
	public function at(inIndex:Int):T;

	public function inc():ConstPointer<T>;
	public function dec():ConstPointer<T>;
	public function postIncBy(inT:Int):ConstPointer<T>;
	public function incBy(inT:Int):ConstPointer<T>;
	public function add(inT:Int):ConstPointer<T>;

}

