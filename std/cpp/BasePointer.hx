package cpp;

@:coreType @:include("cpp/Pointer.h")
extern class BasePointer<T>
{
   // ptr actually returns the pointer - not strictly a 'T' - for pointers to smart pointers
   // Use value or ref to get dereferenced value
	public var ptr:T;

	public var value(get,never):T;

	public function lt(inOther:BasePointer<T>):Bool;
	public function leq(inOther:BasePointer<T>):Bool;
	public function gt(inOther:BasePointer<T>):Bool;
	public function geq(inOther:BasePointer<T>):Bool;

}


