package cpp;

extern class NativeString {

	public static inline function c_str( inString:String ) : ConstPointer<Char> {
		var result:ConstPointer<Char> =  untyped inString.__s;
		return result;
   }
	public static inline function fromPointer(inPtr:ConstPointer<Char> ) : String {
      return untyped __global__.String(inPtr.ptr);
   }
}

