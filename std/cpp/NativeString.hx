package cpp;

extern class NativeString {

	public static inline function raw( inString:String ) : RawConstPointer<Char> {
      return untyped inString.__s;
   }
	public static inline function c_str( inString:String ) : ConstPointer<Char> {
		return cpp.ConstPointer.fromPointer(untyped inString.__s);
   }
	public static inline function fromPointer(inPtr:ConstPointer<Char> ) : String {
      return untyped __global__.String(inPtr.ptr);
   }
}

