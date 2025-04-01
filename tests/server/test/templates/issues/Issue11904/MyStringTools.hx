class MyStringTools {
	public static inline function unsafeCodeAt(s:String, index:Int):Int {
		return (cast s).charCodeAt(index);
	}
}
