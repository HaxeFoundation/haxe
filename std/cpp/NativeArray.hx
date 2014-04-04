package cpp;

class NativeArray {

	public static inline function blit<T>( ioDestArray:Array<T>,
		inDestElement:Int, inSourceArray:Array<T>,
		inSourceElement:Int, inElementCount:Int ): Void  {
	untyped ioDestArray.blit(inDestElement, inSourceArray, inSourceElement, inElementCount);
	};

	public static inline function zero<T>( ioDestArray:Array<T>, ?inFirst:Int, ?inElements:Int ) {
		untyped ioDestArray.zero(inFirst, inElements);
	};
}
