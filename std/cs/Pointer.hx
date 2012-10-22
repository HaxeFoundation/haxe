package cs;

/**
	This type represents pointer types for C# function parameters. It should only
	be used inside an unsafe context (not checked by the Haxe compiler)

	C# code:
		int[] src;
		fixed (int* pSrc = src)
		{
			...
		}
	Haxe code:
		var pSrc:cs.Pointer<Int>;
		cs.Lib.fixed(pSrc = cast src,
		{
			...
		});

**/
#if !unsafe
#error "You need to define 'unsafe' to be able to use unsafe code in hxcs"
#else
extern class Pointer<T> /*extends Int,*/ implements ArrayAccess<T>
{

}
#end