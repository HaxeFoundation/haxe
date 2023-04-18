package unit.issues;

import utest.Assert;

#if (cpp && !cppia)
@:native('MY_DEFINE')
extern class MyDefine {}

@:unreflective
@:structAccess
@:native('my_extern')
extern class MyExtern<T> {
	function new();

	function create() : T;
}

@:headerCode('

typedef int MY_DEFINE;

template<class T>
class my_extern {
public:
	T create() {
		return T();
	}
};

')
#end
class Issue10876 extends Test {
    #if (cpp && !cppia)
    function test() {
        final vec = cpp.Pointer.fromStar(new MyExtern<MyDefine>());
		final num = vec.ptr.create();

        Assert.equals(0, num);
    }
    #end
}