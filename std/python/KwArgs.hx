package python;

import python.lib.Dict;

/**
	This type represents python `**kwargs` feature, supporting
	passing named arguments to a function.

	Example:

        function f(kwargs:KwArgs<{a:Int}>) {}
        f({a: 10});
**/
abstract KwArgs<T:{}>(Dict<String,Dynamic>) {
	inline function new (d:Dict<String,Dynamic>) {
		this = d;
	}

	@:to public inline function toDict():Dict<String,Dynamic> {
		return this;
	}

	@:from static inline function fromDict(d:Dict<String,Dynamic>):KwArgs<Dynamic> {
		return new KwArgs(d);
	}

	@:from static inline function fromT<T:{}>(d:T):KwArgs<T> {
		return new KwArgs(Lib.anonAsDict(d));
	}

	public inline function typed():T {
		return Lib.dictAsAnon(toDict());
	}

	public inline function get<V>(key:String, def:V):V {
		return this.get(key, def);
	}
}
