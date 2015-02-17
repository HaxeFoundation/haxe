package python;

import python.lib.Dict;

abstract KwArgs<T:{}> (Dict<String, Dynamic>)
{
	inline function new (d:Dict<String, Dynamic>) this = d;

	@:to public inline function toDict ():Dict<String, Dynamic>
	{
		return this;
	}
	@:from static inline function fromDict (d:Dict<String, Dynamic>):KwArgs<Dynamic>
	{
		return new KwArgs(d);
	}
	@:from static inline function fromT <T:{}>(d:T):KwArgs<T>
	{
		return new KwArgs(Lib.anonAsDict(d));
	}

	public function typed ():T
	{
		return Lib.dictAsAnon(toDict());
	}

	public function get <V>(key:String, def:V):V
	{
		return this.get(key, def);
	}
}
