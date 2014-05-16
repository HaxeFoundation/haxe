
package python;

import python.lib.Dict;

abstract KwArgs (Dict<String, Dynamic>)
{
	inline function new (d:Dict<String, Dynamic>) this = d;

	@:to public inline function toDict ():Dict<String, Dynamic>
	{
		return this;
	}
	@:from static inline function fromDict (d:Dict<String, Dynamic>):KwArgs
	{
		return new KwArgs(d);
	}

	public function get <V>(key:String, def:V):V
	{
		return this.get(key, def);
	}
}