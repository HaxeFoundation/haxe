package lua;

import lua.Lua;

class PairsIterator<K,V>
{
	private var table : Table<K,V>;
	private var result : NextResult<K,V>;
	
	public function new( table : Table<K,V> )
	{
		this.table = table;
	}
	
	public function hasNext() : Bool
	{
		this.result = Lua.next(table, null == result ? null : result.index);
		return null != result.index;
	}
	
	public function next() : PairsNextResult<K,V>
	{
		return new PairsNextResult(result.index, result.value);
	}
}

class PairsNextResult<K,V> 
{
	public var index : K;
	public var value : V;
	public function new ( index : K, value : V)
	{
		this.index = index;
		this.value = value;
	}
}