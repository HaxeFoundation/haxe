package cs.system.collections;

@:native('System.Collections.IEnumerable') extern interface IEnumerable
{
	function GetEnumerator():IEnumerator;
}