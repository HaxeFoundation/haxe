package php;

/**
	Native PHP interface.
	@see http://php.net/manual/en/class.arrayaccess.php
**/
@:native('ArrayAccess')
extern interface ArrayAccess<K,V> {
	function offsetExists( offset:K ) : Bool;
	function offsetGet( offset:K ) : V;
	function offsetSet( offset:K, value:V ) : Void;
	function offsetUnset( offset:K ) : Void;
}