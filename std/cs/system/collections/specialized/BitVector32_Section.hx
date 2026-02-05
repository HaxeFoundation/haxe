package cs.system.collections.specialized;

@:native("System.Collections.Specialized.BitVector32.Section")
extern class BitVector32_Section extends cs.system.ValueType {
	var Mask(default, never):cs.Int16;
	var Offset(default, never):cs.Int16;
	static function op_Equality(a:cs.system.collections.specialized.BitVector32_Section, b:cs.system.collections.specialized.BitVector32_Section):Bool;
	static function op_Inequality(a:cs.system.collections.specialized.BitVector32_Section, b:cs.system.collections.specialized.BitVector32_Section):Bool;
	static function ToString(value:cs.system.collections.specialized.BitVector32_Section):String;
	@:overload(function(obj:cs.system.collections.specialized.BitVector32_Section):Bool {})
	function Equals(o:Dynamic):Bool;
	function GetHashCode():Int;
	function ToString():String;
}
