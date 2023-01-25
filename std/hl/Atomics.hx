package hl;

@:hlNative("std", "atomic_")
extern class Atomics {
	static function add32(r:hl.Ref<Int>, a:Int):Int;
	static function sub32(r:hl.Ref<Int>, a:Int):Int;
	static function and32(r:hl.Ref<Int>, a:Int):Int;
	static function or32(r:hl.Ref<Int>, a:Int):Int;
	static function xor32(r:hl.Ref<Int>, a:Int):Int;
	static function compareExchange32(r:hl.Ref<Int>, a:Int, b:Int):Int;
	static function exchange32(r:hl.Ref<Int>, val:Int):Int;
	static function load32(r:hl.Ref<Int>):Int;
	static function store32(r:hl.Ref<Int>, val:Int):Int;

	static function compareExchangePtr(r:hl.Ref<Dynamic>, a:Dynamic, b:Dynamic):Dynamic;
	static function exchangePtr(r:hl.Ref<Dynamic>, val:Dynamic):Dynamic;
	static function loadPtr(r:hl.Ref<Dynamic>):Dynamic;
	static function storePtr(r:hl.Ref<Dynamic>, val:Dynamic):Dynamic;
}
