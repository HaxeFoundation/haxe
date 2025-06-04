package hxcoro.components;

import haxe.coro.context.Key;
import haxe.coro.context.IElement;

class CoroName implements IElement<CoroName> {
	static public final key = new Key<CoroName>("Name");

	public final name:String;

	public function new(name:String) {
		this.name = name;
	}

	public function getKey() {
		return key;
	}
}