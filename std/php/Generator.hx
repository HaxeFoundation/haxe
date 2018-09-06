package php;

/**
	@see http://php.net/manual/en/class.generator.php

	Generator is not a Haxe Iterable. It can be iterated one time only.
	Unfortunately Haxe does not know that in PHP generators may have no `return` expression or `return value` with any type of `value`.
	Use `return null` or untyped cast to workaround this issue:
	```
	function generatorWithoutReturn():Generator {
		php.Syntax.yield(1);
		return null;
	}

	function generatorWithReturn():Generator {
		php.Syntax.yield(1);
		return cast "hello";
	}

	var g = generatorWithReturn();
	for(i in g) {
		trace(i);
	}
	trace(g.getReturn()); // "hello"
	```
**/
@:native('Generator')
extern class Generator {
	function current() : Dynamic;
	function getReturn() : Dynamic;
	function key() : Dynamic;
	function next() : Void;
	function rewind() : Void;
	function send(value : Dynamic) : Dynamic;
	@:native('throw')
	function throwError(exception : Throwable) : Dynamic;
	function valid() : Bool;

	inline function iterator():GeneratorIterator {
		return new GeneratorIterator(this);
	}
}

private class GeneratorIterator {
	var generator : Generator;
	/**
		This field is required to maintain execution order of .next()/.valid()/.current() methods.
		@see http://php.net/manual/en/class.iterator.php
	**/
	var first : Bool = true;

	public inline function new(generator : Generator) {
		this.generator = generator;
	}

	public inline function hasNext() : Bool {
		if(first) {
			first = false;
		} else {
			generator.next();
		}
		return generator.valid();
	}

	public inline function next() : Dynamic {
		return generator.current();
	}
}