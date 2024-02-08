/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package php;

/**
	Generator is not a Haxe Iterable. It can be iterated one time only.
	Unfortunately Haxe does not know that in PHP generators may have no `return` expression or `return value` with any type of `value`.
	Use `return null` or untyped cast to workaround this issue:
	```haxe
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

	@see http://php.net/manual/en/class.generator.php
**/
@:native('Generator')
extern class Generator {
	function current():Dynamic;
	function getReturn():Dynamic;
	function key():Dynamic;
	function next():Void;
	function rewind():Void;
	function send(value:Dynamic):Dynamic;
	@:native('throw')
	function throwError(exception:Throwable):Dynamic;
	function valid():Bool;

	inline function iterator():GeneratorIterator {
		return new GeneratorIterator(this);
	}
}

private class GeneratorIterator {
	var generator:Generator;

	/**
		This field is required to maintain execution order of .next()/.valid()/.current() methods.
		@see http://php.net/manual/en/class.iterator.php
	**/
	var first:Bool = true;

	public inline function new(generator:Generator) {
		this.generator = generator;
	}

	public inline function hasNext():Bool {
		if (first) {
			first = false;
		} else {
			generator.next();
		}
		return generator.valid();
	}

	public inline function next():Dynamic {
		return generator.current();
	}
}
