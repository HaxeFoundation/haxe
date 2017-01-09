/*
 * Copyright (C)2005-2017 Haxe Foundation
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

package haxe;

/**
	This class provides advanced methods on enums. It is ideally used with
	`using EnumTools` and then acts as an 
  [extension](https://haxe.org/manual/lf-static-extension.html) to the 
  `enum` types.

	If the first argument to any of the methods is null, the result is
	unspecified.
**/
extern class EnumTools {
	/**
		Returns the name of enum `e`, including its path.

		If `e` is inside a package, the package structure is returned dot-
		separated, with another dot separating the enum name:
			pack1.pack2.(...).packN.EnumName
		If `e` is a sub-type of a Haxe module, that module is not part of the
		package structure.

		If `e` has no package, the enum name is returned.

		If `e` is `null`, the result is unspecified.

		The enum name does not include any type parameters.
	**/
	static public inline function getName<T>(e:Enum<T>):String {
		return Type.getEnumName(e);
	}

	/**
		Creates an instance of enum `e` by calling its constructor `constr` with
		arguments `params`.

		If `e` or `constr` is `null`, or if enum `e` has no constructor named
		`constr`, or if the number of elements in `params` does not match the
		expected number of constructor arguments, or if any argument has an
		invalid type, the result is unspecified.
	**/
	static public inline function createByName<T>(e:Enum<T>, constr:String, ?params:Array<Dynamic>):T {
		return Type.createEnum(e, constr, params);
	}

	/**
		Creates an instance of enum `e` by calling its constructor number
		`index` with arguments `params`.

		The constructor indices are preserved from Haxe syntax, so the first
		declared is index 0, the next index 1 etc.

		If `e` or `index` is `null`, or if enum `e` has no constructor
		corresponding to index `index`, or if the number of elements in `params`
		does not match the expected number of constructor arguments, or if any
		argument has an invalid type, the result is unspecified.
	**/
	static public inline function createByIndex<T>(e:Enum<T>, index:Int, ?params:Array<Dynamic>):T {
		return Type.createEnumIndex(e, index, params);
	}

	/**
		Returns a list of all constructors of enum `e` that require no
		arguments.

		This may return the empty Array `[]` if all constructors of `e` require
		arguments.

		Otherwise an instance of `e` constructed through each of its non-
		argument constructors is returned, in the order of the constructor
		declaration.

		If `e` is `null`, the result is unspecified.
	**/
	static public inline function createAll<T>(e:Enum<T>):Array<T> {
		return Type.allEnums(e);
	}

	/**
		Returns a list of the names of all constructors of enum `e`.

		The order of the constructor names in the returned Array is preserved
		from the original syntax.

		If `c` is `null`, the result is unspecified.
	**/
	static public inline function getConstructors<T>(e:Enum<T>):Array<String> {
		return Type.getEnumConstructs(e);
	}
}

/**
	This class provides advanced methods on enum values. It is ideally used with
	`using EnumValueTools` and then acts as an 
  [extension](https://haxe.org/manual/lf-static-extension.html) to the 
  `EnumValue` types.

	If the first argument to any of the methods is null, the result is
	unspecified.
**/
extern class EnumValueTools {

	/**
		Recursively compares two enum instances `a` and `b` by value.

		Unlike `a == b`, this function performs a deep equality check on the
		arguments of the constructors (if there are any).

		If `a` or `b` are `null`, the result is unspecified.
	**/
	static public inline function equals<T:EnumValue>(a:T, b:T):Bool {
		return Type.enumEq(a, b);
	}

	/**
		Returns the constructor name of enum instance `e`.

		The result String does not contain any constructor arguments.

		If `e` is `null`, the result is unspecified.
	**/
	static public inline function getName(e:EnumValue):String {
		return Type.enumConstructor(e);
	}

	/**
		Returns a list of the constructor arguments of enum instance `e`.

		If `e` has no arguments, the result is `[]`.

		Otherwise the result are the values that were used as arguments to `e`,
		in the order of their declaration.

		If `e` is `null`, the result is unspecified.
	**/
	static public inline function getParameters(e:EnumValue):Array<Dynamic> {
		return Type.enumParameters(e);
	}

	/**
		Returns the index of enum instance `e`.

		This corresponds to the original syntactic position of `e`. The index of
		the first declared constructor is 0, the next one is 1 etc.

		If `e` is `null`, the result is unspecified.
	**/
	static public inline function getIndex(e:EnumValue):Int {
		return Type.enumIndex(e);
	}

	/**
		Matches enum instance `e` against pattern `pattern`, returning `true` if
		matching succeeded and `false` otherwise.

		Example usage:

		```haxe
		if (e.match(pattern)) {
			// codeIfTrue
		} else {
			// codeIfFalse
		}
		```

		This is equivalent to the following code:

		```haxe
		switch (e) {
			case pattern:
				// codeIfTrue
			case _:
				// codeIfFalse
		}
		```

		This method is implemented in the compiler. This definition exists only
		for documentation.
	**/
	static public function match(e:EnumValue, pattern:Dynamic):Bool {
		return false;
	}
}
