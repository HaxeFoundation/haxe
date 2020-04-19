package unit;

import haxe.Exception;
import haxe.ValueException;
import haxe.CallStack;
import utest.Assert;
import unit.HelperMacros;

private enum EnumError {
	EError;
}

private abstract AbstrString(String) from String {}
private abstract AbstrException(CustomHaxeException) from CustomHaxeException {}

private class CustomHaxeException extends Exception {}

#if php
private class CustomNativeException extends php.Exception {}
#elseif js
private class CustomNativeException extends js.lib.Error {}
#elseif flash
private class CustomNativeException extends flash.errors.Error {}
#elseif java
private class CustomNativeException extends java.lang.RuntimeException {}
#elseif cs
private class CustomNativeException extends cs.system.Exception {}
#elseif python
private class CustomNativeException extends python.Exceptions.Exception {}
#elseif (lua || eval || neko || hl || cpp)
private class CustomNativeException { public function new(m:String) {} }
#end

#if java
private class NativeExceptionBase extends java.lang.RuntimeException {}
private class NativeExceptionChild extends NativeExceptionBase {}
private class NativeExceptionOther extends java.lang.RuntimeException {}
#end

private class NoConstructorValueException extends ValueException {}

private class WithConstructorValueException extends ValueException {
	public function new(value:Any, ?previous:Exception, ?native:Any) {
		super(value, previous, native);
	}
}

private typedef ItemData = {?file:String, ?line:Int, ?method:String}

class TestExceptions extends Test {
	/** Had to move to instance var because of https://github.com/HaxeFoundation/haxe/issues/9174 */
	var rethrown:Bool = false;

	public function testWildCardCatch() {
		try {
			throw 123;
		} catch(e:Dynamic) {
			eq(123, e);
		}

		try {
			throw 123;
		} catch(e:Exception) {
			eq('123', e.message);
			t(Std.isOfType(e, ValueException));
		}
	}

	public function testWildCardCatch_rethrow() {
		var thrown = new CustomHaxeException('');
		rethrown = false;
		try {
			try {
				throw thrown;
			} catch(e:Exception) {
				rethrown = true;
				throw e;
			}
		} catch(e:CustomHaxeException) {
			eq(thrown, e);
			t(rethrown);
		}

		var thrown = new CustomNativeException('');
		rethrown = false;
		try {
			try {
				throw thrown;
			} catch(e:Exception) {
				rethrown = true;
				throw e;
			}
		} catch(e:CustomNativeException) {
			eq(thrown, e);
			t(rethrown);
		}
	}

	public function testSpecificCatch_propagatesUnrelatedExceptions() {
		var propagated = false;
		try {
			try {
				throw new ValueException('hello');
			} catch(e:CustomHaxeException) {
				assert();
			}
			assert();
		} catch(e:ValueException) {
			propagated = true;
		}
		t(propagated);
	}

	public function testCatchAbstract() {
		var a:AbstrString = 'hello';
		try {
			throw a;
		} catch(e:AbstrString) {
			eq(a, e);
		}

		var a:AbstrException = new CustomHaxeException('');
		try {
			throw a;
		} catch(e:AbstrException) {
			eq(a, e);
		}
	}

	public function testValueException() {
		try {
			throw 123;
		} catch(e:ValueException) {
			eq(123, e.value);
		}
		try {
			throw 123;
		} catch(e:Int) {
			eq(123, e);
		}

		try {
			throw EError;
		} catch(e:ValueException) {
			eq('EError', e.message);
		}
		try {
			throw EError;
		} catch(e:EnumError) {
			eq(EError, e);
		}

		try {
			throw 'string';
		} catch(e:ValueException) {
			eq('string', e.value);
		}
		try {
			throw 'string';
		} catch(e:String) {
			eq('string', e);
		}
	}

	public function testCustomNativeException() {
		var thrown = new CustomNativeException('');
		rethrown = false;
		try {
			try {
				throw thrown;
			} catch(e:CustomNativeException) {
				eq(thrown, e);
				rethrown = true;
				throw e;
			}
		} catch(e:CustomNativeException) {
			eq(thrown, e);
			t(rethrown);
		}
	}

	public function testCustomNativeException_thrownAsDynamic() {
		var thrown:Any = new CustomNativeException('');
		rethrown = false;
		try {
			try {
				throw thrown;
			} catch(e:CustomNativeException) {
				eq(thrown, e);
				rethrown = true;
				throw e;
			}
		} catch(e:CustomNativeException) {
			eq(thrown, e);
			t(rethrown);
		}
	}

	public function testCustomHaxeException() {
		var thrown = new CustomHaxeException('');
		rethrown = false;
		try {
			try {
				throw thrown;
			} catch(e:CustomHaxeException) {
				eq(thrown, e);
				rethrown = true;
				throw e;
			}
		} catch(e:CustomHaxeException) {
			eq(thrown, e);
			t(rethrown);
		}
	}

	public function testCustomHaxeException_thrownAsDynamic() {
		var thrown:Any = new CustomHaxeException('');
		rethrown = false;
		try {
			try {
				throw thrown;
			} catch(e:CustomHaxeException) {
				eq(thrown, e);
				rethrown = true;
				throw e;
			}
		} catch(e:CustomHaxeException) {
			eq(thrown, e);
			t(rethrown);
		}
	}

	public function testExceptionStack() {
		var data = [
			'_without_ throws' => stacksWithoutThrowLevel1(),
			'_with_ throws' => stacksWithThrowLevel1()
		];
		for(label => stacks in data) {
			Assert.isTrue(stacks.length > 1, '$label: wrong stacks.length');
			var expected = null;
			var lineShift = 0;
			for(s in stacks) {
				if(expected == null) {
					expected = stackItemData(s[0]);
				} else {
					var actual = stackItemData(s[0]);
					if(expected.line != actual.line) {
						if(lineShift == 0) {
							lineShift = actual.line - expected.line;
						}
						expected.line += lineShift;
					}
					Assert.same(expected, actual, '$label: $expected is expected, but got $actual');
				}
			}
		}
	}

	function stacksWithoutThrowLevel1() {
		return stacksWithoutThrowLevel2();
	}

	function stacksWithoutThrowLevel2():Array<CallStack> {
		var result:Array<CallStack> = [];
		// It's critical for `testExceptionStack` test to keep the following lines
		// order with no additional code in between.
		result.push(CallStack.callStack());
		result.push(new Exception('').stack);
		result.push(new ValueException('').stack);
		result.push(new WithConstructorValueException('').stack);
		result.push(new NoConstructorValueException('').stack);
		result.push(@:privateAccess (Exception.thrown(''):Exception).stack);
		return result;
	}

	function stacksWithThrowLevel1() {
		return stacksWithThrowLevel2();
	}

	function stacksWithThrowLevel2():Array<CallStack> {
		var result:Array<CallStack> = [];
		// It's critical for `testExceptionStack` test to keep the following lines
		// order with no additional code in between.
		result.push(try throw new Exception('') catch(e:Exception) e.stack);
		result.push(try throw new ValueException('') catch(e:Exception) e.stack);
		result.push(try throw new WithConstructorValueException('') catch(e:Exception) e.stack);
		result.push(try throw new NoConstructorValueException('') catch(e:Exception) e.stack);
		result.push(try throw @:privateAccess (Exception.thrown(''):Exception) catch(e:Exception) e.stack);
		return result;
	}

	function stackItemData(item:StackItem):ItemData {
		var result:ItemData = {};
		switch item {
			case FilePos(s, f, l, _):
				result.file = f;
				result.line = l;
				switch s {
					case Method(_, m): result.method = m;
					case _:
				}
			case _:
		}
		return result;
	}

	function testCatch_noTypeHint() {
		try {
			throw new Exception('Terrible error');
		} catch(e) {
			Assert.notNull(Std.downcast(e, Exception));
		}

		HelperMacros.parseAndPrint('try { } catch(e) { }');
		eq('haxe.Exception', HelperMacros.typeString(try throw new Exception('') catch(e) e));
	}

#if java
	function testCatchChain() {
		eq("caught NativeExceptionChild: msg", raise(() -> throw new NativeExceptionChild("msg")));
		eq("caught NativeExceptionBase: msg", raise(() -> throw new NativeExceptionBase("msg")));
		eq("caught String: msg", raise(() -> throw "msg"));
		eq("caught NativeExceptionOther: msg", raise(() -> throw new NativeExceptionOther("msg")));
		eq("caught Int: 12", raise(() -> throw 12));
		eq("caught Throwable: 12.1", raise(() -> throw 12.1));
		eq("caught Throwable: false", raise(() -> throw false));
		eq("caught Throwable: msg", raise(() -> throw new java.lang.Exception("msg")));
	}

	function raise<T>(f:Void -> String) {
		return try {
			f();
		} catch(e:NativeExceptionChild) {
			'caught NativeExceptionChild: ${e.getMessage()}';
		} catch(e:NativeExceptionBase) {
			'caught NativeExceptionBase: ${e.getMessage()}';
		} catch(e:String) {
			'caught String: $e';
		} catch(e:NativeExceptionOther) {
			'caught NativeExceptionOther: ${e.getMessage()}';
		} catch(e:Int) {
			'caught Int: $e';
 		} catch(e:java.lang.Throwable) {
			'caught Throwable: ${e.getMessage()}';
		}
	}
#end
}