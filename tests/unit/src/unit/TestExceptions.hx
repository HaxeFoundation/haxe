package unit;

import haxe.Exception;
import haxe.ValueException;

private enum EnumError {
	EError;
}

private class CustomHaxeException extends Exception {}

#if php
private class CustomNativeException extends php.Exception {}
#elseif js
private class CustomNativeException extends js.lib.Error {}
#end

class TestExceptions extends Test {

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
		var rethrown = false;
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
		var rethrown = false;
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
		var rethrown = false;
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
		var rethrown = false;
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
		var rethrown = false;
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
		var rethrown = false;
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
}
