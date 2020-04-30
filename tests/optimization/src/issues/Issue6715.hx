package issues;

class Issue6715 {
	@:js('
		var f = function() {
			var x = 1;
			x = 2;
		};
		issues_Issue6715.x = f;
		issues_Issue6715.x = f;
		issues_Issue6715.x = f;
		var x = 1;
		x = 2;
		var x = 1;
		x = 2;
		var x = 1;
		x = 2;
	')
	@:analyzer(no_local_dce)
	static public function test1() {
		insanity(function() { var x = 1; x = 2; });
	}

	@:js('
		var x = 1;
		x = 2;
		var x = 1;
		x = 2;
		var x = 1;
		x = 2;
	')
	@:analyzer(no_local_dce)
	static public function test2() {
		insanity2(function() { var x = 1; x = 2; });
	}

	@:js('
		issues_Issue6715.x = function() {var x = 1;};
	')
	@:analyzer(no_local_dce)
	static public function test3() {
		insanity3(function() var x = 1);
	}

	@:js('
		var f = function() {var x = 1;};
		issues_Issue6715.x = f;
		issues_Issue6715.x = f;
		issues_Issue6715.x = f;
	')
	@:analyzer(no_local_dce)
	static public function test4() {
		insanity4(function() var x = 1);
	}

	static var x:Void->Void;

	// Mixed: inline calls, reference reads
	static inline function insanity(f:Void -> Void)
	{
		x = f;
		x = f;
		x = f;

		f();
		f();
		f();
	}

	// Only calls: inline all
	static inline function insanity2(f:Void -> Void)
	{
		f();
		f();
		f();
	}

	// Referenced once: inline
	static inline function insanity3(f:Void -> Void)
	{
		x = f;
	}

	// Referenced multiple times: temp var
	static inline function insanity4(f:Void -> Void)
	{
		x = f;
		x = f;
		x = f;
	}
}