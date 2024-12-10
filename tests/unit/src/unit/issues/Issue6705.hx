package unit.issues;

class Issue6705 extends unit.Test {
	function memberFunction() {}

	static function staticFunction() {}

	function memberFunction1(i:Int) {}

	static function staticFunction1(i:Int) {}

	@:pure(false) static function alias<T>(t:T)
		return t;

	@:pure(false) static function equalsT<T>(a:T, b:T)
		return a == b;

	function test() {
		function localFunction() {}

		var localClosure = localFunction;
		var memberClosure = memberFunction;
		var staticClosure = staticFunction;

		t(localFunction == alias(localFunction));
		t(localFunction == alias(localClosure));
		#if !neko
		t(memberFunction == alias(memberFunction));
		t(memberFunction == alias(memberClosure));
		#end
		t(staticFunction == alias(staticFunction));
		t(staticFunction == alias(staticClosure));
		t(localClosure == alias(localClosure));
		t(memberClosure == alias(memberClosure));
		t(staticClosure == alias(staticClosure));
		t(localFunction == alias(localFunction));

		t(equalsT(localFunction, localClosure));
		#if !neko
		t(equalsT(memberFunction, memberFunction));
		t(equalsT(memberFunction, memberClosure));
		#end
		t(equalsT(staticFunction, staticFunction));
		t(equalsT(staticFunction, staticClosure));
		t(equalsT(localClosure, localClosure));
		t(equalsT(memberClosure, memberClosure));
		t(equalsT(staticClosure, staticClosure));

		t(Reflect.compareMethods(localFunction, alias(localFunction)));
		t(Reflect.compareMethods(localFunction, alias(localClosure)));
		t(Reflect.compareMethods(memberFunction, alias(memberFunction)));
		t(Reflect.compareMethods(memberFunction, alias(memberClosure)));
		t(Reflect.compareMethods(staticFunction, alias(staticFunction)));
		t(Reflect.compareMethods(staticFunction, alias(staticClosure)));
		t(Reflect.compareMethods(localClosure, alias(localClosure)));
		t(Reflect.compareMethods(memberClosure, alias(memberClosure)));
		t(Reflect.compareMethods(staticClosure, alias(staticClosure)));

		var array = [localFunction, memberFunction, staticFunction];
		eq(0, array.indexOf(localFunction));
		#if !neko
		eq(1, array.indexOf(memberFunction));
		#end
		eq(2, array.indexOf(staticFunction));
	}

	function testButEverythingIsDynamic() {
		function localFunction() {}

		var localClosure:Dynamic = localFunction;
		var memberClosure:Dynamic = memberFunction;
		var staticClosure:Dynamic = staticFunction;

		t((localFunction : Dynamic) == alias((localFunction : Dynamic)));
		t((localFunction : Dynamic) == alias(localClosure));
		#if !neko
		t((memberFunction : Dynamic) == alias((memberFunction : Dynamic)));
		t((memberFunction : Dynamic) == alias(memberClosure));
		#end
		t((staticFunction : Dynamic) == alias((staticFunction : Dynamic)));
		t((staticFunction : Dynamic) == alias(staticClosure));
		t(localClosure == alias(localClosure));
		t(memberClosure == alias(memberClosure));
		t(staticClosure == alias(staticClosure));
		t((localFunction : Dynamic) == alias((localFunction : Dynamic)));

		t(equalsT((localFunction : Dynamic), localClosure));
		#if !neko
		t(equalsT((memberFunction : Dynamic), (memberFunction : Dynamic)));
		t(equalsT((memberFunction : Dynamic), memberClosure));
		#end
		t(equalsT((staticFunction : Dynamic), (staticFunction : Dynamic)));
		t(equalsT((staticFunction : Dynamic), staticClosure));
		t(equalsT(localClosure, localClosure));
		t(equalsT(memberClosure, memberClosure));
		t(equalsT(staticClosure, staticClosure));

		t(Reflect.compareMethods((localFunction : Dynamic), alias((localFunction : Dynamic))));
		t(Reflect.compareMethods((localFunction : Dynamic), alias(localClosure)));
		t(Reflect.compareMethods((memberFunction : Dynamic), alias((memberFunction : Dynamic))));
		t(Reflect.compareMethods((memberFunction : Dynamic), alias(memberClosure)));
		t(Reflect.compareMethods((staticFunction : Dynamic), alias((staticFunction : Dynamic))));
		t(Reflect.compareMethods((staticFunction : Dynamic), alias(staticClosure)));
		t(Reflect.compareMethods(localClosure, alias(localClosure)));
		t(Reflect.compareMethods(memberClosure, alias(memberClosure)));
		t(Reflect.compareMethods(staticClosure, alias(staticClosure)));

		var array = [(localFunction : Dynamic), (memberFunction : Dynamic), (staticFunction : Dynamic)];
		eq(0, array.indexOf((localFunction : Dynamic)));
		#if !neko
		eq(1, array.indexOf((memberFunction : Dynamic)));
		#end
		eq(2, array.indexOf((staticFunction : Dynamic)));
	}

	function testButEverythingIsBackwards() {
		function localFunction() {}

		var localClosure = localFunction;
		var memberClosure = memberFunction;
		var staticClosure = staticFunction;

		t(alias(localFunction) == localFunction);
		t(alias(localClosure) == localFunction);
		#if !neko
		t(alias(memberFunction) == memberFunction);
		t(alias(memberClosure) == memberFunction);
		#end
		t(alias(staticFunction) == staticFunction);
		t(alias(staticClosure) == staticFunction);
		t(alias(localClosure) == localClosure);
		t(alias(memberClosure) == memberClosure);
		t(alias(staticClosure) == staticClosure);
		t(alias(localFunction) == localFunction);

		t(equalsT(localClosure, localFunction));
		#if !neko
		t(equalsT(memberFunction, memberFunction));
		t(equalsT(memberClosure, memberFunction));
		#end
		t(equalsT(staticFunction, staticFunction));
		t(equalsT(staticClosure, staticFunction));
		t(equalsT(localClosure, localClosure));
		t(equalsT(memberClosure, memberClosure));
		t(equalsT(staticClosure, staticClosure));

		t(Reflect.compareMethods(alias(localFunction), localFunction));
		t(Reflect.compareMethods(alias(localClosure), localFunction));
		t(Reflect.compareMethods(alias(memberFunction), memberFunction));
		t(Reflect.compareMethods(alias(memberClosure), memberFunction));
		t(Reflect.compareMethods(alias(staticFunction), staticFunction));
		t(Reflect.compareMethods(alias(staticClosure), staticFunction));
		t(Reflect.compareMethods(alias(localClosure), localClosure));
		t(Reflect.compareMethods(alias(memberClosure), memberClosure));
		t(Reflect.compareMethods(alias(staticClosure), staticClosure));
	}

	function testButEverythingIsBackwardsAndDynamic() {
		function localFunction() {}

		var localClosure:Dynamic = localFunction;
		var memberClosure:Dynamic = memberFunction;
		var staticClosure:Dynamic = staticFunction;

		t(alias((localFunction : Dynamic)) == (localFunction : Dynamic));
		t(alias(localClosure) == (localFunction : Dynamic));
		#if !neko
		t(alias((memberFunction : Dynamic)) == (memberFunction : Dynamic));
		t(alias(memberClosure) == (memberFunction : Dynamic));
		#end
		t(alias((staticFunction : Dynamic)) == (staticFunction : Dynamic));
		t(alias(staticClosure) == (staticFunction : Dynamic));
		t(alias(localClosure) == localClosure);
		t(alias(memberClosure) == memberClosure);
		t(alias(staticClosure) == staticClosure);
		t(alias((localFunction : Dynamic)) == (localFunction : Dynamic));

		t(equalsT(localClosure, (localFunction : Dynamic)));
		#if !neko
		t(equalsT((memberFunction : Dynamic), (memberFunction : Dynamic)));
		t(equalsT(memberClosure, (memberFunction : Dynamic)));
		#end
		t(equalsT((staticFunction : Dynamic), (staticFunction : Dynamic)));
		t(equalsT(staticClosure, (staticFunction : Dynamic)));
		t(equalsT(localClosure, localClosure));
		t(equalsT(memberClosure, memberClosure));
		t(equalsT(staticClosure, staticClosure));

		t(Reflect.compareMethods(alias((localFunction : Dynamic)), (localFunction : Dynamic)));
		t(Reflect.compareMethods(alias(localClosure), (localFunction : Dynamic)));
		t(Reflect.compareMethods(alias((memberFunction : Dynamic)), (memberFunction : Dynamic)));
		t(Reflect.compareMethods(alias(memberClosure), (memberFunction : Dynamic)));
		t(Reflect.compareMethods(alias((staticFunction : Dynamic)), (staticFunction : Dynamic)));
		t(Reflect.compareMethods(alias(staticClosure), (staticFunction : Dynamic)));
		t(Reflect.compareMethods(alias(localClosure), localClosure));
		t(Reflect.compareMethods(alias(memberClosure), memberClosure));
		t(Reflect.compareMethods(alias(staticClosure), staticClosure));
	}

	function test1() {
		function localFunction1(i:Int) {}

		var localClosure1 = localFunction1;
		var memberClosure1 = memberFunction1;
		var staticClosure1 = staticFunction1;

		t(localFunction1 == alias(localFunction1));
		t(localFunction1 == alias(localClosure1));
		#if !neko
		t(memberFunction1 == alias(memberFunction1));
		t(memberFunction1 == alias(memberClosure1));
		#end
		t(staticFunction1 == alias(staticFunction1));
		t(staticFunction1 == alias(staticClosure1));
		t(localClosure1 == alias(localClosure1));
		t(memberClosure1 == alias(memberClosure1));
		t(staticClosure1 == alias(staticClosure1));

		t(equalsT(localFunction1, localFunction1));
		t(equalsT(localFunction1, localClosure1));
		#if !neko
		t(equalsT(memberFunction1, memberFunction1));
		t(equalsT(memberFunction1, memberClosure1));
		#end
		t(equalsT(staticFunction1, staticFunction1));
		t(equalsT(staticFunction1, staticClosure1));
		t(equalsT(localClosure1, localClosure1));
		t(equalsT(memberClosure1, memberClosure1));
		t(equalsT(staticClosure1, staticClosure1));

		t(Reflect.compareMethods(localFunction1, alias(localFunction1)));
		t(Reflect.compareMethods(localFunction1, alias(localClosure1)));
		t(Reflect.compareMethods(memberFunction1, alias(memberFunction1)));
		t(Reflect.compareMethods(memberFunction1, alias(memberClosure1)));
		t(Reflect.compareMethods(staticFunction1, alias(staticFunction1)));
		t(Reflect.compareMethods(staticFunction1, alias(staticClosure1)));
		t(Reflect.compareMethods(localClosure1, alias(localClosure1)));
		t(Reflect.compareMethods(memberClosure1, alias(memberClosure1)));
		t(Reflect.compareMethods(staticClosure1, alias(staticClosure1)));

		var array = [localFunction1, memberFunction1, staticFunction1];
		eq(0, array.indexOf(localFunction1));
		#if !neko
		eq(1, array.indexOf(memberFunction1));
		#end
		eq(2, array.indexOf(staticFunction1));
	}

	#if !neko
	function testCallsEqualityCheck_tempvarCallExprs() {
		var callCount = 0;
		function getFn():() -> Void {
			callCount++;
			return memberFunction;
		}
		t(getFn() == getFn());
		eq(2, callCount);
	}
	#end

	#if !hl // @see https://github.com/HaxeFoundation/haxe/issues/10031
	function testTypeChange() {
		function f1(x:Float) {}
		var f2:Int->Void = f1;
		t(f1 == f2);
	}
	#end
}
