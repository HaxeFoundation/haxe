package cases;


#if target.threaded
import haxe.atomic.AtomicInt;
import haxe.atomic.AtomicObject;
#end

private enum Outcome {
	Succeeded;
	Failed(error:String);
	Cancelled;
}

@:timeout(2000)
class TestAtomics extends ThreadTestBase {
	#if target.threaded
	function test(async:Async) {
		var a = new AtomicInt(5);
		final thread = Thread.create(() -> {
			while(a.compareExchange(0, 2) != 0) { gcSafePoint(); }
		});
		isTrue(a.compareExchange(5, 0) == 5);

		while (a.compareExchange(2, 2) != 2) { gcSafePoint(); }

		async.done();
	}

	// Enum values are reference types on every target, so an enum with payloads should be
	// usable with AtomicObject like a class instance is.
	function testAtomicObjectEnum(async:Async) {
		final outcome = new AtomicObject<Null<Outcome>>(null);
		final done = new AtomicInt(0);
		final failed = Failed("boom");
		isTrue(outcome.compareExchange(null, failed) == null);
		Thread.create(() -> {
			// The outcome is already set, so this exchange must lose.
			outcome.compareExchange(null, Succeeded);
			done.store(1);
		});
		while (done.load() == 0) { gcSafePoint(); }
		isTrue(outcome.load() == failed);
		switch (outcome.load()) {
			case Failed("boom"):
			case other:
				fail('unexpected outcome $other');
		}
		async.done();
	}

	// Enum constructors without arguments must compare as the same value however they were obtained,
	// including through the reflection API.
	function testAtomicObjectEnumConstructorsWithoutArguments(async:Async) {
		final cases:Array<{label:String, canonical:Outcome, other:Outcome, replacement:Outcome}> = [
			{label: "direct", canonical: Succeeded, other: Succeeded, replacement: Cancelled},
			{label: "createEnum", canonical: Succeeded, other: Type.createEnum(Outcome, "Succeeded"), replacement: Cancelled},
			{label: "createEnum []", canonical: Succeeded, other: Type.createEnum(Outcome, "Succeeded", []), replacement: Cancelled},
			{label: "createEnumIndex", canonical: Cancelled, other: Type.createEnumIndex(Outcome, 2), replacement: Succeeded},
			{label: "createEnumIndex []", canonical: Cancelled, other: Type.createEnumIndex(Outcome, 2, []), replacement: Succeeded},
			{label: "allEnums", canonical: Succeeded, other: Type.allEnums(Outcome)[0], replacement: Cancelled},
			{label: "allEnums 2", canonical: Cancelled, other: Type.allEnums(Outcome)[1], replacement: Succeeded},
		];
		for (c in cases) {
			// Concrete enum type at the call site.
			final a = new AtomicObject<Outcome>(c.canonical);
			a.compareExchange(c.other, c.replacement);
			isTrue(a.load() == c.replacement, 'concrete, stored canonical: ${c.label}');
			final b = new AtomicObject<Outcome>(c.other);
			b.compareExchange(c.canonical, c.replacement);
			isTrue(b.load() == c.replacement, 'concrete, stored other: ${c.label}');
			// Erased type parameter.
			isTrue(swapsGeneric(c.canonical, c.other, c.replacement), 'generic, stored canonical: ${c.label}');
			isTrue(swapsGeneric(c.other, c.canonical, c.replacement), 'generic, stored other: ${c.label}');
		}
		async.done();
	}

	static function swapsGeneric<T:EnumValue>(stored:T, expected:T, replacement:T) {
		final a = new AtomicObject<T>(stored);
		a.compareExchange(expected, replacement);
		return a.load() == replacement;
	}

	// hl/cpp GC must stop all threads: a non-allocating spin loop would never stop and deadlock it.
	static inline function gcSafePoint() {
		#if hl
		hl.Gc.safepoint();
		#elseif cpp
		cpp.vm.Gc.safePoint();
		#end
	}
	#end
}
