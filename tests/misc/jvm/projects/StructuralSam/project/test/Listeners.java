package test;

// None of these interfaces carry @FunctionalInterface. javac still permits
// lambda conversion to them (the annotation is documentation, not a gate);
// Haxe's structural SAM detection in javaModern.ml should match that.

public class Listeners {
    public interface OnClick {
        void onClick(int id);
    }

    // Inherits Object.toString (not abstract here), declares one abstract.
    public interface WithToString {
        String describe(int value);
    }

    // Abstract equals re-declaration must NOT count as an extra abstract method
    // (matches the JLS §9.8 exclusion of Object members).
    public interface AbstractEqualsPlusOne {
        @Override
        boolean equals(Object other);

        int compute(int a);
    }

    // Default + static + private methods do not count as abstract; only
    // `transform` should remain.
    public interface WithDefaults {
        int transform(int x);

        default int doubled(int x) { return transform(x) * 2; }
        static int identity(int x) { return x; }
    }

    // Two abstract methods — NOT a SAM. Lambda assignment must remain rejected.
    public interface NotSam {
        void first();
        void second();
    }

    // Single-arg method that returns a value — exercises the non-void SAM path
    // and overload disambiguation against an Object-parametered overload.
    public interface StringMaker {
        String make(int n);
    }

    // Structurally a SAM, identical in shape to OnClick — but Main never
    // converts a function to it. A closure must therefore NOT implement it:
    // the JVM generator only binds interfaces that are actually used as a
    // conversion target somewhere in the program.
    public interface Unused {
        void onUnused(int id);
    }

    // SAM exercised ONLY in argument position from Haxe — no typed local, no
    // field signature, no explicit cast. The genjvm AST scan can't see this
    // case: AbstractCast's SAM branch leaves the closure expression with its
    // original TFun type (no TCast wrapper), so the only place ArgOnly's
    // TInst exists is the extern callee's parameter signature, which the
    // scan deliberately skips. Without AbstractCast's writeback to
    // functional_interfaces_used, the emitted closure does not implement
    // ArgOnly and the call ClassCastExceptions at runtime.
    public interface ArgOnly {
        void onArg(int n);
    }

    public static String runArgOnly(ArgOnly cb, int n) {
        cb.onArg(n);
        return "arg-ok";
    }

    // Constructor-position SAM: a Haxe `new CtorSam(closure)` is a TNew
    // expression in the typed AST. Unlike TCall (where the callee field has a
    // TFun etype that exposes the parameter types to the scan), TNew has no
    // callee subexpression — the only place CtorOnly's TInst would appear is
    // on this extern's cl_constructor.cf_type, which the AST scan never
    // visits. This is the exact shape that crashed RideAssist:
    // `new TextToSpeech(this, onTtsInit)`.
    public interface CtorOnly {
        void onCtor(int n);
    }

    public static class CtorSam {
        public CtorSam(CtorOnly cb, int n) {
            cb.onCtor(n);
        }
    }

    public static String runOnClick(OnClick cb, int id) {
        cb.onClick(id);
        return "ok";
    }

    public static String runDescribe(WithToString cb, int v) {
        return cb.describe(v);
    }

    public static int runCompute(AbstractEqualsPlusOne cb, int v) {
        return cb.compute(v);
    }

    public static int runTransform(WithDefaults cb, int v) {
        return cb.transform(v);
    }

    public static String runMaker(StringMaker cb, int v) {
        return cb.make(v);
    }

    // Overloaded call sites — disjoint SAM signatures so resolution is
    // unambiguous (lambda arg arity differs). Exercises SAM-aware overload
    // candidate filtering.
    @FunctionalInterface
    public interface UnaryStringFn {
        String apply(String s);
    }

    public static String overloaded(OnClick cb, int id) {
        cb.onClick(id);
        return "click";
    }

    public static String overloaded(UnaryStringFn cb, String s) {
        return cb.apply(s);
    }
}
