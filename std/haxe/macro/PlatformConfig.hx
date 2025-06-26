package haxe.macro;

import haxe.macro.Expr;

/**
	Represents the internal structure generated with options assigned based on
	the target platform.

	Warning: `PlatformConfig` and the typedefs unique to its fields correspond to
	compiler-internal data structures and might change in minor Haxe releases in
	order to adapt to internal changes.
**/
typedef PlatformConfig = {
	/**
		Has a static type system, with not-nullable basic types (Int/Float/Bool)
	**/
	final staticTypeSystem:Bool;

	/**
		Has access to the "sys" package
	**/
	final sys:Bool;

	/**
		Captured variables handling (see before)
	**/
	final capturePolicy:CapturePolicy;

	/**
		When calling a method with optional args, do we replace the missing args with "null" constants
	**/
	final padNulls:Bool;

	/**
		Add a final return to methods not having one already - prevent some compiler warnings
	**/
	final addFinalReturn:Bool;

	/**
		Does the platform natively support overloaded functions
	**/
	final overloadFunctions:Bool;

	/**
		Type paths that are reserved on the platform
	**/
	final reservedTypePaths:Array<TypePath>;

	/**
		Supports function == function
	**/
	final supportsFunctionEquality:Bool;

	/**
		Uses utf16 encoding with ucs2 api
	**/
	final usesUtf16:Bool;

	/**
		Target supports accessing `this` before calling `super(...)`
	**/
	final thisBeforeSuper:Bool;

	/**
		Target supports threads
	**/
	final supportsThreads:Bool;

	/**
		Target supports Unicode
	**/
	final supportsUnicode:Bool;

	/**
		Target supports rest arguments
	**/
	final supportsRestArgs:Bool;

	/**
		Exceptions handling config
	**/
	final exceptions:ExceptionsConfig;

	/**
		The scoping of local variables
	**/
	final scoping:VarScopingConfig;

	/**
		Target supports atomic operations via haxe.Atomic
	**/
	final supportsAtomics:Bool;

}

enum CapturePolicy {
	/**
		Do nothing, let the platform handle it
	**/
	None;

	/**
		Wrap all captured variables into a single-element array to allow modifications
	**/
	WrapRef;

	/**
		Similar to wrap ref, but will only apply to the locals that are declared in loops
	**/
	LoopVars;
}

typedef VarScopingConfig = {
	final scope:VarScope;
	final flags:Array<VarScopingFlags>;
}

enum VarScope {
	FunctionScope;
	BlockScope;
}

enum VarScopingFlags {
	/**
		Variables are hoisted in their scope
	**/
	VarHoisting;

	/**
		It's not allowed to shadow existing variables in a scope.
	**/
	NoShadowing;

	/**
		It's not allowed to shadow a `catch` variable.
	**/
	NoCatchVarShadowing;

	/**
		Local vars cannot have the same name as the current top-level package or
		(if in the root package) current class name
	**/
	ReserveCurrentTopLevelSymbol;

	/**
		Local vars cannot have a name used for any top-level symbol
		(packages and classes in the root package)
	**/
	ReserveAllTopLevelSymbols;

	/**
		Reserve all type-paths converted to "flat path" with `Path.flat_path`
	**/
	ReserveAllTypesFlat;

	/**
		List of names cannot be taken by local vars
	**/
	ReserveNames(names:Array<String>);

	/**
		Cases in a `switch` won't have blocks, but will share the same outer scope.
	**/
	SwitchCasesNoBlocks;
}

typedef ExceptionsConfig = {
	/**
		Base types which may be thrown from Haxe code without wrapping.
	**/
	final nativeThrows:Array<TypePath>;

	/**
		Base types which may be caught from Haxe code without wrapping.
	**/
	final nativeCatches:Array<TypePath>;

	/**
		Hint exceptions filter to avoid wrapping for targets, which can throw/catch any type
		Ignored on targets with a specific native base type for exceptions.
	**/
	final avoidWrapping:Bool;

	/**
		Path of a native class or interface, which can be used for wildcard catches.
	**/
	final wildcardCatch:TypePath;

	/**
		Path of a native base class or interface, which can be thrown.
		This type is used to cast `haxe.Exception.thrown(v)` calls to.
		For example `throw 123` is compiled to `throw (cast Exception.thrown(123):ec_base_throw)`
	**/
	final baseThrow:TypePath;

	/**
		Checks if throwing this expression is a special case for current target
		and should not be modified.
	**/
	// final specialThrow:(TypedExpr)->Bool;
}
