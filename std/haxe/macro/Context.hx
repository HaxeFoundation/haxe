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

package haxe.macro;

import haxe.macro.Expr;
import haxe.macro.Type.TypedExpr;

enum Message {
	Info(msg:String, pos:Position);
	Warning(msg:String, pos:Position);
}

/**
	Context provides an API for macro programming.

	It contains common functions that interact with the macro interpreter to
	query or set information. Other API functions are available in the tools
	classes:

	- `haxe.macro.ComplexTypeTools`
	- `haxe.macro.ExprTools`
	- `haxe.macro.TypeTools`
**/
class Context {
	#if (neko || eval || display)
	/**
		Displays a compilation error `msg` at the given `Position` `pos`
		and aborts the current macro call.
	**/
	public static function error(msg:String, pos:Position, ?depth:Int = 0):Dynamic {
		return load("error", 2)(msg, pos, depth);
	}

	/**
		Displays a compilation error `msg` at the given `Position` `pos`
		and aborts the compilation.
	**/
	public static function fatalError(msg:String, pos:Position, ?depth:Int = 0):Dynamic {
		return load("fatal_error", 2)(msg, pos, depth);
	}

	/**
		Displays a compilation error `msg` at the given `Position` `pos`
		without aborting the current macro call.
	**/
	public static function reportError(msg:String, pos:Position, ?depth:Int = 0):Void {
		load("report_error", 2)(msg, pos, depth);
	}

	/**
		Displays a compilation warning `msg` at the given `Position` `pos`.
	**/
	public static function warning(msg:String, pos:Position, ?depth:Int = 0) {
		load("warning", 2)(msg, pos, depth);
	}

	/**
		Displays a compilation info `msg` at the given `Position` `pos`.
	**/
	public static function info(msg:String, pos:Position, ?depth:Int = 0) {
		load("info", 2)(msg, pos, depth);
	}

	/**
		Gets a list of all current compilation info/warning messages.
	**/
	public static function getMessages():Array<Message> {
		return load("get_messages", 0)();
	}

	/**
		Filters all current info/warning messages. Filtered out messages will
		not be displayed by the compiler.
	**/
	public static function filterMessages(predicate:Message->Bool) {
		load("filter_messages", 1)(predicate);
	}

	/**
		Check if compiler is past initializations macros or not.
		When it is, configuration phase is over and parsing/typing can start.
	**/
	public static function initMacrosDone():Bool {
		return load("init_macros_done", 0)();
	}

	/**
		Resolves a file name `file` based on the current class paths.

		The resolution follows the usual class path rules where the last
		declared class path has priority.

		If a class path was declared relative, this method returns the relative
		file path. Otherwise it returns the absolute file path.

		If no type can be found, an exception of type `String` is thrown.
	**/
	public static function resolvePath(file:String):String {
		return load("resolve_path", 1)(file);
	}

	/**
		Returns an `Array` of current class paths in the order of their
		declaration.

		Modifying the returned array has no effect on the compiler. Class paths
		can be added using `haxe.macro.Compiler.addClassPath`.
	**/
	public static function getClassPath():Array<String> {
		return load("class_path", 0)();
	}

	/**
		Check if current display position is within `pos`.
	**/
	public static function containsDisplayPosition(pos:Position):Bool {
		return load("contains_display_position", 1)(pos);
	}

	public static function getDisplayMode():DisplayMode {
		return load("get_display_mode", 0)();
	}

	/**
		Returns the position at which the macro was called.
	**/
	public static function currentPos():Position {
		return load("current_pos", 0)();
	}

	/**
		Get the call stack (excluding the call to `Context.getMacroStack()`
		that led to current macro.
	**/
	public static function getMacroStack():Array<Position> {
		return load("get_macro_stack", 0)();
	}

	/**
		Returns the type which is expected at the place the macro is called.

		This affects usages such as `var x:Int = macroCall()`, where the
		expected type will be reported as `Int`.

		Might return `null` if no specific type is expected or if the calling
		macro is not an expression-macro.
	**/
	public static function getExpectedType():Null<Type> {
		assertInitMacrosDone(false);
		return load("get_expected_type", 0)();
	}

	/**
		Returns the call arguments that lead to the invocation of the current
		`@:genericBuild` macro, if available.

		Returns `null` if the current macro is not a `@:genericBuild` macro.
	**/
	public static function getCallArguments():Null<Array<Expr>> {
		assertInitMacrosDone(false);
		return load("get_call_arguments", 0)();
	}

	/**
		Returns the current class in which the macro was called.

		If no such class exists, `null` is returned.
	**/
	public static function getLocalClass():Null<Type.Ref<Type.ClassType>> {
		assertInitMacrosDone(false);
		var l:Type = load("get_local_type", 0)();
		if (l == null)
			return null;
		return switch (l) {
			case TInst(c, _): c;
			default: null;
		}
	}

	/**
		Returns the current module path in/on which the macro was called.
	**/
	public static function getLocalModule():String {
		assertInitMacrosDone(false);
		return load("get_local_module", 0)();
	}

	/**
		Returns the current type in/on which the macro was called.

		If no such type exists, `null` is returned.
	**/
	public static function getLocalType():Null<Type> {
		assertInitMacrosDone(false);
		return load("get_local_type", 0)();
	}

	/**
		Returns the name of the method from which the macro was called.

		If no such method exists, `null` is returned.
	**/
	public static function getLocalMethod():Null<String> {
		assertInitMacrosDone(false);
		return load("get_local_method", 0)();
	}

	/**
		Returns an `Array` of classes which are available for `using` usage in
		the context the macro was called.

		Modifying the returned array has no effect on the compiler.
	**/
	public static function getLocalUsing():Array<Type.Ref<Type.ClassType>> {
		assertInitMacrosDone(false);
		return load("get_local_using", 0)();
	}

	/**
		Returns an `Array` of all imports in the context the macro was called.

		Modifying the returned array has no effect on the compiler.
	**/
	public static function getLocalImports():Array<ImportExpr> {
		assertInitMacrosDone(false);
		return load("get_local_imports", 0)();
	}

	/**
		Returns a map of local variables accessible in the context the macro was
		called.

		The keys of the returned map are the variable names, the values are
		their types.

		Modifying the returned map has no effect on the compiler.
	**/
	@:deprecated("Use Context.getLocalTVars() instead")
	public static function getLocalVars():Map<String, Type> {
		assertInitMacrosDone(false);
		return load("local_vars", 1)(false);
	}

	/**
		Similar to `getLocalVars`, but returns elements of type `TVar` instead
		of `Type`.
	**/
	public static function getLocalTVars():Map<String, Type.TVar> {
		assertInitMacrosDone(false);
		return load("local_vars", 1)(true);
	}

	/**
		Tells if the conditional compilation flag `s` has been set.

		Compiler flags are set using the `-D` command line parameter, or
		by calling `haxe.macro.Compiler.define`.

		@see https://haxe.org/manual/lf-condition-compilation.html
	**/
	public static function defined(s:String):Bool {
		return load("defined", 1)(s);
	}

	/**
		Returns the value defined for the conditional compilation flag `key`.

		If no value is defined for `key`, `null` is returned.

		Compiler flags values are set using the `-D key=value` command line
		parameter, or by calling `haxe.macro.Compiler.define`.

		The default value is `"1"`.

		@see https://haxe.org/manual/lf-condition-compilation.html
	**/
	public static function definedValue(key:String):Null<String> {
		return load("defined_value", 1)(key);
	}

	/**
		Returns a map of all conditional compilation flags that have been set.

		Compiler flags are set using the `-D` command line parameter, or
		by calling `haxe.macro.Compiler.define`.

		Modifying the returned map has no effect on the compiler.

		@see https://haxe.org/manual/lf-condition-compilation.html
	**/
	public static function getDefines():Map<String, String> {
		return load("get_defines", 0)();
	}

	/**
		Resolves a type identified by `name`.

		The resolution follows the usual class path rules where the last
		declared class path has priority.

		If no type can be found, an exception of type `String` is thrown.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function getType(name:String):Type {
		assertInitMacrosDone();
		return load("get_type", 1)(name);
	}

	/**
		Resolves a module identified by `name` and returns an `Array` of all
		its contained types.

		The resolution follows the usual class path rules where the last
		declared class path has priority.

		If no module can be found, an exception of type `String` is thrown.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function getModule(name:String):Array<Type> {
		assertInitMacrosDone();
		return load("get_module", 1)(name);
	}

	/**
		Returns the typed expression of the call to the main function.

		This function will only work in the generation phase. Any calls
		made outside a function passed to `haxe.macro.Context.onGenerate`
		or `haxe.macro.Context.onAfterGenerate` will return `null`.
	**/
	public static function getMainExpr():Null<TypedExpr> {
		return load("get_main_expr", 0)();
	}

	/**
		Returns an array of module types to be generated in the output.

		This list may change depending on the phase of compilation and
		should not be treated as conclusive until the generation phase.

		Modifying the returned array has no effect on the compilation.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function getAllModuleTypes():Array<haxe.macro.Type.ModuleType> {
		assertInitMacrosDone();
		return load("get_module_types", 0)();
	}

	/**
		Parses `expr` as Haxe code, returning the corresponding AST.

		String interpolation of single quote strings within `expr` is not
		supported.

		The provided `Position` `pos` is used for all generated inner AST nodes.
	**/
	public static function parse(expr:String, pos:Position):Expr {
		return load("do_parse", 3)(expr, pos, false);
	}

	/**
		Similar to `parse`, but error positions are reported within the provided
		String `expr`.
	**/
	public static function parseInlineString(expr:String, pos:Position):Expr {
		return load("do_parse", 3)(expr, pos, true);
	}

	/**
		Builds an expression from `v`.

		This method generates AST nodes depending on the macro-runtime value of
		`v`. As such, only basic types and enums are supported and the behavior
		for other types is undefined.

		The provided `Position` `pos` is used for all generated inner AST nodes.
	**/
	public static function makeExpr(v:Dynamic, pos:Position):Expr {
		return load("make_expr", 2)(v, pos);
	}

	/**
		Returns a hashed MD5 signature of value `v`.
	**/
	public static function signature(v:Dynamic):String {
		assertInitMacrosDone(false);
		return load("signature", 1)(v);
	}

	/**
		Adds a callback function `callback` which is invoked after the
		compiler's typing phase, just before its generation phase.

		The callback receives an `Array` containing all types which are about
		to be generated. Modifications are limited to metadata, it is mainly
		intended to obtain information.

		By default, the callback is made before types are stored in the compilation
		server, if active. This means that any effect persists for the next compilation.
		If `persistent` is set to `false`, changes to types made by the callback only
		affect the current compilation. If no compilation server is used, this flag has
		no effect.

		*Note*: the callback is still invoked when generation is disabled with  `--no-output`.
	**/
	public static function onGenerate(callback:Array<Type>->Void, persistent:Bool = true) {
		load("on_generate", 2)(callback, persistent);
	}

	/**
		Adds a callback function `callback` which is invoked after the compiler
		generation phase.

		Compilation has completed at this point and cannot be influenced
		anymore. However, contextual information is still available.

		*Note*: the callback is still invoked when generation is disabled with  `--no-output`.
	**/
	public static function onAfterGenerate(callback:Void->Void) {
		load("on_after_generate", 1)(callback);
	}

	/**
		Adds a callback function `callback` which is invoked after the compiler
		is done typing, but before optimization. The callback receives the types
		which have been typed.

		It is possible to define new types in the callback, in which case it
		will be called again with the new types as argument.
	**/
	public static function onAfterTyping(callback:Array<haxe.macro.Type.ModuleType>->Void) {
		load("on_after_typing", 1)(callback);
	}

	/**
		Adds a callback function `callback` which is invoked after the compiler
		is done running initialization macros, when typing begins.

		`onAfterInitMacros` should be used to delay typer-dependant code from
		your initalization macros, to properly separate configuration phase and
		actual typing.
	**/
	public static function onAfterInitMacros(callback:Void->Void):Void {
		if (Context.initMacrosDone()) {
			callback();
		} else {
			load("on_after_init_macros", 1)(callback);
		}
	}

	/**
		Adds a callback function `callback` which is invoked when a type name
		cannot be resolved.

		The callback may return a type definition, which is then used for the
		expected type. If it returns `null`, the type is considered to still not
		exist.
	**/
	public static function onTypeNotFound(callback:String->TypeDefinition) {
		load("on_type_not_found", 1)(callback);
	}

	/**
		Types expression `e` and returns its type.

		Typing the expression may result in a compiler error which can be
		caught using `try ... catch`.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function typeof(e:Expr):Type {
		assertInitMacrosDone();
		return load("typeof", 1)(e);
	}

	/**
		Types expression `e` and returns the corresponding `TypedExpr`.

		Typing the expression may result in a compiler error which can be
		caught using `try ... catch`. Note that not all compiler errors can
		be caught this way because the compiler might delay various checks
		to a later stage, at which point the exception handler is no longer
		active.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function typeExpr(e:Expr):TypedExpr {
		assertInitMacrosDone();
		return load("type_expr", 1)(e);
	}

	/**
		Resolve type `t` and returns the corresponding `Type`.

		Resolving the type may result in a compiler error which can be
		caught using `try ... catch`.
		Resolution is performed based on the current context in which the macro is called.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function resolveType(t:ComplexType, p:Position):Type {
		assertInitMacrosDone();
		return load("resolve_type", 2)(t, p);
	}

	/**
		Resolve type `t` and returns the corresponding `ComplexType`.

		Resolving the type may result in a compiler error which can be
		caught using `try ... catch`.
		Resolution is performed based on the current context in which the macro is called.
		The difference with `resolveType` is that it only performs type resolution, it does not
		build any type or trigger macros.
	**/
	public static function resolveComplexType(t:ComplexType, p:Position):ComplexType {
		assertInitMacrosDone(false);
		return load("resolve_complex_type", 2)(t, p);
	}

	/**
		Returns the `ComplexType` corresponding to the given `Type` `t`.

		See `haxe.macro.TypeTools.toComplexType` for details.
	**/
	public static function toComplexType(t:Type):Null<ComplexType> {
		return load("to_complex_type", 1)(t);
	}

	/**
		Tries to unify `t1` and `t2` and returns `true` if successful.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function unify(t1:Type, t2:Type):Bool {
		assertInitMacrosDone();
		return load("unify", 2)(t1, t2);
	}

	/**
		Follows a type.

		See `haxe.macro.TypeTools.follow` for details.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function follow(t:Type, ?once:Bool):Type {
		assertInitMacrosDone();
		return load("follow", 2)(t, once);
	}

	/**
		Follows a type, including abstracts' underlying implementation

		See `haxe.macro.TypeTools.followWithAbstracts` for details.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function followWithAbstracts(t:Type, once:Bool = false):Type {
		assertInitMacrosDone();
		return load("follow_with_abstracts", 2)(t, once);
	}

	/**
		Returns the information stored in `Position` `p`.
	**/
	public static function getPosInfos(p:Position):{min:Int, max:Int, file:String} {
		return load("get_pos_infos", 1)(p);
	}

	/**
		Builds a `Position` from `inf`.
	**/
	public static function makePosition(inf:{min:Int, max:Int, file:String}):Position {
		return load("make_position", 3)(inf.min, inf.max, inf.file);
	}

	/**
		Returns a map of all registered resources for this compilation unit.

		Modifying the returned map has no effect on the compilation, use
		`haxe.macro.Context.addResource` to add new resources to the compilation unit.
	**/
	public static function getResources():Map<String, haxe.io.Bytes> {
		return load("get_resources", 0)();
	}

	/**
		Makes resource `data` available as `name`.

		The resource is then available using the `haxe.macro.Resource` API.

		If a previous resource was bound to `name`, it is overwritten.

		Compilation server : when using the compilation server, the resource is bound
		to the Haxe module which calls the macro, so it will be included again if
		that module is reused. If this resource concerns several modules, prefix its
		name with a `$` sign, this will bind it to the macro module instead.
	**/
	public static function addResource(name:String, data:haxe.io.Bytes) {
		load("add_resource", 2)(name, data);
	}

	/**
		Returns an `Array` of fields of the class which is to be built.

		This is only defined for `@:build/@:autoBuild` macros.
	**/
	public static function getBuildFields():Array<Field> {
		assertInitMacrosDone(false);
		return load("get_build_fields", 0)();
	}

	/**
		Defines a new type from `TypeDefinition` `t`.

		If `moduleDependency` is given and is not `null`, it should contain
		a module path that will be used as a dependency for the newly defined module
		instead of the current module.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function defineType(t:TypeDefinition, ?moduleDependency:String):Void {
		assertInitMacrosDone();
		load("define_type", 2)(t, moduleDependency);
	}

	/**
		Creates and returns a new instance of monomorph (`TMono`) type.

		Returned monomorph can be used with e.g. `Context.unify` to make the compiler
		bind the monomorph to an actual type and let macro further process the resulting type.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function makeMonomorph():Type {
		assertInitMacrosDone();
		return load("make_monomorph", 0)();
	}

	/**
		Defines a new module as `modulePath` with several `TypeDefinition`
		`types`. This is analogous to defining a .hx file.

		The individual `types` can reference each other and any identifier
		respects the `imports` and `usings` as usual, expect that imports are
		not allowed to have `.*` wildcards or `as s` shorthands.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function defineModule(modulePath:String, types:Array<TypeDefinition>, ?imports:Array<ImportExpr>, ?usings:Array<TypePath>):Void {
		if (imports == null)
			imports = [];
		if (usings == null)
			usings = [];
		assertInitMacrosDone();
		load("define_module", 4)(modulePath, types, imports, usings);
	}

	/**
		Returns a syntax-level expression corresponding to typed expression `t`.

		This process may lose some information.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function getTypedExpr(t:Type.TypedExpr):Expr {
		assertInitMacrosDone();
		return load("get_typed_expr", 1)(t);
	}

	/**
		Store typed expression `t` internally and give a syntax-level expression
		that can be returned from a macro and will be replaced by the stored
		typed expression.

		If `t` is `null` or invalid, an exception is thrown.

		NOTE: the returned value references an internally stored typed expression
		that is reset between compilations, so care should be taken when storing
		the expression returned by this method in a static variable and using the
		compilation server.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function storeTypedExpr(t:Type.TypedExpr):Expr {
		assertInitMacrosDone();
		return load("store_typed_expr", 1)(t);
	}

	/**
		Types expression `e`, stores the resulting typed expression internally and
		returns a syntax-level expression that can be returned from a macro and
		will be replaced by the stored typed expression.

		If `e` is `null` or invalid, an exception is thrown.

		A call to `storeExpr(e)` is equivalent to `storeTypedExpr(typeExpr(e))` without
		the overhead of encoding and decoding between regular and macro runtime.

		NOTE: the returned value references an internally stored typed expression
		that is reset between compilations, so care should be taken when storing
		the expression returned by this method in a static variable and using the
		compilation server.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function storeExpr(e:Expr):Expr {
		assertInitMacrosDone();
		return load("store_expr", 1)(e);
	}

	/**
		This function works like `storeExpr`, but also returns access to the expression's
		type through the `type` field of the return value.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function typeAndStoreExpr(e:Expr):{final type:Type.Ref<Type>; final expr:Expr;} {
		assertInitMacrosDone();
		return load("type_and_store_expr", 1)(e);
	}

	/**
		Manually adds a dependency between module `modulePath` and an external
		file `externFile`.

		This affects the compilation cache, causing the module to be typed if
		`externFile` has changed.

		Has no effect if the compilation cache is not used.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function registerModuleDependency(modulePath:String, externFile:String) {
		onAfterInitMacros(() -> load("register_module_dependency", 2)(modulePath, externFile));
	}

	/**
		Creates a timer which will be printed in the compilation report
		if `--times` compilation argument is set.

		Note that a timer may be omitted from the report if the amount of time
		measured is too small.

		This method immediately starts a timer and returns a function to stop it:
		```
		var stopTimer = haxe.macro.Context.timer("my heavy task");
		runTask();
		stopTimer();
		```
	**/
	public static function timer(id:String):() -> Void {
		return load("timer", 1)(id);
	}

	/**
		Executes `code` in a context that has `imports` and `usings` added.

		This is equivalent to temporarily having `import` and `using` statements in a file. These
		are only active during the execution of `code` and do not affect anything afterwards. This
		is true even if `code` throws an exception.

		If any argument is `null`, the result is unspecified.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function withImports<X>(imports:Array<String>, usings:Array<String>, code:() -> X):X {
		assertInitMacrosDone();
		return load("with_imports", 3)(imports, usings, code);
	}


	/**
		Executes `code` in a context that has some compiler options set, restore the compiler to its
		default behavior afterwards.

		`allowInlining`: enable or disable inlining during typing with `typeExpr`.

		`allowTransform`: when disabled, the code typed with `typeExpr` will be almost exactly the same
		as the input code. This will disable some abstract types transformations.

		Usage of this function from initialization macros is deprecated and may
		cause compilation server issues. Use `Context.onAfterInitMacros` to
		run your code once typer is ready to be used.
	**/
	public static function withOptions<X>(options:{?allowInlining:Bool,?allowTransform:Bool}, code : () -> X) : X {
		assertInitMacrosDone();
		return load("with_options", 2)(options, code);
	}

	@:deprecated
	public static function registerModuleReuseCall(modulePath:String, macroCall:String) {
		throw "This method is no longer supported. See https://github.com/HaxeFoundation/haxe/issues/5746";
	}

	@:deprecated
	public static function onMacroContextReused(callb:Void->Bool) {
		throw "This method is no longer supported. See https://github.com/HaxeFoundation/haxe/issues/5746";
	}

	@:allow(haxe.macro.TypeTools)
	@:allow(haxe.macro.MacroStringTools)
	@:allow(haxe.macro.TypedExprTools)
	@:allow(haxe.macro.PositionTools)
	static function load(f:String, nargs:Int):Dynamic {
		#if neko
		return neko.Lib.load("macro", f, nargs);
		#elseif eval
		return eval.vm.Context.callMacroApi(f);
		#else
		return Reflect.makeVarArgs(function(_) return throw "Can't be called outside of macro");
		#end
	}

	private static function includeFile(file:String, position:String) {
		load("include_file", 2)(file, position);
	}

	private static function sExpr(e:TypedExpr, pretty:Bool):String {
		return load("s_expr", 2)(e, pretty);
	}

	@:allow(haxe.macro.Compiler)
	private static function assertInitMacro():Void {
		if (initMacrosDone()) {
			var stack = getMacroStack();

			warning(
				"This API should only be used from initialization macros.",
				if (stack.length > 2) stack[2] else currentPos()
			);
		}
	}

	@:allow(haxe.macro.Compiler)
	private static function assertInitMacrosDone(includeSuggestion = true):Void {
		if (!initMacrosDone()) {
			var stack = getMacroStack();
			var suggestion = includeSuggestion
				? "\nUse `Context.onAfterInitMacros` to register a callback to run when context is ready."
				: "";

			fatalError(
				"Cannot use this API from initialization macros." + suggestion,
				if (stack.length > 2) stack[2] else currentPos()
			);
		}
	}
	#end
}
