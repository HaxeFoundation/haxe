package safety.macro;

import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Compiler;

using haxe.macro.Tools;
using safety.macro.SafeAst;

private typedef SafetyPath = {path:String, recursive:Bool}

class SafeAst {
	static var safeApiList:Array<SafetyPath> = [];
	static var safeNavigationList:Array<SafetyPath> = [];
	static var safeArrayList:Array<SafetyPath> = [];

	/** Indicates if fields of a type under building were changed */
	static var transformed:Bool = false;
	/** Indicates if a type under building is in a path defined by `Safety.safeApi()` */
	static var isInSafeApi:Bool = false;
	/** Indicates if a type under building is in a path defined by `Safety.safeNavigation()` */
	static var isInSafeNavigation:Bool = false;
	/** Indicates if a type under building is in a path defined by `Safety.safeArray()` */
	static var isInSafeArray:Bool = false;

	/**
	 *  `--macro Safety.safeApi()`
	 */
	static public function addSafeApi(path:String, recursive:Bool) {
		safeApiList.push({path:path, recursive:recursive});
		setGlobalBuildMacro();
	}

	/**
	 *  `--macro Safety.safeNavigation()`
	 */
	static public function addSafeNavigation(path:String, recursive:Bool) {
		safeNavigationList.push({path:path, recursive:recursive});
		setGlobalBuildMacro();
	}

	/**
	 *  `--macro Safety.safeArray()`
	 */
	static public function addSafeArray(path:String, recursive:Bool) {
		safeArrayList.push({path:path, recursive:recursive});
		setGlobalBuildMacro();
	}

	/**
	 *  Adds global build macro. Even if this method is invoked multiple times, only one build macro is added
	 */
	static function setGlobalBuildMacro() {
		if(globalMacroAdded) return;
		globalMacroAdded = true;
		Compiler.addGlobalMetadata('', '@:build(safety.macro.SafeAst.build())', true);
	}
	static var globalMacroAdded:Bool = false;

	/**
	 *  Check if a type with specified fully qualified name belongs to a list of paths.
	 */
	static function isInPaths(fqn:String, paths:Array<SafetyPath>):Bool {
		for(item in paths) {
			if(item.path == '') {
				return true;
			}
			if(fqn.indexOf(item.path) == 0) {
				if(
					item.recursive
					|| fqn == item.path
					|| fqn.substr(item.path.length + 1).indexOf('.') < 0 // `+1` because we want to ignore the dot which separates package `item.path` from the module name
				) {
					return true;
				}
			}
		}
		return false;
	}

	macro static public function build():Array<Field> {
		var ref = Context.getLocalClass();
		if(ref == null) {
			return null;
		}

		var typeName = ref.toString();
		isInSafeApi = typeName.isInPaths(safeApiList);
		isInSafeNavigation = typeName.isInPaths(safeNavigationList);
		isInSafeArray = typeName.isInPaths(safeArrayList);
		if(!isInSafeApi && !isInSafeNavigation && !isInSafeArray) {
			return null;
		}

		transformed = false;

		var fields = Context.getBuildFields();
		for(field in fields) {
			var expr:Null<Expr> = switch(field.kind) {
				case FVar(_, e): e;
				case FProp(_, _, _, e): e;
				case FFun(fn):
					if(isInSafeApi && fn.expr != null && field.isPublic()) {
						injectArgumentsChecking(fn, field.pos, typeName, field.name);
					}
					fn.expr;
			}
			if(expr != null) {
				expr.expr = transform(expr).expr;
			}
		}

		return (transformed ? fields : null);
	}

	/**
	 *  Check if `field` has `public` accessor
	 */
	static inline function isPublic(field:Field):Bool {
		return field.access.indexOf(APublic) >= 0;
	}

	static function injectArgumentsChecking(fn:Function, pos:Position, typeName:String, fieldName:String) {
		if(fn.args.length == 0) {
			return;
		}

		var checks = [];
		for(arg in fn.args) {
			if(arg.opt || arg.value != null) continue;
			var argType = (arg.type == null ? macro:safety.macro.Monomorph<0> : arg.type);
			checks.push(macro @:pos(pos) new safety.macro.ArgumentNullCheck<$argType>($i{arg.name}, $v{typeName}, $v{fieldName}, $v{arg.name}));
		}
		if(checks.length == 0) {
			return;
		}

		switch(fn.expr) {
			case null:
			case macro $b{exprs}:
				transformed = true;
				fn.expr = macro @:pos(fn.expr.pos) $b{checks.concat(exprs)};
			case expr:
				transformed = true;
				fn.expr = macro @:pos(fn.expr.pos) {
					$b{checks};
					$expr;
				}
		}
	}

	/**
	 *  Check if `expr` contains a `=>` binop
	 */
	static function containsArrayOp(expr:Null<Expr>):Bool {
		var contains = false;
		function traverse(e:Null<Expr>) {
			if(contains) return;
			switch(e) {
				case null:
				case macro $_ => $_: contains = true;
				case _: e.iter(traverse);
			}
		}
		traverse(expr);
		return contains;
	}

	static function transform(e:Expr):Expr {

		if(isInSafeNavigation) {
			if(Safety.isDisplay()) switch(e) {
				case macro $target!:
					transformed = true;
					return target.map(transform);
				case _:
			} else switch(e) {
				case macro $target!.$field($a{args}):
					transformed = true;
					e = macro @:pos(e.pos) {
						var _v_ = $target;
						_v_ == null ? null : @:nullSafety(false) _v_.$field($a{args});
					};
					return e.map(transform);
				case macro $target!.$field[$index]:
					transformed = true;
					e = macro @:pos(e.pos) {
						var _v_ = $target;
						_v_ == null ? null : @:nullSafety(false) _v_.$field[$index];
					};
					return e.map(transform);
				case macro $target!.$field:
					transformed = true;
					e = macro @:pos(e.pos) {
						var _v_ = $target;
						_v_ == null ? null : _v_.$field;
					};
					return e.map(transform);
				case _:
			}
		}

		if(isInSafeArray) switch(e) {
			//don't touch patterns in `case`
			case macro ${{expr:ESwitch(target, cases, defaultCase)}}:
				target = switch(target) {
					case macro [$a{exprs}]:
						macro @:pos(target.pos) [$a{exprs.map(transform)}];
					case macro ([$a{exprs}]):
						macro @:pos(target.pos) ([$a{exprs.map(transform)}]);
					case _:
						transform(target);
				}
				for(c in cases) {
					if(c.guard != null) c.guard = transform(c.guard);
					if(c.expr != null) c.expr = transform(c.expr);
				}
				if(defaultCase != null && defaultCase.expr != null) {
					defaultCase = transform(defaultCase);
				}
				return {expr:ESwitch(target, cases, defaultCase), pos:e.pos};
			//don't touch array declaration if it's actually a map declaration
			case (macro [$a{exprs}]) if(exprs.length > 0 && exprs[0].containsArrayOp()):
				exprs = exprs.map(transform);
				return macro @:pos(e.pos) [$a{exprs}];
			//don't touch array declaration if a user is casting it manually
			case macro ([$a{exprs}]:$type):
				exprs = exprs.map(transform);
				return macro @:pos(e.pos) ([$a{exprs}]:$type);
			//otherwise transform to ([...]:SafeArray<T>);
			case macro [$a{exprs}]:
				transformed = true;
				exprs = exprs.map(transform);
				return macro @:pos(e.pos) ([$a{exprs}]:SafeArray<safety.macro.Monomorph<0>>);
			case _:
		}

		return e.map(transform);
	}
}