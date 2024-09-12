package php;

import haxe.extern.*;
import haxe.Constraints;

/**
	This class contains externs for native PHP functions defined in global namespace.
	For native PHP constants in global namespace see `php.Const`.
**/
@:phpGlobal
extern class Global {
	/**
		@see http://php.net/manual/en/function.get-browser.php
	**/
	static function get_browser(?user_agent:String, ?return_array:Bool = false):EitherType<Dynamic, EitherType<NativeArray, Bool>>;

	/**
		@see http://php.net/manual/en/function.get-defined-constants.php
	**/
	static function get_defined_constants(categorize:Bool = false):NativeArray;
	
	/**
		@see http://php.net/manual/en/function.finfo-file.php
	**/
	static function finfo_file(finfo:Finfo, filename:String, flags:Int = 0, ?context:Resource):EitherType<String, Bool>;
	
	/**
		@see http://php.net/manual/en/function.finfo-open.php
	**/
	static function finfo_open(flags:Int = 0, ?magic_database:String):EitherType<Finfo, Bool>;
	
	/**
		@see http://php.net/manual/en/function.finfo-close.php
	**/
	static function finfo_close(finfo:Finfo):Bool;
	
	/**
		@see http://php.net/manual/en/function.fastcgi-finish-request.php
	**/
	static function fastcgi_finish_request():Bool;
	
	/**
		@see http://php.net/manual/en/function.exit.php
	**/
	static function exit(status:EitherType<String, Int>):Void;

	/**
		@see http://php.net/manual/en/function.exit.php
	**/
	static function die(status:EitherType<String, Int>):Void;

	/**
		@see http://php.net/manual/en/function.error-log.php
	**/
	static function error_log(message:String, message_type:Int = 0, ?destination:String, ?extra_headers:String):Bool;

	/**
		@see http://php.net/manual/en/function.error-reporting.php
	**/
	static function error_reporting(?level:Int):Int;

	/**
		@see http://php.net/manual/en/function.set-error-handler.php
	**/
	@:overload(function(error_handler:Int->String->Bool, ?error_types:Int):Dynamic {})
	@:overload(function(error_handler:Int->String->String->Bool, ?error_types:Int):Dynamic {})
	@:overload(function(error_handler:Int->String->String->Int->Bool, ?error_types:Int):Dynamic {})
	static function set_error_handler(?error_handler:Int->String->String->Int->Array<Dynamic>->Bool, ?error_types:Int):Dynamic;

	/**
		@see http://php.net/manual/en/function.restore-error-handler.php
	**/
	static function restore_error_handler():Bool;

	/**
		@see http://php.net/manual/en/function.set-exception-handler.php
	**/
	static function set_exception_handler(exception_handler:Throwable->Void):Dynamic;

	/**
		@see http://php.net/manual/en/function.restore-exception-handler.php
	**/
	static function restore_exception_handler():Bool;

	/**
		@see http://php.net/manual/en/function.is-int.php
	**/
	static function is_int(value:Dynamic):Bool;

	/**
		@see http://php.net/manual/en/function.is-float.php
	**/
	static function is_float(value:Dynamic):Bool;

	/**
		@see http://php.net/manual/en/function.is-string.php
	**/
	static function is_string(value:Dynamic):Bool;

	/**
		@see http://php.net/manual/en/function.is-numeric.php
	**/
	static function is_numeric(value:Dynamic):Bool;

	/**
		@see http://php.net/manual/en/function.is-bool.php
	**/
	static function is_bool(value:Dynamic):Bool;

	/**
		Checks if `values` is `php.NativeArray`
		@see http://php.net/manual/en/function.is-array.php
	**/
	static function is_array(value:Dynamic):Bool;

	/**
		@see http://php.net/manual/en/function.is-object.php
	**/
	static function is_object(value:Dynamic):Bool;

	/**
		@see http://php.net/manual/en/function.is-null.php
	**/
	static function is_null(value:Dynamic):Bool;

	/**
		@see http://php.net/manual/en/function.is-subclass-of.php
	**/
	static function is_subclass_of(value:Dynamic, className:String, allow_string:Bool = true):Bool;

	/**
		@see http://php.net/manual/en/function.class-exists.php
	**/
	static function class_exists(class_name:String, autoload:Bool = true):Bool;

	/**
		@see http://php.net/manual/en/function.interface-exists.php
	**/
	static function interface_exists(interface_name:String, autoload:Bool = true):Bool;

	/**
		@see http://php.net/manual/en/function.intval.php
	**/
	static function intval(value:Dynamic, base:Int = 10):Int;

	/**
		@see http://php.net/manual/en/function.floatval.php
	**/
	static function floatval(value:Dynamic):Float;

	/**
		@see http://php.net/manual/en/function.boolval.php
	**/
	static function boolval(value:Dynamic):Bool;

	/**
		@see http://php.net/manual/en/function.strval.php
	**/
	static function strval(value:Dynamic):String;

	/**
		@see http://php.net/manual/en/function.phpversion.php
	**/
	static function phpversion(?extension:String):String;

	/**
		@see http://php.net/manual/en/function.class-alias.php
	**/
	static function class_alias(original:String, alias:String, autoload:Bool = true):Bool;

	/**
		@see http://php.net/manual/en/function.count.php
	**/
	static function count(array:Dynamic, ?mode:Int):Int;

	/**
		@see http://php.net/manual/en/function.array-filter.php
	**/
	@:overload(function(array:NativeArray, callback:Dynamic->Bool, ?flag:Int):NativeArray {})
	static function array_filter(array:NativeArray, ?callback:Dynamic->?Dynamic->Bool, flag:Int = 0):NativeArray;

	/**
		@see http://php.net/manual/en/function.implode.php
	**/
	@:overload(function(pieces:NativeArray):String {})
	static function implode(glue:String, pieces:NativeArray):String;

	/**
		@see http://php.net/manual/en/function.array-map.php
	**/
	static function array_map(callback:EitherType<Dynamic->Dynamic, String>, array:Rest<NativeArray>):NativeArray;

	/**
		@see http://php.net/manual/en/function.array-merge.php
	**/
	static function array_merge(array:Rest<NativeArray>):NativeArray;

	/**
		@see http://php.net/manual/en/function.array-diff.php
	**/
	static function array_diff(array:Rest<NativeArray>):NativeArray;

	/**
		@see http://php.net/manual/en/function.array-pop.php
	**/
	static function array_pop(array:NativeArray):Dynamic;

	/**
		@see http://php.net/manual/en/function.array-push.php
	**/
	static function array_push(array:Ref<NativeArray>, value:Rest<Dynamic>):Int;

	/**
		@see http://php.net/manual/en/function.array-reverse.php
	**/
	static function array_reverse(array:NativeArray, preserve_keys:Bool = false):NativeArray;

	/**
		@see http://php.net/manual/en/function.array-search.php
	**/
	static function array_search(needle:Dynamic, haystack:NativeArray, strict:Bool = false):EitherType<Bool, EitherType<String, Int>>;

	/**
		@see http://php.net/manual/en/function.array-shift.php
	**/
	static function array_shift(array:Ref<NativeArray>):Dynamic;

	/**
		@see http://php.net/manual/en/function.array-slice.php
	**/
	static function array_slice(array:NativeArray, offset:Int, length:Int = null, preserve_keys:Bool = false):NativeArray;

	/**
		@see http://php.net/manual/en/function.array-splice.php
	**/
	static function array_splice(array:Ref<NativeArray>, offset:Int, lenght:Int = 0, ?replacement:Dynamic):NativeArray;

	/**
		@see http://php.net/manual/en/function.array-unshift.php
	**/
	static function array_unshift(arr:Ref<NativeArray>, value:Rest<Dynamic>):Int;

	/**
		@see http://php.net/manual/en/function.array-values.php
	**/
	static function array_values(arr:NativeArray):NativeIndexedArray<Dynamic>;

	/**
		@see http://php.net/manual/en/function.array-keys.php
	**/
	static function array_keys(arr:NativeArray):NativeIndexedArray<EitherType<String, Int>>;

	/**
		@see http://php.net/manual/en/function.array-key-exists.php
	**/
	static function array_key_exists(key:EitherType<String, Int>, arr:NativeArray):Bool;

	/**
		@see http://php.net/manual/en/function.array-fill.php
	**/
	static function array_fill(start_index:Int, num:Int, value:Dynamic):NativeArray;

	/**
		@see http://php.net/manual/en/function.array-pad.php
	**/
	static function array_pad(array:NativeArray, size:Int, value:Dynamic):NativeArray;

	/**
		@see http://php.net/manual/en/function.in-array.php
	**/
	static function in_array(needle:Dynamic, haystack:NativeArray, strict:Bool = false):Bool;

	/**
		@see http://php.net/manual/en/function.usort.php
	**/
	static function usort(array:Ref<NativeArray>, value_compare_func:Dynamic->Dynamic->Int):Bool;

	/**
		@see http://php.net/manual/en/function.reset.php
	**/
	static function reset(array:Ref<NativeArray>):Dynamic;

	/**
		@see http://php.net/manual/en/function.current.php
	**/
	static function current(array:Ref<NativeArray>):Dynamic;

	/**
		@see http://php.net/manual/en/function.next.php
	**/
	static function next(array:Ref<NativeArray>):Dynamic;

	/**
		@see http://php.net/manual/en/function.prev.php
	**/
	static function prev(array:Ref<NativeArray>):Dynamic;

	/**
		@see http://php.net/manual/en/function.end.php
	**/
	static function end(array:Ref<NativeArray>):Dynamic;

	/**
		@see http://php.net/manual/en/function.key.php
	**/
	static function key(array:NativeArray):EitherType<String, Int>;

	/**
		@see http://php.net/manual/en/function.each.php
	**/
	static function each(array:Ref<NativeArray>):NativeArray;

	/**
		@see http://php.net/manual/en/function.defined.php
	**/
	static function defined(name:String):Bool;

	/**
		@see http://php.net/manual/en/function.constant.php
	**/
	static function constant(name:String):Dynamic;

	/**
		@see http://php.net/manual/en/function.define.php
	**/
	static function define(name:String, value:Dynamic, case_insensitive:Bool = false):Bool;

	/**
		@see http://php.net/manual/en/function.echo.php
	**/
	static function echo(args:Rest<String>):Void;

	/**
		@see http://php.net/manual/en/function.print-r.php
	**/
	static function print_r(expression:Any, ?returnOutput:Bool):EitherType<Bool, String>;

	/**
		@see http://php.net/manual/en/function.sprintf.php
	**/
	static function sprintf(format:String, args:Rest<Dynamic>):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.method-exists.php
	**/
	static function method_exists(object:Dynamic, method_name:String):Bool;

	/**
		@see http://php.net/manual/en/function.property-exists.php
	**/
	static function property_exists(object:Dynamic, property_name:String):Bool;

	/**
		@see http://php.net/manual/en/function.is-callable.php
	**/
	static function is_callable(value:Dynamic, syntax_only:Bool = false, ?callable_name:String):Bool;

	/**
		@see http://php.net/manual/en/function.isset.php
	**/
	static function isset(value:Dynamic, args:Rest<Dynamic>):Bool;

	/**
		@see http://php.net/manual/en/function.unset.php
	**/
	static function unset(value:Dynamic, values:Rest<Dynamic>):Void;

	/**
		@see http://php.net/manual/en/function.get-object-vars.php
	**/
	static function get_object_vars(object:{}):NativeAssocArray<Dynamic>;

	/**
		@see http://php.net/manual/en/function.get-class.php
	**/
	static function get_class(object:{} = null):EitherType<Bool, String>;

	/**
		@see http://php.net/manual/en/function.get-parent-class.php
	**/
	static function get_parent_class(?object:Dynamic):EitherType<Bool, String>;

	/**
		@see http://php.net/manual/en/function.var-dump.php
	**/
	static function var_dump(args:Rest<Dynamic>):Void;

	/**
		@see http://php.net/manual/en/function.ord.php
	**/
	static function ord(string:String):Int;

	/**
		@see http://php.net/manual/en/function.chr.php
	**/
	static function chr(code:Int):String;

	/**
		@see http://php.net/manual/en/function.strpos.php
	**/
	static function strpos(haystack:String, needle:String, offset:Int = 0):EitherType<Bool, Int>;

	/**
		@see http://php.net/manual/en/function.stripos.php
	**/
	static function stripos(haystack:String, needle:String, offset:Int = 0):EitherType<Bool, Int>;

	/**
		@see http://php.net/manual/en/function.strrpos.php
	**/
	static function strrpos(haystack:String, needle:String, offset:Int = 0):EitherType<Bool, Int>;

	/**
		@see http://php.net/manual/en/function.str-split.php
	**/
	static function str_split(string:String, split_length:Int = 1):EitherType<Bool, NativeIndexedArray<String>>;

	/**
		@see http://php.net/manual/en/function.strlen.php
	**/
	static function strlen(string:String):Int;

	/**
		@see http://php.net/manual/en/function.strcmp.php
	**/
	static function strcmp(str1:String, str2:String):Int;

	/**
		@see https://www.php.net/manual/en/function.strspn.php
	**/
	static function strspn(string:String, characters:String, offset:Int = 0, ?length:Int):Int;

	/**
		@see http://php.net/manual/en/function.strtr.php
	**/
	@:overload(function(str:String, from:NativeAssocArray<String>):String {})
	static function strtr(str:String, from:String, to:String):String;

	/**
		@see http://php.net/manual/en/function.str-repeat.php
	**/
	static function str_repeat(input:String, multiplier:Int):String;

	/**
		@see http://php.net/manual/en/function.str-replace.php
	**/
	static function str_replace(search:EitherType<String, NativeArray>, replace:EitherType<String, NativeArray>, subject:EitherType<String, NativeArray>,
		?count:Int):EitherType<String, NativeArray>;

	/**
		@see https://www.php.net/manual/en/function.str-starts-with.php
	**/
	static function str_starts_with(haystack:String, needle:String):Bool;

	/**
		@see http://php.net/manual/en/function.explode.php
	**/
	static function explode(delimiter:String, string:String, ?limit:Int):EitherType<Bool, NativeIndexedArray<String>>;

	/**
		@see http://php.net/manual/en/function.substr.php
	**/
	static function substr(string:String, start:Int, ?length:Int):EitherType<Bool, String>;

	/**
		@see http://php.net/manual/en/function.substr-count.php
	**/
	static function substr_count(haystack:String, needle:String, ?offset:Int, ?length:Int):Int;

	/**
		@see http://php.net/manual/en/function.substr_replace.php
	**/
	static function substr_replace(string:EitherType<String, NativeArray>, replacement:EitherType<String, NativeArray>, start:EitherType<Int, NativeArray>,
		?length:EitherType<Int, NativeArray>):EitherType<String, NativeArray>;

	/**
		@see http://php.net/manual/en/function.strtoupper.php
	**/
	static function strtoupper(string:String):String;

	/**
		@see http://php.net/manual/en/function.strtolower.php
	**/
	static function strtolower(string:String):String;

	/**
		@see http://php.net/manual/en/function.debug-backtrace.php
	**/
	static function debug_backtrace(?options:Int, ?limit:Int):NativeIndexedArray<NativeAssocArray<Dynamic>>;

	/**
		@see http://php.net/manual/en/function.call-user-func.php
	**/
	static function call_user_func(callback:Dynamic, arguments:Rest<Dynamic>):Dynamic;

	/**
		@see http://php.net/manual/en/function.call-user-func-array.php
	**/
	static function call_user_func_array(callback:Dynamic, arguments:NativeArray):Dynamic;

	/**
		@see http://php.net/manual/en/function.func-get-args.php
	**/
	static function func_get_args():NativeIndexedArray<Dynamic>;

	/**
		@see http://php.net/manual/en/function.abs.php
	**/
	static function abs<T:Float>(number:T):T;

	/**
		@see http://php.net/manual/en/function.min.php
	**/
	static function min(values:Rest<Dynamic>):Dynamic;

	/**
		@see http://php.net/manual/en/function.max.php
	**/
	static function max(values:Rest<Dynamic>):Dynamic;

	/**
		@see http://php.net/manual/en/function.sin.php
	**/
	static function sin(arg:Float):Float;

	/**
		@see http://php.net/manual/en/function.cos.php
	**/
	static function cos(arg:Float):Float;

	/**
		@see http://php.net/manual/en/function.atan2.php
	**/
	static function atan2(y:Float, x:Float):Float;

	/**
		@see http://php.net/manual/en/function.tan.php
	**/
	static function tan(arg:Float):Float;

	/**
		@see http://php.net/manual/en/function.exp.php
	**/
	static function exp(arg:Float):Float;

	/**
		@see http://php.net/manual/en/function.log.php
	**/
	static function log(arg:Float):Float;

	/**
		@see http://php.net/manual/en/function.sqrt.php
	**/
	static function sqrt(arg:Float):Float;

	/**
		@see http://php.net/manual/en/function.floor.php
	**/
	static function floor(arg:Float):Float;

	/**
		@see http://php.net/manual/en/function.ceil.php
	**/
	static function ceil(arg:Float):Float;

	/**
		@see http://php.net/manual/en/function.round.php
	**/
	static function round(val:Float, precision:Int = 0, ?mode:Int):Float;

	/**
		@see http://php.net/manual/en/function.atan.php
	**/
	static function atan(arg:Float):Float;

	/**
		@see http://php.net/manual/en/function.asin.php
	**/
	static function asin(arg:Float):Float;

	/**
		@see http://php.net/manual/en/function.acos.php
	**/
	static function acos(arg:Float):Float;

	/**
		@see http://php.net/manual/en/function.pow.php
	**/
	static function pow(base:Float, exp:Float):Float;

	/**
		@see http://php.net/manual/en/function.mt-rand.php
	**/
	@:overload(function():Int {})
	static function mt_rand(base:Int, exp:Int):Int;

	/**
		@see http://php.net/manual/en/function.mt-getrandmax.php
	**/
	static function mt_getrandmax():Int;

	/**
		@see http://php.net/manual/en/function.random-bytes.php
	**/
	static function random_bytes(length:Int):String;

	/**
		@see http://php.net/manual/en/function.random-int.php
	**/
	static function random_int(min:Int, max:Int):Int;

	/**
		@see http://php.net/manual/en/function.is-nan.php
	**/
	static function is_nan(arg:Float):Bool;

	/**
		@see http://php.net/manual/en/function.is-finite.php
	**/
	static function is_finite(arg:Float):Bool;

	/**
		@see http://php.net/manual/en/function.trim.php
	**/
	static function trim(str:String, ?character_mask:String):String;

	/**
		@see http://php.net/manual/en/function.rtrim.php
	**/
	static function rtrim(str:String, ?character_mask:String):String;

	/**
		@see http://php.net/manual/en/function.ltrim.php
	**/
	static function ltrim(str:String, ?character_mask:String):String;

	/**
		@see http://php.net/manual/en/function.getenv.php
	**/
	static function getenv(varname:String):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.putenv.php
	**/
	static function putenv(setting:String):Bool;

	/**
		@see http://php.net/manual/en/function.sleep.php
	**/
	static function sleep(seconds:Int):EitherType<Bool, Int>;

	/**
		@see http://php.net/manual/en/function.usleep.php
	**/
	static function usleep(micro_seconds:Int):Void;

	/**
		@see http://php.net/manual/en/function.setlocale.php
	**/
	@:overload(function(category:Int, locale:NativeIndexedArray<String>):EitherType<Bool, String> {})
	static function setlocale(category:Int, locale:Rest<String>):EitherType<Bool, String>;

	/**
		@see http://php.net/manual/en/function.getcwd.php
	**/
	static function getcwd():EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.chdir.php
	**/
	static function chdir(directory:String):Bool;

	/**
		@see http://php.net/manual/en/function.php-uname.php
	**/
	static function php_uname(mode:String = 'a'):String;

	/**
		@see http://php.net/manual/en/function.system.php
	**/
	static function system(command:String, ?return_var:Ref<Int>):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.microtime.php
	**/
	static function microtime(get_as_float:Bool = false):EitherType<Float, String>;

	/**
		@see http://php.net/manual/en/function.fopen.php
	**/
	static function fopen(filename:String, mode:String, use_include_path:Bool = false, ?context:Resource):EitherType<Bool, Resource>;

	/**
		@see http://php.net/manual/en/function.fclose.php
	**/
	static function fclose(handle:Resource):Bool;

	/**
		@see http://php.net/manual/en/function.feof.php
	**/
	static function feof(handle:Resource):Bool;

	/**
		@see http://php.net/manual/en/function.fseek.php
	**/
	static function fseek(handle:Resource, offset:Int, ?whence:Int):Int;

	/**
		@see http://php.net/manual/en/function.ftell.php
	**/
	static function ftell(handle:Resource):EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.rewind.php
	**/
	static function rewind(handle:Resource):Bool;

	/**
		@see http://php.net/manual/en/function.fgetc.php
	**/
	static function fgetc(handle:Resource):EitherType<Bool, String>;

	/**
		@see http://php.net/manual/en/function.fgets.php
	**/
	static function fgets(handle:Resource):EitherType<Bool, String>;

	/**
		@see http://php.net/manual/en/function.fflush.php
	**/
	static function fflush(handle:Resource):Bool;

	/**
		@see http://php.net/manual/en/function.flock.php
	**/
	static function flock(handle:Resource, operation:Int, ?wouldblock:Ref<Int>):Bool;

	/**
		@see http://php.net/manual/en/function.fwrite.php
	**/
	static function fwrite(handle:Resource, string:String, ?length:Int):EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.fread.php
	**/
	static function fread(handle:Resource, length:Int):EitherType<Bool, String>;

	/**
		@see http://php.net/manual/en/function.file-exists.php
	**/
	static function file_exists(filename:String):Bool;

	/**
		@see http://php.net/manual/en/function.file-get-contents.php
	**/
	static function file_get_contents(filename:String, use_include_path:Bool = false, ?context:Resource, offset:Int = 0, ?maxlen:Int):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.file-put-contents.php
	**/
	static function file_put_contents(filename:String, data:Dynamic, flags:Int = 0, ?context:Resource):EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.fileatime.php
	**/
	static function fileatime(filename:String):EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.filectime.php
	**/
	static function filectime(filename:String):EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.filemtime.php
	**/
	static function filemtime(filename:String):EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.fileowner.php
	**/
	static function fileowner(filename:String):EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.filesize.php
	**/
	static function filesize(filename:String):EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.clearstatcache.php
	**/
	static function clearstatcache(clear_realpath_cache:Bool = false, ?filename:String):Bool;

	/**
		@see http://php.net/manual/en/function.fstat.php
	**/
	static function fstat(handle:Resource):NativeArray;

	/**
		@see http://php.net/manual/en/function.stat.php
	**/
	static function stat(filename:String):EitherType<NativeArray, Bool>;

	/**
		@see http://php.net/manual/en/function.fnmatch.php
	**/
	static function fnmatch(pattern:String, string:String, flags:Int = 0):Bool;

	/**
		@see http://php.net/manual/en/function.pathinfo.php
	**/
	static function pathinfo(path:String, ?options:Int):EitherType<String, NativeAssocArray<String>>;

	/**
		@see http://php.net/manual/en/function.realpath.php
	**/
	static function realpath(path:String):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.filetype.php
	**/
	static function filetype(filename:String):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.mkdir.php
	**/
	static function mkdir(pathname:String, mode:Int = 511, recursive:Bool = false, ?context:Resource):Bool;

	/**
		@see http://php.net/manual/en/function.link.php
	**/
	static function link(target:String, link:String):Bool;

	/**
		@see http://php.net/manual/en/function.symlink.php
	**/
	static function symlink(target:String, link:String):Bool;

	/**
		@see http://php.net/manual/en/function.unlink.php
	**/
	static function unlink(filename:String, ?context:Resource):Bool;

	/**
		@see http://php.net/manual/en/function.rmdir.php
	**/
	static function rmdir(dirname:String, ?context:Resource):Bool;

	/**
		@see http://php.net/manual/en/function.dirname.php
	**/
	static function dirname(path:String, levels:Int = 1):String;

	/**
		@see http://php.net/manual/en/function.basename.php
	**/
	static function basename(path:String, ?suffix:String):String;

	/**
		@see http://php.net/manual/en/function.glob.php
	**/
	static function glob(pattern:String, flags:Int = 0):NativeArray;

	/**
		@see http://php.net/manual/en/function.opendir.php
	**/
	static function opendir(path:String, ?context:Resource):EitherType<Resource, Bool>;

	/**
		@see http://php.net/manual/en/function.closedir.php
	**/
	static function closedir(dir_handle:Resource):Void;

	/**
		@see http://php.net/manual/en/function.readdir.php
	**/
	static function readdir(?dir_handle:Resource):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.readlink.php
	**/
	static function readlink(filename:String):EitherType<Bool,String>;

	/**
		@see http://php.net/manual/en/function.rewinddir.php
	**/
	static function rewinddir(?dir_handle:Resource):Void;

	/**
		@see http://php.net/manual/en/function.is-dir.php
	**/
	static function is_dir(filename:String):Bool;

	/**
		@see http://php.net/manual/en/function.is-executable.php
	**/
	static function is_executable(filename:String):Bool;

	/**
		@see http://php.net/manual/en/function.is-file.php
	**/
	static function is_file(filename:String):Bool;

	/**
		@see http://php.net/manual/en/function.is-link.php
	**/
	static function is_link(filename:String):Bool;

	/**
		@see http://php.net/manual/en/function.is-readable.php
	**/
	static function is_readable(filename:String):Bool;

	/**
		@see http://php.net/manual/en/function.is-writable.php
	**/
	static function is_writable(filename:String):Bool;

	/**
		@see http://php.net/manual/en/function.rename.php
	**/
	static function rename(oldname:String, newname:String, ?context:Resource):Bool;

	/**
		@see http://php.net/manual/en/function.copy.php
	**/
	static function copy(source:String, dest:String, ?context:Resource):Bool;

	/**
		@see http://php.net/manual/en/function.preg-match.php
	**/
	static function preg_match(pattern:String, subject:String, ?matches:NativeArray, ?flags:Int, ?offset:Int):EitherType<Bool, Int>;

	/**
		@see http://php.net/manual/en/function.preg-match-all.php
	**/
	static function preg_match_all(pattern:String, subject:String, ?matches:NativeArray, ?flags:Int, ?offset:Int):EitherType<Bool, Int>;

	/**
		@see http://php.net/manual/en/function.preg-quote.php
	**/
	static function preg_quote(str:String, ?delimiter:String):String;

	/**
		@see http://php.net/manual/en/function.preg-split.php
	**/
	static function preg_split(pattern:String, subject:String, limit:Int = -1, flags:Int = 0):EitherType<Bool, NativeArray>;

	/**
		@see http://php.net/manual/en/function.preg-replace.php
	**/
	static function preg_replace(pattern:EitherType<String, NativeArray>, replacement:EitherType<String, NativeArray>,
		subject:EitherType<String, NativeArray>, limit:Int = -1, ?count:Int):EitherType<String, NativeArray>;

	/**
		@see http://php.net/manual/en/function.preg-last-error.php
	**/
	static function preg_last_error():Int;

	/**
		@see http://php.net/manual/en/function.md5.php
	**/
	static function md5(str:String, raw_output:Bool = false):String;

	/**
		@see http://php.net/manual/en/function.sha1.php
	**/
	static function sha1(str:String, raw_output:Bool = false):String;

	/**
		@see http://php.net/manual/en/function.hash.php
	**/
	static function hash(algo:String, str:String, raw_output:Bool = false):String;

	/**
		@see http://php.net/manual/en/function.hash-algos.php
	**/
	static function hash_algos():NativeIndexedArray<String>;

	/**
		@see http://php.net/manual/en/function.hash-hmac.php
	**/
	static function hash_hmac(algo:String, data:String, key:String, binary:Bool = false):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.hash-hmac-algos.php
	**/
	static function hash_hmac_algos():NativeIndexedArray<String>;

	/**
		@see http://php.net/manual/en/function.hash-hmac-file.php
	**/
	static function hash_hmac_file(algo:String, data:String, key:String, binary:Bool = false):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.pack.php
	**/
	static function pack(format:String, args:Rest<Dynamic>):String;

	/**
		@see http://php.net/manual/en/function.unpack.php
	**/
	static function unpack(format:String, data:String):NativeArray;

	/**
		@see http://php.net/manual/en/function.chunk-split.php
	**/
	static function chunk_split(body:String, chunklen:Int = 76, end:String = "\r\n"):String;

	/**
		@see http://php.net/manual/en/function.urlencode.php
	**/
	static function urlencode(str:String):String;

	/**
		@see http://php.net/manual/en/function.urldecode.php
	**/
	static function urldecode(str:String):String;

	/**
		@see http://php.net/manual/en/function.rawurlencode.php
	**/
	static function rawurlencode(str:String):String;

	/**
		@see http://php.net/manual/en/function.rawurldecode.php
	**/
	static function rawurldecode(str:String):String;

	/**
		@see http://php.net/manual/en/function.header.php
	**/
	static function header(string:String, replace:Bool = true, ?http_response_code:Int):Void;

	/**
		@see http://php.net/manual/en/function.header-remove.php
	**/
	static function header_remove(?name:String):Void;

	/**
		@see http://php.net/manual/en/function.headers-list.php
	**/
	static function headers_list():NativeIndexedArray<String>;

	/**
		@see http://php.net/manual/en/function.headers-sent.php
	**/
	static function headers_sent(?file:Ref<String>, ?line:Ref<Int>):Bool;

	/**
		@see http://php.net/manual/en/function.setcookie.php
	**/
	@:overload(function(name:String, value:String = "", ?options:NativeStructArray<{?expires:Int, ?path:String, ?domain:String, ?secure:Bool, ?httponly:Bool, ?samesite:String}>):Bool {})
	static function setcookie(name:String, value:String = "", expire:Int = 0, path:String = "", domain:String = "",
		secure:Bool = false, httponly:Bool = false):Bool;

	/**
		@see http://php.net/manual/en/function.html-entity-decode.php
	**/
	static function html_entity_decode(string:String, ?flags:Int, ?encoding:String):String;

	/**
		@see http://php.net/manual/en/function.htmlentities.php
	**/
	static function htmlentities(string:String, ?flags:Int, ?encoding:String, double_encode:Bool = true):String;

	/**
		@see http://php.net/manual/en/function.htmlspecialchars.php
	**/
	static function htmlspecialchars(string:String, ?flags:Int, ?encoding:String, double_encode:Bool = true):String;

	/**
		@see http://php.net/manual/en/function.htmlspecialchars-decode.php
	**/
	static function htmlspecialchars_decode(string:String, ?flags:Int):String;

	/**
		@see http://php.net/manual/en/function.str-pad.php
	**/
	static function str_pad(input:String, pad_length:Int, pad_String:String = ' ', ?pad_type:Int):String;

	/**
		@see http://php.net/manual/en/function.dechex.php
	**/
	static function dechex(number:Int):String;

	/**
		@see http://php.net/manual/en/function.hexdec.php
	**/
	static function hexdec(hex_string:String):Int;

	/**
		@see http://php.net/manual/en/function.decbin.php
	**/
	static function decbin(number:Int):String;

	/**
		@see http://php.net/manual/en/function.bindec.php
	**/
	static function bindec(binary_string:String):Float;

	/**
		@see http://php.net/manual/en/function.bin2hex.php
	**/
	static function bin2hex(str:String):String;

	/**
		@see http://php.net/manual/en/function.hex2bin.php
	**/
	static function hex2bin(str:String):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.serialize.php
	**/
	static function serialize(value:Dynamic):String;

	/**
		@see http://php.net/manual/en/function.unserialize.php
	**/
	static function unserialize(str:String, ?options:NativeArray):Dynamic;

	/**
		@see http://php.net/manual/en/function.extension-loaded.php
	**/
	static function extension_loaded(name:String):Bool;

	/**
		@see http://php.net/manual/en/function.strncasecmp.php
	**/
	static function strncasecmp(str1:String, str2:String, len:Int):Int;

	/**
		@see http://php.net/manual/en/function.strcasecmp.php
	**/
	static function strcasecmp(str1:String, str2:String):Int;

	/**
		@see http://php.net/manual/en/function.fpassthru.php
	**/
	static function fpassthru(handle:Resource):Int;

	/**
		@see http://php.net/manual/en/function.json-encode.php
	**/
	static function json_encode(value:Dynamic, options:Int = 0, depth:Int = 512):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.json-decode.php
	**/
	static function json_decode(json:String, assoc:Bool = false, depth:Int = 512, options:Int = 512):Dynamic;

	/**
		@see http://php.net/manual/en/function.json-last-error.php
	**/
	static function json_last_error():Int;

	/**
		@see http://php.net/manual/en/function.json-last-error-msg.php
	**/
	static function json_last_error_msg():EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.spl-object-hash.php
	**/
	static function spl_object_hash(obj:{}):String;

	/**
		@see http://php.net/manual/en/function.spl-object-id.php
	**/
	static function spl_object_id(obj:{}):Int;

	/**
		@see http://php.net/manual/en/function.spl-autoload-call.php
	**/
	static function spl_autoload_call(class_name:String):Void;

	/**
		@see http://php.net/manual/en/function.spl-autoload-extensions.php
	**/
	static function spl_autoload_extensions(?file_extensions:String):String;

	/**
		@see http://php.net/manual/en/function.spl-autoload-functions.php
	**/
	static function spl_autoload_functions():EitherType<NativeIndexedArray<Function>, Bool>;

	/**
		@see http://php.net/manual/en/function.spl-autoload-register.php
	**/
	static function spl_autoload_register(?autoload_function:(className:String) -> Void, throw_exception:Bool = true, prepend:Bool = false):Bool;

	/**
		@see http://php.net/manual/en/function.spl-autoload-unregister.php
	**/
	static function spl_autoload_unregister(autoload_function:(className:String) -> Void):Bool;

	/**
		@see http://php.net/manual/en/function.spl-autoload.php
	**/
	static function spl_autoload(class_name:String, ?file_extensions:String):Void;

	/**
		@see http://php.net/manual/en/function.utf8-encode.php
	**/
	static function utf8_encode(data:String):String;

	/**
		@see http://php.net/manual/en/function.utf8-decode.php
	**/
	static function utf8_decode(data:String):String;

	/**
		@see http://php.net/manual/en/function.mb-internal-encoding.php
	**/
	static function mb_internal_encoding(?encoding:String):EitherType<Bool, String>;

	/**
		@see http://php.net/manual/en/function.mb-convert-encoding.php
	**/
	static function mb_convert_encoding(str:String, to_encoding:String, ?from_encoding:Dynamic):String;

	/**
		@see http://php.net/manual/en/function.mb-check-encoding.php
	**/
	static function mb_check_encoding(str:String = null, ?encoding:String):Bool;

	/**
		@see http://php.net/manual/en/function.mb-language.php
	**/
	@:overload(function(language:String):Bool {})
	static function mb_language():String;

	/**
		@see http://php.net/manual/en/function.mb-scrub.php
	**/
	static function mb_scrub(str:String, ?encoding:String):String;

	/**
		@see http://php.net/manual/en/function.mb-split.php
	**/
	static function mb_split(pattern:String, str:String, ?limit:Int):NativeIndexedArray<String>;

	/**
		@see http://php.net/manual/en/function.mb-strlen.php
	**/
	static function mb_strlen(str:String, ?encoding:String):Int;

	/**
		@see http://php.net/manual/en/function.mb-substr.php
	**/
	static function mb_substr(str:String, start:Int, length:Int = null, ?encoding:String):String;

	/**
		@see http://php.net/manual/en/function.mb-substitute-character.php
	**/
	static function mb_substitute_character(?substchar:EitherType<Int,String>):EitherType<Bool,EitherType<String,Int>>;

	/**
		@see http://php.net/manual/en/function.mb-chr.php
		(Polyfilled for php 7.0)
	**/
	static function mb_chr(cp:Int, ?encoding:String):String;

	/**
		@see http://php.net/manual/en/function.mb-ord.php
		(Polyfilled for php 7.0)
	**/
	static function mb_ord(str:String, ?encoding:String):Int;

	/**
		@see http://php.net/manual/en/function.mb-regex-encoding.php
	**/
	static function mb_regex_encoding(?encoding:String):EitherType<Bool, String>;

	/**
		@see http://php.net/manual/en/function.mb-strtoupper.php
	**/
	static function mb_strtoupper(str:String, ?encoding:String):String;

	/**
		@see http://php.net/manual/en/function.mb-strpos.php
	**/
	static function mb_strpos(haystack:String, needle:String, ?offset:Int, ?encoding:String):EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.mb-strrpos.php
	**/
	static function mb_strrpos(haystack:String, needle:String, ?offset:Int, ?encoding:String):EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.mb-strtolower.php
	**/
	static function mb_strtolower(str:String, ?encoding:String):String;

	/**
		@see http://php.net/manual/en/function.proc-open.php
	**/
	static function proc_open(cmd:String, descriptorspec:NativeArray, pipes:NativeIndexedArray<Resource>, ?cwd:String, ?env:NativeArray,
		?other_options:NativeArray):EitherType<Resource, Bool>;

	/**
		@see http://php.net/manual/en/function.proc-get-status.php
	**/
	static function proc_get_status(process:Resource):EitherType<Bool, NativeAssocArray<Scalar>>;

	/**
		@see http://php.net/manual/en/function.proc-close.php
	**/
	static function proc_close(process:Resource):Int;

	/**
		@see http://php.net/manual/en/function.proc-terminate.php
	**/
	static function proc_terminate(process:Resource, signal:Int = 15):Bool;

	/**
		@see http://php.net/manual/en/function.stream-select.php
	**/
	static function stream_select(read:NativeArray, write:NativeArray, except:NativeArray, tv_sec:Int, tv_usec:Int = 0):Bool;

	/**
		@see http://php.net/manual/en/function.stream-get-contents.php
	**/
	static function stream_get_contents(handle:Resource, maxlength:Int = -1, offset:Int = -1):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.stream-socket-shutdown.php
	**/
	static function stream_socket_shutdown(stream:Resource, how:Int):Bool;

	/**
		@see http://php.net/manual/en/function.stream-set-timeout.php
	**/
	static function stream_set_timeout(stream:Resource, seconds:Int, microseconds:Int = 0):Bool;

	/**
		@see http://php.net/manual/en/function.stream-set-blocking.php
	**/
	static function stream_set_blocking(stream:Resource, mode:Bool):Bool;

	/**
		@see http://php.net/manual/en/function.stream-socket-accept.php
	**/
	static function stream_socket_accept(server_socket:Resource, ?timeout:Float, ?peername:Ref<String>):EitherType<Resource, Bool>;

	/**
		@see http://php.net/manual/en/function.stream-socket-client.php
	**/
	static function stream_socket_client(remote_socket:String, ?errno:Ref<Int>, ?errstr:Ref<String>, ?timeout:Float, ?flags:Int,
		?context:Resource):EitherType<Resource, Bool>;

	/**
		@see http://php.net/manual/en/function.stream-socket-server.php
	**/
	static function stream_socket_server(local_socket:String, ?errno:Ref<Int>, ?errstr:Ref<String>, ?flags:Int, ?context:Resource):EitherType<Resource, Bool>;

	/**
		@see http://php.net/manual/en/function.stream-socket-get-name.php
	**/
	static function stream_socket_get_name(stream:Resource, want_peer:Bool):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.socket-create.php
	**/
	static function socket_create(domain:Int, type:Int, protocol:Int):Resource;

	/**
		@see http://php.net/manual/en/function.socket-connect.php
	**/
	static function socket_connect(stream:Resource, host:String, port:Int):Bool;

	/**
		@see http://php.net/manual/en/function.socket-listen.php
	**/
	static function socket_listen(stream:Resource, connections:Int):Bool;

	/**
		@see http://php.net/manual/en/function.socket-shutdown.php
	**/
	static function socket_shutdown(stream:Resource, rw:Int):Bool;

	/**
		@see http://php.net/manual/en/function.socket-bind.php
	**/
	static function socket_bind(stream:Resource, host:String, port:Int):Bool;

	/**
		@see http://php.net/manual/en/function.socket-accept.php
	**/
	static function socket_accept(stream:Resource):EitherType<Resource, Bool>;

	/**
		@see http://php.net/manual/en/function.socket-getpeername.php
	**/
	static function socket_getpeername(stream:Resource, host:Ref<String>, port:Ref<Int>):Bool;

	/**
		@see http://php.net/manual/en/function.socket-getsockname.php
	**/
	static function socket_getsockname(stream:Resource, host:Ref<String>, port:Ref<Int>):Bool;

	/**
		@see http://php.net/manual/en/function.socket-set-block.php
	**/
	static function socket_set_block(stream:Resource):Bool;

	/**
		@see http://php.net/manual/en/function.socket-set-nonblock.php
	**/
	static function socket_set_nonblock(stream:Resource):Bool;

	/**
		@see http://php.net/manual/en/function.socket-set-option.php
	**/
	static function socket_set_option(stream:Resource, level:Int, option:Int, val:Any):Bool;

	/**
		@see http://php.net/manual/en/function.socket-select.php
	**/
	static function socket_select(read:NativeIndexedArray<Resource>, write:NativeIndexedArray<Resource>, other:NativeIndexedArray<Resource>, tv_sec:Int = 0,
		tv_usec:Int = 0):EitherType<Bool, Int>;

	/**
		@see http://php.net/manual/en/function.socket-read.php
	**/
	static function socket_read(resource:Resource, length:Int, ?type:Int):EitherType<Bool, String>;

	/**
		@see http://php.net/manual/en/function.socket-write.php
	**/
	static function socket_write(resource:Resource, buffer:String, len:Int = 0):EitherType<Bool, Int>;

	/**
		@see http://php.net/manual/en/function.socket-import-stream.php
	**/
	static function socket_import_stream(resource:Resource):EitherType<Bool, Resource>;

	/**
		@see http://php.net/manual/en/function.socket-export-stream.php
	**/
	static function socket_export_stream(resource:Resource):EitherType<Bool, Resource>;

	/**
		@see http://php.net/manual/en/function.socket-close.php
	**/
	static function socket_close(resource:Resource):Void;

	/**
		@see http://php.net/manual/en/function.ini-get.php
	**/
	static function ini_get(var_name:String):EitherType<Bool, String>;

	/**
		@see http://php.net/manual/en/function.ini-set.php
	**/
	static function ini_set(var_name:String, newvalue:String):EitherType<Bool, String>;

	/**
		@see http://php.net/manual/en/function.sqlite-error-string.php
	**/
	static function sqlite_error_string(error_code:Int):String;

	/**
		@see http://php.net/manual/en/function.sqlite-escape-string.php
	**/
	static function sqlite_escape_string(item:String):String;

	/**
		@see http://php.net/manual/en/function.set-time-limit.php
	**/
	static function set_time_limit(seconds:Int):Bool;

	/**
		@see http://php.net/manual/en/function.function-exists.php
	**/
	static function function_exists(function_name:String):Bool;

	/**
		@see http://php.net/manual/en/function.getallheaders.php
	**/
	static function getallheaders():NativeAssocArray<Dynamic>;

	/**
		@see http://php.net/manual/en/function.ucfirst.php
	**/
	static function ucfirst(string:String):String;

	/**
		@see http://php.net/manual/en/function.ucwords.php
	**/
	static function ucwords(str:String, ?delimiters:String):String;

	/**
		@see http://php.net/manual/en/function.base64-encode.php
	**/
	static function base64_encode(data:String):String;

	/**
		@see http://php.net/manual/en/function.base64-decode.php
	**/
	static function base64_decode(data:String, strict:Bool = false):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.gethostname.php
	**/
	static function gethostname():EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.gethostbyname.php
	**/
	static function gethostbyname(hostname:String):String;

	/**
		@see http://php.net/manual/en/function.gethostbyaddr.php
	**/
	static function gethostbyaddr(ip_address:String):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.getprotobyname.php
	**/
	static function getprotobyname(name:String):EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.mktime.php
	**/
	static function mktime(?hour:Int, ?minute:Int, ?second:Int, ?month:Int, ?day:Int, ?year:Int, ?is_dst:Int):EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.gmmktime.php
	**/
	static function gmmktime(?hour:Int, ?minute:Int, ?second:Int, ?month:Int, ?day:Int, ?year:Int, ?is_dst:Int):Int;

	/**
		@see http://php.net/manual/en/function.date.php
	**/
	static function date(format:String, ?timestamp:Int):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.gmdate.php
	**/
	static function gmdate(format:String, ?timestamp:Int):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.time.php
	**/
	static function time():Int;

	/**
		@see http://php.net/manual/en/function.strftime.php
	**/
	static function strftime(format:String, ?timestamp:Int):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.strtotime.php
	**/
	static function strtotime(time:String, ?now:Int):EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.flush.php
	**/
	static function flush():Void;

	/**
		@see http://php.net/manual/en/function.session-cache-limiter.php
	**/
	static function session_cache_limiter(?cache_limiter:String):String;

	/**
		@see http://php.net/manual/en/function.session-cache-expire.php
	**/
	static function session_cache_expire(?new_cache_expire:Int):Int;

	/**
		@see http://php.net/manual/en/function.session-decode.php
	**/
	static function session_decode(data:String):Bool;

	/**
		@see http://php.net/manual/en/function.session-destroy.php
	**/
	static function session_destroy():Bool;

	/**
		@see http://php.net/manual/en/function.session-encode.php
	**/
	static function session_encode():String;

	/**
		@see http://php.net/manual/en/function.session-name.php
	**/
	static function session_name(?name:String):String;

	/**
		@see http://php.net/manual/en/function.session-start.php
	**/
	static function session_start(?options:NativeArray):Bool;

	/**
		@see http://php.net/manual/en/function.session-status.php
	**/
	static function session_status():Int;

	/**
		@see http://php.net/manual/en/function.session-unset.php
	**/
	static function session_unset():Void;

	/**
		@see http://php.net/manual/en/function.session-write-close.php
	**/
	static function session_write_close():Void;

	/**
		@see http://php.net/manual/en/function.session-id.php
	**/
	static function session_id(?id:String):String;

	/**
		@see http://php.net/manual/en/function.session-save-path.php
	**/
	static function session_save_path(?path:String):String;

	/**
		@see http://php.net/manual/en/function.session-module-name.php
	**/
	static function session_module_name(?module:String):String;

	/**
		@see http://php.net/manual/en/function.session-regenerate-id.php
	**/
	static function session_regenerate_id(delete_old_session:Bool = false):Bool;

	/**
		@see http://php.net/manual/en/function.session-set-cookie-params.php
	**/
	@:overload(function(options:NativeStructArray<{?lifetime:Int, ?path:String, ?domain:String, ?secure:Bool, ?httponly:Bool, ?samesite:String}>):Bool {})
	static function session_set_cookie_params(lifetime:Int, ?path:String, ?domain:String, secure:Bool = false, httponly:Bool = false):Bool;

	/**
		@see http://php.net/manual/en/function.session-get-cookie-params.php
	**/
	static function session_get_cookie_params():NativeAssocArray<Dynamic>;

	/**
		@see http://php.net/manual/en/function.session-set-save-handler.php
	**/
	@:overload(function(sessionhandler:SessionHandlerInterface, register_shutdown:Bool = true):Bool {})
	static function session_set_save_handler(open:String->String->Bool, close:Void->Bool, read:String->String, write:String->String->Bool,
		destroy:String->Bool, gc:Int->Bool, ?create_sid:Void->String, ?validate_sid:Function, ?update_timestamp:Function):Bool;

	/**
		@see http://php.net/manual/en/function.mail.php
	**/
	static function mail(to:String, subject:String, message:String, ?additional_headers:String, ?additional_parameters:String):Bool;

	/**
		@see http://php.net/manual/en/function.require.php
	**/
	static function require(include_path:String):Void;

	/**
		@see http://php.net/manual/en/function.require-once.php
	**/
	static function require_once(include_path:String):Void;

	/**
		@see http://php.net/manual/en/function.include.php
	**/
	static function include(include_path:String):Void;

	/**
		@see http://php.net/manual/en/function.include-once.php
	**/
	static function include_once(include_path:String):Void;

	/**
		@see http://php.net/manual/en/function.set-include-path.php
	**/
	static function set_include_path(new_include_path:String):String;

	/**
		@see http://php.net/manual/en/function.get-include-path.php
	**/
	static function get_include_path():String;

	/**
		@see http://php.net/manual/en/function.gzcompress.php
	**/
	static function gzcompress(data:String, ?level:Int, ?encoding:Int):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.gzuncompress.php
	**/
	static function gzuncompress(data:String, ?length:Int):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.mime-content-type.php
	**/
	static function mime_content_type(filename:String):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.move-uploaded-file.php
	**/
	static function move_uploaded_file(filename:String, destination:String):Bool;

	/**
		@see http://php.net/manual/en/function.is-uploaded-file.php
	**/
	static function is_uploaded_file(filename:String):Bool;

	/**
		@see http://php.net/manual/en/function.gc-collect-cycles.php
	**/
	static function gc_collect_cycles():Int;

	/**
		@see http://php.net/manual/en/function.cli-set-process-title.php
	**/
	static function cli_set_process_title(title:String):Bool;

	/**
		@see http://php.net/manual/en/function.http-response-code.php
	**/
	static function http_response_code(?response_code:Int):EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.version-compare.php
	**/
	@:overload(function(version1:String, version2:String, comparisonOperator:String):Bool {})
	static function version_compare(version1:String, version2:String):Int;

	/**
		@see http://php.net/manual/en/function.ob-clean.php
	**/
	static function ob_clean():Void;

	/**
		@see http://php.net/manual/en/function.ob-end-clean.php
	**/
	static function ob_end_clean():Bool;

	/**
		@see http://php.net/manual/en/function.ob-end-flush.php
	**/
	static function ob_end_flush():Bool;

	/**
		@see http://php.net/manual/en/function.ob-flush.php
	**/
	static function ob_flush():Void;

	/**
		@see http://php.net/manual/en/function.ob-get-clean.php
	**/
	static function ob_get_clean():EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.ob-get-contents.php
	**/
	static function ob_get_contents():EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.ob-get-flush.php
	**/
	static function ob_get_flush():EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.ob-get-length.php
	**/
	static function ob_get_length():EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.ob-get-level.php
	**/
	static function ob_get_level():Int;

	/**
		@see http://php.net/manual/en/function.ob-get-status.php
	**/
	static function ob_get_status(full_status:Bool = false):NativeArray;

	/**
		@see http://php.net/manual/en/function.ob-implicit-flush.php
	**/
	static function ob_implicit_flush(flag:Int = 1):Void;

	/**
		@see http://php.net/manual/en/function.ob-list-handlers.php
	**/
	static function ob_list_handlers():NativeIndexedArray<String>;

	/**
		@see http://php.net/manual/en/function.ob-start.php
	**/
	static function ob_start(output_callback: (String, ?Int) -> String = null, chunk_size:Int = 0, ?flags:Int):Bool;

	/**
		@see http://php.net/manual/en/function.strip-tags.php
	**/
	static function strip_tags(str: String, ?allowable_tags:EitherType<String, Array<String>>):String;

	/**
		@see http://php.net/manual/en/function.parse-ini-file.php
	**/
	static function parse_ini_file(filename:String, process_sections:Bool = false, ?scanner_mode:Int): EitherType<NativeAssocArray<Dynamic>, Bool>;

	/**
		@see http://php.net/manual/en/function.parse-ini-string.php
	**/
	static function parse_ini_string(ini:String, process_sections:Bool = false, ?scanner_mode:Int): EitherType<NativeAssocArray<Dynamic>, Bool>;

	/**
		@see http://php.net/manual/en/function.opcache-compile-file.php
	**/
	static function opcache_compile_file(file:String):Bool;

	/**
		@see http://php.net/manual/en/function.opcache-get-configuration.php
	**/
	static function opcache_get_configuration():NativeAssocArray<Dynamic>;

	/**
		@see http://php.net/manual/en/function.opcache-get-status.php
	**/
	static function opcache_get_status(get_scripts:Bool = true):EitherType<NativeAssocArray<Dynamic>, Bool>;

	/**
		@see http://php.net/manual/en/function.opcache-invalidate.php
	**/
	static function opcache_invalidate(script:String, force:Bool = false):Bool;

	/**
		@see http://php.net/manual/en/function.opcache-is-script-cached.php
	**/
	static function opcache_is_script_cached(file:String):Bool;

	/**
		@see http://php.net/manual/en/function.opcache-reset.php
	**/
	static function opcache_reset():Bool;

	/**
		@see http://php.net/manual/en/function.touch.php
	**/
	static function touch(filename:String, ?time:Int, ?atime: Int):Bool;

	/**
		@see http://php.net/manual/en/function.checkdnsrr.php
	**/
	static function checkdnsrr(host:String, type:String = "MX"):Bool;

	/**
		@see http://php.net/manual/en/function.dns-get-record.php
	**/
	static function dns_get_record(hostname:String, ?type:Int, ?authns:Ref<NativeIndexedArray<NativeAssocArray<Dynamic>>>,
		?addtl:Ref<NativeIndexedArray<NativeAssocArray<Dynamic>>>, raw:Bool = false):EitherType<NativeIndexedArray<NativeAssocArray<Dynamic>>, Bool>;

	/**
		@see http://php.net/manual/en/function.idn-to-ascii.php
	**/
	static function idn_to_ascii(domain:String, ?options:Int, ?variant:Int, ?idna_info:Ref<NativeAssocArray<Dynamic>>):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.idn-to-utf8.php
	**/
	static function idn_to_utf8(domain:String, ?options:Int, ?variant:Int, ?idna_info:Ref<NativeAssocArray<Dynamic>>):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.intl-error-name.php
	**/
	static function intl_error_name(error_code:Int):String;

	/**
		@see http://php.net/manual/en/function.intl-get-error-code.php
	**/
	static function intl_get_error_code():Int;

	/**
		@see http://php.net/manual/en/function.intl-get-error-message.php
	**/
	static function intl_get_error_message():String;

	/**
		@see http://php.net/manual/en/function.intl-is-failure.php
	**/
	static function intl_is_failure(error_code:Int):Bool;

	/**
		@see http://php.net/manual/en/function.filter-has-var.php
	**/
	static function filter_has_var(input_type:Int, var_name:String):Bool;

	/**
		@see http://php.net/manual/en/function.filter-id.php
	**/
	static function filter_id(name:String):EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.filter-input.php
	**/
	static function filter_input(type:Int, var_name:String, ?filter:Int, ?options: EitherType<NativeAssocArray<Dynamic>, Int>):Dynamic;

	/**
		@see http://php.net/manual/en/function.filter-list.php
	**/
	static function filter_list():NativeIndexedArray<String>;

	/**
		@see http://php.net/manual/en/function.filter-var.php
	**/
	static function filter_var(value: Any, ?filter:Int, ?options: EitherType<NativeAssocArray<Dynamic>, Int>):Dynamic;

	/**
		@see http://php.net/manual/en/function.number-format.php
	**/
	static function number_format(num:Float, ?decimals:Int, ?decimal_separator:String, ?thousands_separator:String):String;

	/**
		@see http://php.net/manual/en/function.empty.php
	**/
	static function empty(variable:Any):Bool;

	/**
		@see http://php.net/manual/en/function.quoted-printable-decode.php
	**/
	static function quoted_printable_decode(string:String):String;

	/**
		@see http://php.net/manual/en/function.quoted-printable-encode.php
	**/
	static function quoted_printable_encode(string:String):String;

	/**
		@see http://php.net/manual/en/function.easter-date.php
	**/
	static function easter_date(?year:Int, ?mode:Int): Int;

	/**
		@see http://php.net/manual/en/function.easter-days.php
	**/
	static function easter_days(?year:Int, ?mode:Int): Int;

	/**
		@see http://php.net/manual/en/function.sys-get-temp-dir.php
	**/
	static function sys_get_temp_dir():String;

	/**
		@see http://php.net/manual/en/function.tempnam.php
	**/
	static function tempnam(directory:String, prefix:String):EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.tmpfile.php
	**/
	static function tmpfile():EitherType<Resource, Bool>;

	/**
		@see http://php.net/manual/en/function.ftp-alloc.php
	**/
	static function ftp_alloc(ftp: Resource, size: Int, ?response: Ref<String>): Bool;

	/**
		@see http://php.net/manual/en/function.ftp-append.php
	**/
	static function ftp_append(ftp: Resource, remote_filename: String, local_filename: String, ?mode: Int): Bool;

	/**
		@see http://php.net/manual/en/function.ftp-cdup.php
	**/
	static function ftp_cdup(ftp: Resource): Bool;

	/**
		@see http://php.net/manual/en/function.ftp-chdir.php
	**/
	static function ftp_chdir(ftp: Resource, directory: String): Bool;

	/**
		@see http://php.net/manual/en/function.ftp-chmod.php
	**/
	static function ftp_chmod(ftp: Resource, permissions: Int, filename: String): EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.ftp-close.php
	**/
	static function ftp_close(ftp: Resource): Bool;

	/**
		@see http://php.net/manual/en/function.ftp-connect.php
	**/
	static function ftp_connect(hostname: String, port: Int = 21, timeout: Int = 90): EitherType<Resource, Bool>;

	/**
		@see http://php.net/manual/en/function.ftp-delete.php
	**/
	static function ftp_delete(ftp: Resource, filename: String): Bool;

	/**
		@see http://php.net/manual/en/function.ftp-exec.php
	**/
	static function ftp_exec(ftp: Resource, command: String): Bool;

	/**
		@see http://php.net/manual/en/function.ftp-fget.php
	**/
	static function ftp_fget(ftp: Resource, stream: Resource, remote_filename: String, ?mode: Int, offset: Int = 0): Bool;

	/**
		@see http://php.net/manual/en/function.ftp-fput.php
	**/
	static function ftp_fput(ftp: Resource, remote_filename: String, stream: Resource, ?mode: Int, offset: Int = 0): Bool;

	/**
		@see http://php.net/manual/en/function.ftp-get.php
	**/
	static function ftp_get(ftp: Resource, local_filename: String, remote_filename: String, offset: Int = 0): Bool;

	/**
		@see http://php.net/manual/en/function.ftp-get-option.php
	**/
	static function ftp_get_option(ftp: Resource, option: Int): EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.ftp-login.php
	**/
	static function ftp_login(ftp: Resource, username: String, password: String): Bool;

	/**
		@see http://php.net/manual/en/function.ftp-mdtm.php
	**/
	static function ftp_mdtm(ftp: Resource, filename: String): Int;

	/**
		@see http://php.net/manual/en/function.ftp-mkdir.php
	**/
	static function ftp_mkdir(ftp: Resource, directory: String): EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.ftp-mlsd.php
	**/
	static function ftp_mlsd(ftp: Resource, directory: String): EitherType<NativeIndexedArray<NativeAssocArray<String>>, Bool>;

	/**
		@see http://php.net/manual/en/function.ftp-nb-continue.php
	**/
	static function ftp_nb_continue(ftp: Resource): Int;

	/**
		@see http://php.net/manual/en/function.ftp-nb-fget.php
	**/
	static function ftp_nb_fget(ftp: Resource, stream: Resource, remote_filename: String, ?mode: Int, offset: Int = 0): Int;

	/**
		@see http://php.net/manual/en/function.ftp-nb-fput.php
	**/
	static function ftp_nb_fput(ftp: Resource, remote_filename: String, stream: Resource, ?mode: Int, offset: Int = 0): Int;

	/**
		@see http://php.net/manual/en/function.ftp-nb-get.php
	**/
	static function ftp_nb_get(ftp: Resource, local_filename: String, remote_filename: String, offset: Int = 0): Int;

	/**
		@see http://php.net/manual/en/function.ftp-nb-put.php
	**/
	static function ftp_nb_put(ftp: Resource, remote_filename: String, local_filename: String, offset: Int = 0): EitherType<Int, Bool>;

	/**
		@see http://php.net/manual/en/function.ftp-nlist.php
	**/
	static function ftp_nlist(ftp: Resource, directory: String): EitherType<NativeIndexedArray<String>, Bool>;

	/**
		@see http://php.net/manual/en/function.ftp-pasv.php
	**/
	static function ftp_pasv(ftp: Resource, enable: Bool): Bool;

	/**
		@see http://php.net/manual/en/function.ftp-put.php
	**/
	static function ftp_put(ftp: Resource, remote_filename: String, local_filename: String, offset: Int = 0): Bool;

	/**
		@see http://php.net/manual/en/function.ftp-pwd.php
	**/
	static function ftp_pwd(ftp: Resource): EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.ftp-raw.php
	**/
	static function ftp_raw(ftp: Resource, command: String): NativeIndexedArray<String>;

	/**
		@see http://php.net/manual/en/function.ftp-rawlist.php
	**/
	static function ftp_rawlist(ftp: Resource, directory: String, recursive: Bool = false): EitherType<NativeIndexedArray<String>, Bool>;

	/**
		@see http://php.net/manual/en/function.ftp-rename.php
	**/
	static function ftp_rename(ftp: Resource, from: String, to: String): Bool;

	/**
		@see http://php.net/manual/en/function.ftp-rmdir.php
	**/
	static function ftp_rmdir(ftp: Resource, directory: String): Bool;

	/**
		@see http://php.net/manual/en/function.ftp-set-option.php
	**/
	static function ftp_set_option(ftp: Resource, option: Int, value: EitherType<Int, Bool>): Bool;

	/**
		@see http://php.net/manual/en/function.ftp-site.php
	**/
	static function ftp_site(ftp: Resource, command: String): Bool;

	/**
		@see http://php.net/manual/en/function.ftp-size.php
	**/
	static function ftp_size(ftp: Resource, filename: String): Int;

	/**
		@see http://php.net/manual/en/function.ftp-ssl-connect.php
	**/
	static function ftp_ssl_connect(hostname: String, port: Int = 21, timeout: Int = 90): EitherType<Resource, Bool>;

	/**
		@see http://php.net/manual/en/function.ftp-systype.php
	**/
	static function ftp_systype(ftp: Resource): EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.connection-aborted.php
	**/
	static function connection_aborted(): Int;

	/**
		@see http://php.net/manual/en/function.connection-status.php
	**/
	static function connection_status(): Int;

	/**
		@see http://php.net/manual/en/function.ignore-user-abort.php
	**/
	static function ignore_user_abort(enable: Null<Bool> = null): Int;

	/**
		@see http://php.net/manual/en/function.highlight-file.php
	**/
	static function highlight_file(filename: String, returns: Bool = false): EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.highlight-string.php
	**/
	static function highlight_string(string: String, returns: Bool = false): EitherType<String, Bool>;

	/**
		@see http://php.net/manual/en/function.php-strip-whitespace.php
	**/
	static function php_strip_whitespace(filename: String): String;

	/**
		@see http://php.net/manual/en/function.posix-getgid.php
	**/
	static function posix_getgid(): Int;

	/**
		@see http://php.net/manual/en/function.posix-getpid.php
	**/
	static function posix_getpid(): Int;

	/**
		@see http://php.net/manual/en/function.posix-getuid.php
	**/
	static function posix_getuid(): Int;

	/**
		@see http://php.net/manual/en/function.posix-setgid.php
	**/
	static function posix_setgid(group_id: Int): Bool;

	/**
		@see http://php.net/manual/en/function.posix-setuid.php
	**/
	static function posix_setuid(user_id: Int): Bool;
}
