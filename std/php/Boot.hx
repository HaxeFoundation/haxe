package php;

class Boot {
	public static function __trace(v,i : haxe.PosInfos) {
		var msg = if( i != null ) i.fileName+":"+i.lineNumber+": " else "";
		untyped __call__("echo", msg+ __string_rec(v)+"<br/>"); // TODO: __unhtml
	}

	static public function __anonymous(?p : Dynamic) : Dynamic {
		untyped __php__("$o = new Anonymous();
		if(is_array($p)) {
			foreach($p as $k => $v) {
				$o->$k = $v;
			}
		}
		return $o");
	}

	static private var __cid = 0;
	static public var __scopes = [];
	static public function __closure(locals : ArrayAccess<Dynamic>, params : String, body : String) : String {
		var cid = __cid++;
		var n = "__closure__"+cid+"__";
//		if(locals == null) locals = [];
		untyped __php__("php_Boot::$__scopes[$n] = array('scope' => null, 'locals' => $locals)");
		var f : String = untyped __call__(
			"create_function",
			params,
			"$__this =& php_Boot::$__scopes['"+n+"']['scope'];\nforeach(array_keys(php_Boot::$__scopes['"+n+"']['locals']) as ${'%k'}) ${${'%k'}} =& php_Boot::$__scopes['"+n+"']['locals'][${'%k'}];\n"+body);
		var nl = "__"+f.substr(1)+"__";
		untyped __php__("php_Boot::$__scopes[$nl] =& php_Boot::$__scopes[$n]");
		return f;
	}

	static public function __is_lambda(s : String) : Bool {
		return untyped __call__("is_string", s) && s.substr(0, 8) == __call__("chr", 0) + "lambda_";
	}

	static public function __array() : Dynamic {
		return untyped __call__("func_get_args");
	}

	static public function __array_empty() : Dynamic {
		return untyped __call__("array");
	}

	static public function __null() : Dynamic {
		return null;
	}

	static public function __array_copy(a : ArrayAccess<Dynamic>) : Dynamic {
		return cast a;
	}

	static public function __array_iterator<T>(arr : Dynamic) : Iterator<T> {
		return untyped __php__("new HArrayIterator($arr)");
	}

	static public function __array_sort<T>(arr : Array<T>, f : T -> T -> Int) : Void {
		var i = 0;
		var l = arr.length;
		while( i < l ) {
			var swap = false;
			var j = 0;
			var max = l - i - 1;
			while( j < max ) {
				if( f(arr[j],arr[j+1]) > 0 ) {
					var tmp = arr[j+1];
					arr[j+1] = arr[j];
					arr[j] = tmp;
					swap = true;
				}
				j += 1;
			}
			if(!swap) break;
			i += 1;
		}
	}

	static public function __array_insert<T>(arr : Array<T>,  pos : Int, x : T) : Void {
		untyped __php__("array_splice")(arr, pos, 0, __call__("array", x));
	}

	static public function __array_remove<T>(arr : Array<T>, x : T) : Bool {
		for(i in 0...arr.length)
			if(arr[i] == x) {
				untyped __call__("unset", arr[i]);
				arr = untyped __call__("array_values", arr);
				return true;
			}
		return false;
	}

	static public function __array_remove_at(arr : Array<Dynamic>, pos : Int) : Bool {
		if(untyped __php__("array_key_exists")(pos, arr)) {
			untyped __php__("unset")(arr[pos]);
			return true;
		} else return false;
	}

	static public function __array_splice(arr : Array<Dynamic>, pos : Int, len : Int) : Bool {
		if(len < 0) len = 0;
		return untyped __php__("array_splice")(arr, pos, len);
	}

	static public function __array_slice(arr : Array<Dynamic>, pos : Int, ?end : Int) : Bool {
		if(end == null)
			return untyped __php__("array_slice")(arr, pos);
		else
			return untyped __php__("array_slice")(arr, pos, end-pos);
	}

	static public function __array_set<T>(arr : Array<Dynamic>, pos : Int, v : T) : T untyped {
		if(__call__("is_int", pos)) {
			var l = __call__("count", arr);
			if(l < pos) {
			__call__("array_splice", arr, l, 0, __call__("array_fill", l, pos-l, null));
			}
		}
		__php__("$arr[$pos] = $v");
		return v;
	}

	static public function __char_code_at(s : String, pos : Int) : Null<Int> untyped {
		if(__call__("empty", s) || pos >= s.length) return null;
		return s.cca(pos);
	}

	static public function __substr(s : String, pos : Int, ?len : Int) {
		if( pos != null && pos != 0 && len != null && len < 0 ) return '';
		if( len == null ) len = s.length;
		if( pos < 0 ) {
			pos = s.length + pos;
			if( pos < 0 ) pos = 0;
		} else if( len < 0 )
			len = s.length + len - pos;
		var s : Bool = untyped __php__("substr")(s, pos, len);
		if(untyped __physeq__(s, false)) return "" else return untyped s;
	}

	static public function __index_of(s : String, value : String, ?startIndex : Int) {
		var x = untyped __php__("strpos")(s, value, startIndex);
		if(untyped __physeq__(x, false))
			return -1;
		else
			return x;
	}

	static public function __last_index_of(s : String, value : String, ?startIndex : Int) {
		var x = untyped __php__("strrpos")(s, value, startIndex == null ? null : s.length - startIndex);
		if(untyped __php__("$x === false"))
			return -1
		else
			return x;
	}

	static public function __instanceof(v : Dynamic, t : Dynamic) {
		if(t == null) return false;
		switch(t.__tname__) {
			case "Array":
				return untyped __call__("is_array", v);
			case "String":
				return untyped __call__("is_string", v) && !__is_lambda(v);
			case "Bool":
				return untyped __call__("is_bool", v);
			case "Int":
				return untyped __call__("is_int", v);
			case "Float":
				return untyped __call__("is_float", v) || __call__("is_int", v);
			case "Dynamic":
				return true;
			case "Class":
				return untyped __php__("$v instanceof __classtype__") && __php__("$v->__tname__ != 'Enum'");
			case "Enum":
				return untyped __php__("$v instanceof __enumtype__");
			default:
				return untyped __call__("is_a", v, t.__tname__);
		}
	}

	static public function __shift_right(v : Int, n : Int) {
		untyped __php__("$z = 0x80000000;
		if ($z & $v) {
			$v = ($v>>1);
			$v &= (~$z);
			$v |= 0x40000000;
			$v = ($v>>($n-1));
		} else $v = ($v>>$n)");
		return v;
	}

	static public function __error_handler(errno : Int, errmsg : String, filename : String, linenum : Int, vars : Dynamic) {
		var msg = errmsg + " (errno: " + errno + ") in " + filename + " at line #" + linenum;
		var e = new php.HException(msg, errmsg, errno);
		e.setFile(filename);
		e.setLine(linenum);
		untyped __php__("throw $e");
		return null;
	}

	static public function __exception_handler(e : Dynamic) {
		var msg = "<pre>Uncaught exception: <b>"+e.getMessage()+"</b>\nin file: <b>"+e.getFile()+"</b> line <b>"+e.getLine()+"</b>\n\n"+e.getTraceAsString()+"</pre>";
		untyped __php__("die($msg)");
	}

	static public function __equal(x : Dynamic, y : Dynamic) untyped {
		if(__call__("is_null", x)) {
			return __call__("is_null", y);
		} else if(__call__("is_null", y)) {
			return false;
		} else  if((__call__("is_float", x) || __call__("is_int", x)) && (__call__("is_float", y) || __call__("is_int", y))) {
			return __php__("$x == $y");
		} else {
			return __php__("$x === $y");
		}
	}

	static private var __qtypes : Array<Dynamic>;
	static private var __ttypes : Array<Dynamic>;
	static private var __tpaths : Array<Dynamic>;
	static public function __register_type(t : Dynamic) {
		untyped __php__(
		"php_Boot::$__qtypes[$t->__qname__] = $t;
		php_Boot::$__ttypes[$t->__tname__] = $t;
		if($t->__path__ !== null)
			php_Boot::$__tpaths[$t->__tname__] = $t->__path__");
	}

	static public function __qtype(n) untyped {
		if(__call__("isset", __qtypes[n]))
			return __qtypes[n];
		else
			return null;
	}

	static public function __ttype(n) untyped {
		if(__call__("isset", __ttypes[n]))
			return __ttypes[n];
		else
			return null;
	}

	static public function __deref(byref__o : Dynamic) {
		return byref__o;
	}

	static public function __byref__array_get(byref__o : Dynamic, index : Dynamic) {
		return untyped byref__o[index];
	}

	static private var __resources = [];
	static public function __res(n : String) : String untyped {
		if(! __php__("isset(self::$__resources[$n])")) {
			var file = __php__("dirname(__FILE__).'/../../res/'.$n");
			if(!__call__("file_exists", file))
				throw "Invalid Resource name: " + n;
			__php__("self::$__resources[$n] = file_get_contents($file)");
		}
		return __php__("self::$__resources[$n]");
	}

	public static function __string_call(s : Dynamic, method : String, params : ArrayAccess<Dynamic>) {
		if(!untyped __call__("is_string", s)) return untyped __php__("call_user_func_array(array($s, $method), $params)");
		var s2 : String = s;
		switch(method) {
			case "toUpperCase": return untyped s2.toUpperCase();
			case "toLowerCase": return untyped s2.toLowerCase();
			case "charAt"     : return untyped s2.charAt(params[0]);
			case "charCodeAt" : return untyped s2.charCodeAt(params[0]);
			case "indexOf"    : return untyped s2.indexOf(params[0], params.length > 1 ? params[1] : null);
			case "lastIndexOf": return untyped s2.lastIndexOf(params.length > 1 ? params[1] : null);
			case "split"      : return untyped s2.split(params[0]);
			case "substr"     : return untyped s2.substr(params[0], params.length > 1 ? params[1] : null);
			default: throw "Invalid Operation: " + method;
		}
	}

	public static function __array_call(arr : Array<Dynamic>, method : String, params : ArrayAccess<Dynamic>) {
		if(!untyped __call__("is_array", arr)) return untyped __php__("call_user_func_array(array($arr, $method), $params)");
		switch(method) {
			case "concat"  : return untyped arr.concat(params[0]);
			case "copy"    : return untyped arr.copy();
			case "insert"  : return untyped arr.insert(params[0], params[1]);
			case "iterator": return untyped arr.iterator();
			case "join"    : return untyped arr.join(params[0]);
			case "pop"     : return untyped arr.pop();
			case "push"    : return untyped arr.push(params[0]);
			case "remove"  : return untyped arr.remove(params[0]);
			case "reverse" : return untyped arr.reverse();
			case "shift"   : return untyped arr.shift();
			case "slice"   : return untyped arr.slice(params[0], params.length > 1 ? params[1] : null);
			case "sort"    : return untyped arr.sort(params[0]);
			case "splice"  : return untyped arr.splice(params[0], params[1]);
			case "unshift" : return untyped arr.unshift(params[0]);
			default: throw "Invalid Operation: " + method;
		}
	}

	public static function __len(o : Dynamic) {
		return untyped __php__("is_array($o) ? count($o) : (is_string($o) ? strlen($o) : $o->length)");
	}

//	public static var __REGISTERED_CLASSES : Array<Dynamic>;

	static function __init__() untyped {
		__php__("//error_reporting(0);
set_error_handler(array('php_Boot', '__error_handler'), E_ALL);
set_exception_handler(array('php_Boot', '__exception_handler'));

class Anonymous extends stdClass{
	public function __call($m, $a) {
		$v = $this->$m;
		if(is_string($v) && substr($v, 0, 8) == chr(0).'lambda_') {
			$nl = '__'.substr($v, 1).'__';
			php_Boot::$__scopes[$nl]['scope'] =& $this;
		}
		try {
			return call_user_func_array($v, $a);
		} catch(Exception $e) {
			throw new HException('Unable to call «'.$m.'»');
		}
	}

	public function __set($n, $v) {
		$this->$n = $v;
	}

	public function &__get($n) {
		if(isset($this->$n))
			return $this->$n;
		$null = null;
		return $null;
	}

	public function __isset($n) {
		return isset($this->$n);
	}

	public function __unset($n) {
		unset($this->$n);
	}

	public function __toString() {
		return php_Boot::__string_rec($this, null);
	}
}

class __type__ {
	public $__tname__;
	public $__qname__;
	public $__path__;
	public function __construct($cn, $qn, $path = null) {
		$this->__tname__ = $cn;
		$this->__qname__ = $qn;
		$this->__path__ = $path;
	}

	public function toString()   { return $this->__toString(); }

	public function __toString() {
		return $this->__qname__;
	}

	private $rfl = false;
	public function __rfl__() {
		if($this->rfl !== false) return $this->rfl;
		if(class_exists($this->__tname__))
			$this->rfl = new ReflectionClass($this->__tname__);
		else
			$this->rfl = null;
		return $this->rfl;
	}

	public function __call($n, $a) {
		return call_user_func_array(array($this->__tname__, $n), $a);
	}

	public function __get($n) {
		if(($r = $this->__rfl__())==null) return null;
		if($r->hasProperty($n))
			return $r->getStaticPropertyValue($n);
		else if($r->hasMethod($n))
			return array($r, $n);
		else
			return null;
	}

	public function __set($n, $v) {
		if(($r = $this->__rfl__())==null) return null;
		return $r->setStaticPropertyValue($n, $v);
	}

	public function __isset($n) {
		if(($r = $this->__rfl__())==null) return null;
		return $r->hasProperty($n) || $r->hasMethod($n);
	}
}

class __classtype__ extends __type__ { }

class __enumtype__ extends __type__ {}

class __interfacetype__ extends __type__ { }

class HArrayIterator {
	private $a;
	private $i;
	public function __construct($a) {
		$this->a = $a;
		$this->i = 0;
	}

	public function next() {
		if(!$this->hasNext()) return null;
		return $this->a[$this->i++];
	}

	public function hasNext() {
		return $this->i < count($this->a);
	}
}

class HException extends Exception {
	public function __construct($e, $message = null, $code = null, $p = null) { if( !php_Boot::$skip_constructor ) {
		$message = php_Boot::__string_rec($e, null) . $message;
		parent::__construct($message,$code);
		$this->e = $e;
		$this->p = $p;
	}}
	public $e;
	public $p;
	public function setLine($l) {
		$this->line = $l;
	}
	public function setFile($f) {
		$this->file = $f;
	}
}

class enum {
	public function __construct($tag, $index, $params = null) { $this->tag = $tag; $this->index = $index; $this->params = $params; }
	public $tag;
	public $index;
	public $params;

	public function __toString() {
		return $this->tag;
	}
}

php_Boot::$__qtypes = array();
php_Boot::$__ttypes = array();
php_Boot::$__tpaths = array();
php_Boot::__register_type(new __classtype__('String',  'String'));
php_Boot::__register_type(new __classtype__('Array',   'Array'));
php_Boot::__register_type(new __classtype__('Int',     'Int'));
php_Boot::__register_type(new __classtype__('Float',   'Float'));
php_Boot::__register_type(new __classtype__('Class',   'Class'));
php_Boot::__register_type(new __classtype__('Enum',    'Enum'));
php_Boot::__register_type(new __classtype__('Dynamic', 'Dynamic'));
php_Boot::__register_type(new __enumtype__('Bool',     'Bool'));
php_Boot::__register_type(new __enumtype__('Void',     'Void'));


$_lib_dir = dirname(__FILE__) . '/..';
$_autload_cache_file = $_lib_dir . '/../cache/haxe_autoload.php';
if(!file_exists($_autload_cache_file)) {
	function buildPaths($d, &$a, $pack) {
		$h = opendir($d);
		while (false !== ($f = readdir($h))) {
			$p = $d.'/'.$f;
			if($f == '.' || $f == '..')
				continue;
			if(is_file($p) && substr($f, -4) == '.php') {
				$bn = basename($f, '.php');
				if(substr($bn, -6) == '.class') {
					$bn = substr($bn, 0, -6);
					$t = 0;
				} else if(substr($bn, -5) == '.enum') {
					$bn = substr($bn, 0, -5);
					$t = 1;
				} else if(substr($bn, -10) == '.interface') {
					$bn = substr($bn, 0, -10);
					$t = 2;
				} else
					continue;
				$qname = ($bn == 'HList' && empty($pack)) ? 'List' : join(array_merge($pack, array($bn)), '.');
				$a[] = array(
					'path' => $p,
					'name' => $bn,
					'type' => $t,
					'qname' => $qname,
					'phpname' => join(array_merge($pack, array($bn)), '_')
				);
			} else if(is_dir($p))
				buildPaths($p, $a, array_merge($pack, array($f)));
		}
		closedir($h);
	}

	$a = array();

	buildPaths($_lib_dir, $a, array());

	$content = '<?php\n\n';
	for($i=0;$i<count($a);$i++) {
		$content .= 'php_Boot::__register_type(new ';
		$t = null;
		if($a[$i]['type'] == 0) {
			$t = new __classtype__($a[$i]['phpname'], $a[$i]['qname'], $a[$i]['path']);
			$content .= '__classtype__';
		} else if($a[$i]['type'] == 1) {
			$t = new __enumtype__($a[$i]['phpname'], $a[$i]['qname'], $a[$i]['path']);
			$content .= '__enumtype__';
		} else {
			$t = new __interfacetype__($a[$i]['phpname'], $a[$i]['qname'], $a[$i]['path']);
			$content .= '__interfacetype__';
		}
		php_Boot::__register_type($t);
		$content .= '(\\''.$a[$i]['phpname'].'\\', \\''.$a[$i]['qname'].'\\', \\''.$a[$i]['path'].'\\'));\n';
	}
	try {
		file_put_contents($_autload_cache_file, $content);
	} catch(Exception $e) {}
} else {
	require($_autload_cache_file);
}

function __haxe_autoload($name) {
	if(!isset(php_Boot::$__tpaths[$name])) return false;
	require_once php_Boot::$__tpaths[$name];
	return true;
}
spl_autoload_register('__haxe_autoload')");
	}

	static public function __string(o : Dynamic) {
		if( o == null )
			return "null";
		if(untyped __call__("is_int", o) || __call__("is_float", o))
			return o;
		if(untyped __call__("is_bool", o))
			return o ? "true" : "false";
		if(untyped __call__("is_object", o)) {
			var c = untyped __call__("get_class", o);
			if(untyped __php__("$o instanceof Anonymous")) {
				return "Object";
			} else if(untyped __php__("$o instanceof __type__")) {
				return untyped __qtype(o.__qname__);
			} else {
				if(untyped __call__("is_callable", [o, "__toString"]))
					return o.__toString();
				else
					return "[" + __ttype(c) + "]";
			}
		}

		if(untyped __call__("is_string", o)) {
			if(__is_lambda(o)) return "«function»";
			return o;
		}
		if(untyped __call__("is_array", o)) {
			if(untyped __call__("is_callable", o)) return "«function»";
			return "Array";
		}

		return '';
	}

	static public function __string_rec(o : Dynamic, ?s : String) {
		if( o == null )
			return "null";
		if( s.length >= 5 )
			return "<...>"; // too much deep recursion

		if(untyped __call__("is_int", o) || __call__("is_float", o))
			return o;

		if(untyped __call__("is_bool", o))
			return o ? "true" : "false";

		if(untyped __call__("is_object", o)) {
			var c = untyped __call__("get_class", o);
			if(untyped __php__("$o instanceof enum")) {
				var b : String = o.tag;
				if(!untyped __call__("empty", o.params)) {
					s += "\t";
					b += '(';
					for( i in 0...untyped __call__("count", o.params) ) {
						if(i > 0)
							b += ', ' + __string_rec(o.params[i],s);
						else
							b += __string_rec(o.params[i],s);
					}
					b += ')';
				}
				return b;
			} else if(untyped __php__("$o instanceof Anonymous")) {
				var rfl = untyped __php__("new ReflectionObject($o)");
				var b = "{\n";
				s += "\t";
				var properties : Array<Dynamic> = rfl.getProperties();
				for(prop in properties) {
					var f : String = prop.getName();
					if(b.length != 2)
						b += ", \n";
					b += s + f + " : " + __string_rec(prop.getValue(o), s);
				}
				s = s.substr(1);
				b += "\n" + s + "}";
				return b;
			} else if(untyped __php__("$o instanceof __type__")) {
				return untyped o.__qname__;
			} else {
				if(untyped __call__("is_callable", [o, "toString"]))
					return untyped __php__("$o->toString()");
				else if(untyped __call__("is_callable", [o, "__toString"]))
					return o.__toString();
				else
					return "[" + __ttype(c) + "]";
			}
		}

		if(untyped __call__("is_string", o)) {
			if(__is_lambda(o)) return "«function»";
			if(s != null)
				return '"'+untyped __call__("str_replace", '"', '\\"', o)+'"';
			else
				return o;
		}

		if(untyped __call__("is_array", o)) {
			if(untyped __call__("is_callable", o)) return "«function»";
			var str = "[";
			s += "\t";
			for( i in 0...untyped __call__("count", o) )
				str += (if (i > 0) ", " else "")+__string_rec(untyped o[i],s);
			str += "]";
			return str;
		}

		return '';
	}
	static public var skip_constructor = false;
}