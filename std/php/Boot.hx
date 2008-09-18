package php;

class Boot {
	static var qtypes;
	static var ttypes;
	static var tpaths;
	static var skip_constructor = false;
	static function __init__() : Void {
		untyped __php__("//error_reporting(0);

function _hx_register_type($t) {
	php_Boot::$qtypes[$t->__qname__] = $t;
	php_Boot::$ttypes[$t->__tname__] = $t;
	if($t->__path__ !== null)
		php_Boot::$tpaths[$t->__tname__] = $t->__path__;
}

function _hx_qtype($n) {
	return isset(php_Boot::$qtypes[$n]) ? php_Boot::$qtypes[$n] : null;
}

function _hx_ttype($n) {
	return isset(php_Boot::$ttypes[$n]) ? php_Boot::$ttypes[$n] : null;
}

function _hx_error_handler($errno, $errmsg, $filename, $linenum, $vars) {
	$msg = $errmsg . ' (errno: ' . $errno . ') in ' . $filename . ' at line #' . $linenum;
	$e = new HException($msg, $errmsg, $errno, _hx_anonymous(array('fileName' => 'Boot.hx', 'lineNumber' => 41, 'className' => 'php.Boot', 'methodName' => '__error_handler')));
	$e->setFile($filename);
	$e->setLine($linenum);
	throw $e;
	return null;
}

function _hx_exception_handler($e) {
	$msg = '<pre>Uncaught exception: <b>' . $e->getMessage() . '</b>\nin file: <b>' . $e->getFile() . '</b> line <b>' . $e->getLine() . '</b>\n\n' . $e->getTraceAsString() . '</pre>';
	die($msg);
}

set_error_handler('_hx_error_handler', E_ALL);
set_exception_handler('_hx_exception_handler');

function _hx_instanceof($v, $t) {
	if($t === null) {
		return false;
	}
	switch($t->__tname__) {
		case 'Array'  : return is_array($v);
		case 'String' : return is_string($v) && !_hx_is_lambda($v);
		case 'Bool'   : return is_bool($v);
		case 'Int'    : return is_int($v);
		case 'Float'  : return is_float($v) || is_int($v);
		case 'Dynamic': return true;
		case 'Class'  : return $v instanceof _hx_class && $v->__tname__ != 'Enum';
		case 'Enum'   : return $v instanceof _hx_enum;
		default       : return is_a($v, $t->__tname__);
	}
}

function _hx_deref(&$o) {
	return $o;
}

function &_hx_array_get_ref(&$o, $index) {
	$r = null;
	if(isset($o[$index])) $r =& $o[$index];
	return $r;
}
/*
function _hx_res($n) {
	$file = dirname(__FILE__).'/../../res/'.$n;
	if(!file_exists($file))
		throw new HException('Invalid Resource name: ' . $n);
	return file_get_contents($file);
}
*/

function _hx_string_call($s, $method, $params) {
	if(!is_string($s)) return call_user_func_array(array($s, $method), $params);
	switch($method) {
		case 'toUpperCase': return strtoupper($s);
		case 'toLowerCase': return strtolower($s);
		case 'charAt'     : return substr($s, $params[0], 1);
		case 'charCodeAt' : return _hx_char_code_at($s, $params[0]);
		case 'indexOf'    : return _hx_index_of($s, $params[0], (_hx_len($params) > 1 ? $params[1] : null));
		case 'lastIndexOf': return _hx_last_index_of($s, (_hx_len($params) > 1 ? $params[1] : null), null);
		case 'split'      : return explode($params[0], $s);
		case 'substr'     : return _hx_substr($s, $params[0], (_hx_len($params) > 1 ? $params[1] : null));
		default           : throw new HException('Invalid Operation: ' . $method);
	}
}

function _hx_array_call(&$arr, $method, $params) {
	if(!is_array($arr)) return call_user_func_array(array($arr, $method), $params);
	switch($method) {
		case 'concat'  : return array_merge($arr, $params[0]);
		case 'copy'    : return _hx_array_copy($arr);
		case 'insert'  : return _hx_array_insert($arr, $params[0], $params[1]);
		case 'iterator': return new _hx_array_iterator($arr);
		case 'join'    : return join($params[0], $arr);
		case 'pop'     : return array_pop($arr);
		case 'push'    : return array_push($arr, $params[0]);
		case 'remove'  : return _hx_array_remove($arr, $params[0]);
		case 'reverse' : return _hx_array_reverse($arr);
		case 'shift'   : return array_shift($arr);
		case 'slice'   : return _hx_array_slice($arr, $params[0], (_hx_len($params) > 1 ? $params[1] : null));
		case 'sort'    : return _hx_array_sort($arr, $params[0]);
		case 'splice'  : return _hx_array_splice($arr, $params[0], $params[1]);
		case 'unshift' : return array_unshift($arr, $params[0]);
		default        : throw new HException('Invalid Operation: ' . $method);
	}
}


function _hx_shift_right($v, $n) {
	$z = 0x80000000;
	if ($z & $v) {
		$v = ($v>>1);
		$v &= (~$z);
		$v |= 0x40000000;
		$v = ($v>>($n-1));
	} else $v = ($v>>$n);
	return $v;
}

function _hx_equal($x, $y) {
	if(is_null($x)) {
		return is_null($y);
	} else {
		if(is_null($y)) {
			return false;
		} else {
			if((is_float($x) || is_int($x)) && (is_float($y) || is_int($y))) {
				return $x == $y;
			} else {
				return $x === $y;
			}
		}
	}
}

function _hx_has_field($o, $field) {
	return
		(is_object($o) && (method_exists($o, $field) || isset($o->$field) || property_exists($o, $field)))
		||
		(is_string($o) && (in_array($field, array('toUpperCase', 'toLowerCase', 'charAt', 'charCodeAt', 'indexOf', 'lastIndexOf', 'split', 'substr', 'toString', 'length'))))
		||
		(is_array($o)  && (in_array($field, array('concat', 'copy', 'insert', 'iterator', 'join', 'pop', 'push', 'remove', 'reverse', 'shift', 'slice', 'sort', 'splice', 'unshift', 'toString', 'length'))))
	;
}

function _hx_field($o, $field) {
	if(_hx_has_field($o, $field)) {
		if($o instanceof _hx_type) {
			if(is_callable(array($o->__tname__, $field))) {
				return array($o->__tname__, $field);
			} else {
				return eval('return '.$o->__tname__.'::$'.$field.';');
			}
		} else {
			if(is_string($o)) {
				if($field == 'length') {
					return _hx_len($o);
				} else {
					switch($field) {
						case 'charAt'     : return _hx_closure(array('o' => &$o), null, array('index'), 'return substr($o,$index,1);');
						case 'charCodeAt' : return _hx_closure(array('o' => &$o), null, array('index'), 'return ord(substr($o, $index, 1));');
						case 'indexOf'    : return _hx_closure(array('o' => &$o), null, array('value','startIndex'), 'return _hx_index_of($o, $value, $startIndex);');
						case 'lastIndexOf': return _hx_closure(array('o' => &$o), null, array('value','startIndex'), 'return _hx_last_index_of($o, $value, $startIndex);');
						case 'split'      : return _hx_closure(array('o' => &$o), null, array('delimiter'), 'return explode($delimiter, $o);');
						case 'substr'     : return _hx_closure(array('o' => &$o), null, array('pos','len'), 'return _hx_substr($o, $pos, $len);');
						case 'toUpperCase': return _hx_closure(array('o' => &$o), null, array(), 'return strtoupper($o);');
						case 'toLowerCase': return _hx_closure(array('o' => &$o), null, array(), 'return strtolower($o);');
						case 'toString'   : return _hx_closure(array('o' => &$o), null, array(), 'return $o;');
					}
					return null;
				}
			}
			else {
				if(is_array($o)) {
					if($field == 'length') {
						return _hx_len($o);
					} else {
						switch($field) {
							case 'concat'  : return _hx_closure(array('o' => &$o), null, array('a'), 'return array_merge($o, $a);');
							case 'join'    : return _hx_closure(array('o' => &$o), null, array('sep'), 'return join($sep,$o);');
							case 'pop'     : return _hx_closure(array('o' => &$o), null, array(), 'return array_pop($o);');
							case 'push'    : return _hx_closure(array('o' => &$o), null, array('x'), 'return array_push($o,$x);');
							case 'reverse' : return _hx_closure(array('o' => &$o), null, array(), 'return _hx_reverse($o);');
							case 'shift'   : return _hx_closure(array('o' => &$o), null, array(), 'return array_shift($o);');
							case 'slice'   : return _hx_closure(array('o' => &$o), null, array('pos','end'), 'return _hx_array_slice($o, $pos, $end);');
							case 'sort'    : return _hx_closure(array('o' => &$o), null, array('f'), 'return _hx_array_sort($o,$f);');
							case 'splice'  : return _hx_closure(array('o' => &$o), null, array('pos','len'), 'return _hx_array_splice($o, $pos, $len);');
							case 'toString': return _hx_closure(array('o' => &$o), null, array(), 'return \"[\".join(\", \", $o).\"]\";');
							case 'unshift' : return _hx_closure(array('o' => &$o), null, array('x'), 'return array_unshift($o,$x);');
							case 'insert'  : return _hx_closure(array('o' => &$o), null, array('pos','x'), 'return _hx_array_insert($o, $pos, $x);');
							case 'remove'  : return _hx_closure(array('o' => &$o), null, array('x'), '_hx_array_remove($o, $x);');
							case 'iterator': return _hx_closure(array('o' => &$o), null, array(), 'return new _hx_array_iterator($o);');
							case 'copy'    : return _hx_closure(array('o' => &$o), null, array(), 'return $o;');
						}
					}
					return null;
				} else {
					if(property_exists($o, $field)) {
						if(is_array($o->$field) && is_callable($o->$field)) {
							return $o->$field;
						} else {
							if(is_string($o->$field) && _hx_is_lambda($o->$field)) {
								return array($o, $field);
							} else {
								return $o->$field;
							}
						}
					} else {
						return array($o, $field);
					}
				}
			}
		}
	} else {
		return null;
	}
}

function _hx_string_rec($o, $s) {
	if($o === null)                return 'null';
	if(strlen($s) >= 5)            return '<...>';
	if(is_int($o) || is_float($o)) return $o;
	if(is_bool($o))                return $o ? 'true' : 'false';
	if(is_object($o)) {
		$c = get_class($o);
		if($o instanceof Enum) {
			$b = $o->tag;
			if(!empty($o->params)) {
				$s .= \"\t\";
				$b .= '(';
				for($i = 0; $i < count($o->params); $i++) {
					if($i > 0)
						$b .= ', ' . _hx_string_rec($o->params[$i], $s);
					else
						$b .= _hx_string_rec($o->params[$i], $s);
				}
				$b .= ')';
			}
			return $b;
		} else {
			if($o instanceof _hx_anonymous) {
				$rfl = new ReflectionObject($o);
				$b2 = \"{\n\";
				$s .= \"\t\";
				$properties = $rfl->getProperties();

				for($i = 0; $i < count($properties); $i++) {
					$prop = $properties[$i];
					$f = $prop->getName();
					if(strlen($b2) !== 2)
						$b2 .= \", \n\";
					$b2 .= $s . $f . ' : ' . _hx_string_rec($prop->getValue($o), $s);
				}
				$s = substr($s, 1);
				$b2 .= \"\n\" . $s . '}';
				return $b2;
			} else {
				if($o instanceof _hx_type)
					return $o->__qname__;
				else {
					if(is_callable(_hx_array($o, 'toString')))
						return $o->toString();
					else {
						if(is_callable(_hx_array($o, '__toString')))
							return $o->__toString();
						else
							return '[' . _hx_ttype($c) . ']';
					}
				}
			}
		}
	}
	if(is_string($o)) {
		if(_hx_is_lambda($o)) return '«function»';
		if(strlen($s) > 0)    return '\"' . str_replace('\"', '\\\"', $o) . '\"';
		else                  return $o;
	}
	if(is_array($o)) {
		if(is_callable($o)) return '«function»';
		$str = '[';
		$s .= \"\t\";
		for($i = 0; $i < count($o); $i++)
			$str .= ($i > 0 ? ', ' : '') . _hx_string_rec($o[$i], $s);
		$str .= ']';
		return $str;
	}
	return '';
}


function _hx_array_copy($a) {
	return $a;
}

function _hx_array_iterator($arr) {
	return new _hx_array_iterator($arr);
}

function _hx_array_insert(&$arr, $pos, $x) {
	array_splice($arr, $pos, 0, array($x));
}

function _hx_array() {
	return func_get_args();
}

function _hx_array_empty() {
	return array();
}

function _hx_array_remove(&$arr, $x) {
	for($i = 0; $i < count($arr); $i++)
		if($arr[$i] === $x) {
			unset($arr[$i]);
			$arr = array_values($arr);
			return true;
		}
	return false;
}

function _hx_array_remove_at(&$arr, $pos) {
	if(array_key_exists($pos, $arr)) {
		unset($arr[$pos]);
		return true;
	} else
		return false;
}

function _hx_array_sort(&$arr, $f) {
	$i = 0;
	$l = count($arr);
	while($i < $l) {
		$swap = false;
		$j = 0;
		$max = $l - $i - 1;
		while($j < $max) {
			if(call_user_func($f, $arr[$j], $arr[$j+1]) > 0 ) {
				$tmp = $arr[$j+1];
				$arr[$j+1] = $arr[$j];
				$arr[$j] = $tmp;
				$swap = true;
			}
			$j += 1;
		}
		if(!$swap) break;
		$i += 1;
	}
}

function _hx_array_splice(&$arr, $pos, $len) {
	if($len < 0) $len = 0;
	return array_splice($arr, $pos, $len);
}

function _hx_array_slice(&$arr, $pos, $end) {
	if($end == null)
		return array_slice($arr, $pos);
	else
		return array_slice($arr, $pos, $end-$pos);
}

function _hx_null() {
	return null;
}

function _hx_anonymous($p = array()) {
	$o = new _hx_anonymous();
	foreach($p as $k => $v)
		$o->$k = $v;
	return $o;
}

function _hx_trace($v, $i) {
	$msg = $i !== null ? $i->fileName.':'.$i->lineNumber.': ' : '';
	echo $msg._hx_string_rec($v, '').\"\n\";
}

function _hx_len($o) {
	return is_array($o) ? count($o) : (is_string($o) ? strlen($o) : $o->length);
}

function _hx_array_reverse(&$a) {
	$a = array_reverse($a, false);
}

function _hx_array_set(&$arr, $pos, $v) {
	if(is_int($pos)) {
		$l = count($arr);
		if($l < $pos)
			array_splice($arr, $l, 0, array_fill($l, $pos-$l, null));
	}
	return $arr[$pos] = $v;
}

function _hx_char_code_at($s, $pos) {
	if(empty($s) || $pos >= strlen($s)) return null;
	return ord($s{$pos});
}

function _hx_substr($s, $pos, $len) {
	if($pos !== null && $pos !== 0 && $len !== null && $len < 0) return '';
	if($len === null) $len = strlen($s);
	if($pos < 0) {
		$pos = strlen($s) + $pos;
		if($pos < 0) $pos = 0;
	} else if($len < 0 )
		$len = strlen($s) + $len - $pos;
	$s = substr($s, $pos, $len);
	if($s === false)
		return '';
	else
		return $s;
}

function _hx_index_of($s, $value, $startIndex) {
	$x = strpos($s, $value, $startIndex);
	if($x === false)
		return -1;
	else
		return $x;
}

function _hx_last_index_of($s, $value, $startIndex) {
	$x = strrpos($s, $value, $startIndex === null ? null : strlen($s) - $startIndex);
	if($x === false)
		return -1;
	else
		return $x;
}

class _hx_anonymous extends stdClass {
	public function __call($m, $a) {
		$v = $this->$m;
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
		$rfl = new ReflectionObject($this);
		$b = '{ ';
		$properties = $rfl->getProperties();
		foreach($properties as $prop) {
			$f = $prop->getName();
			if(strlen($b) > 2)
				$b .= ', ';
			$b .= $f . ' => ' . $prop->getValue($this);
		}
		$b .= ' }';
		return $b;
	}
}

class _hx_type {
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

class _hx_class extends _hx_type {}

class _hx_enum extends _hx_type {}

class _hx_interface extends _hx_type { }

class _hx_array_iterator {
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
	public function __construct($e, $message = null, $code = null, $p = null) {
		$message = _hx_string_rec($e, '') . $message;
		parent::__construct($message,$code);
		$this->e = $e;
		$this->p = $p;
	}
	public $e;
	public $p;
	public function setLine($l) {
		$this->line = $l;
	}
	public function setFile($f) {
		$this->file = $f;
	}
}

// TODO: optimization, remove as much as possible the calls to this function
function _hx_closure($locals, $scope, $params, $body) {
	return array(new _hx_lambda($locals, $scope, $params, $body), 'execute'.count($params));
}

function _hx_is_lambda($s) {
	return (is_string($s) && substr($s, 0, 8) == chr(0).'lambda_') || (is_array($s) && count($s) > 0 && is_a($s[0], '_hx_lambda'));
}

class _hx_lambda {
	public function __construct($locals, $scope, $args, $body) {
		$this->locals = $locals;
		$this->scope = $scope;
		$this->args = $args;
		$this->body = $body;
	}
	public $locals;
	public $scope;
	public $args;
	public $body;

	public $params = array();
	public function execute() {
		$__this =& $this->scope;
		foreach(array_keys($this->locals) as ${'%k'})
			${${'%k'}} =& $this->locals[${'%k'}];
		for(${'%i'} = 0; ${'%i'} < count($this->args); ${'%i'}++)
			${$this->args[${'%i'}]} =& $this->params[${'%i'}];
		return eval($this->body);
	}

	public function makeArgs() {
		$this->params = array(func_get_args());
		return $this->execute();
	}

	public function execute0() {
		$this->params = array();
		return $this->execute();
	}

	public function execute1(&$_1) {
		if($this->scope == null) $this->scope= &$_1;
		$this->params = array(&$_1);
		return $this->execute();
	}

	public function execute2(&$_1, &$_2) {
		if($this->scope == null) $this->scope= &$_1;
		$this->params = array(&$_1, &$_2);
		return $this->execute();
	}

	public function execute3(&$_1, &$_2, &$_3) {
		if($this->scope == null) $this->scope= &$_1;
		$this->params = array(&$_1, &$_2, &$_3);
		return $this->execute();
	}

	public function execute4(&$_1, &$_2, &$_3, &$_4) {
		if($this->scope == null) $this->scope= &$_1;
		$this->params = array(&$_1, &$_2, &$_3, &$_4);
		return $this->execute();
	}

	public function execute5(&$_1, &$_2, &$_3, &$_4, &$_5) {
		if($this->scope == null) $this->scope= &$_1;
		$this->params = array(&$_1, &$_2, &$_3, &$_4, &$_5);
		return $this->execute();
	}

	public function execute6(&$_1, &$_2, &$_3, &$_4, &$_5, &$_6) {
		if($this->scope == null) $this->scope= &$_1;
		$this->params = array(&$_1, &$_2, &$_3, &$_4, &$_5, &$_6);
		return $this->execute();
	}

	public function execute7(&$_1, &$_2, &$_3, &$_4, &$_5, &$_6, &$_7) {
		if($this->scope == null) $this->scope= &$_1;
		$this->params = array(&$_1, &$_2, &$_3, &$_4, &$_5, &$_6, &$_7);
		return $this->execute();
	}
}

class Enum {
	public function __construct($tag, $index, $params = null) { $this->tag = $tag; $this->index = $index; $this->params = $params; }
	public $tag;
	public $index;
	public $params;

	public function __toString() {
		return $this->tag;
	}
}

php_Boot::$qtypes = array();
php_Boot::$ttypes = array();
php_Boot::$tpaths = array();

_hx_register_type(new _hx_class('String',  'String'));
_hx_register_type(new _hx_class('Array',   'Array'));
_hx_register_type(new _hx_class('Int',     'Int'));
_hx_register_type(new _hx_class('Float',   'Float'));
_hx_register_type(new _hx_class('Class',   'Class'));
_hx_register_type(new _hx_class('Enum',    'Enum'));
_hx_register_type(new _hx_class('Dynamic', 'Dynamic'));
_hx_register_type(new _hx_enum('Bool',     'Bool'));
_hx_register_type(new _hx_enum('Void',     'Void'));


$_hx_libdir = dirname(__FILE__) . '/..';
$_hx_autload_cache_file = $_hx_libdir . '/../cache/haxe_autoload.php';
if(!file_exists($_hx_autload_cache_file)) {
	function _hx_build_paths($d, &$_hx_types_array, $pack) {
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
				} else if(substr($bn, -7) == '.extern') {
					$bn = substr($bn, 0, -7);
					$t = 3;
				} else
					continue;
				$qname = ($bn == 'HList' && empty($pack)) ? 'List' : join(array_merge($pack, array($bn)), '.');
				$_hx_types_array[] = array(
					'path' => $p,
					'name' => $bn,
					'type' => $t,
					'qname' => $qname,
					'phpname' => join(array_merge($pack, array($bn)), '_')
				);
			} else if(is_dir($p))
				_hx_build_paths($p, $_hx_types_array, array_merge($pack, array($f)));
		}
		closedir($h);
	}

	$_hx_cache_content = '<?php\n\n';
	$_hx_types_array = array();

	_hx_build_paths($_hx_libdir, $_hx_types_array, array());

	for($i=0;$i<count($_hx_types_array);$i++) {
		$_hx_cache_content .= '_hx_register_type(new ';
		$t = null;
		if($_hx_types_array[$i]['type'] == 0) {
			$t = new _hx_class($_hx_types_array[$i]['phpname'], $_hx_types_array[$i]['qname'], $_hx_types_array[$i]['path']);
			$_hx_cache_content .= '_hx_class';
		} else if($_hx_types_array[$i]['type'] == 1) {
			$t = new _hx_enum($_hx_types_array[$i]['phpname'], $_hx_types_array[$i]['qname'], $_hx_types_array[$i]['path']);
			$_hx_cache_content .= '_hx_enum';
		} else if($_hx_types_array[$i]['type'] == 2) {
			$t = new _hx_interface($_hx_types_array[$i]['phpname'], $_hx_types_array[$i]['qname'], $_hx_types_array[$i]['path']);
			$_hx_cache_content .= '_hx_interface';
		} else if($_hx_types_array[$i]['type'] == 3) {
			$t = new _hx_class($_hx_types_array[$i]['name'], $_hx_types_array[$i]['qname'], $_hx_types_array[$i]['path']);
			$_hx_cache_content .= '_hx_class';
		}
		_hx_register_type($t);
		$_hx_cache_content .= '(\\''.($_hx_types_array[$i]['type'] == 3 ? $_hx_types_array[$i]['name'] : $_hx_types_array[$i]['phpname']).'\\', \\''.$_hx_types_array[$i]['qname'].'\\', \\''.$_hx_types_array[$i]['path'].'\\'));\n';
	}
	try {
		file_put_contents($_hx_autload_cache_file, $_hx_cache_content);
	} catch(Exception $e) {}
	unset($_hx_types_array);
	unset($_hx_cache_content);
} else {
	require($_hx_autload_cache_file);
}

function _hx_autoload($name) {
	if(!isset(php_Boot::$tpaths[$name])) return false;
	require_once(php_Boot::$tpaths[$name]);
	return true;
}

spl_autoload_register('_hx_autoload')");
	}
}