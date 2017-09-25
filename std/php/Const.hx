package php;

/**
	This class contains externs for native PHP constants defined in global namespace.
	For native PHP functions in global namespace see `php.Global`.
**/
@:phpGlobal
extern class Const {
	/**
		If this constant is defined and equals `true` then Haxe will not set error handler automatically.
	**/
	static var HAXE_CUSTOM_ERROR_HANDLER : Bool;
	/**
		@see http://php.net/manual/en/reserved.constants.php
	**/
	static var PHP_OS : String;
	static var PHP_SAPI : String;
	static var PHP_EOL : String;
	static var PHP_INT_MAX : Int;
	static var PHP_INT_MIN : Int;
	static var PHP_INT_SIZE : Int;
	/**
		@see http://php.net/manual/en/language.constants.predefined.php
	**/
	static var __LINE__ : Int;
	static var __FILE__ : String;
	static var __DIR__ : String;
	static var __FUNCTION__ : String;
	static var __CLASS__ : String;
	static var __TRAIT__ : String;
	static var __METHOD__ : String;
	static var __NAMESPACE__ : String;
	/**
		@see http://php.net/manual/en/errorfunc.constants.php
	**/
	static var E_ERROR : Int;
	static var E_WARNING : Int;
	static var E_PARSE : Int;
	static var E_NOTICE : Int;
	static var E_CORE_ERROR : Int;
	static var E_CORE_WARNING : Int;
	static var E_COMPILE_ERROR : Int;
	static var E_COMPILE_WARNING : Int;
	static var E_USER_ERROR : Int;
	static var E_USER_WARNING : Int;
	static var E_USER_NOTICE : Int;
	static var E_STRICT : Int;
	static var E_RECOVERABLE_ERROR : Int;
	static var E_DEPRECATED : Int;
	static var E_USER_DEPRECATED : Int;
	static var E_ALL : Int;
	/**
		@see http://php.net/manual/en/function.count.php
	**/
	static var COUNT_NORMAL : Int;
	static var COUNT_RECURSIVE : Int;
	/**
		@see http://php.net/manual/en/function.array-filter.php
	**/
	static var ARRAY_FILTER_USE_KEY : Int;
	static var ARRAY_FILTER_USE_BOTH : Int;
	/**
		@see http://php.net/manual/en/function.debug-backtrace.php
	**/
	static var DEBUG_BACKTRACE_PROVIDE_OBJECT : Int;
	static var DEBUG_BACKTRACE_IGNORE_ARGS : Int;
	/**
		@see http://php.net/manual/en/math.constants.php
	**/
	static var M_PI : Float;
	static var M_E : Float;
	static var M_LOG2E : Float;
	static var M_LOG10E : Float;
	static var M_LN2 : Float;
	static var M_LN10 : Float;
	static var M_PI_2 : Float;
	static var M_PI_4 : Float;
	static var M_1_PI : Float;
	static var M_2_PI : Float;
	static var M_SQRTPI : Float;
	static var M_2_SQRTPI : Float;
	static var M_SQRT2 : Float;
	static var M_SQRT3 : Float;
	static var M_SQRT1_2 : Float;
	static var M_LNPI : Float;
	static var M_EULER : Float;
	static var PHP_ROUND_HALF_UP : Int;
	static var PHP_ROUND_HALF_DOWN : Int;
	static var PHP_ROUND_HALF_EVEN : Int;
	static var PHP_ROUND_HALF_ODD : Int;
	static var NAN : Float;
	static var INF : Float;
	/**
		@see http://php.net/manual/en/function.setlocale.php
	**/
	static var LC_ALL : Int;
	static var LC_COLLATE : Int;
	static var LC_CTYPE : Int;
	static var LC_MONETARY : Int;
	static var LC_NUMERIC : Int;
	static var LC_TIME : Int;
	static var LC_MESSAGES : Int;
	/**
		@see http://php.net/manual/en/features.commandline.io-streams.php
	**/
	static var STDIN : Resource;
	static var STDOUT : Resource;
	static var STDERR : Resource;
	/**
		@see http://php.net/manual/en/function.preg-match-all.php
	**/
	static var PREG_PATTERN_ORDER : Int;
	static var PREG_SET_ORDER : Int;
	static var PREG_OFFSET_CAPTURE : Int;
	/**
		@see http://php.net/manual/en/function.preg-split.php
	**/
	static var PREG_SPLIT_NO_EMPTY : Int;
	static var PREG_SPLIT_DELIM_CAPTURE : Int;
	static var PREG_SPLIT_OFFSET_CAPTURE : Int;
	/**
		@see http://php.net/manual/en/function.htmlspecialchars.php
	**/
	static var ENT_COMPAT : Int;
	static var ENT_QUOTES : Int;
	static var ENT_NOQUOTES : Int;
	static var ENT_IGNORE : Int;
	static var ENT_SUBSTITUTE : Int;
	static var ENT_DISALLOWED : Int;
	static var ENT_HTML401 : Int;
	static var ENT_XML1 : Int;
	static var ENT_XHTML : Int;
	static var ENT_HTML5 : Int;
	/**
		@see http://php.net/manual/en/function.str-pad.php
	**/
	static var STR_PAD_RIGHT : Int;
	static var STR_PAD_LEFT : Int;
	static var STR_PAD_BOTH : Int;
	/**
		@see http://php.net/manual/en/function.feof.php
	**/
	static var SEEK_SET : Int;
	static var SEEK_CUR : Int;
	static var SEEK_END : Int;
	/**
		@see http://php.net/manual/en/function.stream-socket-server.php
	**/
	static var STREAM_SERVER_BIND : Int;
	static var STREAM_SERVER_LISTEN : Int;
	/**
		@see http://php.net/manual/en/function.stream-socket-client.php
	**/
	static var STREAM_CLIENT_CONNECT : Int;
	static var STREAM_CLIENT_ASYNC_CONNECT : Int;
	static var STREAM_CLIENT_PERSISTENT : Int;
	/**
		@see http://php.net/manual/en/function.socket-create.php
	**/
	static var SOCK_STREAM : Int;
	static var SOCK_DGRAM : Int;
	static var SOCK_SEQPACKET : Int;
	static var SOCK_RAW : Int;
	static var SOCK_RDM : Int;
	static var AF_INET : Int;
	static var AF_INET6 : Int;
	static var AF_UNIX : Int;
	/**
		@see http://php.net/manual/en/json.constants.php
	**/
	static var JSON_ERROR_NONE : Int;
	static var JSON_ERROR_DEPTH : Int;
	static var JSON_ERROR_STATE_MISMATCH : Int;
	static var JSON_ERROR_CTRL_CHAR : Int;
	static var JSON_ERROR_SYNTAX : Int;
	static var JSON_ERROR_UTF8 : Int;
	static var JSON_ERROR_RECURSION : Int;
	static var JSON_ERROR_INF_OR_NAN : Int;
	static var JSON_ERROR_UNSUPPORTED_TYPE : Int;
	static var JSON_HEX_TAG : Int;
	static var JSON_HEX_AMP : Int;
	static var JSON_HEX_APOS : Int;
	static var JSON_HEX_QUOT : Int;
	static var JSON_FORCE_OBJECT : Int;
	static var JSON_NUMERIC_CHECK : Int;
	static var JSON_BIGINT_AS_STRING : Int;
	static var JSON_PRETTY_PRINT : Int;
	static var JSON_UNESCAPED_SLASHES : Int;
	static var JSON_UNESCAPED_UNICODE : Int;
	static var JSON_PARTIAL_OUTPUT_ON_ERROR : Int;
	static var JSON_PRESERVE_ZERO_FRACTION : Int;
	/**
		@see http://php.net/manual/en/mysqli.constants.php
	**/
	static var MYSQLI_READ_DEFAULT_GROUP : Int;
	static var MYSQLI_READ_DEFAULT_FILE : Int;
	static var MYSQLI_OPT_CONNECT_TIMEOUT : Int;
	static var MYSQLI_OPT_LOCAL_INFILE : Int;
	static var MYSQLI_INIT_COMMAND : Int;
	static var MYSQLI_CLIENT_SSL : Int;
	static var MYSQLI_CLIENT_COMPRESS : Int;
	static var MYSQLI_CLIENT_INTERACTIVE : Int;
	static var MYSQLI_CLIENT_IGNORE_SPACE : Int;
	static var MYSQLI_CLIENT_NO_SCHEMA : Int;
	static var MYSQLI_CLIENT_MULTI_QUERIES : Int;
	static var MYSQLI_STORE_RESULT : Int;
	static var MYSQLI_USE_RESULT : Int;
	static var MYSQLI_ASSOC : Int;
	static var MYSQLI_NUM : Int;
	static var MYSQLI_BOTH : Int;
	static var MYSQLI_NOT_NULL_FLAG : Int;
	static var MYSQLI_PRI_KEY_FLAG : Int;
	static var MYSQLI_UNIQUE_KEY_FLAG : Int;
	static var MYSQLI_MULTIPLE_KEY_FLAG : Int;
	static var MYSQLI_BLOB_FLAG : Int;
	static var MYSQLI_UNSIGNED_FLAG : Int;
	static var MYSQLI_ZEROFILL_FLAG : Int;
	static var MYSQLI_AUTO_INCREMENT_FLAG : Int;
	static var MYSQLI_TIMESTAMP_FLAG : Int;
	static var MYSQLI_SET_FLAG : Int;
	static var MYSQLI_NUM_FLAG : Int;
	static var MYSQLI_PART_KEY_FLAG : Int;
	static var MYSQLI_GROUP_FLAG : Int;
	static var MYSQLI_TYPE_DECIMAL : Int;
	static var MYSQLI_TYPE_NEWDECIMAL : Int;
	static var MYSQLI_TYPE_BIT : Int;
	static var MYSQLI_TYPE_TINY : Int;
	static var MYSQLI_TYPE_SHORT : Int;
	static var MYSQLI_TYPE_LONG : Int;
	static var MYSQLI_TYPE_FLOAT : Int;
	static var MYSQLI_TYPE_DOUBLE : Int;
	static var MYSQLI_TYPE_NULL : Int;
	static var MYSQLI_TYPE_TIMESTAMP : Int;
	static var MYSQLI_TYPE_LONGLONG : Int;
	static var MYSQLI_TYPE_INT24 : Int;
	static var MYSQLI_TYPE_DATE : Int;
	static var MYSQLI_TYPE_TIME : Int;
	static var MYSQLI_TYPE_DATETIME : Int;
	static var MYSQLI_TYPE_YEAR : Int;
	static var MYSQLI_TYPE_NEWDATE : Int;
	static var MYSQLI_TYPE_INTERVAL : Int;
	static var MYSQLI_TYPE_ENUM : Int;
	static var MYSQLI_TYPE_SET : Int;
	static var MYSQLI_TYPE_TINY_BLOB : Int;
	static var MYSQLI_TYPE_MEDIUM_BLOB : Int;
	static var MYSQLI_TYPE_LONG_BLOB : Int;
	static var MYSQLI_TYPE_BLOB : Int;
	static var MYSQLI_TYPE_VAR_STRING : Int;
	static var MYSQLI_TYPE_STRING : Int;
	static var MYSQLI_TYPE_CHAR : Int;
	static var MYSQLI_TYPE_GEOMETRY : Int;
	static var MYSQLI_NEED_DATA : Int;
	static var MYSQLI_NO_DATA : Int;
	static var MYSQLI_DATA_TRUNCATED : Int;
	static var MYSQLI_ENUM_FLAG : Int;
	static var MYSQLI_BINARY_FLAG : Int;
	static var MYSQLI_CURSOR_TYPE_FOR_UPDATE : Int;
	static var MYSQLI_CURSOR_TYPE_NO_CURSOR : Int;
	static var MYSQLI_CURSOR_TYPE_READ_ONLY : Int;
	static var MYSQLI_CURSOR_TYPE_SCROLLABLE : Int;
	static var MYSQLI_STMT_ATTR_CURSOR_TYPE : Int;
	static var MYSQLI_STMT_ATTR_PREFETCH_ROWS : Int;
	static var MYSQLI_STMT_ATTR_UPDATE_MAX_LENGTH : Int;
	static var MYSQLI_SET_CHARSET_NAME : Int;
	static var MYSQLI_REPORT_INDEX : Int;
	static var MYSQLI_REPORT_ERROR : Int;
	static var MYSQLI_REPORT_STRICT : Int;
	static var MYSQLI_REPORT_ALL : Int;
	static var MYSQLI_REPORT_OFF : Int;
	static var MYSQLI_DEBUG_TRACE_ENABLED : Int;
	static var MYSQLI_SERVER_QUERY_NO_GOOD_INDEX_USED : Int;
	static var MYSQLI_SERVER_QUERY_NO_INDEX_USED : Int;
	static var MYSQLI_REFRESH_GRANT : Int;
	static var MYSQLI_REFRESH_LOG : Int;
	static var MYSQLI_REFRESH_TABLES : Int;
	static var MYSQLI_REFRESH_HOSTS : Int;
	static var MYSQLI_REFRESH_STATUS : Int;
	static var MYSQLI_REFRESH_THREADS : Int;
	static var MYSQLI_REFRESH_SLAVE : Int;
	static var MYSQLI_REFRESH_MASTER : Int;
	static var MYSQLI_TRANS_COR_AND_CHAIN : Int;
	static var MYSQLI_TRANS_COR_AND_NO_CHAIN : Int;
	static var MYSQLI_TRANS_COR_RELEASE : Int;
	static var MYSQLI_TRANS_COR_NO_RELEASE : Int;
	static var MYSQLI_TRANS_START_READ_ONLY : Int;
	static var MYSQLI_TRANS_START_READ_WRITE : Int;
	static var MYSQLI_TRANS_START_CONSISTENT_SNAPSHOT : Int;
	/**
		@see http://php.net/manual/en/sqlite3.constants.php
	**/
	static var SQLITE3_ASSOC : Int;
	static var SQLITE3_NUM : Int;
	static var SQLITE3_BOTH : Int;
	static var SQLITE3_INTEGER : Int;
	static var SQLITE3_FLOAT : Int;
	static var SQLITE3_TEXT : Int;
	static var SQLITE3_BLOB : Int;
	static var SQLITE3_NULL : Int;
	static var SQLITE3_OPEN_READONLY : Int;
	static var SQLITE3_OPEN_READWRITE : Int;
	static var SQLITE3_OPEN_CREATE : Int;
	/**
		@see http://php.net/manual/en/function.glob.php
	**/
	static var GLOB_MARK : Int;
	static var GLOB_NOSORT : Int;
	static var GLOB_NOCHECK : Int;
	static var GLOB_NOESCAPE : Int;
	static var GLOB_BRACE : Int;
	static var GLOB_ONLYDIR : Int;
	static var GLOB_ERR : Int;
}