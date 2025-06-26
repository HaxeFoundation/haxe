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
	static final HAXE_CUSTOM_ERROR_HANDLER:Bool;

	/**
		@see http://php.net/manual/en/reserved.constants.php
	**/
	static final PHP_VERSION_ID:Int;

	static final PHP_OS:String;
	static final PHP_SAPI:String;
	static final PHP_BINARY:String;
	static final PHP_EOL:String;
	static final PHP_INT_MAX:Int;
	static final PHP_INT_MIN:Int;
	static final PHP_INT_SIZE:Int;

	/**
		@see http://php.net/manual/en/language.constants.predefined.php
	**/
	static final __LINE__:Int;

	static final __FILE__:String;
	static final __DIR__:String;
	static final __FUNCTION__:String;
	static final __CLASS__:String;
	static final __TRAIT__:String;
	static final __METHOD__:String;
	static final __NAMESPACE__:String;

	/**
		@see https://php.net/manual/en/dir.constants.php
	**/
	static final DIRECTORY_SEPARATOR:String;

	static final PATH_SEPARATOR:String;
	static final SCANDIR_SORT_ASCENDING:Int;
	static final SCANDIR_SORT_DESCENDING:Int;
	static final SCANDIR_SORT_NONE:Int;

	/**
		@see http://php.net/manual/en/errorfunc.constants.php
	**/
	static final E_ERROR:Int;

	static final E_WARNING:Int;
	static final E_PARSE:Int;
	static final E_NOTICE:Int;
	static final E_CORE_ERROR:Int;
	static final E_CORE_WARNING:Int;
	static final E_COMPILE_ERROR:Int;
	static final E_COMPILE_WARNING:Int;
	static final E_USER_ERROR:Int;
	static final E_USER_WARNING:Int;
	static final E_USER_NOTICE:Int;
	static final E_STRICT:Int;
	static final E_RECOVERABLE_ERROR:Int;
	static final E_DEPRECATED:Int;
	static final E_USER_DEPRECATED:Int;
	static final E_ALL:Int;

	/**
		@see http://php.net/manual/en/function.count.php
	**/
	static final COUNT_NORMAL:Int;

	static final COUNT_RECURSIVE:Int;

	/**
		@see http://php.net/manual/en/function.array-filter.php
	**/
	static final ARRAY_FILTER_USE_KEY:Int;

	static final ARRAY_FILTER_USE_BOTH:Int;

	/**
		@see http://php.net/manual/en/function.debug-backtrace.php
	**/
	static final DEBUG_BACKTRACE_PROVIDE_OBJECT:Int;

	static final DEBUG_BACKTRACE_IGNORE_ARGS:Int;

	/**
		@see http://php.net/manual/en/math.constants.php
	**/
	static final M_PI:Float;

	static final M_E:Float;
	static final M_LOG2E:Float;
	static final M_LOG10E:Float;
	static final M_LN2:Float;
	static final M_LN10:Float;
	static final M_PI_2:Float;
	static final M_PI_4:Float;
	static final M_1_PI:Float;
	static final M_2_PI:Float;
	static final M_SQRTPI:Float;
	static final M_2_SQRTPI:Float;
	static final M_SQRT2:Float;
	static final M_SQRT3:Float;
	static final M_SQRT1_2:Float;
	static final M_LNPI:Float;
	static final M_EULER:Float;
	static final PHP_ROUND_HALF_UP:Int;
	static final PHP_ROUND_HALF_DOWN:Int;
	static final PHP_ROUND_HALF_EVEN:Int;
	static final PHP_ROUND_HALF_ODD:Int;
	static final NAN:Float;
	static final INF:Float;

	/**
		@see http://php.net/manual/en/function.setlocale.php
	**/
	static final LC_ALL:Int;

	static final LC_COLLATE:Int;
	static final LC_CTYPE:Int;
	static final LC_MONETARY:Int;
	static final LC_NUMERIC:Int;
	static final LC_TIME:Int;
	static final LC_MESSAGES:Int;

	/**
		@see http://php.net/manual/en/features.commandline.io-streams.php
	**/
	static final STDIN:Resource;

	static final STDOUT:Resource;
	static final STDERR:Resource;

	/**
		@see http://php.net/manual/en/function.preg-match-all.php
	**/
	static final PREG_PATTERN_ORDER:Int;

	static final PREG_SET_ORDER:Int;
	static final PREG_OFFSET_CAPTURE:Int;

	/**
		@see http://php.net/manual/en/function.preg-split.php
	**/
	static final PREG_SPLIT_NO_EMPTY:Int;

	static final PREG_SPLIT_DELIM_CAPTURE:Int;
	static final PREG_SPLIT_OFFSET_CAPTURE:Int;

	/**
		@see http://php.net/manual/en/function.preg-last-error.php
	**/
	static final PREG_NO_ERROR:Int;

	static final PREG_INTERNAL_ERROR:Int;
	static final PREG_BACKTRACK_LIMIT_ERROR:Int;
	static final PREG_RECURSION_LIMIT_ERROR:Int;
	static final PREG_BAD_UTF8_ERROR:Int;
	static final PREG_BAD_UTF8_OFFSET_ERROR:Int;
	static final PREG_JIT_STACKLIMIT_ERROR:Int;

	/**
		@see http://php.net/manual/en/function.htmlspecialchars.php
	**/
	static final ENT_COMPAT:Int;

	static final ENT_QUOTES:Int;
	static final ENT_NOQUOTES:Int;
	static final ENT_IGNORE:Int;
	static final ENT_SUBSTITUTE:Int;
	static final ENT_DISALLOWED:Int;
	static final ENT_HTML401:Int;
	static final ENT_XML1:Int;
	static final ENT_XHTML:Int;
	static final ENT_HTML5:Int;

	/**
		@see http://php.net/manual/en/function.str-pad.php
	**/
	static final STR_PAD_RIGHT:Int;

	static final STR_PAD_LEFT:Int;
	static final STR_PAD_BOTH:Int;

	/**
		@see http://php.net/manual/en/function.feof.php
	**/
	static final SEEK_SET:Int;

	static final SEEK_CUR:Int;
	static final SEEK_END:Int;

	/**
		@see http://php.net/manual/en/function.stream-socket-server.php
	**/
	static final STREAM_SERVER_BIND:Int;

	static final STREAM_SERVER_LISTEN:Int;

	/**
		@see http://php.net/manual/en/function.stream-socket-client.php
	**/
	static final STREAM_CLIENT_CONNECT:Int;

	static final STREAM_CLIENT_ASYNC_CONNECT:Int;
	static final STREAM_CLIENT_PERSISTENT:Int;

	/**
		@see http://php.net/manual/en/sockets.constants.php
	**/
	static final SOCK_STREAM:Int;

	static final SOCK_DGRAM:Int;
	static final SOCK_SEQPACKET:Int;
	static final SOCK_RAW:Int;
	static final SOCK_RDM:Int;
	static final AF_INET:Int;
	static final AF_INET6:Int;
	static final AF_UNIX:Int;
	static final SOL_TCP:Int;
	static final SOL_SOCKET:Int;
	static final SO_RCVTIMEO:Int;
	static final SO_SNDTIMEO:Int;
	static final TCP_NODELAY:Int;
	static final PHP_BINARY_READ:Int;

	/**
		@see http://php.net/manual/en/function.session-status.php
	**/
	static final PHP_SESSION_DISABLED:Int;
	static final PHP_SESSION_NONE:Int;
	static final PHP_SESSION_ACTIVE:Int;

	/**
		@see http://php.net/manual/en/json.constants.php
	**/
	static final JSON_ERROR_NONE:Int;

	static final JSON_ERROR_DEPTH:Int;
	static final JSON_ERROR_STATE_MISMATCH:Int;
	static final JSON_ERROR_CTRL_CHAR:Int;
	static final JSON_ERROR_SYNTAX:Int;
	static final JSON_ERROR_UTF8:Int;
	static final JSON_ERROR_RECURSION:Int;
	static final JSON_ERROR_INF_OR_NAN:Int;
	static final JSON_ERROR_UNSUPPORTED_TYPE:Int;
	static final JSON_HEX_TAG:Int;
	static final JSON_HEX_AMP:Int;
	static final JSON_HEX_APOS:Int;
	static final JSON_HEX_QUOT:Int;
	static final JSON_FORCE_OBJECT:Int;
	static final JSON_NUMERIC_CHECK:Int;
	static final JSON_BIGINT_AS_STRING:Int;
	static final JSON_PRETTY_PRINT:Int;
	static final JSON_UNESCAPED_SLASHES:Int;
	static final JSON_UNESCAPED_UNICODE:Int;
	static final JSON_PARTIAL_OUTPUT_ON_ERROR:Int;
	static final JSON_PRESERVE_ZERO_FRACTION:Int;

	/**
		@see http://php.net/manual/en/mysqli.constants.php
	**/
	static final MYSQLI_READ_DEFAULT_GROUP:Int;

	static final MYSQLI_READ_DEFAULT_FILE:Int;
	static final MYSQLI_OPT_CONNECT_TIMEOUT:Int;
	static final MYSQLI_OPT_LOCAL_INFILE:Int;
	static final MYSQLI_INIT_COMMAND:Int;
	static final MYSQLI_CLIENT_SSL:Int;
	static final MYSQLI_CLIENT_COMPRESS:Int;
	static final MYSQLI_CLIENT_INTERACTIVE:Int;
	static final MYSQLI_CLIENT_IGNORE_SPACE:Int;
	static final MYSQLI_CLIENT_NO_SCHEMA:Int;
	static final MYSQLI_CLIENT_MULTI_QUERIES:Int;
	static final MYSQLI_STORE_RESULT:Int;
	static final MYSQLI_USE_RESULT:Int;
	static final MYSQLI_ASSOC:Int;
	static final MYSQLI_NUM:Int;
	static final MYSQLI_BOTH:Int;
	static final MYSQLI_NOT_NULL_FLAG:Int;
	static final MYSQLI_PRI_KEY_FLAG:Int;
	static final MYSQLI_UNIQUE_KEY_FLAG:Int;
	static final MYSQLI_MULTIPLE_KEY_FLAG:Int;
	static final MYSQLI_BLOB_FLAG:Int;
	static final MYSQLI_UNSIGNED_FLAG:Int;
	static final MYSQLI_ZEROFILL_FLAG:Int;
	static final MYSQLI_AUTO_INCREMENT_FLAG:Int;
	static final MYSQLI_TIMESTAMP_FLAG:Int;
	static final MYSQLI_SET_FLAG:Int;
	static final MYSQLI_NUM_FLAG:Int;
	static final MYSQLI_PART_KEY_FLAG:Int;
	static final MYSQLI_GROUP_FLAG:Int;
	static final MYSQLI_TYPE_DECIMAL:Int;
	static final MYSQLI_TYPE_NEWDECIMAL:Int;
	static final MYSQLI_TYPE_BIT:Int;
	static final MYSQLI_TYPE_TINY:Int;
	static final MYSQLI_TYPE_SHORT:Int;
	static final MYSQLI_TYPE_LONG:Int;
	static final MYSQLI_TYPE_FLOAT:Int;
	static final MYSQLI_TYPE_DOUBLE:Int;
	static final MYSQLI_TYPE_NULL:Int;
	static final MYSQLI_TYPE_TIMESTAMP:Int;
	static final MYSQLI_TYPE_LONGLONG:Int;
	static final MYSQLI_TYPE_INT24:Int;
	static final MYSQLI_TYPE_DATE:Int;
	static final MYSQLI_TYPE_TIME:Int;
	static final MYSQLI_TYPE_DATETIME:Int;
	static final MYSQLI_TYPE_YEAR:Int;
	static final MYSQLI_TYPE_NEWDATE:Int;
	static final MYSQLI_TYPE_INTERVAL:Int;
	static final MYSQLI_TYPE_ENUM:Int;
	static final MYSQLI_TYPE_SET:Int;
	static final MYSQLI_TYPE_TINY_BLOB:Int;
	static final MYSQLI_TYPE_MEDIUM_BLOB:Int;
	static final MYSQLI_TYPE_LONG_BLOB:Int;
	static final MYSQLI_TYPE_BLOB:Int;
	static final MYSQLI_TYPE_VAR_STRING:Int;
	static final MYSQLI_TYPE_STRING:Int;
	static final MYSQLI_TYPE_CHAR:Int;
	static final MYSQLI_TYPE_GEOMETRY:Int;
	static final MYSQLI_NEED_DATA:Int;
	static final MYSQLI_NO_DATA:Int;
	static final MYSQLI_DATA_TRUNCATED:Int;
	static final MYSQLI_ENUM_FLAG:Int;
	static final MYSQLI_BINARY_FLAG:Int;
	static final MYSQLI_CURSOR_TYPE_FOR_UPDATE:Int;
	static final MYSQLI_CURSOR_TYPE_NO_CURSOR:Int;
	static final MYSQLI_CURSOR_TYPE_READ_ONLY:Int;
	static final MYSQLI_CURSOR_TYPE_SCROLLABLE:Int;
	static final MYSQLI_STMT_ATTR_CURSOR_TYPE:Int;
	static final MYSQLI_STMT_ATTR_PREFETCH_ROWS:Int;
	static final MYSQLI_STMT_ATTR_UPDATE_MAX_LENGTH:Int;
	static final MYSQLI_SET_CHARSET_NAME:Int;
	static final MYSQLI_REPORT_INDEX:Int;
	static final MYSQLI_REPORT_ERROR:Int;
	static final MYSQLI_REPORT_STRICT:Int;
	static final MYSQLI_REPORT_ALL:Int;
	static final MYSQLI_REPORT_OFF:Int;
	static final MYSQLI_DEBUG_TRACE_ENABLED:Int;
	static final MYSQLI_SERVER_QUERY_NO_GOOD_INDEX_USED:Int;
	static final MYSQLI_SERVER_QUERY_NO_INDEX_USED:Int;
	static final MYSQLI_REFRESH_GRANT:Int;
	static final MYSQLI_REFRESH_LOG:Int;
	static final MYSQLI_REFRESH_TABLES:Int;
	static final MYSQLI_REFRESH_HOSTS:Int;
	static final MYSQLI_REFRESH_STATUS:Int;
	static final MYSQLI_REFRESH_THREADS:Int;
	static final MYSQLI_REFRESH_SLAVE:Int;
	static final MYSQLI_REFRESH_MASTER:Int;
	static final MYSQLI_TRANS_COR_AND_CHAIN:Int;
	static final MYSQLI_TRANS_COR_AND_NO_CHAIN:Int;
	static final MYSQLI_TRANS_COR_RELEASE:Int;
	static final MYSQLI_TRANS_COR_NO_RELEASE:Int;
	static final MYSQLI_TRANS_START_READ_ONLY:Int;
	static final MYSQLI_TRANS_START_READ_WRITE:Int;
	static final MYSQLI_TRANS_START_CONSISTENT_SNAPSHOT:Int;

	/**
		@see http://php.net/manual/en/sqlite3.constants.php
	**/
	static final SQLITE3_ASSOC:Int;

	static final SQLITE3_NUM:Int;
	static final SQLITE3_BOTH:Int;
	static final SQLITE3_INTEGER:Int;
	static final SQLITE3_FLOAT:Int;
	static final SQLITE3_TEXT:Int;
	static final SQLITE3_BLOB:Int;
	static final SQLITE3_NULL:Int;
	static final SQLITE3_OPEN_READONLY:Int;
	static final SQLITE3_OPEN_READWRITE:Int;
	static final SQLITE3_OPEN_CREATE:Int;

	/**
		@see http://php.net/manual/en/function.glob.php
	**/
	static final GLOB_MARK:Int;

	static final GLOB_NOSORT:Int;
	static final GLOB_NOCHECK:Int;
	static final GLOB_NOESCAPE:Int;
	static final GLOB_BRACE:Int;
	static final GLOB_ONLYDIR:Int;
	static final GLOB_ERR:Int;

	/**
		@see http://php.net/manual/en/zlib.constants.php
	**/
	static final FORCE_GZIP:Int;

	static final FORCE_DEFLATE:Int;
	static final ZLIB_ENCODING_RAW:Int;
	static final ZLIB_ENCODING_DEFLATE:Int;
	static final ZLIB_ENCODING_GZIP:Int;
	static final ZLIB_FILTERED:Int;
	static final ZLIB_HUFFMAN_ONLY:Int;
	static final ZLIB_FIXED:Int;
	static final ZLIB_RLE:Int;
	static final ZLIB_DEFAULT_STRATEGY:Int;
	static final ZLIB_BLOCK:Int;
	static final ZLIB_NO_FLUSH:Int;
	static final ZLIB_PARTIAL_FLUSH:Int;
	static final ZLIB_SYNC_FLUSH:Int;
	static final ZLIB_FULL_FLUSH:Int;
	static final ZLIB_FINISH:Int;

	/**
		@see http://php.net/manual/en/outcontrol.constants.php
	**/
	static final PHP_OUTPUT_HANDLER_CLEAN:Int;
	static final PHP_OUTPUT_HANDLER_CLEANABLE:Int;
	static final PHP_OUTPUT_HANDLER_CONT:Int;
	static final PHP_OUTPUT_HANDLER_END:Int;
	static final PHP_OUTPUT_HANDLER_FINAL:Int;
	static final PHP_OUTPUT_HANDLER_FLUSH:Int;
	static final PHP_OUTPUT_HANDLER_FLUSHABLE:Int;
	static final PHP_OUTPUT_HANDLER_REMOVABLE:Int;
	static final PHP_OUTPUT_HANDLER_START:Int;
	static final PHP_OUTPUT_HANDLER_STDFLAGS:Int;
	static final PHP_OUTPUT_HANDLER_WRITE:Int;

	/**
		@see http://php.net/manual/en/function.flock.php
	**/
	static final LOCK_EX:Int;
	static final LOCK_NB:Int;
	static final LOCK_SH:Int;
	static final LOCK_UN:Int;

	/**
		@see http://php.net/manual/en/function.fnmatch.php
	**/
	static final FNM_CASEFOLD:Int;
	static final FNM_NOESCAPE:Int;
	static final FNM_PATHNAME:Int;
	static final FNM_PERIOD:Int;

	/**
		@see http://php.net/manual/en/function.parse-ini-file.php
	**/
	static final INI_SCANNER_NORMAL:Int;
	static final INI_SCANNER_RAW:Int;
	static final INI_SCANNER_TYPED:Int;

	/**
		@see http://php.net/manual/en/function.pathinfo.php
	**/
	static final PATHINFO_BASENAME:Int;
	static final PATHINFO_DIRNAME:Int;
	static final PATHINFO_EXTENSION:Int;
	static final PATHINFO_FILENAME:Int;

	/**
		@see http://php.net/manual/en/function.dns-get-record.php
	**/
	static final DNS_A:Int;
	static final DNS_AAAA:Int;
	static final DNS_ALL:Int;
	static final DNS_ANY:Int;
	static final DNS_CAA:Int;
	static final DNS_CNAME:Int;
	static final DNS_HINFO:Int;
	static final DNS_MX:Int;
	static final DNS_NS:Int;
	static final DNS_PTR:Int;
	static final DNS_SOA:Int;
	static final DNS_TXT:Int;

	/**
		@see http://php.net/manual/en/intl.constants.php
	**/
	static final IDNA_ALLOW_UNASSIGNED:Int;
	static final IDNA_CHECK_BIDI:Int;
	static final IDNA_CHECK_CONTEXTJ:Int;
	static final IDNA_DEFAULT:Int;
	static final IDNA_ERROR_BIDI:Int;
	static final IDNA_ERROR_CONTEXTJ:Int;
	static final IDNA_ERROR_DISALLOWED:Int;
	static final IDNA_ERROR_DOMAIN_NAME_TOO_LONG:Int;
	static final IDNA_ERROR_EMPTY_LABEL:Int;
	static final IDNA_ERROR_HYPHEN_3_4:Int;
	static final IDNA_ERROR_INVALID_ACE_LABEL:Int;
	static final IDNA_ERROR_LABEL_HAS_DOT:Int;
	static final IDNA_ERROR_LABEL_TOO_LONG:Int;
	static final IDNA_ERROR_LEADING_COMBINING_MARK:Int;
	static final IDNA_ERROR_LEADING_HYPHEN:Int;
	static final IDNA_ERROR_PUNYCODE:Int;
	static final IDNA_ERROR_TRAILING_HYPHEN:Int;
	static final IDNA_NONTRANSITIONAL_TO_ASCII:Int;
	static final IDNA_NONTRANSITIONAL_TO_UNICODE:Int;
	static final IDNA_USE_STD3_RULES:Int;
	static final INTL_ICU_VERSION:String;
	static final INTL_IDNA_VARIANT_2003:Int;
	static final INTL_IDNA_VARIANT_UTS46:Int;
	static final INTL_MAX_LOCALE_LEN:Int;

	/**
		@see http://php.net/manual/en/array.constants.php
	**/
	static final CASE_LOWER:Int;
	static final CASE_UPPER:Int;
	static final EXTR_IF_EXISTS:Int;
	static final EXTR_OVERWRITE:Int;
	static final EXTR_PREFIX_ALL:Int;
	static final EXTR_PREFIX_IF_EXISTS:Int;
	static final EXTR_PREFIX_INVALID:Int;
	static final EXTR_PREFIX_SAME:Int;
	static final EXTR_REFS:Int;
	static final EXTR_SKIP:Int;
	static final SORT_ASC:Int;
	static final SORT_DESC:Int;
	static final SORT_FLAG_CASE:Int;
	static final SORT_LOCALE_STRING:Int;
	static final SORT_NATURAL:Int;
	static final SORT_NUMERIC:Int;
	static final SORT_REGULAR:Int;
	static final SORT_STRING:Int;

	/**
		@see http://php.net/manual/en/filter.constants.php
	**/
	static final FILTER_CALLBACK:Int;
	static final FILTER_DEFAULT:Int;
	static final FILTER_FLAG_ALLOW_FRACTION:Int;
	static final FILTER_FLAG_ALLOW_HEX:Int;
	static final FILTER_FLAG_ALLOW_OCTAL:Int;
	static final FILTER_FLAG_ALLOW_SCIENTIFIC:Int;
	static final FILTER_FLAG_ALLOW_THOUSAND:Int;
	static final FILTER_FLAG_EMAIL_UNICODE:Int;
	static final FILTER_FLAG_EMPTY_STRING_NULL:Int;
	static final FILTER_FLAG_ENCODE_AMP:Int;
	static final FILTER_FLAG_ENCODE_HIGH:Int;
	static final FILTER_FLAG_ENCODE_LOW:Int;
	static final FILTER_FLAG_HOST_REQUIRED:Int;
	static final FILTER_FLAG_HOSTNAME:Int;
	static final FILTER_FLAG_IPV4:Int;
	static final FILTER_FLAG_IPV6:Int;
	static final FILTER_FLAG_NO_ENCODE_QUOTES:Int;
	static final FILTER_FLAG_NO_PRIV_RANGE:Int;
	static final FILTER_FLAG_NO_RES_RANGE:Int;
	static final FILTER_FLAG_NONE:Int;
	static final FILTER_FLAG_PATH_REQUIRED:Int;
	static final FILTER_FLAG_QUERY_REQUIRED:Int;
	static final FILTER_FLAG_SCHEME_REQUIRED:Int;
	static final FILTER_FLAG_STRIP_BACKTICK:Int;
	static final FILTER_FLAG_STRIP_HIGH:Int;
	static final FILTER_FLAG_STRIP_LOW:Int;
	static final FILTER_FORCE_ARRAY:Int;
	static final FILTER_NULL_ON_FAILURE:Int;
	static final FILTER_REQUIRE_ARRAY:Int;
	static final FILTER_REQUIRE_SCALAR:Int;
	static final FILTER_SANITIZE_ADD_SLASHES:Int;
	static final FILTER_SANITIZE_EMAIL:Int;
	static final FILTER_SANITIZE_ENCODED:Int;
	static final FILTER_SANITIZE_MAGIC_QUOTES:Int;
	static final FILTER_SANITIZE_NUMBER_FLOAT:Int;
	static final FILTER_SANITIZE_NUMBER_INT:Int;
	static final FILTER_SANITIZE_SPECIAL_CHARS:Int;
	static final FILTER_SANITIZE_STRING:Int;
	static final FILTER_SANITIZE_STRIPPED:Int;
	static final FILTER_SANITIZE_URL:Int;
	static final FILTER_UNSAFE_RAW:Int;
	static final FILTER_VALIDATE_BOOLEAN:Int;
	static final FILTER_VALIDATE_DOMAIN:Int;
	static final FILTER_VALIDATE_EMAIL:Int;
	static final FILTER_VALIDATE_FLOAT:Int;
	static final FILTER_VALIDATE_INT:Int;
	static final FILTER_VALIDATE_IP:Int;
	static final FILTER_VALIDATE_MAC:Int;
	static final FILTER_VALIDATE_REGEXP:Int;
	static final FILTER_VALIDATE_URL:Int;
	static final INPUT_COOKIE:Int;
	static final INPUT_ENV:Int;
	static final INPUT_GET:Int;
	static final INPUT_POST:Int;
	static final INPUT_REQUEST:Int;
	static final INPUT_SERVER:Int;
	static final INPUT_SESSION:Int;

	/**
		@see http://php.net/manual/en/fileinfo.constants.php
	**/
	static final FILEINFO_COMPRESS: Int;
	static final FILEINFO_CONTINUE: Int;
	static final FILEINFO_DEVICES: Int;
	static final FILEINFO_EXTENSION: Int;
	static final FILEINFO_MIME: Int;
	static final FILEINFO_MIME_ENCODING: Int;
	static final FILEINFO_MIME_TYPE: Int;
	static final FILEINFO_NONE: Int;
	static final FILEINFO_PRESERVE_ATIME: Int;
	static final FILEINFO_RAW: Int;
	static final FILEINFO_SYMLINK: Int;

	/**
		@see http://php.net/manual/en/calendar.constants.php
	**/
	static final CAL_DOW_DAYNO: Int;
	static final CAL_DOW_LONG: Int;
	static final CAL_DOW_SHORT: Int;
	static final CAL_EASTER_ALWAYS_GREGORIAN: Int;
	static final CAL_EASTER_ALWAYS_JULIAN: Int;
	static final CAL_EASTER_DEFAULT: Int;
	static final CAL_EASTER_ROMAN: Int;
	static final CAL_FRENCH: Int;
	static final CAL_GREGORIAN: Int;
	static final CAL_JEWISH: Int;
	static final CAL_JEWISH_ADD_ALAFIM: Int;
	static final CAL_JEWISH_ADD_ALAFIM_GERESH: Int;
	static final CAL_JEWISH_ADD_GERESHAYIM: Int;
	static final CAL_JULIAN: Int;
	static final CAL_MONTH_FRENCH: Int;
	static final CAL_MONTH_GREGORIAN_LONG: Int;
	static final CAL_MONTH_GREGORIAN_SHORT: Int;
	static final CAL_MONTH_JEWISH: Int;
	static final CAL_MONTH_JULIAN_LONG: Int;
	static final CAL_MONTH_JULIAN_SHORT: Int;
	static final CAL_NUM_CALS: Int;

	/**
		@see http://php.net/manual/en/ftp.constants.php
	**/
	static final FTP_ASCII: Int;
	static final FTP_AUTORESUME: Int;
	static final FTP_AUTOSEEK: Int;
	static final FTP_BINARY: Int;
	static final FTP_FAILED: Int;
	static final FTP_FINISHED: Int;
	static final FTP_IMAGE: Int;
	static final FTP_MOREDATA: Int;
	static final FTP_TEXT: Int;
	static final FTP_TIMEOUT_SEC: Int;
	static final FTP_USEPASVADDRESS: Bool;

	/**
		@see http://php.net/manual/en/misc.constants.php
	**/
	static final CONNECTION_ABORTED: Int;
	static final CONNECTION_NORMAL: Int;
	static final CONNECTION_TIMEOUT: Int;

	/**
		@see http://php.net/manual/fr/filesystem.constants.php
	**/
	static final FILE_APPEND: Int;
	static final FILE_BINARY: Int;
	static final FILE_IGNORE_NEW_LINES: Int;
	static final FILE_NO_DEFAULT_CONTEXT: Int;
	static final FILE_SKIP_EMPTY_LINES: Int;
	static final FILE_TEXT: Int;
	static final FILE_USE_INCLUDE_PATH: Int;
}
