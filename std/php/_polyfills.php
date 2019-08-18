<?php
/**
 * Polyfills for some functions, which are required by Haxe-generated code, but not available in PHP 7.0.
 * No Haxe-generated code is available at this point.
 * No code should be executed from this file.
 * Symbols declarations are the only code allowed here.
 */
namespace { //Namespace declaration is required because this file is included under non-root namespace.

	/**
	 * @see http://php.net/manual/en/function.mb-chr.php
	 */
	if(!function_exists('mb_chr')) {
		function mb_chr($code, $encoding = null) {
			if($encoding && $encoding !== 'UTF-8') {
				throw new Exception("$encoding is not supported in mb_chr() polyfill.");
			}
			if (0x80 > $code %= 0x200000) {
				$s = chr($code);
			} elseif (0x800 > $code) {
				$s = chr(0xC0 | $code >> 6) . chr(0x80 | $code & 0x3F);
			} elseif (0x10000 > $code) {
				$s = chr(0xE0 | $code >> 12) . chr(0x80 | $code >> 6 & 0x3F) . chr(0x80 | $code & 0x3F);
			} else {
				$s = chr(0xF0 | $code >> 18) . chr(0x80 | $code >> 12 & 0x3F) . chr(0x80 | $code >> 6 & 0x3F) . chr(0x80 | $code & 0x3F);
			}
			return $s;
		}
	}

	/**
	 * @see http://php.net/manual/en/function.mb-ord.php
	 */
	if(!function_exists('mb_ord')) {
		function mb_ord($s, $encoding = null) {
			if($encoding && $encoding !== 'UTF-8') {
				throw new Exception("$encoding is not supported in mb_ord() polyfill.");
			}
			$code = ($s = unpack('C*', substr($s, 0, 4))) ? $s[1] : 0;
			if (0xF0 <= $code) {
				return (($code - 0xF0) << 18) + (($s[2] - 0x80) << 12) + (($s[3] - 0x80) << 6) + $s[4] - 0x80;
			}
			if (0xE0 <= $code) {
				return (($code - 0xE0) << 12) + (($s[2] - 0x80) << 6) + $s[3] - 0x80;
			}
			if (0xC0 <= $code) {
				return (($code - 0xC0) << 6) + $s[2] - 0x80;
			}
			return $code;
		}
	}

}