package haxe.web;
/**
* The scheme consists of a sequence of characters beginning with a letter and followed by any combination of letters, digits, plus (+), period (.), or hyphen (-). Although schemes are case-insensitive, the canonical form is lowercase and documents that specify schemes must do so with lowercase letters. It is followed by a colon (:).
* @class Scheme
* @module Tamina
*/
@:enum abstract Scheme(String) from String to String  {

/**
 * @property HTTP
 * @type Scheme
 * @static
 * @readOnly
 */
    var HTTP = 'http';

/**
 * @property HTTPS
 * @type Scheme
 * @static
 * @readOnly
 */
    var HTTPS = 'https';

/**
 * @property FTP
 * @type Scheme
 * @static
 * @readOnly
 */
    var FTP = 'ftp';

/**
 * @property MAIL_TO
 * @type Scheme
 * @static
 * @readOnly
 */
    var MAIL_TO = 'mailto';

/**
 * @property FILE
 * @type Scheme
 * @static
 * @readOnly
 */
    var FILE = 'file';

/**
 * @property DATA
 * @type Scheme
 * @static
 * @readOnly
 */
    var DATA = 'data';

}
