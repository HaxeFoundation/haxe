package haxe.io;
/**
    The scheme consists of a sequence of characters beginning with a letter and followed by any combination of letters, digits, plus (+), period (.), or hyphen (-). Although schemes are case-insensitive, the canonical form is lowercase and documents that specify schemes must do so with lowercase letters. It is followed by a colon (:).
**/
@:enum abstract Scheme(String) from String to String  {

    var HTTP = 'http';
    var HTTPS = 'https';
    var FTP = 'ftp';
    var MAIL_TO = 'mailto';
    var FILE = 'file';
    var DATA = 'data';

}
