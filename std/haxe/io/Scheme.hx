package haxe.io;

/**
	A scheme consists of a sequence of characters beginning with a letter and followed
	by any combination of letters, digits, plus (`+`, period (`.`), or hyphen (`-`).

	Although schemes are case-insensitive, the canonical form is lowercase
	and documents that specify schemes must do so with lowercase letters.
	It is followed by a colon (`:`).
**/
enum abstract Scheme(String) from String to String {
	var Http = 'http';
	var Https = 'https';
	var Ftp = 'ftp';
	var MailTo = 'mailto';
	var File = 'file';
	var Data = 'data';
}
