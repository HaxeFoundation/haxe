package cs.system.net.mime;

/** Specifies the Content-Transfer-Encoding header information for an email message attachment. */
@:native("System.Net.Mime.TransferEncoding")
extern enum abstract TransferEncoding(Int) {
	var Base64 = 1;
	var EightBit = 3;
	var QuotedPrintable = 0;
	var SevenBit = 2;
	var Unknown = -1;
}
