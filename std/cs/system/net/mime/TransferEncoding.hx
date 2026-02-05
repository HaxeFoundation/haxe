package cs.system.net.mime;

/** Specifies the Content-Transfer-Encoding header information for an email message attachment. */
@:native("System.Net.Mime.TransferEncoding")
extern enum TransferEncoding {
	Base64;
	EightBit;
	QuotedPrintable;
	SevenBit;
	Unknown;
}
