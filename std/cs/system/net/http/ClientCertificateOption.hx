package cs.system.net.http;

/** Specifies how client certificates are provided. */
@:native("System.Net.Http.ClientCertificateOption")
extern enum abstract ClientCertificateOption(Int) {
	var Automatic = 1;
	var Manual = 0;
}
