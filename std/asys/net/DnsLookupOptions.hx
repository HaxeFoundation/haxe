package asys.net;

typedef DnsLookupOptions = {
	?family:IpFamily,
	?hints:DnsHints
};

enum abstract DnsHints(Int) from Int {
	var AddrConfig = 1 << 0;
	var V4Mapped = 1 << 1;

	inline function get_raw():Int return this;

	@:op(A | B)
	inline function join(other:DnsHints):DnsHints return this | other.get_raw();
}
