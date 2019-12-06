open EvalHash
open EvalValue
open EvalEncode
open EvalDecode
open EvalExceptions
open Mbedtls

let as_cert vthis = match vthis with
	| VInstance {ikind = IEmbedtlsCertificate i} -> i
	| _ -> unexpected_value vthis "Certificate"

let as_config vthis = match vthis with
	| VInstance {ikind = IEmbedtlsConfig i} -> i
	| _ -> unexpected_value vthis "Config"

let as_socket vthis = match vthis with
	| VInstance {ikind = ISocket sock} -> sock
	| _ -> unexpected_value vthis "NativeSocket"

let as_ctr_drbg vthis = match vthis with
	| VInstance {ikind = IEmbedtlsCtrDrbg i} -> i
	| _ -> unexpected_value vthis "CtrDrbg"

let as_entropy vthis = match vthis with
	| VInstance {ikind = IEmbedtlsEntropy i} -> i
	| _ -> unexpected_value vthis "Entropy"

let as_ssl vthis = match vthis with
	| VInstance {ikind = IEmbedtlsSsl ctx} -> ctx
	| _ -> unexpected_value vthis "Ssl"

let init_constructors add =
	add key_mbedtls_Certificate
		(fun _ ->
			let cert = mbedtls_x509_crt_init() in
			encode_instance key_mbedtls_Certificate ~kind:(IEmbedtlsCertificate cert)
		);
	add key_mbedtls_Config
		(fun _ ->
			let cfg = mbedtls_ssl_config_init() in
			encode_instance key_mbedtls_Config ~kind:(IEmbedtlsConfig cfg)
		);
	add key_mbedtls_CtrDrbg
		(fun _ ->
			let ctr = mbedtls_ctr_drbg_init() in
			encode_instance key_mbedtls_CtrDrbg ~kind:(IEmbedtlsCtrDrbg ctr)
		);
	add key_mbedtls_Entropy
		(fun _ ->
			let entropy = mbedtls_entropy_init() in
			encode_instance key_mbedtls_Entropy ~kind:(IEmbedtlsEntropy entropy)
		);
	add key_mbedtls_Ssl
		(fun _ ->
			let ssl = mbedtls_ssl_init() in
			encode_instance key_mbedtls_Ssl ~kind:(IEmbedtlsSsl ssl)
		)

let init_fields init_fields builtins =
	let socket_send socket bytes =
		Unix.send socket bytes 0 (Bytes.length bytes) []
	in
	let socket_receive socket bytes =
		Unix.recv socket bytes 0 (Bytes.length bytes) []
	in
	init_fields builtins (["sys";"ssl"],"Mbedtls") [
		"loadDefaults",vfun1 (fun this ->
			vint (hx_cert_load_defaults (as_cert this));
		);
		"setSocket",vfun2 (fun this socket ->
			let ctx = as_ssl this in
			let socket = as_socket socket in
			mbedtls_ssl_set_bio ctx socket socket_send socket_receive;
			vnull
		);
	] [];
	init_fields builtins (["mbedtls"],"Certificate") [] [
		"parse_file",vifun1 (fun this path ->
			vint (mbedtls_x509_crt_parse_file (as_cert this) (decode_string path));
		);
		"parse_path",vifun1 (fun this path ->
			vint (mbedtls_x509_crt_parse_path (as_cert this) (decode_string path));
		);
	];
	init_fields builtins (["mbedtls"],"Config") [] [
		"authmode",vifun1 (fun this authmode ->
			mbedtls_ssl_config_authmode (as_config this) (decode_int authmode);
			vnull;
		);
		"ca_chain",vifun1 (fun this ca_chain ->
			mbedtls_ssl_conf_ca_chain (as_config this) (as_cert ca_chain);
			vnull;
		);
		"defaults",vifun3 (fun this endpoint transport preset ->
			vint (mbedtls_ssl_config_defaults (as_config this) (decode_int endpoint) (decode_int transport) (decode_int preset));
		);
		"rng",vifun1(fun this p_rng ->
			mbedtls_ssl_config_rng (as_config this) (as_ctr_drbg p_rng);
			vnull
		)
	];
	init_fields builtins (["mbedtls"],"CtrDrbg") [] [
		"random",vifun2 (fun this output output_len ->
			vint (mbedtls_ctr_drbg_random (as_ctr_drbg this) (decode_bytes output) (decode_int output_len));
		);
		"seed",vifun2(fun this entropy custom ->
			vint (mbedtls_ctr_drbg_seed (as_ctr_drbg this) (as_entropy entropy) (match custom with VString s -> Some s.sstring | _ -> None))
		)
	];
	init_fields builtins (["mbedtls"],"Error") [
		"strerror",vfun1 (fun code -> encode_string (mbedtls_strerror (decode_int code)));
	] [];
	init_fields builtins (["mbedtls"],"Ssl") [] [
		"handshake",vifun0 (fun this ->
			vint (mbedtls_ssl_handshake (as_ssl this));
		);
		"read",vifun3(fun this buf pos len ->
			vint (mbedtls_ssl_read (as_ssl this) (decode_bytes buf) (decode_int pos) (decode_int len);)
		);
		"set_hostname",vifun1 (fun this hostname ->
			vint (mbedtls_ssl_set_hostname (as_ssl this) (decode_string hostname));
		);
		"setup",vifun1 (fun this conf ->
			vint (mbedtls_ssl_setup (as_ssl this) (as_config conf))
		);
		"write",vifun3(fun this buf pos len ->
			vint (mbedtls_ssl_write (as_ssl this) (decode_bytes buf) (decode_int pos) (decode_int len);)
		);
	];
	let statics a = List.map (fun (s,i) -> s,vint i) (Array.to_list a) in
	init_fields builtins (["mbedtls"],"SslAuthmode") (statics (hx_get_ssl_authmode_flags())) [];
	init_fields builtins (["mbedtls"],"SslEndpoint") (statics (hx_get_ssl_endpoint_flags())) [];
	init_fields builtins (["mbedtls"],"SslPreset") (statics (hx_get_ssl_preset_flags())) [];
	init_fields builtins (["mbedtls"],"SslTransport") (statics (hx_get_ssl_transport_flags())) [];
