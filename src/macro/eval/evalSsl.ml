open EvalHash
open EvalValue
open EvalEncode
open EvalDecode
open EvalExceptions
open Mbedtls

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
	(* add key_sys_ssl_Config
		(fun vl -> match vl with
			| [server] ->
				let cfg = Mbedtls.mbedtls_ssl_config_init () in
				encode_instance key_sys_ssl_Context ~kind:(ISslConfig cfg)
			| _ ->
				assert false
		);
	add key_sys_ssl_Context
		(fun vl -> match vl with
			| [config] ->
				let config = as_config config in
				let ctx = Mbedtls.mbedtls_ssl_init() in
				ignore(Mbedtls.mbedtls_ssl_setup ctx config); (* TODO *)
				encode_instance key_sys_ssl_Context ~kind:(ISslContext ctx)
			| _ ->
				assert false
		); *)

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
	init_fields builtins (["sys";"ssl"],"Lib") [
		"strerror",vfun1 (fun code -> encode_string (mbedtls_strerror (decode_int code)));
	] []; (* TODO: move *)
	init_fields builtins (["mbedtls"],"Config") [] [
		"defaults",vifun3 (fun this endpoint transport preset ->
			vint (mbedtls_ssl_config_defaults (as_config this) (decode_int endpoint) (decode_int transport) (decode_int preset));
		);
		"free",vifun0 (fun this ->
			mbedtls_ssl_config_free (as_config this);
			vnull
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
	let socket_send socket bytes =
		Unix.send socket bytes 0 (Bytes.length bytes) []
	in
	let socket_receive socket bytes =
		Unix.recv socket bytes 0 (Bytes.length bytes) []
	in
	init_fields builtins (["mbedtls"],"Ssl") [] [
		"setSocket",vifun1 (fun this socket ->
			let ctx = as_ssl this in
			let socket = as_socket socket in
			mbedtls_ssl_set_bio ctx socket socket_send socket_receive;
			vnull
		); (* TODO: remove this *)
		"free",vifun0 (fun this ->
			mbedtls_ssl_free (as_ssl this);
			vnull
		);
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
	init_fields builtins (["mbedtls"],"SslEndpoint") (statics (hx_get_ssl_endpoint_flags())) [];
	init_fields builtins (["mbedtls"],"SslPreset") (statics (hx_get_ssl_preset_flags())) [];
	init_fields builtins (["mbedtls"],"SslTransport") (statics (hx_get_ssl_transport_flags())) [];
