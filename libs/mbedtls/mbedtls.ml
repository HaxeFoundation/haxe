type mbedtls_ctr_drbg_context
type mbedtls_entropy_context
type mbedtls_ssl_config
type mbedtls_ssl_context
type mbedtls_x509_crt
type mbedtls_pk_context

type mbedtls_result = int

type t_mbedtls_entropy_func = mbedtls_entropy_context -> bytes -> int -> mbedtls_result

external mbedtls_strerror : int -> string = "ml_mbedtls_strerror"

external mbedtls_ctr_drbg_init : unit -> mbedtls_ctr_drbg_context = "ml_mbedtls_ctr_drbg_init"
external mbedtls_ctr_drbg_random : mbedtls_ctr_drbg_context -> bytes -> int -> mbedtls_result = "ml_mbedtls_ctr_drbg_random"
external mbedtls_ctr_drbg_seed :
	mbedtls_ctr_drbg_context ->
	'a ->
	string option ->
	mbedtls_result = "ml_mbedtls_ctr_drbg_seed"

external mbedtls_entropy_func : mbedtls_entropy_context -> bytes -> int -> mbedtls_result = "ml_mbedtls_entropy_func"
external mbedtls_entropy_init : unit -> mbedtls_entropy_context = "ml_mbedtls_entropy_init"

external mbedtls_ssl_conf_ca_chain : mbedtls_ssl_config -> mbedtls_x509_crt -> unit = "ml_mbedtls_ssl_conf_ca_chain"
external mbedtls_ssl_config_authmode : mbedtls_ssl_config -> int -> unit = "ml_mbedtls_ssl_conf_authmode"
external mbedtls_ssl_config_defaults : mbedtls_ssl_config -> int -> int -> int -> mbedtls_result = "ml_mbedtls_ssl_config_defaults"
external mbedtls_ssl_config_init : unit -> mbedtls_ssl_config = "ml_mbedtls_ssl_config_init"
external mbedtls_ssl_config_rng : mbedtls_ssl_config -> 'a -> unit = "ml_mbedtls_ssl_conf_rng"

external mbedtls_ssl_init : unit -> mbedtls_ssl_context = "ml_mbedtls_ssl_init"
external mbedtls_ssl_get_peer_cert : mbedtls_ssl_context -> mbedtls_x509_crt option = "ml_mbedtls_ssl_get_peer_cert"
external mbedtls_ssl_handshake : mbedtls_ssl_context -> mbedtls_result = "ml_mbedtls_ssl_handshake"
external mbedtls_ssl_read : mbedtls_ssl_context -> bytes -> int -> int -> mbedtls_result = "ml_mbedtls_ssl_read"
external mbedtls_ssl_set_bio :
	mbedtls_ssl_context ->
	'a ->
	('a -> bytes -> mbedtls_result) ->
	('a -> bytes -> mbedtls_result) ->
	unit = "ml_mbedtls_ssl_set_bio"
external mbedtls_ssl_set_hostname : mbedtls_ssl_context -> string -> mbedtls_result = "ml_mbedtls_ssl_set_hostname"
external mbedtls_ssl_setup : mbedtls_ssl_context -> mbedtls_ssl_config -> mbedtls_result = "ml_mbedtls_ssl_setup"
external mbedtls_ssl_write : mbedtls_ssl_context -> bytes -> int -> int -> mbedtls_result = "ml_mbedtls_ssl_write"

external mbedtls_pk_init : unit -> mbedtls_pk_context = "ml_mbedtls_pk_init"
external mbedtls_pk_parse_key : mbedtls_pk_context -> bytes -> string option -> mbedtls_result = "ml_mbedtls_pk_parse_key"
external mbedtls_pk_parse_keyfile : mbedtls_pk_context -> string -> string option -> mbedtls_result = "ml_mbedtls_pk_parse_keyfile"
external mbedtls_pk_parse_public_keyfile : mbedtls_pk_context -> string -> mbedtls_result = "ml_mbedtls_pk_parse_public_keyfile"
external mbedtls_pk_parse_public_key : mbedtls_pk_context -> bytes -> mbedtls_result = "ml_mbedtls_pk_parse_public_key"

external mbedtls_x509_crt_init : unit -> mbedtls_x509_crt = "ml_mbedtls_x509_crt_init"
external mbedtls_x509_next : mbedtls_x509_crt -> mbedtls_x509_crt option = "ml_mbedtls_x509_next"
external mbedtls_x509_crt_parse : mbedtls_x509_crt -> bytes -> mbedtls_result = "ml_mbedtls_x509_crt_parse"
external mbedtls_x509_crt_parse_file : mbedtls_x509_crt -> string -> mbedtls_result = "ml_mbedtls_x509_crt_parse_file"
external mbedtls_x509_crt_parse_path : mbedtls_x509_crt -> string -> mbedtls_result = "ml_mbedtls_x509_crt_parse_path"

external hx_cert_get_alt_names : mbedtls_x509_crt -> string array = "hx_cert_get_alt_names"
external hx_cert_get_issuer : mbedtls_x509_crt -> string -> string option = "hx_cert_get_issuer"
external hx_cert_get_notafter : mbedtls_x509_crt -> float = "hx_cert_get_notafter"
external hx_cert_get_notbefore : mbedtls_x509_crt -> float = "hx_cert_get_notbefore"
external hx_cert_get_subject : mbedtls_x509_crt -> string -> string option = "hx_cert_get_subject"

(* glue *)

external hx_cert_load_defaults : mbedtls_x509_crt -> int = "hx_cert_load_defaults"
external hx_get_ssl_authmode_flags : unit -> (string * int) array = "hx_get_ssl_authmode_flags"
external hx_get_ssl_endpoint_flags : unit -> (string * int) array = "hx_get_ssl_endpoint_flags"
external hx_get_ssl_preset_flags : unit -> (string * int) array = "hx_get_ssl_preset_flags"
external hx_get_ssl_transport_flags : unit -> (string * int) array = "hx_get_ssl_transport_flags"
