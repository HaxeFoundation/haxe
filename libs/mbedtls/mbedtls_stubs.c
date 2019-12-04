#include <ctype.h>
#include <string.h>
#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>

#include "mbedtls/debug.h"
#include "mbedtls/error.h"
#include "mbedtls/config.h"
#include "mbedtls/ssl.h"
#include "mbedtls/entropy.h"
#include "mbedtls/ctr_drbg.h"
#include "mbedtls/net_sockets.h"
#include "mbedtls/certs.h"

#define SSL(v) (mbedtls_ssl_context*)Field(v, 0)
#define CFG(v) (mbedtls_ssl_config*)Field(v, 0)
#define CTR(v) (mbedtls_ctr_drbg_context*)Field(v, 0)
#define VOID(v) (void*)Field(v, 0)

void debug(void* ctx, int debug_level, const char* file_name, int line, const char* message) {
	printf("%s:%i: %s", file_name, line, message);
}

CAMLprim value ml_mbedtls_strerror(value code) {
	CAMLparam1(code);
	CAMLlocal1(r);
	char buf[128];
	mbedtls_strerror(Int_val(code), buf, sizeof(buf));
	r = caml_copy_string(buf);
	CAMLreturn(r);
}

// CtrDrbg

CAMLprim value ml_mbedtls_ctr_drbg_init() {
	CAMLparam0();
	CAMLlocal1(obj);
	mbedtls_ctr_drbg_context* ctr_drbg = malloc(sizeof(mbedtls_ctr_drbg_context));
	obj = caml_alloc(1, Abstract_tag);
	Store_field(obj, 0, (value)ctr_drbg);
	mbedtls_ctr_drbg_init(ctr_drbg);
	CAMLreturn(obj);
}

CAMLprim value ml_mbedtls_ctr_drbg_random(value p_rng, value output, value output_len) {
	CAMLparam3(p_rng, output, output_len);
	CAMLlocal1(r);
	r = mbedtls_ctr_drbg_random(CTR(p_rng), Bytes_val(output), Int_val(output_len));
	CAMLreturn(Val_int(r));
}

CAMLprim value ml_mbedtls_ctr_drbg_seed(value ctx, value p_entropy, value custom) {
	CAMLparam2(ctx, custom);
	CAMLlocal1(r);
	r = mbedtls_ctr_drbg_seed(CTR(ctx), mbedtls_entropy_func, VOID(p_entropy), NULL, 0);
	CAMLreturn(Val_int(r));
}

// Entropy

CAMLprim value ml_mbedtls_entropy_func(value data, value output, value len) {
	CAMLparam3(data, output, len);
	CAMLlocal1(r);
	r = mbedtls_entropy_func(VOID(data), String_val(output), Int_val(len));
	CAMLreturn(Val_int(r));
}

CAMLprim value ml_mbedtls_entropy_init() {
	CAMLparam0();
	CAMLlocal1(obj);
	mbedtls_entropy_context* entropy = malloc(sizeof(mbedtls_entropy_context));
	obj = caml_alloc(1, Abstract_tag);
	Store_field(obj, 0, (value)entropy);
	mbedtls_entropy_init(entropy);
	CAMLreturn(obj);
}

// Config

CAMLprim void ml_mbedtls_ssl_conf_authmode(value conf, value authmode) {
	CAMLparam2(conf, authmode);
	mbedtls_ssl_conf_authmode(CFG(conf), Int_val(authmode));
	CAMLreturn0;
}

CAMLprim value ml_mbedtls_ssl_config_defaults(value conf, value endpoint, value transport, value preset) {
	CAMLparam4(conf, endpoint, transport, preset);
	CAMLlocal1(r);
	r = mbedtls_ssl_config_defaults(CFG(conf), Int_val(endpoint), Int_val(transport), Int_val(preset));
	CAMLreturn(Val_int(r));
}

CAMLprim void ml_mbedtls_ssl_config_free(value conf) {
	CAMLparam1(conf);
	mbedtls_ssl_config_free(CFG(conf));
	CAMLreturn0;
}

CAMLprim value ml_mbedtls_ssl_config_init() {
	CAMLparam0();
	CAMLlocal1(obj);
	mbedtls_ssl_config* conf = malloc(sizeof(mbedtls_ssl_config));
	obj = caml_alloc(1, Abstract_tag);
	Store_field(obj, 0, (value)conf);
	mbedtls_ssl_config_init(conf);
	// mbedtls_x509_crt cacert;
	// mbedtls_x509_crt_init( &cacert );
	// mbedtls_x509_crt_parse( &cacert, (const unsigned char *) mbedtls_test_cas_pem,
    //                       mbedtls_test_cas_pem_len );
	// mbedtls_ssl_conf_ca_chain( c, &cacert, NULL );
	// mbedtls_debug_set_threshold(5);
	// mbedtls_ssl_conf_dbg(c, debug, NULL);
	CAMLreturn(obj);
}

CAMLprim void ml_mbedtls_ssl_conf_rng(value conf, value p_rng) {
	CAMLparam2(conf, p_rng);
	mbedtls_ssl_conf_rng(CFG(conf), mbedtls_ctr_drbg_random, VOID(p_rng));
	CAMLreturn0;
}

// Ssl

CAMLprim void ml_mbedtls_ssl_free(value ssl) {
	CAMLparam1(ssl);
	mbedtls_ssl_free(SSL(ssl));
	CAMLreturn0;
}

CAMLprim value ml_mbedtls_ssl_init() {
	CAMLparam0();
	CAMLlocal1(obj);
	mbedtls_ssl_context* ssl = malloc(sizeof(mbedtls_ssl_context));
	obj = caml_alloc(1, Abstract_tag);
	Store_field(obj, 0, (value)ssl);
	mbedtls_ssl_init(ssl);
	CAMLreturn(obj);
}

CAMLprim value ml_mbedtls_ssl_handshake(value ssl) {
	CAMLparam1(ssl);
	CAMLlocal1(r);
	r = mbedtls_ssl_handshake(SSL(ssl));
	CAMLreturn(Val_int(r));
}

CAMLprim value ml_mbedtls_ssl_read(value ssl, value buf, value pos, value len) {
	CAMLparam4(ssl, buf, pos, len);
	CAMLlocal1(r);
	r = mbedtls_ssl_read(SSL(ssl), Bytes_val(buf) + Int_val(pos), Int_val(len));
	CAMLreturn(Val_int(r));
}

int bio_write_cb(void* ctx, const unsigned char* buf, size_t len) {
	CAMLparam0();
	CAMLlocal3(r, s, vctx);
	vctx = (value)ctx;
	s = caml_alloc_initialized_string(len, buf);
	r = caml_callback2(Field(vctx, 1), Field(vctx, 0), s);
	return Int_val(r);
}

int bio_read_cb(void* ctx, unsigned char* buf, size_t len) {
	CAMLparam0();
	CAMLlocal3(r, s, vctx);
	vctx = (value)ctx;
	s = caml_alloc_string(len);
	r = caml_callback2(Field(vctx, 2), Field(vctx, 0), s);
	memcpy(buf, String_val(s), len);
	return Int_val(r);
}

CAMLprim void ml_mbedtls_ssl_set_bio(value ssl, value p_bio, value f_send, value f_recv) {
	CAMLparam4(ssl, p_bio, f_send, f_recv);
	CAMLlocal1(ctx);
	ctx = caml_alloc(3, 0);
	Store_field(ctx, 0, p_bio);
	Store_field(ctx, 1, f_send);
	Store_field(ctx, 2, f_recv);
	mbedtls_ssl_set_bio(SSL(ssl), (void*)ctx, bio_write_cb, bio_read_cb, NULL);
	CAMLreturn0;
}

CAMLprim value ml_mbedtls_ssl_set_hostname(value ssl, value hostname) {
	CAMLparam2(ssl, hostname);
	CAMLlocal1(r);
	r = mbedtls_ssl_set_hostname(SSL(ssl), String_val(hostname));
	CAMLreturn(r);
}

CAMLprim value ml_mbedtls_ssl_setup(value ssl, value conf) {
	CAMLparam2(ssl, conf);
	CAMLlocal1(r);
	r = mbedtls_ssl_setup(SSL(ssl), CFG(conf));
	CAMLreturn(Val_int(r));
}

CAMLprim value ml_mbedtls_ssl_write(value ssl, value buf, value pos, value len) {
	CAMLparam4(ssl, buf, pos, len);
	CAMLlocal1(r);
	r = mbedtls_ssl_write(SSL(ssl), Bytes_val(buf) + Int_val(pos), Int_val(len));
	CAMLreturn(Val_int(r));
}

// glue

static value build_fields(int num_fields, const char* names[], int values[]) {
	CAMLparam0();
	CAMLlocal2(ret, tuple);
	ret = caml_alloc(num_fields, 0);
	for (int i = 0; i < num_fields; ++i) {
		tuple = caml_alloc_tuple(2);
		Store_field(tuple, 0, caml_copy_string(names[i]));
		Store_field(tuple, 1, Val_int(values[i]));
		Store_field(ret, i, tuple);
	}
	CAMLreturn(ret);
}

CAMLprim value hx_get_ssl_authmode_flags(value unit) {
	CAMLparam1(unit);
	const char* names[] = {"SSL_VERIFY_NONE", "SSL_VERIFY_OPTIONAL", "SSL_VERIFY_REQUIRED"};
	int values[] = {MBEDTLS_SSL_VERIFY_NONE, MBEDTLS_SSL_VERIFY_OPTIONAL, MBEDTLS_SSL_VERIFY_REQUIRED};
	CAMLreturn(build_fields(sizeof(values) / sizeof(values[0]), names, values));
}

CAMLprim value hx_get_ssl_endpoint_flags(value unit) {
	CAMLparam1(unit);
	const char* names[] = {"SSL_IS_CLIENT", "SSL_IS_SERVER"};
	int values[] = {MBEDTLS_SSL_IS_CLIENT, MBEDTLS_SSL_IS_SERVER};
	CAMLreturn(build_fields(sizeof(values) / sizeof(values[0]), names, values));
}

CAMLprim value hx_get_ssl_preset_flags(value unit) {
	CAMLparam1(unit);
	const char* names[] = {"SSL_PRESET_DEFAULT", "SSL_PRESET_SUITEB"};
	int values[] = {MBEDTLS_SSL_PRESET_DEFAULT, MBEDTLS_SSL_PRESET_SUITEB};
	CAMLreturn(build_fields(sizeof(values) / sizeof(values[0]), names, values));
}

CAMLprim value hx_get_ssl_transport_flags(value unit) {
	CAMLparam1(unit);
	const char* names[] = {"SSL_TRANSPORT_STREAM", "SSL_TRANSPORT_DATAGRAM"};
	int values[] = {MBEDTLS_SSL_TRANSPORT_STREAM, MBEDTLS_SSL_TRANSPORT_DATAGRAM};
	CAMLreturn(build_fields(sizeof(values) / sizeof(values[0]), names, values));
}