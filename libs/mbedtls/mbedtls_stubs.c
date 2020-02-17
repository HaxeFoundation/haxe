#include <ctype.h>
#include <string.h>
#include <stdio.h>

#ifdef _WIN32
#include <windows.h>
#include <wincrypt.h>
#endif

#ifdef __APPLE__
#include <Security/Security.h>
#endif

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>

#include "mbedtls/debug.h"
#include "mbedtls/error.h"
#include "mbedtls/config.h"
#include "mbedtls/ssl.h"
#include "mbedtls/entropy.h"
#include "mbedtls/ctr_drbg.h"
#include "mbedtls/certs.h"
#include "mbedtls/oid.h"

#define PVoid_val(v) (*((void**) Data_custom_val(v)))

void debug(void* ctx, int debug_level, const char* file_name, int line, const char* message) {
	printf("%s:%i: %s", file_name, line, message);
}

#define Val_none Val_int(0)

static value Val_some(value v) {
    CAMLparam1(v);
    CAMLlocal1(some);
    some = caml_alloc(1, 0);
    Store_field(some, 0, v);
    CAMLreturn(some);
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

#define CtrDrbg_val(v) (*((mbedtls_ctr_drbg_context**) Data_custom_val(v)))

static void ml_mbedtls_ctr_drbg_finalize(value v) {
	mbedtls_ctr_drbg_context* ctr_drbg = CtrDrbg_val(v);
	if (ctr_drbg != NULL) {
		mbedtls_ctr_drbg_free(ctr_drbg);
	}
}

static struct custom_operations ctr_drbg_ops = {
	.identifier  = "ml_ctr_drbg",
	.finalize    = ml_mbedtls_ctr_drbg_finalize,
	.compare     = custom_compare_default,
	.hash        = custom_hash_default,
	.serialize   = custom_serialize_default,
	.deserialize = custom_deserialize_default,
};

CAMLprim value ml_mbedtls_ctr_drbg_init(void) {
	CAMLparam0();
	CAMLlocal1(obj);
	obj = caml_alloc_custom(&ctr_drbg_ops, sizeof(mbedtls_ctr_drbg_context*), 0, 1);
	mbedtls_ctr_drbg_context* ctr_drbg = malloc(sizeof(mbedtls_ctr_drbg_context));
	mbedtls_ctr_drbg_init(ctr_drbg);
	CtrDrbg_val(obj) = ctr_drbg;
	CAMLreturn(obj);
}

CAMLprim value ml_mbedtls_ctr_drbg_random(value p_rng, value output, value output_len) {
	CAMLparam3(p_rng, output, output_len);
	CAMLreturn(Val_int(mbedtls_ctr_drbg_random(CtrDrbg_val(p_rng), String_val(output), Int_val(output_len))));
}

CAMLprim value ml_mbedtls_ctr_drbg_seed(value ctx, value p_entropy, value custom) {
	CAMLparam2(ctx, custom);
	CAMLreturn(Val_int(mbedtls_ctr_drbg_seed(CtrDrbg_val(ctx), mbedtls_entropy_func, PVoid_val(p_entropy), NULL, 0)));
}

// Entropy

#define Entropy_val(v) (*((mbedtls_entropy_context**) Data_custom_val(v)))

static void ml_mbedtls_entropy_finalize(value v) {
	mbedtls_entropy_context* entropy = Entropy_val(v);
	if (entropy != NULL) {
		mbedtls_entropy_free(entropy);
	}
}

static struct custom_operations entropy_ops = {
	.identifier  = "ml_entropy",
	.finalize    = ml_mbedtls_entropy_finalize,
	.compare     = custom_compare_default,
	.hash        = custom_hash_default,
	.serialize   = custom_serialize_default,
	.deserialize = custom_deserialize_default,
};

CAMLprim value ml_mbedtls_entropy_init(void) {
	CAMLparam0();
	CAMLlocal1(obj);
	obj = caml_alloc_custom(&entropy_ops, sizeof(mbedtls_entropy_context*), 0, 1);
	mbedtls_entropy_context* entropy = malloc(sizeof(mbedtls_entropy_context));
	mbedtls_entropy_init(entropy);
	Entropy_val(obj) = entropy;
	CAMLreturn(obj);
}

CAMLprim value ml_mbedtls_entropy_func(value data, value output, value len) {
	CAMLparam3(data, output, len);
	CAMLreturn(Val_int(mbedtls_entropy_func(PVoid_val(data), String_val(output), Int_val(len))));
}

// Certificate

#define X509Crt_val(v) (*((mbedtls_x509_crt**) Data_custom_val(v)))

static void ml_mbedtls_x509_crt_finalize(value v) {
	mbedtls_x509_crt* x509_crt = X509Crt_val(v);
	if (x509_crt != NULL) {
		mbedtls_x509_crt_free(x509_crt);
	}
}

static struct custom_operations x509_crt_ops = {
	.identifier  = "ml_x509_crt",
	.finalize    = ml_mbedtls_x509_crt_finalize,
	.compare     = custom_compare_default,
	.hash        = custom_hash_default,
	.serialize   = custom_serialize_default,
	.deserialize = custom_deserialize_default,
};

CAMLprim value ml_mbedtls_x509_crt_init(void) {
	CAMLparam0();
	CAMLlocal1(obj);
	obj = caml_alloc_custom(&x509_crt_ops, sizeof(mbedtls_x509_crt*), 0, 1);
	mbedtls_x509_crt* x509_crt = malloc(sizeof(mbedtls_x509_crt));
	mbedtls_x509_crt_init(x509_crt);
	X509Crt_val(obj) = x509_crt;
	CAMLreturn(obj);
}

CAMLprim value ml_mbedtls_x509_next(value chain) {
	CAMLparam1(chain);
	CAMLlocal2(r, obj);
	mbedtls_x509_crt* cert = X509Crt_val(chain);
	if (cert->next == NULL) {
		CAMLreturn(Val_none);
	}
	obj = caml_alloc_custom(&x509_crt_ops, sizeof(mbedtls_x509_crt*), 0, 1);
	X509Crt_val(obj) = cert->next;
	CAMLreturn(Val_some(obj));
}

CAMLprim value ml_mbedtls_x509_crt_parse(value chain, value bytes) {
	CAMLparam2(chain, bytes);
	const char* buf = String_val(bytes);
	int len = caml_string_length(bytes);
	CAMLreturn(Val_int(mbedtls_x509_crt_parse(X509Crt_val(chain), buf, len + 1)));
}

CAMLprim value ml_mbedtls_x509_crt_parse_file(value chain, value path) {
	CAMLparam2(chain, path);
	CAMLreturn(Val_int(mbedtls_x509_crt_parse_file(X509Crt_val(chain), String_val(path))));
}

CAMLprim value ml_mbedtls_x509_crt_parse_path(value chain, value path) {
	CAMLparam2(chain, path);
	CAMLreturn(Val_int(mbedtls_x509_crt_parse_path(X509Crt_val(chain), String_val(path))));
}

// Certificate Haxe API

value caml_string_of_asn1_buf(mbedtls_asn1_buf* dat) {
	CAMLparam0();
	CAMLlocal1(s);
	s = caml_alloc_string(dat->len);
	memcpy(String_val(s), dat->p, dat->len);
	CAMLreturn(s);
}

CAMLprim value hx_cert_get_alt_names(value chain) {
	CAMLparam1(chain);
	CAMLlocal1(obj);
	mbedtls_x509_crt* cert = X509Crt_val(chain);
	if (cert->ext_types & MBEDTLS_X509_EXT_SUBJECT_ALT_NAME == 0 || &cert->subject_alt_names == NULL) {
		obj = Atom(0);
	} else {
		mbedtls_asn1_sequence* cur = &cert->subject_alt_names;
		int i = 0;
		while (cur != NULL) {
			++i;
			cur = cur->next;
		}
		obj = caml_alloc(i, 0);
		cur = &cert->subject_alt_names;
		i = 0;
		while (cur != NULL) {
			Store_field(obj, i, caml_string_of_asn1_buf(&cur->buf));
			++i;
			cur = cur->next;
		}
	}
	CAMLreturn(obj);
}

CAMLprim value hx_cert_get_subject(value chain, value objname) {
	CAMLparam2(chain, objname);
	mbedtls_x509_name *obj;
	mbedtls_x509_crt* cert = X509Crt_val(chain);
	const char *oname, *rname;
	obj = &cert->subject;
	rname = String_val(objname);
	while (obj != NULL) {
		int r = mbedtls_oid_get_attr_short_name(&obj->oid, &oname);
		if (r == 0 && strcmp(oname, rname) == 0) {
			CAMLreturn(Val_some(caml_string_of_asn1_buf(&obj->val)));
		}
		obj = obj->next;
	}
	CAMLreturn(Val_none);
}

CAMLprim value hx_cert_get_issuer(value chain, value objname) {
	CAMLparam2(chain, objname);
	mbedtls_x509_name *obj;
	mbedtls_x509_crt* cert = X509Crt_val(chain);
	int r;
	const char *oname, *rname;
	obj = &cert->issuer;
	rname = String_val(objname);
	while (obj != NULL) {
		r = mbedtls_oid_get_attr_short_name(&obj->oid, &oname);
		if (r == 0 && strcmp(oname, rname) == 0) {
			CAMLreturn(Val_some(caml_string_of_asn1_buf(&obj->val)));
		}
		obj = obj->next;
	}
	CAMLreturn(Val_none);
}

time_t time_to_time_t(mbedtls_x509_time* t) {
	struct tm info;
	info.tm_year = t->year - 1900;
	info.tm_mon = t->mon - 1;
	info.tm_mday = t->day;
	info.tm_hour = t->hour;
	info.tm_min = t->min;
	info.tm_sec = t->sec;
	return mktime(&info);
}

CAMLprim value hx_cert_get_notafter(value chain) {
	CAMLparam1(chain);
	mbedtls_x509_crt* cert = X509Crt_val(chain);
	mbedtls_x509_time *t = &cert->valid_to;
	time_t time = time_to_time_t(t);
	CAMLreturn(caml_copy_double((double)time));
}

CAMLprim value hx_cert_get_notbefore(value chain) {
	CAMLparam1(chain);
	mbedtls_x509_crt* cert = X509Crt_val(chain);
	mbedtls_x509_time *t = &cert->valid_from;
	time_t time = time_to_time_t(t);
	CAMLreturn(caml_copy_double((double)time));
}

// Config

#define Config_val(v) (*((mbedtls_ssl_config**) Data_custom_val(v)))

static void ml_mbedtls_ssl_config_finalize(value v) {
	mbedtls_ssl_config* ssl_config = Config_val(v);
	if (ssl_config != NULL) {
		mbedtls_ssl_config_free(ssl_config);
	}
}

static struct custom_operations ssl_config_ops = {
	.identifier  = "ml_ssl_config",
	.finalize    = ml_mbedtls_ssl_config_finalize,
	.compare     = custom_compare_default,
	.hash        = custom_hash_default,
	.serialize   = custom_serialize_default,
	.deserialize = custom_deserialize_default,
};

CAMLprim value ml_mbedtls_ssl_config_init(void) {
	CAMLparam0();
	CAMLlocal1(obj);
	obj = caml_alloc_custom(&ssl_config_ops, sizeof(mbedtls_ssl_config*), 0, 1);
	mbedtls_ssl_config* ssl_config = malloc(sizeof(mbedtls_ssl_config));
	mbedtls_ssl_config_init(ssl_config);
	Config_val(obj) = ssl_config;
	CAMLreturn(obj);
}

CAMLprim value ml_mbedtls_ssl_conf_authmode(value conf, value authmode) {
	CAMLparam2(conf, authmode);
	mbedtls_ssl_conf_authmode(Config_val(conf), Int_val(authmode));
	CAMLreturn(Val_unit);
}

CAMLprim value ml_mbedtls_ssl_conf_ca_chain(value conf, value ca_chain) {
	CAMLparam2(conf, ca_chain);
	mbedtls_ssl_conf_ca_chain(Config_val(conf), X509Crt_val(ca_chain), NULL);
	CAMLreturn(Val_unit);
}

CAMLprim value ml_mbedtls_ssl_config_defaults(value conf, value endpoint, value transport, value preset) {
	CAMLparam4(conf, endpoint, transport, preset);
	CAMLreturn(Val_int(mbedtls_ssl_config_defaults(Config_val(conf), Int_val(endpoint), Int_val(transport), Int_val(preset))));
}

CAMLprim value ml_mbedtls_ssl_conf_rng(value conf, value p_rng) {
	CAMLparam2(conf, p_rng);
	mbedtls_ssl_conf_rng(Config_val(conf), mbedtls_ctr_drbg_random, PVoid_val(p_rng));
	CAMLreturn(Val_unit);
}

// Pk

#define PkContext_val(v) (*((mbedtls_pk_context**) Data_custom_val(v)))

static void ml_mbedtls_pk_context_finalize(value v) {
	mbedtls_pk_context* pk_context = PkContext_val(v);
	if (pk_context != NULL) {
		mbedtls_pk_free(pk_context);
	}
}

static struct custom_operations pk_context_ops = {
	.identifier  = "ml_pk_context",
	.finalize    = ml_mbedtls_pk_context_finalize,
	.compare     = custom_compare_default,
	.hash        = custom_hash_default,
	.serialize   = custom_serialize_default,
	.deserialize = custom_deserialize_default,
};

CAMLprim value ml_mbedtls_pk_init(void) {
	CAMLparam0();
	CAMLlocal1(obj);
	obj = caml_alloc_custom(&pk_context_ops, sizeof(mbedtls_pk_context*), 0, 1);
	mbedtls_pk_context* pk_context = malloc(sizeof(mbedtls_pk_context));
	mbedtls_pk_init(pk_context);
	PkContext_val(obj) = pk_context;
	CAMLreturn(obj);
}

CAMLprim value ml_mbedtls_pk_parse_key(value ctx, value key, value password) {
	CAMLparam3(ctx, key, password);
	const char* pwd = NULL;
	size_t pwdlen = 0;
	if (password != Val_none) {
		pwd = String_val(Field(password, 0));
		pwdlen = caml_string_length(Field(password, 0));
	}
	CAMLreturn(mbedtls_pk_parse_key(PkContext_val(ctx), String_val(key), caml_string_length(key) + 1, pwd, pwdlen));
}

CAMLprim value ml_mbedtls_pk_parse_keyfile(value ctx, value path, value password) {
	CAMLparam3(ctx, path, password);
	const char* pwd = NULL;
	if (password != Val_none) {
		pwd = String_val(Field(password, 0));
	}
	CAMLreturn(mbedtls_pk_parse_keyfile(PkContext_val(ctx), String_val(path), pwd));
}

CAMLprim value ml_mbedtls_pk_parse_public_key(value ctx, value key) {
	CAMLparam2(ctx, key);
	CAMLreturn(mbedtls_pk_parse_public_key(PkContext_val(ctx), String_val(key), caml_string_length(key) + 1));
}

CAMLprim value ml_mbedtls_pk_parse_public_keyfile(value ctx, value path) {
	CAMLparam2(ctx, path);
	CAMLreturn(mbedtls_pk_parse_public_keyfile(PkContext_val(ctx), String_val(path)));
}

// Ssl

#define SslContext_val(v) (*((mbedtls_ssl_context**) Data_custom_val(v)))

static void ml_mbedtls_ssl_context_finalize(value v) {
	mbedtls_ssl_context* ssl_context = SslContext_val(v);
	if (ssl_context != NULL) {
		mbedtls_ssl_free(ssl_context);
	}
}

static struct custom_operations ssl_context_ops = {
	.identifier  = "ml_ssl_context",
	.finalize    = ml_mbedtls_ssl_context_finalize,
	.compare     = custom_compare_default,
	.hash        = custom_hash_default,
	.serialize   = custom_serialize_default,
	.deserialize = custom_deserialize_default,
};

CAMLprim value ml_mbedtls_ssl_init(void) {
	CAMLparam0();
	CAMLlocal1(obj);
	obj = caml_alloc_custom(&ssl_context_ops, sizeof(mbedtls_ssl_context*), 0, 1);
	mbedtls_ssl_context* ssl_context = malloc(sizeof(mbedtls_ssl_context));
	mbedtls_ssl_init(ssl_context);
	SslContext_val(obj) = ssl_context;
	CAMLreturn(obj);
}

CAMLprim value ml_mbedtls_ssl_get_peer_cert(value ssl) {
	CAMLparam1(ssl);
	CAMLlocal1(obj);
	mbedtls_ssl_context* ssl_context = SslContext_val(ssl);
	mbedtls_x509_crt* crt = (mbedtls_x509_crt*)mbedtls_ssl_get_peer_cert(ssl_context);
	if (crt == NULL) {
		CAMLreturn(Val_none);
	}
	obj = caml_alloc_custom(&x509_crt_ops, sizeof(mbedtls_x509_crt*), 0, 1);
	X509Crt_val(obj) = crt;
	CAMLreturn(Val_some(obj));
}

CAMLprim value ml_mbedtls_ssl_handshake(value ssl) {
	CAMLparam1(ssl);
	CAMLreturn(Val_int(mbedtls_ssl_handshake(SslContext_val(ssl))));
}

CAMLprim value ml_mbedtls_ssl_read(value ssl, value buf, value pos, value len) {
	CAMLparam4(ssl, buf, pos, len);
	CAMLreturn(Val_int(mbedtls_ssl_read(SslContext_val(ssl), String_val(buf) + Int_val(pos), Int_val(len))));
}

static int bio_write_cb(void* ctx, const unsigned char* buf, size_t len) {
	CAMLparam0();
	CAMLlocal3(r, s, vctx);
	vctx = (value)ctx;
	s = caml_alloc_string(len);
	memcpy(String_val(s), buf, len);
	r = caml_callback2(Field(vctx, 1), Field(vctx, 0), s);
	CAMLreturn(Int_val(r));
}

static int bio_read_cb(void* ctx, unsigned char* buf, size_t len) {
	CAMLparam0();
	CAMLlocal3(r, s, vctx);
	vctx = (value)ctx;
	s = caml_alloc_string(len);
	r = caml_callback2(Field(vctx, 2), Field(vctx, 0), s);
	memcpy(buf, String_val(s), len);
	CAMLreturn(Int_val(r));
}

CAMLprim value ml_mbedtls_ssl_set_bio(value ssl, value p_bio, value f_send, value f_recv) {
	CAMLparam4(ssl, p_bio, f_send, f_recv);
	CAMLlocal1(ctx);
	ctx = caml_alloc(3, 0);
	Store_field(ctx, 0, p_bio);
	Store_field(ctx, 1, f_send);
	Store_field(ctx, 2, f_recv);
	mbedtls_ssl_set_bio(SslContext_val(ssl), (void*)ctx, bio_write_cb, bio_read_cb, NULL);
	CAMLreturn(Val_unit);
}

CAMLprim value ml_mbedtls_ssl_set_hostname(value ssl, value hostname) {
	CAMLparam2(ssl, hostname);
	CAMLreturn(Val_int(mbedtls_ssl_set_hostname(SslContext_val(ssl), String_val(hostname))));
}

CAMLprim value ml_mbedtls_ssl_setup(value ssl, value conf) {
	CAMLparam2(ssl, conf);
	CAMLreturn(Val_int(mbedtls_ssl_setup(SslContext_val(ssl), Config_val(conf))));
}

CAMLprim value ml_mbedtls_ssl_write(value ssl, value buf, value pos, value len) {
	CAMLparam4(ssl, buf, pos, len);
	CAMLreturn(Val_int(mbedtls_ssl_write(SslContext_val(ssl), String_val(buf) + Int_val(pos), Int_val(len))));
}

// glue

CAMLprim value hx_cert_load_defaults(value certificate) {
	CAMLparam1(certificate);
	int r = 1;

	mbedtls_x509_crt *chain = X509Crt_val(certificate);

	#ifdef _WIN32
	HCERTSTORE store;
	PCCERT_CONTEXT cert;

	if (store = CertOpenSystemStore(0, "Root")) {
		cert = NULL;
		while (cert = CertEnumCertificatesInStore(store, cert)) {
			r = mbedtls_x509_crt_parse_der(chain, (unsigned char *)cert->pbCertEncoded, cert->cbCertEncoded);
			if (r != 0) {
				CAMLreturn(Val_int(r));
			}
		}
		CertCloseStore(store, 0);
	}
	#endif

	#ifdef __APPLE__
	CFMutableDictionaryRef search;
	CFArrayRef result;
	SecKeychainRef keychain;
	SecCertificateRef item;
	CFDataRef dat;
	// Load keychain
	if (SecKeychainOpen("/System/Library/Keychains/SystemRootCertificates.keychain", &keychain) == errSecSuccess) {
		// Search for certificates
		search = CFDictionaryCreateMutable(NULL, 0, NULL, NULL);
		CFDictionarySetValue(search, kSecClass, kSecClassCertificate);
		CFDictionarySetValue(search, kSecMatchLimit, kSecMatchLimitAll);
		CFDictionarySetValue(search, kSecReturnRef, kCFBooleanTrue);
		CFDictionarySetValue(search, kSecMatchSearchList, CFArrayCreate(NULL, (const void **)&keychain, 1, NULL));
		if (SecItemCopyMatching(search, (CFTypeRef *)&result) == errSecSuccess) {
			CFIndex n = CFArrayGetCount(result);
			for (CFIndex i = 0; i < n; i++) {
				item = (SecCertificateRef)CFArrayGetValueAtIndex(result, i);

				// Get certificate in DER format
				dat = SecCertificateCopyData(item);
				if (dat) {
					r = mbedtls_x509_crt_parse_der(chain, (unsigned char *)CFDataGetBytePtr(dat), CFDataGetLength(dat));
					CFRelease(dat);
					if (r != 0) {
						CAMLreturn(Val_int(r));
					}
				}
			}
		}
		CFRelease(keychain);
	}
	#endif

	CAMLreturn(Val_int(r));
}

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