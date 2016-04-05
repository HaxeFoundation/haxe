#if (cpp || (neko && !macro && !interp))
// Digest
var bin = haxe.io.Bytes.ofString("Hello World!");
var bin2 = haxe.io.Bytes.ofString("Hello World?");
sys.ssl.Digest.make( bin, sys.ssl.DigestAlgorithm.MD5       ).toHex().toLowerCase() == "ed076287532e86365e841e92bfc50d8c";
sys.ssl.Digest.make( bin, sys.ssl.DigestAlgorithm.SHA1      ).toHex().toLowerCase() == "2ef7bde608ce5404e97d5f042f95f89f1c232871";
sys.ssl.Digest.make( bin, sys.ssl.DigestAlgorithm.SHA224    ).toHex().toLowerCase() == "4575bb4ec129df6380cedde6d71217fe0536f8ffc4e18bca530a7a1b";
sys.ssl.Digest.make( bin, sys.ssl.DigestAlgorithm.SHA256    ).toHex().toLowerCase() == "7f83b1657ff1fc53b92dc18148a1d65dfc2d4b1fa3d677284addd200126d9069";
sys.ssl.Digest.make( bin, sys.ssl.DigestAlgorithm.SHA384    ).toHex().toLowerCase() == "bfd76c0ebbd006fee583410547c1887b0292be76d582d96c242d2a792723e3fd6fd061f9d5cfd13b8f961358e6adba4a";
sys.ssl.Digest.make( bin, sys.ssl.DigestAlgorithm.SHA512    ).toHex().toLowerCase() == "861844d6704e8573fec34d967e20bcfef3d424cf48be04e6dc08f2bd58c729743371015ead891cc3cf1c9d34b49264b510751b1ff9e537937bc46b5d6ff4ecc8";
sys.ssl.Digest.make( bin, sys.ssl.DigestAlgorithm.RIPEMD160 ).toHex().toLowerCase() == "8476ee4631b9b30ac2754b0ee0c47e161d3f724c";

// Key loading
var privkey = sys.ssl.Key.readPEM("-----BEGIN RSA PRIVATE KEY-----
Proc-Type: 4,ENCRYPTED
DEK-Info: DES-EDE3-CBC,AE34A89AA63FDD47

/kUM/ePW36lW0pg7X6bujFLISlf6r2u28VJ6sZ2TzS4o6x6o8imzX7q0lmTMYCJa
fpjWp9DJPhY6p1a6uEJoyuIJIWKeb1Hc40cpyrC216kHSkdbizlkcqBVGvSZfL1f
KTuLgM3JvRi/i0YYxI6FlLZcAHPQ5mIwr7Jf45azcotNHKlEf0y5RJEzoOGfAAgK
wsVTU7tvJ7E0NJy+DxTuNKzja95jDiCA/DtCqOHVBRkw1sFSOQEISza5HnPce9PG
/bjwI/I9c/UV0hSRo2clSOslljnH3HsroZw94Ek9OJbHWSTcY7/YHyXVI5bbcU9d
fOATh3RLuaRQ6NxgHGLVbcJL6cM+ZqSnLDEAft3UFfxd1KOFDKj6XCJtnJd/UZYv
rq4hEY6RJI8SzbUs3LVnVrNaZjLop+tCK45a2kwWuZztGGMGPZSn3VP0VAts2Iyo
221a9NN+gzQ2CekU/cz1hi8SiRN6g7zwZwwcFZwFUzTdt4ARZTDbf0GSBaqp9QFK
t8LpPR0SsKOFcrpyFDTJ0Ik0YS9K0hruvYWa1g2bcYVRK5hI1qVPgBP46+CwocPA
bUOdenpC3wpAeqrPsRY9jCYc1yeClZaceLe7aRUJkOggyrzRzDn6kbO20IuSRXBS
CSgYNOnoQD2tSqEXq5ui2Ti/JL7qybHYNG4GMhz3waKyyOl1JdmyvqxI91kp2Iwh
SwG1KfWejIvL1UUvuyZI4qrzUnub498vEmqe2l3U7yS8ggu6D3k6lT42LQ6Rzgey
SqOvWugED/b91dP5AXPq0VJAT1A0Rn75YYYpBuq8SFZRfXmFr5K3qgeCFA3hB+TD
qVHxjirxIGM6umV8tJXWeRLWgIPRoBjTi6zYvckhy5hDCw17GCgwe9jHb3xDfpyx
uoTYcPADk4kwq0ylsp4HIsfZ5AjPMDEXi+2A0a9wREJpK8jAF56SaSHRIAfQNeGn
iuihj9fqM6pDL4jx0lIAMH4c8vypraGFTWO4amMtd/67tVMhO4V1el5w61U5qXXl
RYFVQ6TExpX3uKXgomIcWvs214c1ZOdmpC/OZciqv1ga8+zaswQ4zsIuAQD8BYRe
TBEwNUhRg5PqE0Z6I+beWtkD1giLeaKweJ0Dymh9D2mhX7e1WJIuI9j0m8+kT6/k
jURamNaQYLDnIOpUDPm4TqQ4kH6PYX/I7XHx78cfL+Ki58tyCR0Kgk8TJH4o05pZ
hujvigWTp7WM4bL/0GGVTTsGGT36a300rronkOJGh1gWX9HtSAk3QdnxiNJqrhP6
ZP3mWHaa/qZUr2tfvk5pg7vQ9u1Qny9Xo7JkrNWMUCfvgimYE37BgO8uGsKwjx1a
IMsFlGStF+KKUeCrt2nI+JUFPvQtu//kmnHuqP4dNvr3GTHigS7t2MFYHzKpyIsC
ObNZXrv+q5eMpineWTTetTwfMXX/4oVfAED415i2frAbDeGEH7MFEZcJmkcQY88M
wuvjR5hB9UTMYQPZvRVysrTzCBskKVafMarAeGat42gHVDBKTc5FJ3D4FeQnHN9T
LidMKXdXhV2XNArLSksIaqO3HWiCVwHCsXhwNuYaHuwi2/S2pS6uuzur74fQLYjw
-----END RSA PRIVATE KEY-----
", false, "testpassword");

var pub64 = "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAy0+P8DyzLT52TQ6v7Z8eScRP6QaTQUUa7kyN9mDi2GgvELZB2caBC8uNDNkovLSzMMnM2c3aMkNmKz0NN+IsMV8uFMX2mIiTBDzGuvR6NfEiUIenfcn1ZeX7cWsu0DpJtPCME8nc7MH6FxvK2zZalXO2a2cIDUicI0a4Z1kRaRym2UE+2V6hoT/g5/1e9AxAdCmDzMj4RMhWjRP2XVAZN/6toYMfLmu1aWZr6KJscsS4mG+xDPs3DuCtswLAaMLIrdLNT75ULN170PggUbMSbN/U/SPBjVokq1AX9eVg+0ncrMH/wKA3XosK9Wl10eAnPlnnLm6Wo6T59wJVfOI0xQIDAQAB";
var pubkey = sys.ssl.Key.readDER(haxe.crypto.Base64.decode(pub64), true);

// Digest sign / verify
var validSignature = haxe.crypto.Base64.decode("xLuiR9cnoJ3wGcFw1cbo/MC/FsRhTDwIuiMymNrKXj2a5HyUQe4TMSqA4E0KYnJ5kgNk7HDH2fnkNb+avXmA3SSDZf0SIki+SuDxfzyq67XsPnOXKQ7a7uGPl83hCf0pxeUFmRUdhvE6j7dlWZtAx7JFfuCDrk3uibGeEVzlYBUnYpEGY62Bz+299aZwxt+OLxzXafFiM4miirwdXDvCZYPN47xWsQPk3dWav5h1BM/tFswFDVVpef308NI0AAlx7JV+V6aIHQ1gyd2Ul8h0dRLsZ4dGYKjJ1KjGx2jTwSu7kjlEbC8xDsWPvXtS9T4BDDcRKj24nUca0At+vf9XDQ");

sys.ssl.Digest.sign( bin, privkey, SHA256 ).toHex() == validSignature.toHex();
sys.ssl.Digest.verify( bin, validSignature, pubkey, SHA256 ) == true;
sys.ssl.Digest.verify( bin2, validSignature, pubkey, SHA256 ) == false;

// Certificate loading
var cert = sys.ssl.Certificate.fromString("-----BEGIN CERTIFICATE-----
MIIC5DCCAcygAwIBAgIJAJWLSmriPMaaMA0GCSqGSIb3DQEBCwUAMBYxFDASBgNV
BAoMC1Rlc3QgRGV2IENBMB4XDTE2MDQwNTE5MzE0NVoXDTE3MDQwNTE5MzE0NVow
EjEQMA4GA1UEAwwHZm9vLmJhcjCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoC
ggEBAOPjbM5TJTt6jw6Uc7Yz9/60Krl9zK2Ym1iwO72dXyMKhE+PgxbVPzk4rW+G
yzZ1f3FsOhR/eYNjnYFzf3Zy6sQSZYAGgN50pEBp/toHJOEZs226x6b9TzkG5iTf
3LQocdQKJ4QiMWPah6pktin+3QShIku6Re3AGQd1SvierdaHZog5iolxhj79xIKn
NTTwIC1G5MPSp3GR+V/QK+OtKcWq1UK2gW/Er5EcwpqIkipGmDO6NfhEsaMNeavl
McjyByacnlcgfAbYZjjAR7Ai14538Pac/GElhj8NAu9iz9+ZZknqjtIEPEMDDKbN
lDH6LSEAwM5eWF7C/2FWQ5R3CMECAwEAAaM5MDcwCQYDVR0TBAIwADALBgNVHQ8E
BAMCBeAwHQYDVR0RBBYwFIIHZm9vLmJhcoIJKi5mb28uYmFyMA0GCSqGSIb3DQEB
CwUAA4IBAQDXNBkh3eX8le0sCThAixyiz1JuHbL8Tafe2wIg4HyMzyIjuXpxWfW7
jyev8uyCygi0pf8TyNsKody7HFiQoOVWRnC0LbePyqR2h1p7vT2BgGsIiaSIHbW4
rQoKt0y/KdWiGJLUSMHLEVgWPklGiA9ishReDOGvCVgVd304fvUP6ZV/mxf4ELMT
Np5OwAWm5fI34hFaiE0r0RrIH5+8w/gVHSfd/rIjj8fDOY+sVtfzqrKPQphCyGLF
n/v3xUnxsxxifJh/2MRYfE/O2u30/ykqA1y7X4eIABCbgCC+rz9TVZ/DyjD4BygV
28oMByzQLr93aM98ckvjO5wxRy6Mo71K
-----END CERTIFICATE-----
-----BEGIN CERTIFICATE-----
MIIC6jCCAdKgAwIBAgIJAJWLSmriPMaZMA0GCSqGSIb3DQEBCwUAMBYxFDASBgNV
BAoMC1Rlc3QgRGV2IENBMB4XDTE2MDQwNTE5MzE0NVoXDTE3MDQwNTE5MzE0NVow
FDESMBAGA1UEAwwJbG9jYWxob3N0MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIB
CgKCAQEA0VoP00HD8xWuUIgwWBrBPX1Gnbja8fhSvmnNV/jOuM2MXqvFNkuAyQFF
1SpwkIy1ooRLoBdo8choXe9BjPjTC2YH+Vwemn0J+cIV9aIZnXpmHS5ZCLBW+HDa
2b0AhaabwsxBA1vG+FR8o/BRFkv66NvGbLcsIzu1QioEm2Sd2/oPiyDTmrjPPcEM
a2aZiupJTZ+3KCpLwiU+BysHOkGlrKyjw82UPjN1LQAMuh25MMR+RsupQvACK8Zh
PxWrNK8mrXW8riRhtOk/IVNYlZzNQDn6OiRaVSJ8qGkCPqm7MnHN6WMpFBK3wQ02
2y3NNbeFjp4jYbeQHOEzlHtK6g9YCwIDAQABoz0wOzAJBgNVHRMEAjAAMAsGA1Ud
DwQEAwIF4DAhBgNVHREEGjAYgglsb2NhbGhvc3SCCyoubG9jYWxob3N0MA0GCSqG
SIb3DQEBCwUAA4IBAQD1msPYV9RtW38bt8lpPQMF4Y4BdAJLn4pOVAD055vIWNVw
aBUxLWwcbrQaumBW3umC431htJ5tNNAJotO948j6lXXbRh9VJ4+MOW8XOr+/1zND
/5V3Cz5H4164pNnDpHZ5rW+TqzGSxsrTZ0FERxoCFhQo0oEN+AYV8B6eIddpiNWt
MccLWmDU6orMFKgr00yCrXEaGTii6WDxp4EvY+cu/a7XgABJ/OFFe+zEw0gZR1IC
SNEHn0VdnXaqmzA95EK7N0bBQ/95InK1DGU91UdTeERGM2hDxTTfV9VLHqHSgwWC
0unMCkyljEla27PLNyDnNEFc77cscjtUUEt2V0Jv
-----END CERTIFICATE-----
");

cert.commonName == "foo.bar";
cert.altNames[0] == "foo.bar";
cert.altNames[1] == "*.foo.bar";
cert.notBefore.getFullYear() == 2016;
cert.notAfter.getFullYear() == 2017;
cert.issuer("O") == "Test Dev CA";

var cert2 = cert.next();
cert2.commonName == "localhost";

#end
