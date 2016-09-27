haxe.crypto.Sha224.encode("") == "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f";
haxe.crypto.Sha224.encode("abc") == "23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7";
haxe.crypto.Sha224.encode("The quick brown fox jumps over the lazy dog") == "730e109bd7a8a32b1cb9d9a09aa2325d2430587ddbc0c38bad911525";
haxe.crypto.Sha224.encode("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq") == "75388b16512776cc5dba5da1fd890150b0c6455cb4f58b1952522525";
haxe.crypto.Sha224.encode("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu") == "c97ca9a559850ce97a04a96def6d99a9e0e0e2ab14e6b8df265fc0b3";