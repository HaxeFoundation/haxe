package haxe.crypto;

import haxe.io.Bytes;

class Des
{
	
    static inline var PC1:Array<Int> =
    [
        57, 49, 41, 33, 25, 17, 9,
        1,  58, 50, 42, 34, 26, 18,
        10, 2,  59, 51, 43, 35, 27,
        19, 11, 3,  60, 52, 44, 36,
        63, 55, 47, 39, 31, 23, 15,
        7,  62, 54, 46, 38, 30, 22,
        14, 6,  61, 53, 45, 37, 29,
        21, 13, 5,  28, 20, 12, 4
    ];

    static inline var PC2:Array<Int>=
    [
        14, 17, 11, 24, 1,  5,
        3,  28, 15, 6,  21, 10,
        23, 19, 12, 4,  26, 8,
        16, 7,  27, 20, 13, 2,
        41, 52, 31, 37, 47, 55,
        30, 40, 51, 45, 33, 48,
        44, 49, 39, 56, 34, 53,
        46, 42, 50, 36, 29, 32
    ];

    static inline var ITERATION_SHIFT:Array<Int> =
    [
        1, 1, 2, 2,
        2, 2, 2, 2,
        1, 2, 2, 2,
        2, 2, 2, 1
    ];

    static inline var E:Array<Int> =
    [
        32, 1,  2,  3,  4,  5,
        4,  5,  6,  7,  8,  9,
        8,  9,  10, 11, 12, 13,
        12, 13, 14, 15, 16, 17,
        16, 17, 18, 19, 20, 21,
        20, 21, 22, 23, 24, 25,
        24, 25, 26, 27, 28, 29,
        28, 29, 30, 31, 32, 1
    ];

    static inline var S_BOX:Array<Array<Array<Int>>> =
    [
     [ // S1
    	[ 14, 4, 13, 1, 2, 15, 11, 8, 3, 10, 6, 12, 5, 9, 0, 7 ],
	[ 0, 15, 7, 4, 14, 2, 13, 1, 10, 6, 12, 11, 9, 5, 3, 8 ],
	[ 4, 1, 14, 8, 13, 6, 2, 11, 15, 12, 9, 7, 3, 10, 5, 0 ],
	[ 15, 12, 8, 2, 4, 9, 1, 7, 5, 11, 3, 14, 10, 0, 6, 13 ]
     ],
     [ // S2
        [ 15, 1, 8, 14, 6, 11, 3, 4, 9, 7, 2, 13, 12, 0, 5, 10 ],
	[ 3, 13, 4, 7, 15, 2, 8, 14, 12, 0, 1, 10, 6, 9, 11, 5 ],
	[ 0, 14, 7, 11, 10, 4, 13, 1, 5, 8, 12, 6, 9, 3, 2, 15 ],
	[ 13, 8, 10, 1, 3, 15, 4, 2, 11, 6, 7, 12, 0, 5, 14, 9 ]
    ],
    [ // S3
        [ 10, 0, 9, 14, 6, 3, 15, 5, 1, 13, 12, 7, 11, 4, 2, 8 ],
	[ 13, 7, 0, 9, 3, 4, 6, 10, 2, 8, 5, 14, 12, 11, 15, 1 ],
	[ 13, 6, 4, 9, 8, 15, 3, 0, 11, 1, 2, 12, 5, 10, 14, 7 ],
	[ 1, 10, 13, 0, 6, 9, 8, 7, 4, 15, 14, 3, 11, 5, 2, 12 ]
    ],
    [ // S4
        [ 7, 13, 14, 3, 0, 6, 9, 10, 1, 2, 8, 5, 11, 12, 4, 15 ],
	[ 13, 8, 11, 5, 6, 15, 0, 3, 4, 7, 2, 12, 1, 10, 14, 9 ],
	[ 10, 6, 9, 0, 12, 11, 7, 13, 15, 1, 3, 14, 5, 2, 8, 4 ],
	[ 3, 15, 0, 6, 10, 1, 13, 8, 9, 4, 5, 11, 12, 7, 2, 14 ]
    ],
    [ // S5
        [ 2, 12, 4, 1, 7, 10, 11, 6, 8, 5, 3, 15, 13, 0, 14, 9 ],
	[ 14, 11, 2, 12, 4, 7, 13, 1, 5, 0, 15, 10, 3, 9, 8, 6 ],
	[ 4, 2, 1, 11, 10, 13, 7, 8, 15, 9, 12, 5, 6, 3, 0, 14 ],
	[ 11, 8, 12, 7, 1, 14, 2, 13, 6, 15, 0, 9, 10, 4, 5, 3 ]
    ],
    [ // S6
        [ 12, 1, 10, 15, 9, 2, 6, 8, 0, 13, 3, 4, 14, 7, 5, 11 ],
	[ 10, 15, 4, 2, 7, 12, 9, 5, 6, 1, 13, 14, 0, 11, 3, 8 ],
	[ 9, 14, 15, 5, 2, 8, 12, 3, 7, 0, 4, 10, 1, 13, 11, 6 ],
	[ 4, 3, 2, 12, 9, 5, 15, 10, 11, 14, 1, 7, 6, 0, 8, 13 ]
    ],
    [ // S7
        [ 4, 11, 2, 14, 15, 0, 8, 13, 3, 12, 9, 7, 5, 10, 6, 1 ],
	[ 13, 0, 11, 7, 4, 9, 1, 10, 14, 3, 5, 12, 2, 15, 8, 6 ],
	[ 1, 4, 11, 13, 12, 3, 7, 14, 10, 15, 6, 8, 0, 5, 9, 2 ],
	[ 6, 11, 13, 8, 1, 4, 10, 7, 9, 5, 0, 15, 14, 2, 3, 12 ]
    ],
    [ // S8
        [ 13, 2, 8, 4, 6, 15, 11, 1, 10, 9, 3, 14, 5, 0, 12, 7 ],
	[ 1, 15, 13, 8, 10, 3, 7, 4, 12, 5, 6, 11, 0, 14, 9, 2 ],
	[ 7, 11, 4, 1, 9, 12, 14, 2, 0, 6, 10, 13, 15, 3, 5, 8 ],
	[ 2, 1, 14, 7, 4, 10, 8, 13, 15, 12, 9, 0, 3, 5, 6, 11 ]
    ]];

    static inline var P_BOX:Array<Int> =
    [
        16, 7,  20, 21,
        29, 12, 28, 17,
        1,  15, 23, 26,
        5,  18, 31, 10,
        2,  8,  24, 14,
        32, 27, 3,  9,
        19, 13, 30, 6,
        22, 11, 4,  25
    ];

    static inline var IP:Array<Int> =
    [
        58, 50, 42, 34, 26, 18, 10, 2,
        60, 52, 44, 36, 28, 20, 12, 4,
        62, 54, 46, 38, 30, 22, 14, 6,
        64, 56, 48, 40, 32, 24, 16, 8,
        57, 49, 41, 33, 25, 17, 9,  1,
        59, 51, 43, 35, 27, 19, 11, 3,
        61, 53, 45, 37, 29, 21, 13, 5,
        63, 55, 47, 39, 31, 23, 15, 7
    ];

    static inline var FP:Array<Int> =
    [
        40, 8, 48, 16, 56, 24, 64, 32,
        39, 7, 47, 15, 55, 23, 63, 31,
        38, 6, 46, 14, 54, 22, 62, 30,
        37, 5, 45, 13, 53, 21, 61, 29,
        36, 4, 44, 12, 52, 20, 60, 28,
        35, 3, 43, 11, 51, 19, 59, 27,
        34, 2, 42, 10, 50, 18, 58, 26,
        33, 1, 41,  9, 49, 17, 57, 25
    ];

    public function new(?key:Bytes, ?iv:Bytes) 
    {
    }

    public function desEncrypt(data:Bytes,keys:Array<Int64>):Int64
    {
        var block = bytesToInt64(data,0);
    	return cipher(block, true, keys);
    }

    public function desDecrypt(block:Int64, keys:Array<Int64>):Bytes
    {
        var data = Bytes.alloc(8);
        int64ToBytes(cipher(block, false,keys),data,0);
    	return data;
    }

    public function keygen(data:Bytes,offset:Int=0):Array<Int64>
    {
        var key:Int64 = bytesToInt64(data,offset);
        var subKeys:Array<Int64> = new Array<Int64>();
    	key = pch1(key);
	var highB:Int32 = (key.high << 4 ) | ( (key.low  >> 28) & 0x0f );
    	var lowB:Int32 = (key.low & 0x0FFFFFFF);

    	for (i in 0...16)
    	{
    		highB = iterationShift(highB, ITERATION_SHIFT[i]);
    		lowB = iterationShift(lowB, ITERATION_SHIFT[i]);
    		var permutedChoice2:Int64 =  Int64.make( highB >> 4, (highB  << 28) | lowB );
            subKeys[i] = pch2(permutedChoice2);
    	}

    	return subKeys;
    }

    private function cipher(block:Int64,  encrypt:Bool, subKeys:Array<Int64>):Int64
    {
    	block = ip(block);

    	var leftBytes:Int32 = block.high;
    	var rightBytes:Int32 = block.low;
    	var feistelOutput;

    	for ( i in 0...16)
    	{
    		if (encrypt)
    			feistelOutput = f(rightBytes, subKeys[i]);
    		else
    			feistelOutput = f(rightBytes, subKeys[15-i]);

    		leftBytes ^= feistelOutput;

    		leftBytes ^= rightBytes;
    		rightBytes ^= leftBytes;
    		leftBytes ^= rightBytes;
    	}

    	var joinedBlock = Int64.make(rightBytes, leftBytes);

    	return fp(joinedBlock);
    }

    public function f(input:Int, rKey:Int64):Int32
    {
        var output:Int64 = ep(input);
        output ^= rKey;

        var sBoxOut:Int = 0;
        for (i in 0...8)
        {
            sBoxOut <<= 4;	
            sBoxOut |= sBoxSwap(i, ((output.high & 0x0000FC00) >> 10) );
            output = output << 6;
        }
        return pb(sBoxOut);
    }

    private function sBoxSwap(num:Int, input:Int):Int
    {
    	var row:Int = ( ((input & 0x20) >> 4) | (input & 0x01));
    	var col:Int = ((input & 0x1E) >> 1);

        return S_BOX[num][row][col];
    }

    private function iterationShift( input:Int, shift:Int):Int
    {
    	return ((input << shift) & 0x0FFFFFFF) | (input >> (28 - shift));
    }

    public function ip(input:Int64):Int64
    {
    	return permutation(input, IP, 64);
    }

    public function fp(input:Int64):Int64
    {
    	return permutation(input, FP, 64);
    }

    private function ep(input:Int):Int64
    {
        return permutation(input, E, 32);
    }

    private function pb(input:Int):Int32
    {
        return permutation(input, P_BOX, 32).low;
    }

    private function pch1(input:Int64):Int64
    {
    	return permutation(input, PC1, 64);
    }

    private function pch2(input:Int64):Int64
    {
    	return permutation(input, PC2, 56);
    }

    private function permutation(input:Int64, indexTable:Array<Int>, len:Int):Int64
    {
        var out:Int64 = 0;
        var idx = 0;

        for ( aIdxTable in indexTable) {
            idx = len - aIdxTable;
            out = (out << 1) | ((input >> idx) & 1);
        }

        return out;
    }

    private function bytesToInt64(bs:Bytes, off:Int):Int64
    {
	var high:Int32 = bs.get(off) << 24;
	high |= bs.get(++off) << 16;
	high |= bs.get(++off) << 8;
	high |= bs.get(++off) ;
    	var low:Int32 = bs.get(++off) << 24;
	low |= bs.get(++off) << 16;
	low |= bs.get(++off) << 8;
	low |= bs.get(++off);

	return Int64.make(high,low);
    }

    private function int64ToBytes(n:Int64, bs:Bytes, off:Int):Void
    {
    	bs.set( off , (n >> 56).low);
	bs.set(++off, (n >> 48).low);
	bs.set(++off, (n >> 40).low);
	bs.set(++off, (n >> 32).low);
	bs.set(++off, (n >> 24).low);
	bs.set(++off, (n >> 16).low);
	bs.set(++off, (n >>  8).low);
	bs.set(++off, (n      ).low);
    }
}
