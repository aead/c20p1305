// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This file was originally from https://golang.org/cl/24717 by Vlad Krasnov of CloudFlare.

// +build go1.7,amd64,!gccgo,!appengine

#include "const_amd64.s"

// General register allocation
#define oup DI
#define inp SI
#define inl BX
#define adp CX // free to reuse, after we hash the additional data
#define keyp R8 // free to reuse, when we copy the key to stack
#define itr2 R9 // general iterator
#define itr1 CX // general iterator
#define acc0 R10
#define acc1 R11
#define acc2 R12
#define t0 R13
#define t1 R14
#define t2 R15
#define t3 R8
// Register and stack allocation for the SSE code
#define rStore (0*16)(BP)
#define sStore (1*16)(BP)
#define state1Store (2*16)(BP)
#define state2Store (3*16)(BP)
#define tmpStore (4*16)(BP)
#define ctr0Store (5*16)(BP)
#define ctr1Store (6*16)(BP)
#define ctr2Store (7*16)(BP)
#define ctr3Store (8*16)(BP)
#define A0 X0
#define A1 X1
#define A2 X2
#define B0 X3
#define B1 X4
#define B2 X5
#define C0 X6
#define C1 X7
#define C2 X8
#define D0 X9
#define D1 X10
#define D2 X11
#define T0 X12
#define T1 X13
#define T2 X14
#define T3 X15
#define A3 T0
#define B3 T1
#define C3 T2
#define D3 T3

#define CHACHA20_QROUND(A, B, C, D, T) \
	PADDD  B, A;            \
	PXOR   A, D;            \
	PSHUFB ·rol16<>(SB), D; \
	PADDD  D, C;            \
	PXOR   C, B;            \
	MOVO   B, T;            \
	PSLLL  $12, T;          \
	PSRLL  $20, B;          \
	PXOR   T, B             \
	PADDD  B, A;            \
	PXOR   A, D;            \
	PSHUFB ·rol8<>(SB), D;  \
	PADDD  D, C;            \
	PXOR   C, B;            \
	MOVO   B, T;            \
	PSLLL  $7, T;           \
	PSRLL  $25, B;          \
	PXOR   T, B

#define CHACHA20_SHUF(c0, c1, c2, B, C, D) \
	PSHUFD $c0, B, B; \
	PSHUFD $c1, C, C; \
	PSHUFD $c2, D, D

#define POLY1305_ADD(src, h0, h1, h2) \
	ADDQ src, h0; \
	ADCQ 8+src, h1; \ 
	ADCQ $1, h2

// The POLY1305_MUL makro is taken from the sum_amd64.s file in /x/crypto/poly1305
#define POLY1305_MUL(h0, h1, h2, r0, r1, t0, t1, t2, t3) \
	MOVQ  r0, AX;                  \
	MULQ  h0;                      \
	MOVQ  AX, t0;                  \
	MOVQ  DX, t1;                  \
	MOVQ  r0, AX;                  \
	MULQ  h1;                      \
	ADDQ  AX, t1;                  \
	ADCQ  $0, DX;                  \
	MOVQ  r0, t2;                  \
	IMULQ h2, t2;                  \
	ADDQ  DX, t2;                  \
	                               \
	MOVQ  r1, AX;                  \
	MULQ  h0;                      \
	ADDQ  AX, t1;                  \
	ADCQ  $0, DX;                  \
	MOVQ  DX, h0;                  \
	MOVQ  r1, t3;                  \
	IMULQ h2, t3;                  \
	MOVQ  r1, AX;                  \
	MULQ  h1;                      \
	ADDQ  AX, t2;                  \
	ADCQ  DX, t3;                  \
	ADDQ  h0, t2;                  \
	ADCQ  $0, t3;                  \
	                               \
	MOVQ  t0, h0;                  \
	MOVQ  t1, h1;                  \
	MOVQ  t2, h2;                  \
	ANDQ  $3, h2;                  \
	MOVQ  t2, t0;                  \
	ANDQ  $0xFFFFFFFFFFFFFFFC, t0; \
	ADDQ  t0, h0;                  \
	ADCQ  t3, h1;                  \
	ADCQ  $0, h2;                  \
	SHRQ  $2, t3, t2;              \
	SHRQ  $2, t3;                  \
	ADDQ  t2, h0;                  \
	ADCQ  t3, h1;                  \
	ADCQ  $0, h2

// authAdditionalData authenticates the additional data.
// itr2 := length of additional data
TEXT authAdditionalData<>(SB), NOSPLIT, $0
	XORQ acc0, acc0
	XORQ acc1, acc1
	XORQ acc2, acc2

	CMPQ itr2, $13                 // Special treatment for TLS
	JNE  auth_additional_data_loop

	MOVQ 0(adp), acc0
	MOVQ 5(adp), acc1
	SHRQ $24, acc1
	MOVQ $1, acc2
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)
	RET

auth_additional_data_loop:
	CMPQ itr2, $16
	JB   auth_additional_data_finalize

	POLY1305_ADD(0(adp), acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)

	LEAQ 16(adp), adp
	SUBQ $16, itr2
	JMP  auth_additional_data_loop

auth_additional_data_finalize:
	CMPQ itr2, $0
	JE   auth_additional_data_done

	XORQ t0, t0
	XORQ t1, t1
	XORQ t2, t2
	ADDQ itr2, adp

auth_additional_data_flush:
	SHLQ $8, t1:t0
	SHLQ $8, t0
	MOVB -1(adp), t2
	XORQ t2, t0
	DECQ adp
	DECQ itr2
	JNE  auth_additional_data_flush

	ADDQ t0, acc0
	ADCQ t1, acc1
	ADCQ $1, acc2
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)

auth_additional_data_done:
	RET

// ----------------------------------------------------------------------------
// func chacha20Poly1305Open(dst, key, src, ad []byte) bool
TEXT ·chacha20Poly1305Open(SB), 0, $288-97
	MOVQ dst+0(FP), oup
	MOVQ key+24(FP), keyp
	MOVQ src+48(FP), inp
	MOVQ src_len+56(FP), inl
	MOVQ ad+72(FP), adp

	// For aligned stack access
	MOVQ SP, BP
	ADDQ $32, BP
	ANDQ $-32, BP

	MOVOU ·chacha20Constants<>(SB), A0
	MOVOU (1*16)(keyp), B0
	MOVOU (2*16)(keyp), C0
	MOVOU (3*16)(keyp), D0

	CMPQ inl, $128
	JA   open_sse_start // skip special optimization for short buffers

	// Special optimization for buffers smaller than 129 bytes - about 16% faster
	// For up to 128 bytes of ciphertext and 64 bytes for the poly key, we require to process three blocks
	MOVO  A0, A1
	MOVO  B0, B1
	MOVO  C0, C1
	MOVO  D0, D1
	PADDL ·sseIncMask<>(SB), D1
	MOVO  A1, A2
	MOVO  B1, B2
	MOVO  C1, C2
	MOVO  D1, D2
	PADDL ·sseIncMask<>(SB), D2
	MOVO  B0, T1
	MOVO  C0, T2
	MOVO  D1, T3

	MOVQ $10, itr2

open_sse_special_keystream_loop:
	CHACHA20_QROUND(A0, B0, C0, D0, T0)
	CHACHA20_QROUND(A1, B1, C1, D1, T0)
	CHACHA20_QROUND(A2, B2, C2, D2, T0)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B0, C0, D0)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B1, C1, D1)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B2, C2, D2)
	CHACHA20_QROUND(A0, B0, C0, D0, T0)
	CHACHA20_QROUND(A1, B1, C1, D1, T0)
	CHACHA20_QROUND(A2, B2, C2, D2, T0)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B0, C0, D0)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B1, C1, D1)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B2, C2, D2)
	DECQ itr2
	JNE  open_sse_special_keystream_loop

	// A0|B0 hold the poly1305 32-byte key - C0 and D0 can be discarded
	PADDL ·chacha20Constants<>(SB), A0
	PADDL ·chacha20Constants<>(SB), A1
	PADDL ·chacha20Constants<>(SB), A2
	PADDL T1, B0
	PADDL T1, B1
	PADDL T1, B2
	PADDL T2, C1
	PADDL T2, C2
	PADDL T3, D1
	PADDL T3, D2
	PADDL ·sseIncMask<>(SB), D2

	// Clamp and store the key
	PAND  ·polyClampMask<>(SB), A0
	MOVOU A0, rStore
	MOVOU B0, sStore

	// Hash
	MOVQ ad_len+80(FP), itr2
	CALL authAdditionalData<>(SB)

open_sse_special_decrypt_verify_loop:
	CMPQ inl, $16
	JB   open_sse_16_bytes_remaining
	SUBQ $16, inl

	POLY1305_ADD(0(inp), acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)

	MOVOU (inp), T0
	PXOR  T0, A1
	MOVOU A1, (oup)

	LEAQ 16(inp), inp
	LEAQ 16(oup), oup

	// Shift the stream "left"
	MOVO B1, A1
	MOVO C1, B1
	MOVO D1, C1
	MOVO A2, D1
	MOVO B2, A2
	MOVO C2, B2
	MOVO D2, C2
	JMP  open_sse_special_decrypt_verify_loop

// --------------------------------------------------
// end of special version for short buffers

open_sse_start:
	// Store state on stack for future use
	MOVO B0, state1Store
	MOVO C0, state2Store
	MOVO D0, ctr3Store

	// MOVO D0, T1	// maybe unnecessary

	MOVQ $10, itr2

open_sse_create_poly1305_key_loop:
	CHACHA20_QROUND(A0, B0, C0, D0, T0)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B0, C0, D0)
	CHACHA20_QROUND(A0, B0, C0, D0, T0)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B0, C0, D0)
	DECQ itr2
	JNE  open_sse_create_poly1305_key_loop

	// A0|B0 hold the Poly1305 32-byte key, C0,D0 can be discarded
	PADDL ·chacha20Constants<>(SB), A0
	PADDL state1Store, B0

	// Clamp and store the key
	PAND ·polyClampMask<>(SB), A0
	MOVO A0, rStore
	MOVO B0, sStore

	// Hash AD
	MOVQ ad_len+80(FP), itr2
	CALL authAdditionalData<>(SB)

open_sse_main_loop:
	CMPQ inl, $256
	JB   open_sse_main_loop_done

	// Load state, increment counter blocks
	MOVO  ·chacha20Constants<>(SB), A0
	MOVO  state1Store, B0
	MOVO  state2Store, C0
	MOVO  ctr3Store, D0
	PADDL ·sseIncMask<>(SB), D0
	MOVO  A0, A1
	MOVO  B0, B1
	MOVO  C0, C1
	MOVO  D0, D1
	PADDL ·sseIncMask<>(SB), D1
	MOVO  A1, A2
	MOVO  B1, B2
	MOVO  C1, C2
	MOVO  D1, D2
	PADDL ·sseIncMask<>(SB), D2
	MOVO  A2, A3
	MOVO  B2, B3
	MOVO  C2, C3
	MOVO  D2, D3
	PADDL ·sseIncMask<>(SB), D3

	// Store counters
	MOVO D0, ctr0Store
	MOVO D1, ctr1Store
	MOVO D2, ctr2Store
	MOVO D3, ctr3Store

	// There are 10 ChaCha20 iterations of 2QR each, so for 6 iterations we hash 2 blocks
	// and for the remaining 4 only 1 block - for a total of 16
	MOVQ $4, itr1
	MOVQ inp, itr2

open_sse_decrypt_verify_256_loop:
	MOVO C3, tmpStore
	CHACHA20_QROUND(A0, B0, C0, D0, C3)
	CHACHA20_QROUND(A1, B1, C1, D1, C3)
	CHACHA20_QROUND(A2, B2, C2, D2, C3)
	MOVO tmpStore, C3
	MOVO C1, tmpStore
	CHACHA20_QROUND(A3, B3, C3, D3, C1)
	MOVO tmpStore, C1
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B0, C0, D0)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B1, C1, D1)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B2, C2, D2)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B3, C3, D3)

	POLY1305_ADD(0(itr2),acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)
	LEAQ 16(itr2), itr2

	MOVO C3, tmpStore
	CHACHA20_QROUND(A0, B0, C0, D0, C3)
	CHACHA20_QROUND(A1, B1, C1, D1, C3)
	CHACHA20_QROUND(A2, B2, C2, D2, C3)
	MOVO tmpStore, C3
	MOVO C1, tmpStore
	CHACHA20_QROUND(A3, B3, C3, D3, C1)
	MOVO tmpStore, C1
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B0, C0, D0)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B1, C1, D1)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B2, C2, D2)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B3, C3, D3)

	DECQ itr1
	JGE  open_sse_decrypt_verify_256_loop

	POLY1305_ADD(0(itr2),acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)
	LEAQ 16(itr2), itr2

	CMPQ itr1, $-6
	JG   open_sse_decrypt_verify_256_loop

	// Add in the state
	PADDD ·chacha20Constants<>(SB), A0
	PADDD ·chacha20Constants<>(SB), A1
	PADDD ·chacha20Constants<>(SB), A2
	PADDD ·chacha20Constants<>(SB), A3
	PADDD state1Store, B0
	PADDD state1Store, B1
	PADDD state1Store, B2
	PADDD state1Store, B3
	PADDD state2Store, C0
	PADDD state2Store, C1
	PADDD state2Store, C2
	PADDD state2Store, C3
	PADDD ctr0Store, D0
	PADDD ctr1Store, D1
	PADDD ctr2Store, D2
	PADDD ctr3Store, D3

	// Load - xor - store
	MOVO D3, tmpStore
	XOR(oup, inp, 0, A0, B0, C0, D0, D3)
	XOR(oup, inp, 64, A1, B1, C1, D1, D0)
	XOR(oup, inp, 128, A2, B2, C2, D2, D0)
	XOR(oup, inp, 192, A3, B3, C3, tmpStore, D0)

	LEAQ 256(inp), inp
	LEAQ 256(oup), oup
	SUBQ $256, inl
	JMP  open_sse_main_loop

open_sse_main_loop_done:
	// Handle the various tail sizes efficiently
	TESTQ inl, inl
	JE    open_sse_finalize
	CMPQ  inl, $64
	JBE   open_sse_64_bytes_remaining
	CMPQ  inl, $128
	JBE   open_sse_128_bytes_remaining
	CMPQ  inl, $192
	JBE   open_sse_192_bytes_remaining
	JMP   open_sse_256_bytes_remaining

open_sse_16_bytes_remaining:
	TESTQ inl, inl
	JE    open_sse_finalize

	// We can safely load the ciphertext from the end, because it is padded with the MAC
	MOVQ  inl, itr2
	SHLQ  $4, itr2
	LEAQ  ·andMask<>(SB), t0
	MOVOU (inp), T0
	ADDQ  inl, inp
	PAND  -16(t0)(itr2*1), T0
	MOVO  T0, tmpStore
	MOVQ  T0, t0
	MOVQ  8+tmpStore, t1
	PXOR  A1, T0

	// We can only store one byte at a time, since plaintext can be shorter than 16 bytes
open_sse_16_bytes_remaining_store_plaintext:
	MOVQ   T0, t3
	MOVB   t3, (oup)
	PSRLDQ $1, T0
	INCQ   oup
	DECQ   inl
	JNE    open_sse_16_bytes_remaining_store_plaintext

	ADDQ t0, acc0
	ADCQ t1, acc1
	ADCQ $1, acc2
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)
	JMP  open_sse_finalize

// ----------------------------------------------------------------------------
// Special optimization for the last 64 bytes of ciphertext
open_sse_64_bytes_remaining:
	// Need to decrypt up to 64 bytes - prepare single block
	MOVO  ·chacha20Constants<>(SB), A0
	MOVO  state1Store, B0
	MOVO  state2Store, C0
	MOVO  ctr3Store, D0
	PADDL ·sseIncMask<>(SB), D0
	MOVO  D0, ctr0Store
	XORQ  itr2, itr2
	MOVQ  inl, itr1
	CMPQ  itr1, $16
	JB    open_sse_64_bytes_remaining_decrypt

open_sse_64_bytes_remaining_decrypt_verify:
	// Perform ChaCha rounds, while hashing the remaining input
	POLY1305_ADD(0(inp)(itr2*1), acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)
	SUBQ $16, itr1

open_sse_64_bytes_remaining_decrypt:
	ADDQ $16, itr2
	CHACHA20_QROUND(A0, B0, C0, D0, T0)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B0, C0, D0)
	CHACHA20_QROUND(A0, B0, C0, D0, T0)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B0, C0, D0)

	CMPQ itr1, $16
	JAE  open_sse_64_bytes_remaining_decrypt_verify

	CMPQ itr2, $160
	JNE  open_sse_64_bytes_remaining_decrypt

	PADDL ·chacha20Constants<>(SB), A0
	PADDL state1Store, B0
	PADDL state2Store, C0
	PADDL ctr0Store, D0

open_sse_64_bytes_remaining_store_plaintext:
	CMPQ inl, $16
	JB   open_sse_64_bytes_remaining_done
	SUBQ $16, inl

	MOVOU (inp), T0
	PXOR  T0, A0
	MOVOU A0, (oup)
	LEAQ  16(inp), inp
	LEAQ  16(oup), oup

	// Shift the stream "left"
	MOVO B0, A0
	MOVO C0, B0
	MOVO D0, C0
	JMP  open_sse_64_bytes_remaining_store_plaintext

open_sse_64_bytes_remaining_done:
	MOVO A0, A1
	JMP  open_sse_16_bytes_remaining

// ----------------------------------------------------------------------------
// Special optimization for the last 128 bytes of ciphertext
open_sse_128_bytes_remaining:
	// Need to decrypt up to 128 bytes - prepare two blocks
	MOVO  ·chacha20Constants<>(SB), A1
	MOVO  state1Store, B1
	MOVO  state2Store, C1
	MOVO  ctr3Store, D1
	PADDL ·sseIncMask<>(SB), D1
	MOVO  D1, ctr0Store
	MOVO  A1, A0
	MOVO  B1, B0
	MOVO  C1, C0
	MOVO  D1, D0
	PADDL ·sseIncMask<>(SB), D0
	MOVO  D0, ctr1Store
	XORQ  itr2, itr2
	MOVQ  inl, itr1
	ANDQ  $-16, itr1

open_sse_128_bytes_remaining_decrypt_verify:
	// Perform ChaCha rounds, while hashing the remaining input
	POLY1305_ADD(0(inp)(itr2*1), acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)

open_sse_128_bytes_remaining_decrypt:
	ADDQ $16, itr2
	CHACHA20_QROUND(A0, B0, C0, D0, T0)
	CHACHA20_QROUND(A1, B1, C1, D1, T0)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B0, C0, D0)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B1, C1, D1)
	CHACHA20_QROUND(A0, B0, C0, D0, T0)
	CHACHA20_QROUND(A1, B1, C1, D1, T0)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B0, C0, D0)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B1, C1, D1)

	CMPQ itr2, itr1
	JB   open_sse_128_bytes_remaining_decrypt_verify

	CMPQ itr2, $160
	JNE  open_sse_128_bytes_remaining_decrypt

	PADDL ·chacha20Constants<>(SB), A0
	PADDL ·chacha20Constants<>(SB), A1
	PADDL state1Store, B0
	PADDL state1Store, B1
	PADDL state2Store, C0
	PADDL state2Store, C1
	PADDL ctr1Store, D0
	PADDL ctr0Store, D1

	XOR(oup, inp, 0, A1, B1, C1, D1, T0)

	SUBQ $64, inl
	LEAQ 64(inp), inp
	LEAQ 64(oup), oup
	JMP  open_sse_64_bytes_remaining_store_plaintext

// ----------------------------------------------------------------------------
// Special optimization for the last 192 bytes of ciphertext
open_sse_192_bytes_remaining:
	// Need to decrypt up to 192 bytes - prepare three blocks
	MOVO  ·chacha20Constants<>(SB), A2
	MOVO  state1Store, B2
	MOVO  state2Store, C2
	MOVO  ctr3Store, D2
	PADDL ·sseIncMask<>(SB), D2
	MOVO  D2, ctr0Store
	MOVO  A2, A1
	MOVO  B2, B1
	MOVO  C2, C1
	MOVO  D2, D1
	PADDL ·sseIncMask<>(SB), D1
	MOVO  D1, ctr1Store
	MOVO  A1, A0
	MOVO  B1, B0
	MOVO  C1, C0
	MOVO  D1, D0
	PADDL ·sseIncMask<>(SB), D0
	MOVO  D0, ctr2Store

	MOVQ    inl, itr1
	MOVQ    $160, itr2
	CMPQ    itr1, $160
	CMOVQGT itr2, itr1
	ANDQ    $-16, itr1
	XORQ    itr2, itr2

open_sse_192_bytes_remaining_decrypt_verify:
	// Perform ChaCha rounds, while hashing the remaining input
	POLY1305_ADD(0(inp)(itr2*1), acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)

open_sse_192_bytes_remaining_decrypt:
	ADDQ $16, itr2
	CHACHA20_QROUND(A0, B0, C0, D0, T0)
	CHACHA20_QROUND(A1, B1, C1, D1, T0)
	CHACHA20_QROUND(A2, B2, C2, D2, T0)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B0, C0, D0)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B1, C1, D1)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B2, C2, D2)
	CHACHA20_QROUND(A0, B0, C0, D0, T0)
	CHACHA20_QROUND(A1, B1, C1, D1, T0)
	CHACHA20_QROUND(A2, B2, C2, D2, T0)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B0, C0, D0)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B1, C1, D1)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B2, C2, D2)

	CMPQ itr2, itr1
	JB   open_sse_192_bytes_remaining_decrypt_verify

	CMPQ itr2, $160
	JNE  open_sse_192_bytes_remaining_decrypt

	CMPQ inl, $176
	JB   open_sse_192_bytes_remaining_store_plaintext

	POLY1305_ADD(160(inp), acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)

	CMPQ inl, $192
	JB   open_sse_192_bytes_remaining_store_plaintext

	POLY1305_ADD(176(inp), acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)

open_sse_192_bytes_remaining_store_plaintext:
	PADDL ·chacha20Constants<>(SB), A0
	PADDL ·chacha20Constants<>(SB), A1
	PADDL ·chacha20Constants<>(SB), A2
	PADDL state1Store, B0
	PADDL state1Store, B1
	PADDL state1Store, B2
	PADDL state2Store, C0
	PADDL state2Store, C1
	PADDL state2Store, C2
	PADDL ctr2Store, D0
	PADDL ctr1Store, D1
	PADDL ctr0Store, D2

	XOR(oup, inp, 0, A2, B2, C2, D2, T0)
	XOR(oup, inp, 64, A1, B1, C1, D1, T0)

	SUBQ $128, inl
	LEAQ 128(inp), inp
	LEAQ 128(oup), oup
	JMP  open_sse_64_bytes_remaining_store_plaintext

// ----------------------------------------------------------------------------
// Special optimization for the last 256 bytes of ciphertext
open_sse_256_bytes_remaining:
	// Need to decrypt up to 256 bytes - prepare four blocks
	MOVO  ·chacha20Constants<>(SB), A0
	MOVO  state1Store, B0
	MOVO  state2Store, C0
	MOVO  ctr3Store, D0
	PADDL ·sseIncMask<>(SB), D0
	MOVO  A0, A1
	MOVO  B0, B1
	MOVO  C0, C1
	MOVO  D0, D1
	PADDL ·sseIncMask<>(SB), D1
	MOVO  A1, A2
	MOVO  B1, B2
	MOVO  C1, C2
	MOVO  D1, D2
	PADDL ·sseIncMask<>(SB), D2
	MOVO  A2, A3
	MOVO  B2, B3
	MOVO  C2, C3
	MOVO  D2, D3
	PADDL ·sseIncMask<>(SB), D3

	// Store counters
	MOVO D0, ctr0Store; MOVO D1, ctr1Store; MOVO D2, ctr2Store; MOVO D3, ctr3Store
	XORQ itr2, itr2

	// This loop inteleaves 8 ChaCha quarter rounds with 1 poly multiplication
open_sse_256_bytes_remaining_decrypt_verify:
	MOVO C3, tmpStore
	CHACHA20_QROUND(A0, B0, C0, D0, C3)
	CHACHA20_QROUND(A1, B1, C1, D1, C3)
	CHACHA20_QROUND(A2, B2, C2, D2, C3)
	MOVO tmpStore, C3
	MOVO C1, tmpStore
	CHACHA20_QROUND(A3, B3, C3, D3, C1)
	MOVO tmpStore, C1
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B0, C0, D0)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B1, C1, D1)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B2, C2, D2)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B3, C3, D3)

	POLY1305_ADD(0(inp)(itr2*1), acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)

	MOVO C3, tmpStore
	CHACHA20_QROUND(A0, B0, C0, D0, C3)
	CHACHA20_QROUND(A1, B1, C1, D1, C3)
	CHACHA20_QROUND(A2, B2, C2, D2, C3)
	MOVO tmpStore, C3
	MOVO C1, tmpStore
	CHACHA20_QROUND(A3, B3, C3, D3, C1)
	MOVO tmpStore, C1
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B0, C0, D0)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B1, C1, D1)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B2, C2, D2)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B3, C3, D3)

	ADDQ $2*8, itr2
	CMPQ itr2, $160
	JB   open_sse_256_bytes_remaining_decrypt_verify
	MOVQ inl, itr1
	ANDQ $-16, itr1

open_sse_256_bytes_remaining_verify:
	POLY1305_ADD(0(inp)(itr2*1), acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)
	ADDQ $2*8, itr2
	CMPQ itr2, itr1
	JB   open_sse_256_bytes_remaining_verify

	// Add in the state
	PADDD ·chacha20Constants<>(SB), A0
	PADDD ·chacha20Constants<>(SB), A1
	PADDD ·chacha20Constants<>(SB), A2
	PADDD ·chacha20Constants<>(SB), A3
	PADDD state1Store, B0
	PADDD state1Store, B1
	PADDD state1Store, B2
	PADDD state1Store, B3
	PADDD state2Store, C0
	PADDD state2Store, C1
	PADDD state2Store, C2
	PADDD state2Store, C3
	PADDD ctr0Store, D0
	PADDD ctr1Store, D1
	PADDD ctr2Store, D2
	PADDD ctr3Store, D3
	MOVO  D3, tmpStore

	// Load - xor - store
	XOR(oup, inp, 0, A0, B0, C0, D0, D3)
	XOR(oup, inp, 64, A1, B1, C1, D1, D3)
	XOR(oup, inp, 128, A2, B2, C2, D2, D3)

	LEAQ 192(inp), inp
	LEAQ 192(oup), oup
	SUBQ $192, inl
	MOVO A3, A0
	MOVO B3, B0
	MOVO C3, C0
	MOVO tmpStore, D0

	JMP open_sse_64_bytes_remaining_store_plaintext

open_sse_finalize:
	ADDQ ad_len+80(FP), acc0  // Hash length of additional data
	ADCQ src_len+56(FP), acc1 // Hash length of ciphertext
	ADCQ $1, acc2
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)

	// Final reduce
	MOVQ    acc0, t0
	MOVQ    acc1, t1
	SUBQ    $-5, acc0
	SBBQ    $-1, acc1
	SBBQ    $3, acc2
	CMOVQCS t0, acc0
	CMOVQCS t1, acc1

	// Add in the "s" part of the key
	ADDQ 0+sStore, acc0
	ADCQ 8+sStore, acc1

	// Finally, constant time compare to the tag at the end of the message
	XORQ    AX, AX
	MOVQ    $1, DX
	XORQ    (0*8)(inp), acc0
	XORQ    (1*8)(inp), acc1
	ORQ     acc1, acc0
	CMOVQEQ DX, AX

	// Return true iff tags are equal
	MOVB AX, ret+96(FP)
	RET

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// func chacha20Poly1305Seal(dst, key, src, ad []byte)
TEXT ·chacha20Poly1305Seal(SB), 0, $288-96
	MOVQ dst+0(FP), oup
	MOVQ key+24(FP), keyp
	MOVQ src+48(FP), inp
	MOVQ src_len+56(FP), inl
	MOVQ ad+72(FP), adp

	// For aligned stack access
	MOVQ SP, BP
	ADDQ $32, BP
	ANDQ $-32, BP

	// Special optimization for buffers smaller than 129 bytes - about 15% faster
	// Prepare the poly1305 key + 3 blocks of stream
	CMPQ inl, $128
	JA   seal_sse_start // skip special optimization for short buffers

	MOVOU ·chacha20Constants<>(SB), A0
	MOVOU (1*16)(keyp), B0
	MOVOU (2*16)(keyp), C0
	MOVOU (3*16)(keyp), D0
	MOVO  A0, A1
	MOVO  B0, B1
	MOVO  C0, C1
	MOVO  D0, D1
	PADDL ·sseIncMask<>(SB), D1
	MOVO  A1, A2
	MOVO  B1, B2
	MOVO  C1, C2
	MOVO  D1, D2
	PADDL ·sseIncMask<>(SB), D2
	MOVO  B0, T1
	MOVO  C0, T2
	MOVO  D1, T3

	MOVQ $10, itr2

seal_sse_special_keystream_loop:
	CHACHA20_QROUND(A0, B0, C0, D0, T0)
	CHACHA20_QROUND(A1, B1, C1, D1, T0)
	CHACHA20_QROUND(A2, B2, C2, D2, T0)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B0, C0, D0)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B1, C1, D1)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B2, C2, D2)
	CHACHA20_QROUND(A0, B0, C0, D0, T0)
	CHACHA20_QROUND(A1, B1, C1, D1, T0)
	CHACHA20_QROUND(A2, B2, C2, D2, T0)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B0, C0, D0)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B1, C1, D1)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B2, C2, D2)
	DECQ itr2
	JNE  seal_sse_special_keystream_loop

	// A0|B0 hold the Poly1305 32-byte key, C0,D0 can be discarded
	PADDL ·chacha20Constants<>(SB), A0
	PADDL ·chacha20Constants<>(SB), A1
	PADDL ·chacha20Constants<>(SB), A2
	PADDL T1, B0
	PADDL T1, B1
	PADDL T1, B2
	PADDL T2, C1
	PADDL T2, C2
	PADDL T3, D1
	PADDL ·sseIncMask<>(SB), T3
	PADDL T3, D2
	PAND  ·polyClampMask<>(SB), A0
	MOVOU A0, rStore
	MOVOU B0, sStore

	// Hash
	MOVQ ad_len+80(FP), itr2
	CALL authAdditionalData<>(SB)
	XORQ itr1, itr1

sealSSE128SealHash:
	// itr1 holds the number of bytes encrypted but not yet hashed
	CMPQ itr1, $16
	JB   sealSSE128Seal
	POLY1305_ADD(0(oup), acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)

	SUBQ $16, itr1
	ADDQ $16, oup

	JMP sealSSE128SealHash

sealSSE128Seal:
	CMPQ inl, $16
	JB   sealSSETail
	SUBQ $16, inl

	// Load for decryption
	MOVOU (inp), T0
	PXOR  T0, A1
	MOVOU A1, (oup)
	LEAQ  (1*16)(inp), inp
	LEAQ  (1*16)(oup), oup

	// Extract for hashing
	MOVQ   A1, t0
	PSRLDQ $8, A1
	MOVQ   A1, t1
	ADDQ   t0, acc0
	ADCQ   t1, acc1
	ADCQ   $1, acc2
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)

	// Shift the stream "left"
	MOVO B1, A1
	MOVO C1, B1
	MOVO D1, C1
	MOVO A2, D1
	MOVO B2, A2
	MOVO C2, B2
	MOVO D2, C2
	JMP  sealSSE128Seal

seal_sse_start:
	MOVOU ·chacha20Constants<>(SB), A0
	MOVOU (1*16)(keyp), B0
	MOVOU (2*16)(keyp), C0
	MOVOU (3*16)(keyp), D0

	// Store state on stack for future use
	MOVO B0, state1Store
	MOVO C0, state2Store

	// Load state, increment counter blocks
	MOVO  A0, A1
	MOVO  B0, B1
	MOVO  C0, C1
	MOVO  D0, D1
	PADDL ·sseIncMask<>(SB), D1
	MOVO  A1, A2
	MOVO  B1, B2
	MOVO  C1, C2
	MOVO  D1, D2
	PADDL ·sseIncMask<>(SB), D2
	MOVO  A2, A3
	MOVO  B2, B3
	MOVO  C2, C3
	MOVO  D2, D3
	PADDL ·sseIncMask<>(SB), D3

	// Store counters
	MOVO D0, ctr0Store
	MOVO D1, ctr1Store
	MOVO D2, ctr2Store
	MOVO D3, ctr3Store
	MOVQ $10, itr2

seal_sse_keystream_loop:
	MOVO C3, tmpStore
	CHACHA20_QROUND(A0, B0, C0, D0, C3)
	CHACHA20_QROUND(A1, B1, C1, D1, C3)
	CHACHA20_QROUND(A2, B2, C2, D2, C3)
	MOVO tmpStore, C3
	MOVO C1, tmpStore
	CHACHA20_QROUND(A3, B3, C3, D3, C1)
	MOVO tmpStore, C1
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B0, C0, D0)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B1, C1, D1)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B2, C2, D2)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B3, C3, D3)

	MOVO C3, tmpStore
	CHACHA20_QROUND(A0, B0, C0, D0, C3)
	CHACHA20_QROUND(A1, B1, C1, D1, C3)
	CHACHA20_QROUND(A2, B2, C2, D2, C3)
	MOVO tmpStore, C3
	MOVO C1, tmpStore
	CHACHA20_QROUND(A3, B3, C3, D3, C1)
	MOVO tmpStore, C1
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B0, C0, D0)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B1, C1, D1)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B2, C2, D2)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B3, C3, D3)
	DECQ itr2
	JNE  seal_sse_keystream_loop

	// Add in the state
	PADDD ·chacha20Constants<>(SB), A0
	PADDD ·chacha20Constants<>(SB), A1
	PADDD ·chacha20Constants<>(SB), A2
	PADDD ·chacha20Constants<>(SB), A3
	PADDD state1Store, B0
	PADDD state1Store, B1
	PADDD state1Store, B2
	PADDD state1Store, B3
	PADDD state2Store, C1
	PADDD state2Store, C2
	PADDD state2Store, C3
	PADDD ctr1Store, D1
	PADDD ctr2Store, D2
	PADDD ctr3Store, D3

	// Clamp and store the key
	PAND ·polyClampMask<>(SB), A0
	MOVO A0, rStore
	MOVO B0, sStore

	// Hash AD
	MOVQ ad_len+80(FP), itr2
	CALL authAdditionalData<>(SB)

	XOR(oup, inp, 0, A1, B1, C1, D1, A0)
	XOR(oup, inp, 64, A2, B2, C2, D2, A0)

	MOVQ $128, itr1
	SUBQ $128, inl
	LEAQ 128(inp), inp

	MOVO A3, A1
	MOVO B3, B1
	MOVO C3, C1
	MOVO D3, D1

	CMPQ inl, $64
	JBE  sealSSE128SealHash

	// XOR(oup, inp, 0, A3, B3, C3, D3, A0)
	MOVOU (0*16)(inp), A0
	MOVOU (1*16)(inp), B0
	MOVOU (2*16)(inp), C0
	MOVOU (3*16)(inp), D0
	PXOR  A0, A3
	PXOR  B0, B3
	PXOR  C0, C3
	PXOR  D0, D3
	MOVOU A3, (8*16)(oup)
	MOVOU B3, (9*16)(oup)
	MOVOU C3, (10*16)(oup)
	MOVOU D3, (11*16)(oup)

	ADDQ $64, itr1
	SUBQ $64, inl
	LEAQ 64(inp), inp

	MOVQ $2, itr1
	MOVQ $8, itr2

	CMPQ inl, $64
	JBE  seal_sse_64_bytes_remaining
	CMPQ inl, $128
	JBE  seal_sse_128_bytes_remaining
	CMPQ inl, $192
	JBE  seal_sse_192_bytes_remaining

seal_sse_main_loop:
	// Load state, increment counter blocks
	MOVO  ·chacha20Constants<>(SB), A0
	MOVO  state1Store, B0
	MOVO  state2Store, C0
	MOVO  ctr3Store, D0
	PADDL ·sseIncMask<>(SB), D0
	MOVO  A0, A1
	MOVO  B0, B1
	MOVO  C0, C1
	MOVO  D0, D1
	PADDL ·sseIncMask<>(SB), D1
	MOVO  A1, A2
	MOVO  B1, B2
	MOVO  C1, C2
	MOVO  D1, D2
	PADDL ·sseIncMask<>(SB), D2
	MOVO  A2, A3
	MOVO  B2, B3
	MOVO  C2, C3
	MOVO  D2, D3
	PADDL ·sseIncMask<>(SB), D3

	// Store counters
	MOVO D0, ctr0Store
	MOVO D1, ctr1Store
	MOVO D2, ctr2Store
	MOVO D3, ctr3Store

seal_sse_encrypt_authenticate_256_loop:
	MOVO C3, tmpStore
	CHACHA20_QROUND(A0, B0, C0, D0, C3)
	CHACHA20_QROUND(A1, B1, C1, D1, C3)
	CHACHA20_QROUND(A2, B2, C2, D2, C3)
	MOVO tmpStore, C3
	MOVO C1, tmpStore
	CHACHA20_QROUND(A3, B3, C3, D3, C1)
	MOVO tmpStore, C1
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B0, C0, D0)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B1, C1, D1)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B2, C2, D2)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B3, C3, D3)

	POLY1305_ADD(0(oup), acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)
	LEAQ 16(oup), oup

	MOVO C3, tmpStore
	CHACHA20_QROUND(A0, B0, C0, D0, C3)
	CHACHA20_QROUND(A1, B1, C1, D1, C3)
	CHACHA20_QROUND(A2, B2, C2, D2, C3)
	MOVO tmpStore, C3
	MOVO C1, tmpStore
	CHACHA20_QROUND(A3, B3, C3, D3, C1)
	MOVO tmpStore, C1
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B0, C0, D0)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B1, C1, D1)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B2, C2, D2)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B3, C3, D3)
	DECQ itr2
	JGE  seal_sse_encrypt_authenticate_256_loop

	POLY1305_ADD(0(oup), acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)
	LEAQ (2*8)(oup), oup
	DECQ itr1
	JG   seal_sse_encrypt_authenticate_256_loop

	// Add in the state
	PADDD ·chacha20Constants<>(SB), A0
	PADDD ·chacha20Constants<>(SB), A1
	PADDD ·chacha20Constants<>(SB), A2
	PADDD ·chacha20Constants<>(SB), A3
	PADDD state1Store, B0
	PADDD state1Store, B1
	PADDD state1Store, B2
	PADDD state1Store, B3
	PADDD state2Store, C0
	PADDD state2Store, C1
	PADDD state2Store, C2
	PADDD state2Store, C3
	PADDD ctr0Store, D0
	PADDD ctr1Store, D1
	PADDD ctr2Store, D2
	PADDD ctr3Store, D3
	MOVO  D3, tmpStore

	// Load - xor - store
	XOR(oup, inp, 0, A0, B0, C0, D0, D3)
	XOR(oup, inp, 64, A1, B1, C1, D1, D3)
	XOR(oup, inp, 128, A2, B2, C2, D2, D3)
	MOVO tmpStore, D3

	ADDQ $192, inp
	MOVQ $192, itr1
	SUBQ $192, inl
	MOVO A3, A1
	MOVO B3, B1
	MOVO C3, C1
	MOVO D3, D1
	CMPQ inl, $64
	JBE  sealSSE128SealHash

	MOVOU (0*16)(inp), A0
	MOVOU (1*16)(inp), B0
	MOVOU (2*16)(inp), C0
	MOVOU (3*16)(inp), D0
	PXOR  A0, A3
	PXOR  B0, B3
	PXOR  C0, C3
	PXOR  D0, D3
	MOVOU A3, (12*16)(oup)
	MOVOU B3, (13*16)(oup)
	MOVOU C3, (14*16)(oup)
	MOVOU D3, (15*16)(oup)

	LEAQ 64(inp), inp
	SUBQ $64, inl
	MOVQ $6, itr1
	MOVQ $4, itr2
	CMPQ inl, $192
	JG   seal_sse_main_loop

	MOVQ  inl, itr1
	TESTQ inl, inl
	JE    sealSSE128SealHash

	MOVQ $6, itr1
	CMPQ inl, $64
	JBE  seal_sse_64_bytes_remaining

	CMPQ inl, $128
	JBE  seal_sse_128_bytes_remaining
	JMP  seal_sse_192_bytes_remaining

// ----------------------------------------------------------------------------
// Special optimization for the last 64 bytes of plaintext
seal_sse_64_bytes_remaining:
	// Need to encrypt up to 64 bytes - prepare single block, hash 192 or 256 bytes
	MOVO  ·chacha20Constants<>(SB), A1
	MOVO  state1Store, B1
	MOVO  state2Store, C1
	MOVO  ctr3Store, D1
	PADDL ·sseIncMask<>(SB), D1
	MOVO  D1, ctr0Store

sealSSETail64LoopA:
	// Perform ChaCha rounds, while hashing the prevsiosly encrpyted ciphertext
	POLY1305_ADD(0(oup), acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)
	LEAQ 16(oup), oup

sealSSETail64LoopB:
	CHACHA20_QROUND(A1, B1, C1, D1, T1)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B1, C1, D1)
	CHACHA20_QROUND(A1, B1, C1, D1, T1)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B1, C1, D1)
	POLY1305_ADD(0(oup), acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)
	LEAQ 16(oup), oup

	DECQ itr1
	JG   sealSSETail64LoopA

	DECQ  itr2
	JGE   sealSSETail64LoopB
	PADDL ·chacha20Constants<>(SB), A1
	PADDL state1Store, B1
	PADDL state2Store, C1
	PADDL ctr0Store, D1

	JMP sealSSE128Seal

// ----------------------------------------------------------------------------
// Special optimization for the last 128 bytes of plaintext
seal_sse_128_bytes_remaining:
	// Need to encrypt up to 128 bytes - prepare two blocks, hash 192 or 256 bytes
	MOVO  ·chacha20Constants<>(SB), A0
	MOVO  state1Store, B0
	MOVO  state2Store, C0
	MOVO  ctr3Store, D0
	PADDL ·sseIncMask<>(SB), D0
	MOVO  D0, ctr0Store
	MOVO  A0, A1
	MOVO  B0, B1
	MOVO  C0, C1
	MOVO  D0, D1
	PADDL ·sseIncMask<>(SB), D1
	MOVO  D1, ctr1Store

sealSSETail128LoopA:
	// Perform ChaCha rounds, while hashing the prevsiosly encrpyted ciphertext
	POLY1305_ADD(0(oup), acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)
	LEAQ 16(oup), oup

sealSSETail128LoopB:
	CHACHA20_QROUND(A0, B0, C0, D0, T0)
	CHACHA20_QROUND(A1, B1, C1, D1, T0)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B0, C0, D0)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B1, C1, D1)
	POLY1305_ADD(0(oup), acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)
	LEAQ 16(oup), oup
	CHACHA20_QROUND(A0, B0, C0, D0, T0)
	CHACHA20_QROUND(A1, B1, C1, D1, T0)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B0, C0, D0)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B1, C1, D1)

	DECQ itr1
	JG   sealSSETail128LoopA

	DECQ itr2
	JGE  sealSSETail128LoopB

	PADDL ·chacha20Constants<>(SB), A0; PADDL ·chacha20Constants<>(SB), A1
	PADDL state1Store, B0; PADDL state1Store, B1
	PADDL state2Store, C0; PADDL state2Store, C1
	PADDL ctr0Store, D0; PADDL ctr1Store, D1

	XOR(oup, inp, 0, A0, B0, C0, D0, T0)

	MOVQ $64, itr1
	LEAQ 64(inp), inp
	SUBQ $64, inl

	JMP sealSSE128SealHash

// ----------------------------------------------------------------------------
// Special optimization for the last 192 bytes of plaintext
seal_sse_192_bytes_remaining:
	// Need to encrypt up to 192 bytes - prepare three blocks, hash 192 or 256 bytes
	MOVO  ·chacha20Constants<>(SB), A0
	MOVO  state1Store, B0
	MOVO  state2Store, C0
	MOVO  ctr3Store, D0
	PADDL ·sseIncMask<>(SB), D0
	MOVO  D0, ctr0Store
	MOVO  A0, A1
	MOVO  B0, B1
	MOVO  C0, C1
	MOVO  D0, D1
	PADDL ·sseIncMask<>(SB), D1
	MOVO  D1, ctr1Store
	MOVO  A1, A2
	MOVO  B1, B2
	MOVO  C1, C2
	MOVO  D1, D2
	PADDL ·sseIncMask<>(SB), D2
	MOVO  D2, ctr2Store

sealSSETail192LoopA:
	// Perform ChaCha rounds, while hashing the prevsiosly encrpyted ciphertext
	POLY1305_ADD(0(oup), acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)
	LEAQ 16(oup), oup

sealSSETail192LoopB:
	CHACHA20_QROUND(A0, B0, C0, D0, T0)
	CHACHA20_QROUND(A1, B1, C1, D1, T0)
	CHACHA20_QROUND(A2, B2, C2, D2, T0)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B0, C0, D0)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B1, C1, D1)
	CHACHA20_SHUF(0x39, 0x4E, 0x93, B2, C2, D2)

	POLY1305_ADD(0(oup), acc0, acc1, acc2)
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)
	LEAQ 16(oup), oup

	CHACHA20_QROUND(A0, B0, C0, D0, T0)
	CHACHA20_QROUND(A1, B1, C1, D1, T0)
	CHACHA20_QROUND(A2, B2, C2, D2, T0)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B0, C0, D0)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B1, C1, D1)
	CHACHA20_SHUF(0x93, 0x4E, 0x39, B2, C2, D2)

	DECQ itr1
	JG   sealSSETail192LoopA

	DECQ itr2
	JGE  sealSSETail192LoopB

	PADDL ·chacha20Constants<>(SB), A0
	PADDL ·chacha20Constants<>(SB), A1
	PADDL ·chacha20Constants<>(SB), A2
	PADDL state1Store, B0
	PADDL state1Store, B1
	PADDL state1Store, B2
	PADDL state2Store, C0
	PADDL state2Store, C1
	PADDL state2Store, C2
	PADDL ctr0Store, D0
	PADDL ctr1Store, D1
	PADDL ctr2Store, D2

	XOR(oup, inp, 0, A0, B0, C0, D0, T0)
	XOR(oup, inp, 64, A1, B1, C1, D1, T0)

	MOVO A2, A1
	MOVO B2, B1
	MOVO C2, C1
	MOVO D2, D1
	MOVQ $128, itr1
	LEAQ 128(inp), inp
	SUBQ $128, inl

	JMP sealSSE128SealHash

sealSSETail:
	TESTQ inl, inl
	JE    seal_sse_finalize

	// We can only load the PT one byte at a time to avoid read after end of buffer
	MOVQ inl, itr2
	SHLQ $4, itr2
	LEAQ ·andMask<>(SB), t0
	MOVQ inl, itr1
	LEAQ -1(inp)(inl*1), inp
	XORQ t2, t2
	XORQ t3, t3
	XORQ AX, AX

sealSSETailLoadLoop:
	SHLQ   $8, t2, t3
	SHLQ   $8, t2
	MOVB   (inp), AX
	XORQ   AX, t2
	LEAQ   -1(inp), inp
	DECQ   itr1
	JNE    sealSSETailLoadLoop
	MOVQ   t2, 0+tmpStore
	MOVQ   t3, 8+tmpStore
	PXOR   0+tmpStore, A1
	MOVOU  A1, (oup)
	MOVOU  -16(t0)(itr2*1), T0
	PAND   T0, A1
	MOVQ   A1, t0
	PSRLDQ $8, A1
	MOVQ   A1, t1
	ADDQ   t0, acc0; ADCQ t1, acc1; ADCQ $1, acc2
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)

	ADDQ inl, oup

seal_sse_finalize:
	// Hash in the buffer lengths
	ADDQ ad_len+80(FP), acc0
	ADCQ src_len+56(FP), acc1
	ADCQ $1, acc2
	POLY1305_MUL(acc0, acc1, acc2, 0(BP), 8(BP), t0, t1, t2, t3)

	// Final reduce
	MOVQ    acc0, t0
	MOVQ    acc1, t1
	SUBQ    $-5, acc0
	SBBQ    $-1, acc1
	SBBQ    $3, acc2
	CMOVQCS t0, acc0
	CMOVQCS t1, acc1

	// Add in the "s" part of the key
	ADDQ 0+sStore, acc0
	ADCQ 8+sStore, acc1

	// Finally store the tag at the end of the message
	MOVQ acc0, 0(oup)
	MOVQ acc1, 8(oup)
	RET

// func haveSSSE3() bool
TEXT ·haveSSSE3(SB), NOSPLIT, $0
	XORQ AX, AX
	INCL AX
	CPUID
	SHRQ $9, CX
	ANDQ $1, CX
	MOVB CX, ret+0(FP)
	RET

