// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This file was originally from https://golang.org/cl/24717 by Vlad Krasnov of CloudFlare.

// +build go1.7,amd64,!gccgo,!appengine

#include "textflag.h"

// ChaCha20 constants
DATA ·chacha20Constants<>+0x00(SB)/4, $0x61707865
DATA ·chacha20Constants<>+0x04(SB)/4, $0x3320646e
DATA ·chacha20Constants<>+0x08(SB)/4, $0x79622d32
DATA ·chacha20Constants<>+0x0c(SB)/4, $0x6b206574
DATA ·chacha20Constants<>+0x10(SB)/4, $0x61707865
DATA ·chacha20Constants<>+0x14(SB)/4, $0x3320646e
DATA ·chacha20Constants<>+0x18(SB)/4, $0x79622d32
DATA ·chacha20Constants<>+0x1c(SB)/4, $0x6b206574

// <<< 16 with PSHUFB
DATA ·rol16<>+0x00(SB)/8, $0x0504070601000302
DATA ·rol16<>+0x08(SB)/8, $0x0D0C0F0E09080B0A
DATA ·rol16<>+0x10(SB)/8, $0x0504070601000302
DATA ·rol16<>+0x18(SB)/8, $0x0D0C0F0E09080B0A

// <<< 8 with PSHUFB
DATA ·rol8<>+0x00(SB)/8, $0x0605040702010003
DATA ·rol8<>+0x08(SB)/8, $0x0E0D0C0F0A09080B
DATA ·rol8<>+0x10(SB)/8, $0x0605040702010003
DATA ·rol8<>+0x18(SB)/8, $0x0E0D0C0F0A09080B

DATA ·avx2InitMask<>+0x00(SB)/8, $0x0
DATA ·avx2InitMask<>+0x08(SB)/8, $0x0
DATA ·avx2InitMask<>+0x10(SB)/8, $0x1
DATA ·avx2InitMask<>+0x18(SB)/8, $0x0

DATA ·sseIncMask<>+0x00(SB)/8, $0x1
DATA ·sseIncMask<>+0x08(SB)/8, $0x0

DATA ·avx2IncMask<>+0x00(SB)/8, $0x2
DATA ·avx2IncMask<>+0x08(SB)/8, $0x0
DATA ·avx2IncMask<>+0x10(SB)/8, $0x2
DATA ·avx2IncMask<>+0x18(SB)/8, $0x0

// Poly1305 key clamp
DATA ·polyClampMask<>+0x00(SB)/8, $0x0FFFFFFC0FFFFFFF
DATA ·polyClampMask<>+0x08(SB)/8, $0x0FFFFFFC0FFFFFFC
DATA ·polyClampMask<>+0x10(SB)/8, $0xFFFFFFFFFFFFFFFF
DATA ·polyClampMask<>+0x18(SB)/8, $0xFFFFFFFFFFFFFFFF

// To load/store the last < 16 bytes in a buffer
DATA ·andMask<>+0x00(SB)/8, $0x00000000000000ff
DATA ·andMask<>+0x08(SB)/8, $0x0000000000000000
DATA ·andMask<>+0x10(SB)/8, $0x000000000000ffff
DATA ·andMask<>+0x18(SB)/8, $0x0000000000000000
DATA ·andMask<>+0x20(SB)/8, $0x0000000000ffffff
DATA ·andMask<>+0x28(SB)/8, $0x0000000000000000
DATA ·andMask<>+0x30(SB)/8, $0x00000000ffffffff
DATA ·andMask<>+0x38(SB)/8, $0x0000000000000000
DATA ·andMask<>+0x40(SB)/8, $0x000000ffffffffff
DATA ·andMask<>+0x48(SB)/8, $0x0000000000000000
DATA ·andMask<>+0x50(SB)/8, $0x0000ffffffffffff
DATA ·andMask<>+0x58(SB)/8, $0x0000000000000000
DATA ·andMask<>+0x60(SB)/8, $0x00ffffffffffffff
DATA ·andMask<>+0x68(SB)/8, $0x0000000000000000
DATA ·andMask<>+0x70(SB)/8, $0xffffffffffffffff
DATA ·andMask<>+0x78(SB)/8, $0x0000000000000000
DATA ·andMask<>+0x80(SB)/8, $0xffffffffffffffff
DATA ·andMask<>+0x88(SB)/8, $0x00000000000000ff
DATA ·andMask<>+0x90(SB)/8, $0xffffffffffffffff
DATA ·andMask<>+0x98(SB)/8, $0x000000000000ffff
DATA ·andMask<>+0xa0(SB)/8, $0xffffffffffffffff
DATA ·andMask<>+0xa8(SB)/8, $0x0000000000ffffff
DATA ·andMask<>+0xb0(SB)/8, $0xffffffffffffffff
DATA ·andMask<>+0xb8(SB)/8, $0x00000000ffffffff
DATA ·andMask<>+0xc0(SB)/8, $0xffffffffffffffff
DATA ·andMask<>+0xc8(SB)/8, $0x000000ffffffffff
DATA ·andMask<>+0xd0(SB)/8, $0xffffffffffffffff
DATA ·andMask<>+0xd8(SB)/8, $0x0000ffffffffffff
DATA ·andMask<>+0xe0(SB)/8, $0xffffffffffffffff
DATA ·andMask<>+0xe8(SB)/8, $0x00ffffffffffffff

GLOBL ·chacha20Constants<>(SB), (NOPTR+RODATA), $32
GLOBL ·rol16<>(SB), (NOPTR+RODATA), $32
GLOBL ·rol8<>(SB), (NOPTR+RODATA), $32
GLOBL ·sseIncMask<>(SB), (NOPTR+RODATA), $16
GLOBL ·avx2IncMask<>(SB), (NOPTR+RODATA), $32
GLOBL ·avx2InitMask<>(SB), (NOPTR+RODATA), $32
GLOBL ·polyClampMask<>(SB), (NOPTR+RODATA), $32
GLOBL ·andMask<>(SB), (NOPTR+RODATA), $240

#define XOR(dst, src, off, A, B, C, D, T) \
    MOVOU 0+off(src), T; \
    PXOR A, T; \
    MOVOU T, 0+off(dst); \
    MOVOU 16+off(src), T; \
    PXOR B, T; \
    MOVOU T, 16+off(dst); \
    MOVOU 32+off(src), T; \
    PXOR C, T; \
    MOVOU T, 32+off(dst); \
    MOVOU 48+off(src), T; \
    PXOR D, T; \
    MOVOU T, 48+off(dst)
