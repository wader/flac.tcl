#!/usr/bin/env tclsh
# Copyright (c) 2020 Mattias Wadman
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is furnished to do
# so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
# References used:
# https://xiph.org/flac/format.html
# https://github.com/mewkiz/flac
# https://github.com/xiph/flac
#
# Uses modified version of md5.tcl -q Copyright (C) 2003 Pat Thoyts <patthoyts@users.sourceforge.net>
#

namespace eval ::log {
    namespace export *
    variable uid
    if {![info exists uid]} {
        set uid 0
    }

    proc new {br} {
        variable uid
        set t [namespace current]::[incr uid]
        upvar #0 $t s

        array set s [list \
            br $br \
            depth 0 \
        ]

        return $t
    }

    proc section {t label body} {
        upvar #0 $t s

        set indent [string repeat "  " $s(depth)]
        set start [bitreader::bytepos $s(br)]
        puts stderr [format "%.8d-%.8d      %s- %s:" $start $start $indent $label]
        incr s(depth) 1
        uplevel 1 $body
        incr s(depth) -1
    }

    proc entry {t label body} {
        upvar #0 $t s

        set indent [string repeat "  " $s(depth)]
        set start [bitreader::bytepos $s(br)]
        set startbits [bitreader::bitpos $s(br)]
        set r [uplevel 1 $body]
        set end [bitreader::bytepos $s(br)]
        set endbits [bitreader::bitpos $s(br)]
        set bytealign [bitreader::bytealign $s(br)]
        if {$bytealign == 0} {
            incr end -1
        }
        set bitsize [expr $endbits - $startbits]
        lassign $r value
        puts stderr [format "%.8d-%.8d %6d %s%s: %s" $start $end $bitsize $indent $label $value]

        return $r
    }
}

namespace eval ::bitreader {
    namespace export *
    variable uid
    if {![info exists uid]} {
        set uid 0
    }

    proc new {data} {
        variable uid
        set t [namespace current]::[incr uid]
        upvar #0 $t s

        array set s [list \
            data $data \
            buf 0 \
            bufbits 0 \
            pos 0 \
            bitpos 0 \
        ]

        return $t
    }
    proc uint {t bits} {
        upvar #0 $t s

        while {$bits > $s(bufbits)} {
            set d [string index $s(data) $s(pos)]
            if {$d == ""} {
                error "read index $s(pos) is outside file length [string length $s(data)]"
            }
            set b [scan $d %c]
            set s(buf) [expr ($s(buf)<<8) | $b]
            incr s(bufbits) 8
            incr s(pos) 1
        }
        incr s(bitpos) $bits

        set n [expr $s(buf)>>($s(bufbits)-$bits)]
        set s(buf) [expr $s(buf) & ((1<<($s(bufbits)-$bits))-1)]
        incr s(bufbits) [expr -$bits]

        return $n
    }
    proc int {t bits} {
        upvar #0 $t s

        set n [uint $t $bits]
        # two's complement
        if {$n & (1 << ($bits-1))} {
            set n [expr -((~$n & (1<<$bits)-1)+1)]
        }

        return $n
    }
    proc unary {t} {
        upvar #0 $t s
        for {set n 0} {[uint $t 1] == 0} {incr n} {
            # nop
        }
        return $n
    }
    proc bytes {t size} {
        upvar #0 $t s

        set b ""
        for {set i 0} {$i < $size} {incr i} {
            append b [format %c [uint $t 8]]
        }

        return $b
    }
    proc skip {t bits} {
        upvar #0 $t s

        incr s(bitpos) $bits
        if {$bits > $s(bufbits)} {
            incr bits [expr -$s(bufbits)]
            set s(buf) 0
            set s(bufbits) 0

            set skip_bytes [expr $bits / 8]
            set bits [expr $bits % 8]
            incr s(pos) $skip_bytes
        }

        if {$s(pos) >= [string length $s(data)]} {
            error "skip to offset $s(pos) is outside file length [string length $s(data)]"
        }

        uint $t $bits

        return ""
    }
    proc bytepos {t} {
        upvar #0 $t s

        if {$s(bufbits) > 0} {
            return [expr $s(pos)-1]
        }

        return $s(pos)
    }
    proc bitpos {t} {
        upvar #0 $t s
        return $s(bitpos)
    }
    proc end {t} {
        upvar #0 $t s
        return [expr $s(pos) >= [string length $s(data)]]
    }
    proc bytealign {t} {
        upvar #0 $t s
        return $s(bufbits)
    }
    proc byterange {t start end} {
        upvar #0 $t s
        set b [string range $s(data) $start $end]
        set rangelen [expr $end-$start+1]
        if {[string length $b] != $rangelen} {
            error "byte range $start-$end of length $rangelen is outside file length [string length $s(data)]"
        }
        return $b
    }
    proc delete {t} {
        upvar #0 $t s
        unset s
    }
}

namespace eval ::wavwriter {
    namespace export *
    variable uid
    if {![info exists uid]} {
        set uid 0
    }

    proc new {ch config} {
        variable uid
        set t [namespace current]::[incr uid]
        upvar #0 $t s

        set bitdepth [dict get $config bitdepth]
        set channels [dict get $config channels]
        set sample_rate [dict get $config sample_rate]
        set data_size [expr $channels * $bitdepth/8 * [dict get $config total_samples]]
        puts -nonewline $ch "RIFF"
        # length "WAVE" + fmt header/content + data header/content
        wuint32le $ch [expr 4+8+16+8+$data_size]
        wstring $ch "WAVE"
        # fmt subchunk
        wstring $ch "fmt "
        # fmt subchunk length
        wuint32le $ch 16
        # 1=PCM
        wuint16le $ch 1
        # channels
        wuint16le $ch $channels
        # sample rate
        wuint32le $ch $sample_rate
        # byte rate
        wuint32le $ch [expr $sample_rate * $channels * $bitdepth/8]
        # block align
        wuint16le $ch [expr $channels * $bitdepth/8]
        # bits per sample
        wuint16le $ch $bitdepth
        # data subchunk
        wstring $ch "data"
        # data subchunk length
        wuint32le $ch $data_size

        array set s [list \
            ch $ch \
            bitdepth $bitdepth \
        ]

        return $t
    }
    proc write {t n} {
        upvar #0 $t s
        switch -exact -- $s(bitdepth) {
            8 {wuint8 $s(ch) [expr $n+128]}
            16 {wint16le $s(ch) $n}
            24 {wint24le $s(ch) $n}
        }
    }
    proc delete {t} {
        upvar #0 $t s
        unset s
    }

    proc wstring {ch s} {puts -nonewline $ch $s}
    proc wuint8 {ch n} {wstring $ch [binary format c $n]}
    proc wint16le {ch n} {wstring $ch [binary format s $n]}
    proc wint24le {ch n} {wstring $ch [binary format ccc [expr $n&0xff] [expr ($n&0xff00)>>8] [expr ($n&0xff0000)>>16]]}
    proc wuint16le {ch n} {wstring $ch [binary format su1 $n]}
    proc wuint32le {ch n} {wstring $ch [binary format iu1 $n]}
}

proc crc8_make_table {poly bits} {
    set table [list]
    for {set i 0} {$i < 256} {incr i} {
        set crc $i
        for {set j 0} {$j < 8} {incr j} {
            if {($crc & (1<<($bits-1)))} {
                set crc [expr (($crc<<1) ^ $poly) & 0xff]
            } else {
                set crc [expr ($crc<<1) & 0xff]
            }
        }
        lappend table $crc
    }

    return $table
}

set crc8table [crc8_make_table 0x7 8]
proc crc8 {s} {
    global crc8table
    set crc 0
    for {set i 0} {$i < [string length $s]} {incr i} {
        set n [scan [string index $s $i] %c]
        set crc [lindex $crc8table [expr $crc^$n]]
    }
    return $crc
}

proc crc16make_table {poly bits} {
    set table [list]
    for {set i 0} {$i < 256} {incr i} {
        set crc [expr $i<<8]
        for {set j 0} {$j < 8} {incr j} {
            if {($crc & (1<<($bits-1)))} {
                set crc [expr (($crc<<1) ^ $poly) & 0xffff]
            } else {
                set crc [expr ($crc<<1) & 0xffff]
            }
        }
        lappend table $crc
    }

    return $table
}

set crc16table [crc16make_table 0x8005 16]
proc crc16 {s} {
    global crc16table
    set crc 0
    for {set i 0} {$i < [string length $s]} {incr i} {
        set n [scan [string index $s $i] %c]
        set crc [expr ($crc<<8 ^ [lindex $crc16table [expr ($crc>>8) ^ $n]]) & 0xffff]
    }
    return $crc
}

# md5.tcl - Copyright (C) 2003 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# MD5  defined by RFC 1321, "The MD5 Message-Digest Algorithm"
# HMAC defined by RFC 2104, "Keyed-Hashing for Message Authentication"
#
# This is an implementation of MD5 based upon the example code given in
# RFC 1321 and upon the tcllib MD4 implementation and taking some ideas
# from the earlier tcllib md5 version by Don Libes.
#
# This implementation permits incremental updating of the hash and
# provides support for external compiled implementations either using
# critcl (md5c) or Trf.
#
# -------------------------------------------------------------------------
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# -------------------------------------------------------------------------
#
# $Id: md5.bench,v 1.6 2006/11/13 21:49:47 hobbs Exp $

namespace eval ::md5 {
    variable version 2.0.4
    variable rcsid {$Id: md5.bench,v 1.6 2006/11/13 21:49:47 hobbs Exp $}

    namespace export MD5Init MD5Update MD5Final

    variable uid
    if {![info exists uid]} {
        set uid 0
    }
}

# -------------------------------------------------------------------------

# MD5Init --
#
#   Create and initialize an MD5 state variable. This will be
#   cleaned up when we call MD5Final
#
proc ::md5::MD5Init {} {
    variable uid
    set token [namespace current]::[incr uid]
    upvar #0 $token state

    # RFC1321:3.3 - Initialize MD5 state structure
    array set state \
        [list \
             A [expr {0x67452301}] \
             B [expr {0xefcdab89}] \
             C [expr {0x98badcfe}] \
             D [expr {0x10325476}] \
             n 0 i "" ]

    return $token
}

# MD5Update --
#
#   This is called to add more data into the hash. You may call this
#   as many times as you require. Note that passing in "ABC" is equivalent
#   to passing these letters in as separate calls -- hence this proc
#   permits hashing of chunked data
#
proc ::md5::MD5Update {token data} {
    upvar #0 $token state

    # Update the state values
    incr state(n) [string length $data]
    append state(i) $data

    # Calculate the hash for any complete blocks
    set len [string length $state(i)]
    for {set n 0} {($n + 64) <= $len} {} {
        MD5Hash $token [string range $state(i) $n [incr n 64]]
    }

    # Adjust the state for the blocks completed.
    set state(i) [string range $state(i) $n end]
    return
}

# MD5Final --
#
#    This procedure is used to close the current hash and returns the
#    hash data. Once this procedure has been called the hash context
#    is freed and cannot be used again.
#
#    Note that the output is 128 bits represented as binary data.
#
proc ::md5::MD5Final {token} {
    upvar #0 $token state

    # RFC1321:3.1 - Padding
    #
    set len [string length $state(i)]
    set pad [expr {56 - ($len % 64)}]
    if {$len % 64 > 56} {
        incr pad 64
    }
    if {$pad == 0} {
        incr pad 64
    }
    append state(i) [binary format a$pad \x80]

    # RFC1321:3.2 - Append length in bits as little-endian wide int.
    append state(i) [binary format ii [expr {8 * $state(n)}] 0]

    # Calculate the hash for the remaining block.
    set len [string length $state(i)]
    for {set n 0} {($n + 64) <= $len} {} {
        MD5Hash $token [string range $state(i) $n [incr n 64]]
    }

    # RFC1321:3.5 - Output
    set r [bytes $state(A)][bytes $state(B)][bytes $state(C)][bytes $state(D)]
    unset state
    return $r
}

# -------------------------------------------------------------------------
# Description:
#  This is the core MD5 algorithm. It is a lot like the MD4 algorithm but
#  includes an extra round and a set of constant modifiers throughout.
#
# Note:
#  This function body is substituted later on to inline some of the
#  procedures and to make is a bit more comprehensible.
#
set ::md5::MD5Hash_body {
    variable $token
    upvar 0 $token state

    # RFC1321:3.4 - Process Message in 16-Word Blocks
    binary scan $msg i* blocks
    foreach {X0 X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12 X13 X14 X15} $blocks {
        set A $state(A)
        set B $state(B)
        set C $state(C)
        set D $state(D)

        # Round 1
        # Let [abcd k s i] denote the operation
        #   a = b + ((a + F(b,c,d) + X[k] + T[i]) <<< s).
        # Do the following 16 operations.
        # [ABCD  0  7  1]  [DABC  1 12  2]  [CDAB  2 17  3]  [BCDA  3 22  4]
        set A [expr {$B + (($A + [F $B $C $D] + $X0 + $T01) <<< 7)}]
        set D [expr {$A + (($D + [F $A $B $C] + $X1 + $T02) <<< 12)}]
        set C [expr {$D + (($C + [F $D $A $B] + $X2 + $T03) <<< 17)}]
        set B [expr {$C + (($B + [F $C $D $A] + $X3 + $T04) <<< 22)}]
        # [ABCD  4  7  5]  [DABC  5 12  6]  [CDAB  6 17  7]  [BCDA  7 22  8]
        set A [expr {$B + (($A + [F $B $C $D] + $X4 + $T05) <<< 7)}]
        set D [expr {$A + (($D + [F $A $B $C] + $X5 + $T06) <<< 12)}]
        set C [expr {$D + (($C + [F $D $A $B] + $X6 + $T07) <<< 17)}]
        set B [expr {$C + (($B + [F $C $D $A] + $X7 + $T08) <<< 22)}]
        # [ABCD  8  7  9]  [DABC  9 12 10]  [CDAB 10 17 11]  [BCDA 11 22 12]
        set A [expr {$B + (($A + [F $B $C $D] + $X8 + $T09) <<< 7)}]
        set D [expr {$A + (($D + [F $A $B $C] + $X9 + $T10) <<< 12)}]
        set C [expr {$D + (($C + [F $D $A $B] + $X10 + $T11) <<< 17)}]
        set B [expr {$C + (($B + [F $C $D $A] + $X11 + $T12) <<< 22)}]
        # [ABCD 12  7 13]  [DABC 13 12 14]  [CDAB 14 17 15]  [BCDA 15 22 16]
        set A [expr {$B + (($A + [F $B $C $D] + $X12 + $T13) <<< 7)}]
        set D [expr {$A + (($D + [F $A $B $C] + $X13 + $T14) <<< 12)}]
        set C [expr {$D + (($C + [F $D $A $B] + $X14 + $T15) <<< 17)}]
        set B [expr {$C + (($B + [F $C $D $A] + $X15 + $T16) <<< 22)}]

        # Round 2.
        # Let [abcd k s i] denote the operation
        #   a = b + ((a + G(b,c,d) + X[k] + Ti) <<< s)
        # Do the following 16 operations.
        # [ABCD  1  5 17]  [DABC  6  9 18]  [CDAB 11 14 19]  [BCDA  0 20 20]
        set A [expr {$B + (($A + [G $B $C $D] + $X1  + $T17) <<<  5)}]
        set D [expr {$A + (($D + [G $A $B $C] + $X6  + $T18) <<<  9)}]
        set C [expr {$D + (($C + [G $D $A $B] + $X11 + $T19) <<< 14)}]
        set B [expr {$C + (($B + [G $C $D $A] + $X0  + $T20) <<< 20)}]
        # [ABCD  5  5 21]  [DABC 10  9 22]  [CDAB 15 14 23]  [BCDA  4 20 24]
        set A [expr {$B + (($A + [G $B $C $D] + $X5  + $T21) <<<  5)}]
        set D [expr {$A + (($D + [G $A $B $C] + $X10 + $T22) <<<  9)}]
        set C [expr {$D + (($C + [G $D $A $B] + $X15 + $T23) <<< 14)}]
        set B [expr {$C + (($B + [G $C $D $A] + $X4  + $T24) <<< 20)}]
        # [ABCD  9  5 25]  [DABC 14  9 26]  [CDAB  3 14 27]  [BCDA  8 20 28]
        set A [expr {$B + (($A + [G $B $C $D] + $X9  + $T25) <<<  5)}]
        set D [expr {$A + (($D + [G $A $B $C] + $X14 + $T26) <<<  9)}]
        set C [expr {$D + (($C + [G $D $A $B] + $X3  + $T27) <<< 14)}]
        set B [expr {$C + (($B + [G $C $D $A] + $X8  + $T28) <<< 20)}]
        # [ABCD 13  5 29]  [DABC  2  9 30]  [CDAB  7 14 31]  [BCDA 12 20 32]
        set A [expr {$B + (($A + [G $B $C $D] + $X13 + $T29) <<<  5)}]
        set D [expr {$A + (($D + [G $A $B $C] + $X2  + $T30) <<<  9)}]
        set C [expr {$D + (($C + [G $D $A $B] + $X7  + $T31) <<< 14)}]
        set B [expr {$C + (($B + [G $C $D $A] + $X12 + $T32) <<< 20)}]

        # Round 3.
        # Let [abcd k s i] denote the operation
        #   a = b + ((a + H(b,c,d) + X[k] + T[i]) <<< s)
        # Do the following 16 operations.
        # [ABCD  5  4 33]  [DABC  8 11 34]  [CDAB 11 16 35]  [BCDA 14 23 36]
        set A [expr {$B + (($A + [H $B $C $D] + $X5  + $T33) <<<  4)}]
        set D [expr {$A + (($D + [H $A $B $C] + $X8  + $T34) <<< 11)}]
        set C [expr {$D + (($C + [H $D $A $B] + $X11 + $T35) <<< 16)}]
        set B [expr {$C + (($B + [H $C $D $A] + $X14 + $T36) <<< 23)}]
        # [ABCD  1  4 37]  [DABC  4 11 38]  [CDAB  7 16 39]  [BCDA 10 23 40]
        set A [expr {$B + (($A + [H $B $C $D] + $X1  + $T37) <<<  4)}]
        set D [expr {$A + (($D + [H $A $B $C] + $X4  + $T38) <<< 11)}]
        set C [expr {$D + (($C + [H $D $A $B] + $X7  + $T39) <<< 16)}]
        set B [expr {$C + (($B + [H $C $D $A] + $X10 + $T40) <<< 23)}]
        # [ABCD 13  4 41]  [DABC  0 11 42]  [CDAB  3 16 43]  [BCDA  6 23 44]
        set A [expr {$B + (($A + [H $B $C $D] + $X13 + $T41) <<<  4)}]
        set D [expr {$A + (($D + [H $A $B $C] + $X0  + $T42) <<< 11)}]
        set C [expr {$D + (($C + [H $D $A $B] + $X3  + $T43) <<< 16)}]
        set B [expr {$C + (($B + [H $C $D $A] + $X6  + $T44) <<< 23)}]
        # [ABCD  9  4 45]  [DABC 12 11 46]  [CDAB 15 16 47]  [BCDA  2 23 48]
        set A [expr {$B + (($A + [H $B $C $D] + $X9  + $T45) <<<  4)}]
        set D [expr {$A + (($D + [H $A $B $C] + $X12 + $T46) <<< 11)}]
        set C [expr {$D + (($C + [H $D $A $B] + $X15 + $T47) <<< 16)}]
        set B [expr {$C + (($B + [H $C $D $A] + $X2  + $T48) <<< 23)}]

        # Round 4.
        # Let [abcd k s i] denote the operation
        #   a = b + ((a + I(b,c,d) + X[k] + T[i]) <<< s)
        # Do the following 16 operations.
        # [ABCD  0  6 49]  [DABC  7 10 50]  [CDAB 14 15 51]  [BCDA  5 21 52]
        set A [expr {$B + (($A + [I $B $C $D] + $X0  + $T49) <<<  6)}]
        set D [expr {$A + (($D + [I $A $B $C] + $X7  + $T50) <<< 10)}]
        set C [expr {$D + (($C + [I $D $A $B] + $X14 + $T51) <<< 15)}]
        set B [expr {$C + (($B + [I $C $D $A] + $X5  + $T52) <<< 21)}]
        # [ABCD 12  6 53]  [DABC  3 10 54]  [CDAB 10 15 55]  [BCDA  1 21 56]
        set A [expr {$B + (($A + [I $B $C $D] + $X12 + $T53) <<<  6)}]
        set D [expr {$A + (($D + [I $A $B $C] + $X3  + $T54) <<< 10)}]
        set C [expr {$D + (($C + [I $D $A $B] + $X10 + $T55) <<< 15)}]
        set B [expr {$C + (($B + [I $C $D $A] + $X1  + $T56) <<< 21)}]
        # [ABCD  8  6 57]  [DABC 15 10 58]  [CDAB  6 15 59]  [BCDA 13 21 60]
        set A [expr {$B + (($A + [I $B $C $D] + $X8  + $T57) <<<  6)}]
        set D [expr {$A + (($D + [I $A $B $C] + $X15 + $T58) <<< 10)}]
        set C [expr {$D + (($C + [I $D $A $B] + $X6  + $T59) <<< 15)}]
        set B [expr {$C + (($B + [I $C $D $A] + $X13 + $T60) <<< 21)}]
        # [ABCD  4  6 61]  [DABC 11 10 62]  [CDAB  2 15 63]  [BCDA  9 21 64]
        set A [expr {$B + (($A + [I $B $C $D] + $X4  + $T61) <<<  6)}]
        set D [expr {$A + (($D + [I $A $B $C] + $X11 + $T62) <<< 10)}]
        set C [expr {$D + (($C + [I $D $A $B] + $X2  + $T63) <<< 15)}]
        set B [expr {$C + (($B + [I $C $D $A] + $X9  + $T64) <<< 21)}]

        # Then perform the following additions. (That is, increment each
        # of the four registers by the value it had before this block
        # was started.)
        incr state(A) $A
        incr state(B) $B
        incr state(C) $C
        incr state(D) $D
    }

    return
}

proc ::md5::byte {n v} {expr {((0xFF << (8 * $n)) & $v) >> (8 * $n)}}
proc ::md5::bytes {v} {
    format %c%c%c%c \
        [expr {0xFF & $v}] \
        [expr {(0xFF00 & $v) >> 8}] \
        [expr {(0xFF0000 & $v) >> 16}] \
        [expr {((0xFF000000 & $v) >> 24) & 0xFF}]
}

# 32bit rotate-left
proc ::md5::<<< {v n} {
    return [expr {((($v << $n) \
                        | (($v >> (32 - $n)) \
                               & (0x7FFFFFFF >> (31 - $n))))) \
                      & 0xFFFFFFFF}]
}

# Convert our <<< pseuodo-operator into a procedure call.
regsub -all -line \
    {\[expr {(\$[ABCD]) \+ \(\((.*)\)\s+<<<\s+(\d+)\)}\]} \
    $::md5::MD5Hash_body \
    {[expr {int(\1 + [<<< [expr {\2}] \3])}]} \
    ::md5::MD5Hash_bodyX

# RFC1321:3.4 - function F
proc ::md5::F {X Y Z} {
    return [expr {($X & $Y) | ((~$X) & $Z)}]
}

# Inline the F function
regsub -all -line \
    {\[F (\$[ABCD]) (\$[ABCD]) (\$[ABCD])\]} \
    $::md5::MD5Hash_bodyX \
    {( (\1 \& \2) | ((~\1) \& \3) )} \
    ::md5::MD5Hash_bodyX

# RFC1321:3.4 - function G
proc ::md5::G {X Y Z} {
    return [expr {(($X & $Z) | ($Y & (~$Z)))}]
}

# Inline the G function
regsub -all -line \
    {\[G (\$[ABCD]) (\$[ABCD]) (\$[ABCD])\]} \
    $::md5::MD5Hash_bodyX \
    {(((\1 \& \3) | (\2 \& (~\3))))} \
    ::md5::MD5Hash_bodyX

# RFC1321:3.4 - function H
proc ::md5::H {X Y Z} {
    return [expr {$X ^ $Y ^ $Z}]
}

# Inline the H function
regsub -all -line \
    {\[H (\$[ABCD]) (\$[ABCD]) (\$[ABCD])\]} \
    $::md5::MD5Hash_bodyX \
    {(\1 ^ \2 ^ \3)} \
    ::md5::MD5Hash_bodyX

# RFC1321:3.4 - function I
proc ::md5::I {X Y Z} {
    return [expr {$Y ^ ($X | (~$Z))}]
}

# Inline the I function
regsub -all -line \
    {\[I (\$[ABCD]) (\$[ABCD]) (\$[ABCD])\]} \
    $::md5::MD5Hash_bodyX \
    {(\2 ^ (\1 | (~\3)))} \
    ::md5::MD5Hash_bodyX


# RFC 1321:3.4 step 4: inline the set of constant modifiers.
namespace eval md5 {
    foreach tName {
        T01 T02 T03 T04 T05 T06 T07 T08 T09 T10
        T11 T12 T13 T14 T15 T16 T17 T18 T19 T20
        T21 T22 T23 T24 T25 T26 T27 T28 T29 T30
        T31 T32 T33 T34 T35 T36 T37 T38 T39 T40
        T41 T42 T43 T44 T45 T46 T47 T48 T49 T50
        T51 T52 T53 T54 T55 T56 T57 T58 T59 T60
        T61 T62 T63 T64
    }  tVal {
        0xd76aa478 0xe8c7b756 0x242070db 0xc1bdceee
        0xf57c0faf 0x4787c62a 0xa8304613 0xfd469501
        0x698098d8 0x8b44f7af 0xffff5bb1 0x895cd7be
        0x6b901122 0xfd987193 0xa679438e 0x49b40821

        0xf61e2562 0xc040b340 0x265e5a51 0xe9b6c7aa
        0xd62f105d 0x2441453  0xd8a1e681 0xe7d3fbc8
        0x21e1cde6 0xc33707d6 0xf4d50d87 0x455a14ed
        0xa9e3e905 0xfcefa3f8 0x676f02d9 0x8d2a4c8a

        0xfffa3942 0x8771f681 0x6d9d6122 0xfde5380c
        0xa4beea44 0x4bdecfa9 0xf6bb4b60 0xbebfbc70
        0x289b7ec6 0xeaa127fa 0xd4ef3085 0x4881d05
        0xd9d4d039 0xe6db99e5 0x1fa27cf8 0xc4ac5665

        0xf4292244 0x432aff97 0xab9423a7 0xfc93a039
        0x655b59c3 0x8f0ccc92 0xffeff47d 0x85845dd1
        0x6fa87e4f 0xfe2ce6e0 0xa3014314 0x4e0811a1
        0xf7537e82 0xbd3af235 0x2ad7d2bb 0xeb86d391
    } {
        lappend map \$$tName $tVal
    }
    set ::md5::MD5Hash_bodyX [string map $map $::md5::MD5Hash_bodyX]
    unset map
}

# Define the MD5 hashing procedure with inline functions.
proc ::md5::MD5Hash {token msg} $::md5::MD5Hash_bodyX

namespace eval ::flac {
    set block_type_to_string [dict create \
        0 Streaminfo \
        1 Padding \
        2 Application \
        3 Seektable \
        4 "Vorbis comment" \
        5 Cuesheet \
        6 Picture \
    ]

    # http://www.hpl.hp.com/techreports/1999/HPL-1999-144.pdf
    set fixed_coeffs [list \
        [list] \
        [list 1] \
        [list 2 -1] \
        [list 3 -3 1] \
        [list 4 -6 4 -1] \
    ]

    # decode zigag encoded integer
    # https://developers.google.com/protocol-buffers/docs/encoding
    proc zigzag {n} {
        return [expr $n>>1 ^ -($n & 1)]
    }

    proc hex {s} {
        set h ""
        for {set i 0} {$i < [string length $s]} {incr i} {
            append h [format %.2x [scan [string index $s $i] %c]]
        }
        return $h
    }

    proc count_leading_ones {n} {
        set b 0x80
        for {set i 0} {($n & $b) == $b} {incr i} {
            set b [expr $b>>1]
        }
        return $i
    }

    proc utf8_uint {br} {
        set n [bitreader::uint $br 8]
        set c [count_leading_ones $n]

        switch -exact -- $c {
            0 {}
            # TODO: handle better?
            1 {error "invalid utf8_uint width 1"}
            default {
                set width $c
                set n [expr $n & ((1<<(8-$width-1))-1)]
                for {set i 1} {$i < $width} {incr i} {
                    set n [expr ($n<<6) | ([bitreader::uint $br 8]&0x3f)]
                }
            }
        }

        return $n
    }

    proc reverse_bytes32 {n} {
        return  [expr ($n&0xff)<<24 | ($n&0xff00)<<8 | ($n&0xff0000)>>8 | ($n&0xff000000)>>24]
    }

    proc parse_flac_metdata_block_streaminfo {l br _len} {
        return [dict create \
            minimum_block_size [log::entry $l "Minimum block size (samples)" {bitreader::uint $br 16}] \
            maximum_block_size [log::entry $l "Maximum block size (samples)" {bitreader::uint $br 16}] \
            minimum_frame_size [log::entry $l "Minimum frame size (bytes)" {bitreader::uint $br 24}] \
            maximum_frame_size [log::entry $l "Maximum frame size (bytes)" {bitreader::uint $br 24}] \
            sample_rate [log::entry $l "Sample rate" {bitreader::uint $br 20}] \
            channels [log::entry $l "Channels" {expr [bitreader::uint $br 3]+1}] \
            bit_per_sample [log::entry $l "Bits per sample" {expr [bitreader::uint $br 5]+1}] \
            total_samples [log::entry $l "Total samples in stream" {bitreader::uint $br 36}] \
            md5 [log::entry $l "MD5" {hex [bitreader::bytes $br 16]}] \
        ]
    }

    proc parse_flac_metdata_block_seektable {l br len} {
        set seekpoint_count [expr $len / 18]
        for {set i 0} {$i < $seekpoint_count} {incr i} {
            log::section $l "Seekpoint" {
                log::entry $l "Sample number" {
                    set n [bitreader::uint $br 64]
                    set s ""
                    if {$n == 0xffffffffffffffff} {
                        set s "Placeholder"
                    }
                    list [format "%d %s" $n $s]
                }
                log::entry $l "Offset" {bitreader::uint $br 64}
                log::entry $l "Number of samples" {bitreader::uint $br 16}
            }
        }
        return [dict create]
    }

    proc parse_flac_metdata_block_vorbis_comment {l br _len} {
        # vorbis comments uses little endian
        set vendor_length [log::entry $l "Vendor length" {reverse_bytes32 [bitreader::uint $br 32]}]
        log::entry $l "Vendor string" {bitreader::bytes $br $vendor_length}
        set user_comment_list_length [log::entry $l "User comment list length" {reverse_bytes32 [bitreader::uint $br 32]}]
        log::section $l "User comments" {
            for {set i 0} {$i < $user_comment_list_length} {incr i} {
                log::section $l $i {
                    set comment_len [log::entry $l "Length" {reverse_bytes32 [bitreader::uint $br 32]}]
                    log::entry $l "String" {bitreader::bytes $br $comment_len}
                }
            }
        }
        return [dict create]
    }

    proc parse_flac_metdata_block_picture {l br _len} {
        log::entry $l "The picture type" {bitreader::uint $br 32}
        set mime_length [log::entry $l "MIME length" {bitreader::uint $br 32}]
        log::entry $l "MIME type" {bitreader::bytes $br $mime_length}
        set desc_length [log::entry $l "Description length" {bitreader::uint $br 32}]
        log::entry $l "Description" {bitreader::bytes $br $desc_length}
        log::entry $l "Width" {bitreader::uint $br 32}
        log::entry $l "Height" {bitreader::uint $br 32}
        log::entry $l "Color depth" {bitreader::uint $br 32}
        log::entry $l "Number of indexed colors" {bitreader::uint $br 32}
        set picture_length [log::entry $l "Picture length" {bitreader::uint $br 32}]
        log::entry $l "Picture" {bitreader::skip $br [expr $picture_length*8]}
        return [dict create]
    }

    proc parse_flac_metdata_block_cuesheet {l br _len} {
        log::entry $l "Media catalog number" {bitreader::bytes $br 128}
        log::entry $l "Lead-in samples" {bitreader::uint $br 64}
        log::entry $l "Compact disc" {bitreader::uint $br 1}
        log::entry $l "Reserved" {bitreader::uint $br [expr 7+258*8]}
        set track_count [log::entry $l "Number of tracks" {bitreader::uint $br 8}]
        for {set i 0} {$i < $track_count} {incr i} {
            log::section $l  "Track" {
                log::entry $l "Track offset" {bitreader::uint $br 64}
                log::entry $l "Track number" {bitreader::uint $br 8}
                log::entry $l "ISRC" {bitreader::bytes $br 12}
                log::section $l  "Flags" {
                    set track_type [log::entry $l "Track type" {
                        set n [bitreader::uint $br 1]
                        list [format "%d %s" $n [switch -exact -- $n {
                            0 {list Audio}
                            1 {list Non-audio}
                        }]]
                    }]
                    set pre_emphasis [log::entry $l "Pre-emphasis" {bitreader::uint $br 1}]
                    log::entry $l "Unused" {bitreader::uint $br 6}
                }
                log::entry $l "Reserved" {bitreader::bytes $br 13}
                set track_index_count [log::entry $l "Track index count" {bitreader::uint $br 8}]
                for {set j 0} {$j < $track_index_count} {incr j} {
                    log::section $l  "Index" {
                        log::entry $l "Offset" {bitreader::uint $br 64}
                        log::entry $l "Index number" {bitreader::uint $br 8}
                        log::entry $l "Reserved" {bitreader::bytes $br 3}
                    }
                }
            }
        }
        return [dict create]
    }

    proc parse_flac_metadata_block {l br} {
        variable block_type_to_string

        set last_block [log::entry $l "Last block" {bitreader::uint $br 1}]
        log::entry $l "Type" {
            set type [bitreader::uint $br 7]
            set type_name "Unknown"
            if {[dict exists $block_type_to_string $type]} {
                set type_name [dict get $block_type_to_string $type]
            }
            list $type_name
        }
        set len [log::entry $l "Length" {bitreader::uint $br 24}]

        set metablock [switch -exact -- $type_name {
            Streaminfo {parse_flac_metdata_block_streaminfo $l $br $len}
            Seektable {parse_flac_metdata_block_seektable $l $br $len}
            "Vorbis comment" {parse_flac_metdata_block_vorbis_comment $l $br $len}
            Picture {parse_flac_metdata_block_picture $l $br $len}
            Cuesheet {parse_flac_metdata_block_cuesheet $l $br $len}
            default {
                log::entry $l "Data" {bitreader::skip $br [expr $len*8]}
            }
        }]

        return [dict merge [dict create \
            last_block $last_block \
            type $type_name \
        ] $metablock]
    }

    proc parse_frame {l br streaminfo} {
        set frame_start [bitreader::bytepos $br]

        # <14> 11111111111110
        log::entry $l "Sync" {
            set sync [bitreader::uint $br 14]
            set sync_correct "(incorrect)"
            if {$sync == 0x3ffe} {
                set sync_correct "(correct)"
            }
            list [format "%.4x %s" $sync $sync_correct]
        }

        # <1> Reserved
        # 0 : mandatory value
        # 1 : reserved for future use
        log::entry $l "Reserved" {
            set r [bitreader::uint $br 1]
            set r_correct "(incorrect)"
            if {$r == 0} {
                set r_correct "(correct)"
            }
            list [format "%d %s" $r $r_correct]
        }

        # <1> Blocking strategy:
        # 0 : fixed-blocksize stream; frame header encodes the frame number
        # 1 : variable-blocksize stream; frame header encodes the sample number
        set blocking_strategy [log::entry $l "Blocking strategy" {
            switch -exact -- [bitreader::uint $br 1] {
                0 {list Fixed}
                1 {list Variable}
            }
        }]

        # <4> Block size in inter-channel samples:
        # 0000 : reserved
        # 0001 : 192 samples
        # 0010-0101 : 576 * (2^(n-2)) samples, i.e. 576/1152/2304/4608
        # 0110 : get 8 bit (blocksize-1) from end of header
        # 0111 : get 16 bit (blocksize-1) from end of header
        # 1000-1111 : 256 * (2^(n-8)) samples, i.e. 256/512/1024/2048/4096/8192/16384/32768
        lassign [log::entry $l "Block size" {
            set bits [bitreader::uint $br 4]
            switch -exact -- $bits {
                0 {list "reserved" 0 $bits}
                1 {list 192 192 $bits}
                2 -
                3 -
                4 -
                5 {
                    set b [expr 576 * (1<<($bits-2))]
                    list $b $b $bits
                }
                6 {list "end of header (8 bit)" 0 $bits}
                7 {list "end of header (16 bit)" 0 $bits}
                default {
                    # TODO: -8 seems correct, should be +8 in spec?
                    set b [expr 256 * (1<<($bits-8))]
                    list $b $b $bits
                }
            }
        }] _desc block_size block_size_bits

        set sample_rate_pos [bitreader::bytepos $br]
        # <4> Sample rate:
        # 0000 : get from STREAMINFO metadata block
        # 0001 : 88.2kHz
        # 0010 : 176.4kHz
        # 0011 : 192kHz
        # 0100 : 8kHz
        # 0101 : 16kHz
        # 0110 : 22.05kHz
        # 0111 : 24kHz
        # 1000 : 32kHz
        # 1001 : 44.1kHz
        # 1010 : 48kHz
        # 1011 : 96kHz
        # 1100 : get 8 bit sample rate (in kHz) from end of header
        # 1101 : get 16 bit sample rate (in Hz) from end of header
        # 1110 : get 16 bit sample rate (in tens of Hz) from end of header
        # 1111 : invalid, to prevent sync-fooling string of 1s
        lassign [log::entry $l "Sample rate" {
            set bits [bitreader::uint $br 4]
            switch -exact -- $bits {
                0 {list "streaminfo" [dict get $streaminfo sample_rate] $bits}
                1 {list 88200 88200 $bits}
                2 {list 176000 176000 $bits}
                3 {list 19200 19200 $bits}
                4 {list 800 800 $bits}
                5 {list 1600 1600 $bits}
                6 {list 22050 22050 $bits}
                7 {list 44100 44100 $bits}
                8 {list 32000 32000 $bits}
                9 {list 44100 44100 $bits}
                10 {list 48000 48000 $bits}
                11 {list 96000 96000 $bits}
                12 {list "end of header (8 bit*1000)" 0 $bits}
                13 {list "end of header (16 bit)" 0 $bits}
                14 {list "end of header (16 bit*10)" 0 $bits}
                default {list "invalid" 0 $bits}
            }
        }] _desc sample_rate sample_rate_bits

        # <4> Channel assignment
        # 0000-0111 : (number of independent channels)-1. Where defined, the channel order follows SMPTE/ITU-R recommendations. The assignments are as follows:
        # 1 channel: mono
        # 2 channels: left, right
        # 3 channels: left, right, center
        # 4 channels: front left, front right, back left, back right
        # 5 channels: front left, front right, front center, back/surround left, back/surround right
        # 6 channels: front left, front right, front center, LFE, back/surround left, back/surround right
        # 7 channels: front left, front right, front center, LFE, back center, side left, side right
        # 8 channels: front left, front right, front center, LFE, back left, back right, side left, side right
        # 1000 : left/side stereo: channel 0 is the left channel, channel 1 is the side(difference) channel
        # 1001 : right/side stereo: channel 0 is the side(difference) channel, channel 1 is the right channel
        # 1010 : mid/side stereo: channel 0 is the mid(average) channel, channel 1 is the side(difference) channel
        # 1011-1111 : reserved
        lassign [log::entry $l "Channel assignment" {
            switch -exact -- [bitreader::uint $br 4] {
                0  {list "mono" "" 1}
                1  {list "left, right" "" 2}
                2  {list "left, right, center" "" 3}
                3  {list "front left, front right, back left, back right" "" 4}
                4  {list "front left, front right, front center, back/surround left, back/surround right" "" 5}
                5  {list "front left, front right, front center, LFE, back/surround left, back/surround right" "" 6}
                6  {list "front left, front right, front center, LFE, back center, side left, side right" "" 7}
                7  {list "front left, front right, front center, LFE, back left, back right, side left, side right" "" 8}
                8  {list "left/side" LeftSide 2}
                9  {list "side/right" SideRight 2}
                10 {list "mid/side" MidSide 2}
                default {list "reserved" 0 0}
            }
        }] _desc side_channel channels

        # <3> Sample size in bits:
        # 000 : get from STREAMINFO metadata block
        # 001 : 8 bits per sample
        # 010 : 12 bits per sample
        # 011 : reserved
        # 100 : 16 bits per sample
        # 101 : 20 bits per sample
        # 110 : 24 bits per sample
        # 111 : reserved
        lassign [log::entry $l "Sample size" {
            switch -exact -- [bitreader::uint $br 3] {
                0 {list "streaminfo" [dict get $streaminfo bit_per_sample]}
                1 {list 8 8}
                2 {list 12 12}
                3 {list "reserved" 0}
                4 {list 16 16}
                5 {list 20 20}
                6 {list 24 24}
                7 {list "reserved" 0}
            }
        }] _desc sample_size

        # <1> Reserved:
        # 0 : mandatory value
        # 1 : reserved for future use
        log::entry $l "Reserved" {
            set r [bitreader::uint $br 1]
            set r_correct "(incorrect)"
            if {$r == 0} {
                set r_correct "(correct)"
            }
            list [format "%d %s" $r $r_correct]
        }

        log::section $l "End of header" {
            # if(variable blocksize)
            #    <8-56>:"UTF-8" coded sample number (decoded number is 36 bits) [4]
            # else
            #    <8-48>:"UTF-8" coded frame number (decoded number is 31 bits) [4]
            switch -exact -- $blocking_strategy {
                Fixed {log::entry $l "Frame number" {utf8_uint $br}}
                Variable {log::entry $l "Sample number" {utf8_uint $br}}
            }

            # if(blocksize bits == 011x)
            #    8/16 bit (blocksize-1)
            switch -exact -- $block_size_bits {
                6 {set block_size [log::entry $l "Block size" {expr [bitreader::uint $br 8]+1}]}
                7 {set block_size [log::entry $l "Block size" {expr [bitreader::uint $br 16]+1}]}
            }

            # if(sample rate bits == 11xx)
            #    8/16 bit sample rate
            switch -exact -- $sample_rate_bits {
                12 {set sample_rate [log::entry $l "Sample rate" {expr [bitreader::uint $br 8]*1000}]}
                13 {set sample_rate [log::entry $l "Sample rate" {expr [bitreader::uint $br 16]}]}
                14 {set sample_rate [log::entry $l "Sample rate" {expr [bitreader::uint $br 16]*10}]}
            }
        }

        # CRC-8 (polynomial = x^8 + x^2 + x^1 + x^0, initialized with 0) of everything before the crc, including the sync code
        log::entry $l "CRC" {
            set frame_end [expr [bitreader::bytepos $br]-1]
            set ccrc [crc8 [bitreader::byterange $br $frame_start $frame_end]]
            set crc [bitreader::uint $br 8]
            set crc_correct "(correct)"
            if {$crc != $ccrc} {
                set crc_correct [format "(incorrect %.2x)" $ccrc]
            }
            list [format "%.2x %s" $crc $crc_correct]
        }

        set subframe_samples [list]

        for {set c 0} {$c < $channels} {incr c} {
            set extra_inter_chan_bps 0

            # if channel is side, add en extra sample bit
            # TODO: channel is a side channel and has one extra sample bit? where documneted in spec?
            # https://github.com/xiph/flac/blob/37e675b777d4e0de53ac9ff69e2aea10d92e729c/src/libFLAC/stream_decoder.c#L2040
            if {$side_channel == "LeftSide" && $c == 1} {
                set extra_inter_chan_bps 1
            } elseif {$side_channel == "SideRight" && $c == 0} {
                set extra_inter_chan_bps 1
            } elseif {$side_channel == "MidSide" && $c == 1} {
                set extra_inter_chan_bps 1
            }

            log::section $l "Subframe" {
                log::entry $l "Side channel bits" {list $extra_inter_chan_bps}
                lappend subframe_samples [parse_subframe $l $br [expr $sample_size+$extra_inter_chan_bps] $block_size]
            }
        }
        log::entry $l "Byte align padding" {bitreader::uint $br [bitreader::bytealign $br]}

        # <16> CRC-16 (polynomial = x^16 + x^15 + x^2 + x^0, initialized with 0) of everything before the crc, back to and including the frame header sync code
        log::entry $l "Footer CRC" {
            set frame_end [expr [bitreader::bytepos $br]-1]
            set ccrc [crc16 [bitreader::byterange $br $frame_start $frame_end]]
            set crc [bitreader::uint $br 16]
            set crc_correct "(correct)"
            if {$crc != $ccrc} {
                set crc_correct [format "(incorrect %.4x)" $ccrc]
            }
            list [format "%.4x %s" $crc $crc_correct]
        }

        # Transform mid/side channels into left, right
        # mid = (left + right)/2
        # side = left - right
        set nsamples [llength [lindex $subframe_samples 0]]
        set subframe_pcm_samples $subframe_samples
        if {$side_channel == "LeftSide"} {
            set left [lindex $subframe_samples 0]
            set side [lindex $subframe_samples 1]
            set right [list]
            for {set i 0} {$i < $nsamples} {incr i} {
                set l [lindex $left $i]
                set s [lindex $side $i]
                lappend right [expr $l - $s]
            }
            set subframe_pcm_samples [list $left $right]
        } elseif {$side_channel == "SideRight"} {
            set side [lindex $subframe_samples 0]
            set right [lindex $subframe_samples 1]
            set left [list]
            for {set i 0} {$i < $nsamples} {incr i} {
                set s [lindex $side $i]
                set r [lindex $right $i]
                lappend left [expr $s + $r]
            }
            set subframe_pcm_samples [list $left $right]
        } elseif {$side_channel == "MidSide"} {
            set mid [lindex $subframe_samples 0]
            set side [lindex $subframe_samples 1]
            set left [list]
            set right [list]
            for {set i 0} {$i < $nsamples} {incr i} {
                set m [lindex $mid $i]
                set s [lindex $side $i]
                set m [expr ($m <<1) | ($s&1)]
                lappend left [expr ($m + $s) >> 1]
                lappend right [expr ($m - $s) >> 1]
            }
            set subframe_pcm_samples [list $left $right]
        }

        return $subframe_pcm_samples
    }

    proc parse_subframe {l br sample_size block_size} {
        variable fixed_coeffs

        # <1> Zero bit padding, to prevent sync-fooling string of 1s
        log::entry $l "Zero bit" {
            set z [bitreader::uint $br 1]
            set z_correct "(incorrect)"
            if {$z == 0} {
                set z_correct "(correct)"
            }
            list [format "%d %s" $z $z_correct]
        }

        # <6> Subframe type:
        # 000000 : SUBFRAME_CONSTANT
        # 000001 : SUBFRAME_VERBATIM
        # 00001x : reserved
        # 0001xx : reserved
        # 001xxx : if(xxx <= 4) SUBFRAME_FIXED, xxx=order ; else reserved
        # 01xxxx : reserved
        # 1xxxxx : SUBFRAME_LPC, xxxxx=order-1
        lassign [log::entry $l "Subframe type" {
            set bits [bitreader::uint $br 6]
            switch -exact -- $bits {
                0 {list Constant}
                1 {list Verbatim}
                8 -
                9 -
                10 -
                11 -
                12 {
                    list Fixed [expr $bits & 0x7]
                }
                default {
                    if {($bits & 0x20) == 0x20} {
                        list LPC [expr ($bits & 0x1f) + 1]
                    } else {
                        list Reserved
                    }
                }
            }
        }] subframe_type lpc_order
        log::entry $l "Order" {list $lpc_order}

        # 'Wasted bits-per-sample' flag:
        # 0 : no wasted bits-per-sample in source subblock, k=0
        # 1 : k wasted bits-per-sample in source subblock, k-1 follows, unary coded; e.g. k=3 => 001 follows, k=7 => 0000001 follows.
        set wasted_bits_flag [log::entry $l "Wasted bits flag" {bitreader::uint $br 1}]
        set wasted_bits_k 0
        if {$wasted_bits_flag} {
            set wasted_bits_k [log::entry $l "Wasted bits k" {expr [bitreader::unary $br]+1}]
        }
        incr sample_size [expr -$wasted_bits_k]

        set pcm_samples [list]

        switch -exact -- $subframe_type {
            Constant {
                # <n> Unencoded constant value of the subblock, n = frame's bits-per-sample.
                set value [log::entry $l "Value" {bitreader::int $br $sample_size}]
                set pcm_samples [lrepeat $block_size $value]
            }
            Verbatim {
                # <n*i> Unencoded subblock; n = frame's bits-per-sample, i = frame's blocksize.
                log::entry $l "Samples" {
                    set pcm_samples [list]
                    for {set i 0} {$i < $block_size} {incr i} {
                        lappend pcm_samples [bitreader::int $br $sample_size]
                    }
                    list [format "%d samples" $block_size]
                }
            }
            Fixed {
                # <n> Unencoded warm-up samples (n = frame's bits-per-sample * predictor order).
                set warmup_samples [list]
                log::entry $l "Warmup samples" {
                    for {set i 0} {$i < $lpc_order} {incr i} {
                        lappend warmup_samples [bitreader::int $br $sample_size]
                    }
                    list [format "%d samples" $lpc_order]
                }
                # Encoded residual
                set residuals [parse_residual $l $br $block_size $lpc_order]
                set coeffs [lindex $fixed_coeffs $lpc_order]
                set samples [concat $warmup_samples $residuals]
                set pcm_samples [decode_lpc $lpc_order $samples $coeffs 0]
            }
            LPC {
                # <n> Unencoded warm-up samples (n = frame's bits-per-sample * lpc order).
                log::entry $l "Warmup samples" {
                    set warmup_samples [list]
                    for {set i 0} {$i < $lpc_order} {incr i} {
                        lappend warmup_samples [bitreader::int $br $sample_size]
                    }
                    list [format "%d samples" $lpc_order]
                }
                # <4> (Quantized linear predictor coefficients' precision in bits)-1 (1111 = invalid).
                set precision [log::entry $l "Precision" {expr [bitreader::uint $br 4]+1}]
                # <5> Quantized linear predictor coefficient shift needed in bits (NOTE: this number is signed two's-complement).
                set shift [log::entry $l "Shift" {bitreader::int $br 5}]
                # <n> Unencoded predictor coefficients (n = qlp coeff precision * lpc order) (NOTE: the coefficients are signed two's-complement).
                log::entry $l "Coefficients" {
                    set coeffs [list]
                    for {set i 0 } {$i < $lpc_order} {incr i} {
                        lappend coeffs [bitreader::int $br $precision]
                    }
                    list [format "%d coefficients" $lpc_order]
                }
                # Encoded residual
                set residuals [parse_residual $l $br $block_size $lpc_order]
                set samples [concat $warmup_samples $residuals]
                set pcm_samples [decode_lpc $lpc_order $samples $coeffs $shift]
            }
        }

        for {set i 0} {$i < [llength $pcm_samples]} {incr i} {
            lset pcm_samples $i [expr [lindex $pcm_samples $i] << $wasted_bits_k]
        }

        return $pcm_samples
    }

    proc parse_residual {l br block_size lpc_order} {
        # <2> Residual coding method:
        # 00 : partitioned Rice coding with 4-bit Rice parameter; RESIDUAL_CODING_METHOD_PARTITIONED_RICE follows
        # 01 : partitioned Rice coding with 5-bit Rice parameter; RESIDUAL_CODING_METHOD_PARTITIONED_RICE2 follows
        # 10-11 : reserved
        lassign [log::entry $l "Residual coding method" {
            switch -exact -- [bitreader::uint $br 2] {
                0 {list "Rice (4)" 4 15}
                1 {list "Rice2 (5)" 5 31}
                default {error "reserved"}
            }
        }] _desc rice_parameter_bits rice_parameter_escape
        log::entry $l "Rice parameter bits" {list $rice_parameter_bits}
        log::entry $l "Rice escape code" {list $rice_parameter_escape}

        # <4> Partition order.
        set partition_order [log::entry $l "Partition order" {bitreader::uint $br 4}]
        # There will be 2^order partitions.
        set rice_partitions [expr 1 << $partition_order]
        log::entry $l "Rice partitions" {list $rice_partitions}

        set samples [list]

        for {set i 0} {$i < $rice_partitions} {incr i} {
            log::section $l "Partition" {
                # Encoding parameter:
                # <4(+5)> Encoding parameter:
                # 0000-1110 : Rice parameter.
                # 1111 : Escape code, meaning the partition is in unencoded binary form using n bits per sample; n follows as a 5-bit number.
                # Or:
                # <5(+5)> Encoding parameter:
                # 00000-11110 : Rice parameter.
                # 11111 : Escape code, meaning the partition is in unencoded binary form using n bits per sample; n follows as a 5-bit number.
                #
                # Encoded residual. The number of samples (n) in the partition is determined as follows:
                # if the partition order is zero, n = frame's blocksize - predictor order
                # else if this is not the first partition of the subframe, n = (frame's blocksize / (2^partition order))
                # else n = (frame's blocksize / (2^partition order)) - predictor order
                if {$partition_order == 0} {
                    set samples_count [expr $block_size-$lpc_order]
                } elseif {$i != 0} {
                    set samples_count [expr $block_size / $rice_partitions]
                } else {
                    set samples_count [expr ($block_size / $rice_partitions)-$lpc_order]
                }
                set rice_parameter [log::entry $l "Rice parameter" {bitreader::uint $br $rice_parameter_bits}]
                if {$rice_parameter == $rice_parameter_escape} {
                    set sample_size [log::entry $l "Escape sample size" {bitreader::uint $br 5}]
                    log::entry $l "Samples" {
                        for {set j 0} {$j < $samples_count} {incr j} {
                            lappend samples [bitreader::uint $br $sample_size]
                        }
                        list [format "%d samples" $samples_count]
                    }
                } else {
                    log::entry $l "Samples" {
                        for {set j 0} {$j < $samples_count} {incr j} {
                            set high [bitreader::unary $br]
                            set low [bitreader::uint $br $rice_parameter]
                            set residual [zigzag [expr $high<<$rice_parameter | $low]]
                            lappend samples $residual
                        }
                        list [format "%d samples" $samples_count]
                    }
                }
            }
        }

        return $samples
    }

    proc decode_lpc {order residuals coeffs shift} {
        for {set i $order} {$i < [llength $residuals]} {incr i} {
            set r 0
            for {set j 0} {$j < [llength $coeffs]} {incr j} {
                set c [lindex $coeffs $j]
                set s [lindex $residuals [expr $i-$j-1]]
                incr r [expr $c * $s]
            }

            set r [expr [lindex $residuals $i] + ($r >> $shift)]
            lset residuals $i $r
        }

        return $residuals
    }

    proc decode {data metablockscript framescript} {
        set br [bitreader::new $data]
        set l [log::new $br]
        set md5samples [md5::MD5Init]

        log::entry $l "Magic" {bitreader::bytes $br 4}
        while {1} {
            log::section $l "Metablock" {
                set metablock [parse_flac_metadata_block $l $br]
                switch -exact -- [dict get $metablock type] {
                    Streaminfo {set streaminfo $metablock}
                }
            }

            {*}$metablockscript $metablock

            if {[dict get $metablock last_block]} {
                break
            }
        }

        set stream_samples 0
        set decoded_samples 0
        set total_samples [dict get $streaminfo total_samples]
        set bit_per_sample [dict get $streaminfo bit_per_sample]

        while {![bitreader::end $br]} {
            log::section $l "Frame" {
                set subframes [parse_frame $l $br $streaminfo]

                set channels [llength $subframes]
                set nsamples [llength [lindex $subframes 0]]
                set md5data ""
                for {set i 0 } {$i < $nsamples && ($total_samples == 0 || $stream_samples < $total_samples)} {incr i} {
                    for {set c 0} {$c < $channels} {incr c} {
                        set s [lindex $subframes $c $i]
                        switch -exact -- $bit_per_sample {
                            8 {append md5data [binary format c $s]}
                            16 {append md5data [binary format s $s]}
                            24 {append md5data [binary format ccc [expr $s&0xff] [expr ($s&0xff00)>>8] [expr ($s&0xff0000)>>16]]}
                            32 {append md5data [binary format i $s]}
                        }
                    }

                    incr stream_samples
                }

                md5::MD5Update $md5samples $md5data
                incr decoded_samples $nsamples

                {*}$framescript $subframes
            }
        }

        log::entry $l "MD5" {
            set calc_md5 [hex [md5::MD5Final $md5samples]]
            set md5_correct "(correct)"
            set si_md5 [dict get $streaminfo md5]
            if {$si_md5 != $calc_md5} {
                set md5_correct [format "(metadata %s)" $si_md5]
            }
            list [format "%s %s" $calc_md5 $md5_correct]
        }
        log::entry $l "Stream samples" {list $stream_samples}
        log::entry $l "Decoded samples" {list $decoded_samples}

        bitreader::delete $br
    }
}

namespace eval ::flac_to_wav {
    namespace export *
    variable uid
    if {![info exists uid]} {
        set uid 0
    }

    proc transcode {flacch wavch} {
        variable uid
        set t [namespace current]::[incr uid]
        upvar #0 $t s

        set flacdata [read $flacch]

        array set s [list \
            wavch $wavch
        ]

        flac::decode \
            $flacdata \
            [list ::flac_to_wav::metablock $t] \
            [list ::flac_to_wav::subframes $t]

        wavwriter::delete $s(ww)

        unset s
    }

    proc metablock {t metablock} {
        upvar #0 $t s

        if {[dict get $metablock type] != "Streaminfo"} {
            return
        }

        set s(ww) [wavwriter::new $s(wavch) [dict create \
            channels [dict get $metablock channels] \
            sample_rate [dict get $metablock sample_rate] \
            bitdepth [dict get $metablock bit_per_sample] \
            total_samples [dict get $metablock total_samples] \
        ]]
    }

    proc subframes {t subframes} {
        upvar #0 $t s

        set channels [llength $subframes]
        set nsamples [llength [lindex $subframes 0]]
        for {set i 0 } {$i < $nsamples} {incr i} {
            for {set c 0} {$c < $channels} {incr c} {
                wavwriter::write $s(ww) [lindex $subframes $c $i]
            }
        }
    }
}

set flacch stdin
set wavch stdout
lassign $argv flacfile wavfile
if {$flacfile != "-" && $flacfile != ""} {
    set flacch [open $flacfile r]
}
if {$wavfile != "-" && $wavfile != ""} {
    set wavch [open $wavfile w]
}
fconfigure $flacch -translation binary
fconfigure $wavch -translation binary

if {[catch {flac_to_wav::transcode $flacch $wavch}]} {
    puts stderr $errorInfo
    exit 1
}

close $flacch
close $wavch
