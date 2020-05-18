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
# TODO:
# MD5 of samples

set section_depth 0

proc section {br label body} {
    global section_depth

    set indent [string repeat "  " $section_depth]
    set start [bitreader bytepos $br]
    puts [format "%.8d-%.8d      %s- %s:" $start $start $indent $label]
    incr section_depth 1
    uplevel 1 $body
    incr section_depth -1
}

proc entry {br label body} {
    global section_depth

    set indent [string repeat "  " $section_depth]
    set start [bitreader bytepos $br]
    set startbits [bitreader bitpos $br]
    set r [uplevel 1 $body]
    set end [bitreader bytepos $br]
    set endbits [bitreader bitpos $br]
    set bytealign [bitreader bytealign $br]
    if {$bytealign == 0} {
        incr end -1
    }
    set bitsize [expr $endbits - $startbits]
    lassign $r value
    puts [format "%.8d-%.8d %6d %s%s: %s" $start $end $bitsize $indent $label $value]
    return $r
}

proc bitreader args {
    lassign $args cmd
    set args [lreplace $args 0 0]

    switch $cmd {
        new {}
        uint -
        int -
        unary -
        bytes -
        ascii -
        skip -
        bytepos -
        bitpos -
        end -
        bytealign -
        byterange -
        delete {
            lassign $args handlevar
            upvar #0 $handlevar h
            set args [lreplace $args 0 0]
        }
        default {
            error "unknown cmd $cmd"
        }
    }

    switch -exact -- $cmd {
        new {
            lassign $args data

            for {set i 0} {1} {incr i} {
                set handlevar "::__bitreader$i"
                if {![info exists $handlevar]} {
                    upvar #0 $handlevar h
                    break
                }
            }

            array set h [list \
                data $data \
                buf 0 \
                bufbits 0 \
                pos 0 \
                bitpos 0 \
            ]

            return $handlevar
        }
        uint {
            lassign $args bits

            while {$bits > $h(bufbits)} {
                set b [scan [string index $h(data) $h(pos)] %c]
                set h(buf) [expr ($h(buf)<<8) | $b]
                incr h(bufbits) 8
                incr h(pos) 1
            }
            incr h(bitpos) $bits

            set n [expr $h(buf)>>($h(bufbits)-$bits)]
            set h(buf) [expr $h(buf) & ((1<<($h(bufbits)-$bits))-1)]
            incr h(bufbits) [expr -$bits]

            return $n
        }
        int {
            lassign $args bits
            set n [bitreader uint $handlevar $bits]
            # two's complement
            if {$n & (1 << ($bits-1))} {
                set n [expr -((~$n & (1<<$bits)-1)+1)]
            }
            return $n
        }
        unary {
            for {set n 0} {[bitreader uint $handlevar 1] == 0} {incr n} {
               # nop
            }
            return $n
        }
        bytes -
        ascii {
            lassign $args bytes
            set s ""
            for {set i 0} {$i < $bytes} {incr i} {
                append s [format %c [bitreader uint $handlevar 8]]
            }
            return $s
        }
        skip {
            lassign $args bits

            if {$bits > $h(bufbits)} {
                incr bits [expr -$h(bufbits)]
                set h(buf) 0
                set h(bufbits) 0

                set skip_bytes [expr $bits / 8]
                set bits [expr $bits % 8]
                incr h(pos) $skip_bytes
            }
            incr h(bitpos) $bits

            bitreader uint $handlevar $bits
            return ""
        }
        bytepos {
            if {$h(bufbits) > 0} {
                return [expr $h(pos)-1]
            }
            return $h(pos)
        }
        bitpos {
            return $h(bitpos)
        }
        end {
            return [expr $h(pos) >= [string length $h(data)]]
        }
        bytealign {
            return $h(bufbits)
        }
        byterange {
            lassign $args start end
            return [string range $h(data) $start $end]
        }
        delete {
            unset h
        }
    }
}

proc wstring {ch s} {
    puts -nonewline $ch $s
}

proc wuint8 {ch n} {
    puts -nonewline $ch [binary format c $n]
}

proc wint16le {ch n} {
    puts -nonewline $ch [binary format s $n]
}

proc wint24le {ch n} {
    puts -nonewline $ch [binary format ccc [expr $n&0xff] [expr ($n&0xff00)>>8] [expr ($n&0xff0000)>>16]]
}

proc wuint16le {ch n} {
    puts -nonewline $ch [binary format su1 $n]
}

proc wuint32le {ch n} {
    puts -nonewline $ch [binary format iu1 $n]
}

proc wavwriter args {
    lassign $args cmd
    set args [lreplace $args 0 0]

    switch $cmd {
        new {}
        write -
        delete {
            lassign $args handlevar
            upvar #0 $handlevar h
            set args [lreplace $args 0 0]
        }
        default {
            error "unknown cmd $cmd"
        }
    }

    switch -exact -- $cmd {
        new {
            lassign $args ch config

            for {set i 0} {1} {incr i} {
                set handlevar "::__wavwriter$i"
                if {![info exists $handlevar]} {
                    upvar #0 $handlevar h
                    break
                }
            }

            set bitdepth [dict get $config bitdepth]
            set channels [dict get $config channels]
            set sample_rate [dict get $config sample_rate]
            set data_size [expr $channels * $bitdepth/8 * [dict get $config total_samples]]
            puts -nonewline $ch "RIFF"
            # length
            wuint32le $ch [expr 16+$data_size]
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

            array set h [list \
                ch $ch \
                bitdepth $bitdepth \
            ]

            return $handlevar
        }
        write {
            lassign $args n
            switch -exact -- $h(bitdepth) {
                8 {wuint8 $h(ch) [expr $n+128]}
                16 {wint16le $h(ch) $n}
                24 {wint24le $h(ch) $n}
            }
        }
        delete {
            unset h
        }
    }
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
    set n [bitreader uint $br 8]
    set c [count_leading_ones $n]

    switch -exact -- $c {
        0 {set width 1}
        # TODO: handle better?
        1 {error "invalid utf8_uint width 1"}
        default {
            set width $c
            set n [expr $n & ((1<<(8-$width-1))-1)]
            for {set i 1} {$i < $width} {incr i} {
                set n [expr ($n<<6) | ([bitreader uint $br 8]&0x3f)]
            }
        }
    }

    return $n
}

set block_type_to_string [dict create \
    0 Streaminfo \
    1 Padding \
    2 Application \
    3 Seektable \
    4 "Vorbis comment" \
    5 Cuesheet \
    6 Picture \
]

proc parse_flac_metdata_block_streaminfo {br} {
    return [dict create \
        minimum_block_size [entry $br "Minimum block size (samples)" {bitreader uint $br 16}] \
        maximum_block_size [entry $br "Maximum block size (samples)" {bitreader uint $br 16}] \
        minimum_frame_size [entry $br "Minimum frame size (bytes)" {bitreader uint $br 24}] \
        maximum_frame_size [entry $br "Maximum frame size (bytes)" {bitreader uint $br 24}] \
        sample_rate [entry $br "Sample rate" {bitreader uint $br 20}] \
        channels [entry $br "Channels" {expr [bitreader uint $br 3]+1}] \
        bit_per_sample [entry $br "Bits per sample" {expr [bitreader uint $br 5]+1}] \
        total_samples [entry $br "Total samples in stream" {bitreader uint $br 36}] \
        md5 [entry $br "MD5" {hex [bitreader bytes $br 16]}] \
    ]
}

proc parse_flac_metadata_block {br} {
    global block_type_to_string

    set last_block [entry $br "Last block" {bitreader uint $br 1}]
    entry $br "Type" {
        set type [bitreader uint $br 7]
        set type_name "Unknown"
        if {[dict exists $block_type_to_string $type]} {
            set type_name [dict get $block_type_to_string $type]
        }
        list $type_name
    }
    set len [entry $br "Length" {bitreader uint $br 24}]

    set metablock [switch -exact -- $type {
        0 {parse_flac_metdata_block_streaminfo $br}
        default {
            entry $br "Data" {bitreader skip $br [expr $len*8]}
        }
    }]

    return [dict merge [dict create \
        last_block $last_block \
        type $type_name \
    ] $metablock]
}

proc parse_frame {br streaminfo} {
    set frame_start [bitreader bytepos $br]

    # <14> 11111111111110
    entry $br "Sync" {
        set sync [bitreader uint $br 14]
        set sync_correct "(incorrect)"
        if {$sync == 0x3ffe} {
            set sync_correct "(correct)"
        }
        list [format "%.4x %s" $sync $sync_correct]
    }

    # <1> Reserved
    # 0 : mandatory value
    # 1 : reserved for future use
    entry $br "Reserved" {
        set r [bitreader uint $br 1]
        set r_correct "(incorrect)"
        if {$r == 0} {
            set r_correct "(correct)"
        }
        list [format "%d %s" $r $r_correct]
    }

    # <1> Blocking strategy:
    # 0 : fixed-blocksize stream; frame header encodes the frame number
    # 1 : variable-blocksize stream; frame header encodes the sample number
    set blocking_strategy [entry $br "Blocking strategy" {
        switch -exact -- [bitreader uint $br 1] {
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
    lassign [entry $br "Block siz" {
        set bits [bitreader uint $br 4 "Block size"]
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

    set sample_rate_pos [bitreader bytepos $br]
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
    lassign [entry $br "Sample rate" {
        set bits [bitreader uint $br 4]
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
    lassign [entry $br "Channel assignment" {
        switch -exact -- [bitreader uint $br 4] {
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
    lassign [entry $br "Sample size" {
        switch -exact -- [bitreader uint $br 3] {
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
    entry $br "Reserved" {
        set r [bitreader uint $br 1]
        set r_correct "(incorrect)"
        if {$r == 0} {
            set r_correct "(correct)"
        }
        list [format "%d %s" $r $r_correct]
    }

    section $br "End of header" {
        # if(variable blocksize)
        #    <8-56>:"UTF-8" coded sample number (decoded number is 36 bits) [4]
        # else
        #    <8-48>:"UTF-8" coded frame number (decoded number is 31 bits) [4]
        switch -exact -- $blocking_strategy {
            Fixed {entry $br "Frame number" {utf8_uint $br}}
            Variable {entry $br "Sample number" {utf8_uint $br}}
        }

        # if(blocksize bits == 011x)
        #    8/16 bit (blocksize-1)
        switch -exact -- $block_size_bits {
            6 {set block_size [entry $br "Block size" {expr [bitreader uint $br 8]+1}]}
            7 {set block_size [entry $br "Block size" {expr [bitreader uint $br 16]+1}]}
        }

        # if(sample rate bits == 11xx)
        #    8/16 bit sample rate
        switch -exact -- $sample_rate_bits {
            12 {set sample_rate [entry $br "Sample rate" {expr [bitreader uint $br 8]*1000}]}
            13 {set sample_rate [entry $br "Sample rate" {expr [bitreader uint $br 16]}]}
            14 {set sample_rate [entry $br "Sample rate" {expr [bitreader uint $br 16]*10}]}
        }
    }

    # CRC-8 (polynomial = x^8 + x^2 + x^1 + x^0, initialized with 0) of everything before the crc, including the sync code
    entry $br "CRC" {
        set frame_end [expr [bitreader bytepos $br]-1]
        set ccrc [crc8 [bitreader byterange $br $frame_start $frame_end]]
        set crc [bitreader uint $br 8]
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

        section $br "Subframe" {
            entry $br "Side channel bits" {list $extra_inter_chan_bps}
            lappend subframe_samples [parse_subframe $br [expr $sample_size+$extra_inter_chan_bps] $block_size]
        }
    }
    entry $br "Byte align padding" {bitreader uint $br [bitreader bytealign $br]}

    # <16> CRC-16 (polynomial = x^16 + x^15 + x^2 + x^0, initialized with 0) of everything before the crc, back to and including the frame header sync code
    entry $br "Footer CRC" {
        set frame_end [expr [bitreader bytepos $br]-1]
        set ccrc [crc16 [bitreader byterange $br $frame_start $frame_end]]
        set crc [bitreader uint $br 16]
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

proc parse_subframe {br sample_size block_size} {
    global fixed_coeffs

    # <1> Zero bit padding, to prevent sync-fooling string of 1s
    entry $br "Zero bit" {
        set z [bitreader uint $br 1]
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
    lassign [entry $br "Subframe type" {
        set bits [bitreader uint $br 6]
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
    entry $br "Order" {list $lpc_order}

    # 'Wasted bits-per-sample' flag:
    # 0 : no wasted bits-per-sample in source subblock, k=0
    # 1 : k wasted bits-per-sample in source subblock, k-1 follows, unary coded; e.g. k=3 => 001 follows, k=7 => 0000001 follows.
    set wasted_bits_flag [entry $br "Wasted bits flag" {bitreader uint $br 1}]
    set wasted_bits_k 0
    if {$wasted_bits_flag} {
        set wasted_bits_k [entry $br "Wasted bits k" {expr [bitreader unary $br]+1}]
    }
    incr sample_size [expr -$wasted_bits_k]

    set pcm_samples [list]

    switch -exact -- $subframe_type {
        Constant {
            # <n> Unencoded constant value of the subblock, n = frame's bits-per-sample.
            set value [entry $br "Value" {bitreader int $br $sample_size}]
            set pcm_samples [lrepeat $block_size $value]
        }
        Verbatim {
            # <n> Unencoded warm-up samples (n = frame's bits-per-sample * predictor order).
            entry $br "Samples" {
                set pcm_samples [list]
                for {set i 0} {$i < $block_size} {incr i} {
                    lappend pcm_samples [bitreader int $br $sample_size]
                }
                list [format "%d samples" $block_size]
            }
        }
        Fixed {
            # <n> Unencoded predictor coefficients (n = qlp coeff precision * lpc order) (NOTE: the coefficients are signed two's-complement).
            set warmup_samples [list]
            entry $br "Warmup samples" {
                for {set i 0} {$i < $lpc_order} {incr i} {
                    lappend warmup_samples [bitreader int $br $sample_size]
                }
                list [format "%d samples" $lpc_order]
            }
            # Encoded residual
            set residuals [parse_residual $br $block_size $lpc_order]
            set coeffs [lindex $fixed_coeffs $lpc_order]
            set samples [concat $warmup_samples $residuals]
            set pcm_samples [decode_lpc $lpc_order $samples $coeffs 0]
        }
        LPC {
            # <n> Unencoded warm-up samples (n = frame's bits-per-sample * lpc order).
            entry $br "Warmup samples" {
                set warmup_samples [list]
                for {set i 0} {$i < $lpc_order} {incr i} {
                    lappend warmup_samples [bitreader int $br $sample_size]
                }
                list [format "%d samples" $lpc_order]
            }
            # <4> (Quantized linear predictor coefficients' precision in bits)-1 (1111 = invalid).
            set precision [entry $br "Precision" {expr [bitreader uint $br 4]+1}]
            # <5> Quantized linear predictor coefficient shift needed in bits (NOTE: this number is signed two's-complement).
            set shift [entry $br "Shift" {bitreader int $br 5}]

            # <n> Unencoded predictor coefficients (n = qlp coeff precision * lpc order) (NOTE: the coefficients are signed two's-complement).
            entry $br "Coefficients" {
                set coeffs [list]
                for {set i 0 } {$i < $lpc_order} {incr i} {
                    lappend coeffs [bitreader int $br $precision]
                }
                list [format "%d coefficients" $lpc_order]
            }
            # Encoded residual
            set residuals [parse_residual $br $block_size $lpc_order]
            set samples [concat $warmup_samples $residuals]
            set pcm_samples [decode_lpc $lpc_order $samples $coeffs $shift]
        }
    }

    for {set i 0} {$i < [llength $pcm_samples]} {incr i} {
        lset pcm_samples $i [expr [lindex $pcm_samples $i] << $wasted_bits_k]
    }

    return $pcm_samples
}

proc parse_residual {br block_size lpc_order} {
    # <2> Residual coding method:
    # 00 : partitioned Rice coding with 4-bit Rice parameter; RESIDUAL_CODING_METHOD_PARTITIONED_RICE follows
    # 01 : partitioned Rice coding with 5-bit Rice parameter; RESIDUAL_CODING_METHOD_PARTITIONED_RICE2 follows
    # 10-11 : reserved
    lassign [entry $br "Residual coding method" {
        switch -exact -- [bitreader uint $br 2] {
            0 {list "Rice (4)" 4 15}
            1 {list "Rice2 (5)" 5 31}
            default {error "reserved"}
        }
    }] _desc rice_parameter_bits rice_parameter_escape
    entry $br "Rice parameter bits" {list $rice_parameter_bits}
    entry $br "Rice escape code" {list $rice_parameter_escape}

    # <4> Partition order.
    set partition_order [entry $br "Partition order" {bitreader uint $br 4}]
    # There will be 2^order partitions.
    set rice_partitions [expr 1 << $partition_order]
    entry $br "Rice partitions" {list $rice_partitions}

    set samples [list]

    for {set i 0} {$i < $rice_partitions} {incr i} {
        section $br "Partition" {
            if {$partition_order == 0} {
                set samples_count [expr $block_size-$lpc_order]
            } elseif {$i != 0} {
                set samples_count [expr $block_size / $rice_partitions]
            } else {
                set samples_count [expr ($block_size / $rice_partitions)-$lpc_order]
            }

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
            set rice_parameter [entry $br "Rice parameter" {bitreader uint $br $rice_parameter_bits}]
            if {$rice_parameter == $rice_parameter_escape} {
                set sample_size [entry $br "Escape sample size" {bitreader uint $br 5}]
                entry $br "Samples" {
                    for {set j 0} {$j < $samples_count} {incr j} {
                        lappend samples [bitreader uint $br $sample_size]
                    }
                    list [format "%d samples" $samples_count]
                }
            } else {
                entry $br "Samples" {
                    for {set j 0} {$j < $samples_count} {incr j} {
                        set high [bitreader unary $br]
                        set low [bitreader uint $br $rice_parameter]
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
        set sample 0
        for {set j 0} {$j < [llength $coeffs]} {incr j} {
            set c [lindex $coeffs $j]
            set s [lindex $residuals [expr $i-$j-1]]
            incr sample [expr $c * $s]
        }

        set sample [expr [lindex $residuals $i] + ($sample >> $shift)]
        lset residuals $i $sample
    }

    return $residuals
}

proc decode_flac {data} {
    set br [bitreader new $data]

    entry $br "Magic" {bitreader ascii $br 4}
    while {1} {
        section $br "Metablock" {
            set metablock [parse_flac_metadata_block $br]
            switch -exact -- [dict get $metablock type] {
                Streaminfo {set streaminfo $metablock}
            }
        }
        if {[dict get $metablock last_block]} {
            break
        }
    }

    set frames [list]
    while {![bitreader end $br]} {
        section $br "Frame" {
            set subframes [parse_frame $br $streaminfo]
            lappend frames $subframes
        }
    }

    bitreader delete $br

    return [dict create \
        streaminfo $streaminfo \
        frames $frames \
    ]
}

proc flac_to_wav {flacfile wavfile} {
    set flacch [open $flacfile r]
    fconfigure $flacch -translation binary
    set flacdata [read $flacch]
    close $flacch

    set wavch [open $wavfile w]
    fconfigure $wavch -translation binary

    set flac [decode_flac $flacdata]

    set ww [wavwriter new $wavch [dict create \
        channels [dict get $flac streaminfo channels] \
        sample_rate [dict get $flac streaminfo sample_rate] \
        bitdepth [dict get $flac streaminfo bit_per_sample] \
        total_samples [dict get $flac streaminfo total_samples] \
    ]]
    foreach frame [dict get $flac frames] {
        set channels [llength $frame]
        set nsamples [llength [lindex $frame 0]]
        for {set i 0 } {$i < $nsamples} {incr i} {
            for {set c 0} {$c < $channels} {incr c} {
                set s [lindex $frame $c $i]
                wavwriter write $ww $s
            }
        }
    }
    wavwriter delete $ww
    close $wavch
}

lassign $argv flacfile wavfile
if {$flacfile == "" || $wavfile == ""} {
    puts "Usage: $argv0 file.flac file.wav"
    exit 0
}

if {[catch {flac_to_wav $flacfile $wavfile}]} {
    puts $errorInfo
    exit 1
}
