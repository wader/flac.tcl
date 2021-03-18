# flac.tcl

Probably the slowest FLAC decoder in the world.

Features:

- Very verbose output
- 8, 16, 24 bit depth. Probably supports 32 bit also but not tested as there seems to be no encoder that supports it
- All channel configurations but only mono, stereo and side channel stereo files tested
- Parses all metadata blocks
- Frame header and footer CRC and samples MD5

## Usage

```sh
./flac.tcl [file.flac] [file.wav]
```
If input or output file is not specified stdin and stdout is used. Log is written to stderr.

Run tests, requires ffmpeg, tcl and flac cli tool:

```sh
./test.sh ./flac.tcl [TEMPDIR]
```
Or run tests using docker:
```sh
docker build .
```

## Example output

```
00000000-00000003     32 Magic: fLaC
00000004-00000004      - Metablock:
00000004-00000004      1   Last block: 0
00000004-00000004      7   Type: Streaminfo
00000005-00000007     24   Length: 34
00000008-00000009     16   Minimum block size (samples): 4608
0000000a-0000000b     16   Maximum block size (samples): 4608
0000000c-0000000e     24   Minimum frame size (bytes): 16
0000000f-00000011     24   Maximum frame size (bytes): 28106
00000012-00000014     20   Sample rate: 44100
00000014-00000014      3   Channels: 2
00000014-00000015      5   Bits per sample: 24
00000015-00000019     36   Total samples in stream: 44100
0000001a-00000029    128   MD5: 6e852e22654d74254205c304a6fa5d69
0000002a-0000002a      - Metablock:
0000002a-0000002a      1   Last block: 0
0000002a-0000002a      7   Type: Vorbis comment
0000002b-0000002d     24   Length: 46
0000002e-00000031     32   Vendor length: 13
00000032-0000003e    104   Vendor string: Lavf58.45.100
0000003f-00000042     32   User comment list length: 1
00000043-00000043        - User comments:
00000043-00000043          - 0:
00000043-00000046     32       Length: 21
00000047-0000005b    168       String: encoder=Lavf58.45.100
0000005c-0000005c      - Metablock:
0000005c-0000005c      1   Last block: 1
0000005c-0000005c      7   Type: Padding
0000005d-0000005f     24   Length: 8192
00000060-0000205f  65536   Data:
00002060-00002060      - Frame:
00002060-00002061     14   Sync: 3ffe (correct)
00002061-00002061      1   Reserved: 0 (correct)
00002061-00002061      1   Blocking strategy: Fixed
...
0001fa82-0001fa82        - Subframe:
0001fa82-0001fa82      0     Side channel bits: 0
0001fa82-0001fa82      1     Zero bit: 0 (correct)
0001fa82-0001fa83      6     Subframe type: LPC
0001fa83-0001fa83      0     Order: 5
0001fa83-0001fa83      1     Wasted bits flag: 1
0001fa83-0001fa84      8     Wasted bits k: 8
0001fa84-0001fa8e     80     Warmup samples: 5 samples
0001fa8e-0001fa8e      4     Precision: 15
0001fa8f-0001fa8f      5     Shift: 13
0001fa8f-0001fa98     75     Coefficients: 5 coefficients
0001fa99-0001fa99      2     Residual coding method: Rice (4)
0001fa99-0001fa99      0     Rice parameter bits: 4
0001fa99-0001fa99      0     Rice escape code: 15
0001fa99-0001fa99      4     Partition order: 0
0001fa99-0001fa99      0     Rice partitions: 1
0001fa99-0001fa99          - Partition:
0001fa99-0001fa9a      4       Rice parameter: 0
0001fa9a-0001fd45   5466       Samples: 2623 samples
0001fd45-0001fd45      4   Byte align padding: 0
0001fd46-0001fd47     16   Footer CRC: 643a (correct)
0001fd48-0001fd47      0 MD5: 6e852e22654d74254205c304a6fa5d69 (correct)
0001fd48-0001fd47      0 Stream samples: 44100
0001fd48-0001fd47      0 Decoded samples: 44100
```

## Why?

I wanted to learn more about FLAC and in the process write a
[HexFiend](https://github.com/ridiculousfish/HexFiend) binary template to parse the overall
structure. It turns out you have to parse quite a lot to figure out the size of a subframe
so why not try to decode the whole thing.

## Notes

Overall structure

```
Magic "fLaC"
One or more metablocks:
  <Streaminfo>
    Sample size
    Sample rate
    Block size
    Number of channels
  <Seektable etc>
Zero or more <Frame>:
  <Frame>
    Order
    Channel configuration (mono, stereo, side/mid)
    CRC
    One <Subframe> for each channel:
      <Subframe>
        One of:
          <Constant>
            Same sample value whole frame
          <Verbatim>
            Uncompressed samples
          <Fixed> (LPC with predefined coefficients)
            <Warmup samples>
            <Residuals>
          <LPC>
            <Warmup samples>
              Order * sample size samples
            <Coefficients>
              Precision
              Shift
              Order * precision coefficients
            <Residuals>
              One of:
                Rice encoding with 4 bit:
                Rice encoding with 5 bit:
                  One or more partitions:
                    Rice encoded samples:
                      Escaped uncompressed samples
                      Zigzag integer:
                        Unary encoded high bits + rice bits low bits
    Byte align bits
    CRC
```

Side/Mid subframes calculate final left/right samples based on other or all subframes in same frame

Side subframe has one extra sample size bit. Where is it documented?

How to handle frame with different sample rate or sample size than in streaminfo? always same?

Where is zigzag integer encoding usage documented?

Where is UTF-8 encoded integer documented?

Where are fixed LPC coefficients documented?

Wasted bits are subtracted from sample size before reading subframe and shift back after decoding

Number of samples in a subframe is warmup samples + rice partition samples, should add up to frame.blocksize
