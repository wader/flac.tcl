# flac.tcl

Probably the slowest and most verbose FLAC decoder in the world.

Features:

- Very verbose output
- 8, 16, 24 bit depth. Probably supports 32 bit also but not tested as there seems to be no encoder that supports it
- All channel configurations but only mono, stereo and side channel stereo files tested
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
00000008-00000009     16   Minimum block size (samples): 4096
00000010-00000011     16   Maximum block size (samples): 4096
00000012-00000014     24   Minimum frame size (bytes): 14
00000015-00000017     24   Maximum frame size (bytes): 14652
00000018-00000020     20   Sample rate: 44100
00000020-00000020      3   Channels: 2
00000020-00000021      5   Bits per sample: 16
00000021-00000025     36   Total samples in stream: 10162992
00000026-00000041    128   MD5: f9261a51f2004e050c9b7475030861ed
00000042-00000042      - Metablock:
00000042-00000042      1   Last block: 0
00000042-00000042      7   Type: Seektable
00000043-00000045     24   Length: 432
00000046-00000477      0   Data:
00000478-00000478      - Metablock:
00000478-00000478      1   Last block: 0
00000478-00000478      7   Type: Vorbis comment
00000479-00000481     24   Length: 40
00000482-00000521      0   Data:
00000522-00000522      - Metablock:
00000522-00000522      1   Last block: 1
00000522-00000522      7   Type: Padding
00000523-00000525     24   Length: 8192
00000526-00008717      0   Data:
...
00075789-00075789      - Frame:
00075789-00075790     14   Sync: 3ffe (correct)
00075790-00075790      1   Reserved: 0 (correct)
00075790-00075790      1   Blocking strategy: Fixed
00075791-00075791      4   Block siz: 4096
00075791-00075791      4   Sample rate: 44100
00075792-00075792      4   Channel assignment: side/right
00075792-00075792      3   Sample size: 16
00075792-00075792      1   Reserved: 0 (correct)
00075793-00075793        - End of header:
00075793-00075793      8     Frame number: 27
00075794-00075794      8   CRC: 35 (correct)
00075795-00075795        - Subframe:
00075795-00075794      0     Side channel bits: 1
00075795-00075795      1     Zero bit: 0 (correct)
00075795-00075795      6     Subframe type: LPC
00075795-00075795      0     Order: 5
00075795-00075795      1     Wasted bits flag: 0
00075796-00075806     85     Warmup samples: 5 samples
00075806-00075807      4     Precision: 12
00075807-00075807      5     Shift: 9
00075807-00075815     60     Coefficients: 5 coefficients
00075815-00075815      2     Residual coding method: Rice (4)
00075815-00075815      0     Rice parameter bits: 4
00075815-00075815      0     Rice escape code: 15
00075815-00075815      4     Partition order: 6
00075816-00075815      0     Rice partitions: 64
00075816-00075816          - Partition:
00075816-00075816      4       Rice parameter: 2
00075816-00075844    228       Samples: 59 samples
00075845-00075845          - Partition:
00075845-00075845      4       Rice parameter: 2
00075845-00075875    237       Samples: 64 samples
...
00197209-00197209      2   Byte align padding: 0
00197210-00197211     16   Footer CRC: 6163 (correct)
00197212-00197211      0 MD5: f9261a51f2004e050c9b7475030861ed (correct)
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
