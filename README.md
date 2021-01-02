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
00000008-00000009     16   Minimum block size (samples): 4096
00000010-00000011     16   Maximum block size (samples): 4096
00000012-00000014     24   Minimum frame size (bytes): 16
00000015-00000017     24   Maximum frame size (bytes): 24586
00000018-00000020     20   Sample rate: 44100
00000020-00000020      3   Channels: 2
00000020-00000021      5   Bits per sample: 24
00000021-00000025     36   Total samples in stream: 44100
00000026-00000041    128   MD5: 6e852e22654d74254205c304a6fa5d69
00000042-00000042      - Metablock:
00000042-00000042      1   Last block: 0
00000042-00000042      7   Type: Seektable
00000043-00000045     24   Length: 18
00000046-00000046        - Seekpoint:
00000046-00000053     64     Sample number: 0
00000054-00000061     64     Offset: 0
00000062-00000063     16     Number of samples: 4096
00000064-00000064      - Metablock:
00000064-00000064      1   Last block: 0
00000064-00000064      7   Type: Vorbis comment
00000065-00000067     24   Length: 84
00000068-00000071     32   Vendor length: 32
00000072-00000103    256   Vendor string: reference
00000104-00000107     32   User comment list length: 1
00000108-00000108        - User comments:
00000108-00000108          - 0:
00000108-00000111     32       Length: 40
00000112-00000151    320       String: WAVEFORMATEXTENSIBLE_CHANNEL_MASK=0x0003
00000152-00000152      - Metablock:
00000152-00000152      1   Last block: 1
00000152-00000152      7   Type: Padding
00000153-00000155     24   Length: 8192
00000156-00008347  65536   Data:
...
00121602-00121602      - Frame:
00121602-00121603     14   Sync: 3ffe (correct)
00121603-00121603      1   Reserved: 0 (correct)
00121603-00121603      1   Blocking strategy: Fixed
00121604-00121604      4   Block siz: end of header (16 bit)
00121604-00121604      4   Sample rate: 44100
00121605-00121605      4   Channel assignment: left, right
00121605-00121605      3   Sample size: 24
00121605-00121605      1   Reserved: 0 (correct)
00121606-00121606        - End of header:
00121606-00121606      8     Frame number: 9
00121607-00121608     16     Block size: 2628
00121609-00121609      8   CRC: 28 (correct)
00121610-00121610        - Subframe:
00121610-00121609      0     Side channel bits: 0
00121610-00121610      1     Zero bit: 0 (correct)
00121610-00121610      6     Subframe type: LPC
00121610-00121610      0     Order: 1
00121610-00121610      1     Wasted bits flag: 0
00121611-00121613     24     Warmup samples: 1 samples
00121614-00121614      4     Precision: 15
00121614-00121615      5     Shift: 15
00121615-00121616     15     Coefficients: 1 coefficients
00121617-00121617      2     Residual coding method: Rice2 (5)
00121617-00121617      0     Rice parameter bits: 5
00121617-00121617      0     Rice escape code: 31
00121617-00121617      4     Partition order: 1
00121617-00121617      0     Rice partitions: 2
00121617-00121617          - Partition:
00121617-00121618      5       Rice parameter: 22
00121618-00125637  32150       Samples: 1313 samples
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
