# flac.tcl

Probably the slowest and most verbose FLAC decoder in the world.

Features:

- Very verbose output
- 8, 16, 24 bit depth. Probably supports 32 bit also but not tested as there seems to be no encoder that supports it
- All channel configurations but only mono, stereo and side channel stereo files tested

## Usage

```sh
./flac.tcl file.flac file.wav
```

Run tests, requires ffmpeg, tcl and flac cli tool:

```sh
./test.sh ./flac.tcl [TEMPDIR]
```
Or run tests using docker:
```sh
docker build .
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
