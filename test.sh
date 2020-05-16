#!/bin/sh

FLACTLC="$1"
TEMPDIR="$2"
if [ "$TEMPDIR" = "" ]; then
    TEMPDIR=$(mktemp -d)
fi

# anullsrc to get constant frames
# sine to get fixed/lpc frames
# anoisesrc to get verbatim frames
# mix them to get side/mid/left/right side channels
ffmpeg \
    -y \
    -t 200ms -f lavfi -i anullsrc=sample_rate=44100 \
    -t 200ms -f lavfi -i anullsrc=sample_rate=44100 \
    -t 200ms -f lavfi -i sine=sample_rate=44100:frequency=100 \
    -t 200ms -f lavfi -i sine=sample_rate=44100:frequency=600 \
    -t 200ms -f lavfi -i anoisesrc=sample_rate=44100:seed=1 \
    -t 200ms -f lavfi -i anoisesrc=sample_rate=44100:seed=2 \
    -t 200ms -f lavfi -i sine=sample_rate=44100:frequency=600 \
    -t 200ms -f lavfi -i sine=sample_rate=44100:frequency=50 \
    -t 200ms -f lavfi -i anoisesrc=sample_rate=44100:seed=3 \
    -t 200ms -f lavfi -i sine=sample_rate=44100:frequency=20 \
    -filter_complex \
'[0:0][1:0]amerge=inputs=2[merge0];
[2:0][3:0]amerge=inputs=2[merge1];
[4:0][5:0]amerge=inputs=2[merge2];
[6:0][7:0]amerge=inputs=2[merge3];
[8:0][9:0]amerge=inputs=2[merge4];
[merge0][merge1][merge2][merge3][merge4]concat=n=5:v=0:a=1[concat0];
[concat0]asplit=outputs=3[out0][out1][out2]' \
    -map '[out0]' -ar 44100 -ac 2 -c:a pcm_u8 -f wav "$TEMPDIR/8.wav" \
    -map '[out1]' -ar 44100 -ac 2 -c:a pcm_s16le -f wav "$TEMPDIR/16.wav" \
    -map '[out2]' -ar 44100 -ac 2 -c:a pcm_s24le -f wav "$TEMPDIR/24.wav"

for BITS in 8 16 24; do
    SFMT=s${BITS}le
    if [ "$BITS" = "8" ]; then
        SFMT=u8
    fi

    for ENCODER in ffmpeg libflac; do
        # skip, ffmpeg flac encoder seems to not support 8bit and falls back to 16bit
        if [ "$ENCODER" = "ffmpeg" ] && [ $BITS = 8 ]; then
            continue
        fi

        case $ENCODER in
        ffmpeg) ffmpeg -i "$TEMPDIR/$BITS.wav" "$TEMPDIR/$BITS.ffmpeg.flac";;
        libflac) flac -o "$TEMPDIR/${BITS}.libflac.flac" "$TEMPDIR/$BITS.wav";;
        esac
        ffmpeg -i "$TEMPDIR/$BITS.wav" -f $SFMT -ac 2 "$TEMPDIR/$BITS.wav.pcm"

        "$FLACTLC" "$TEMPDIR/$BITS.$ENCODER.flac" "$TEMPDIR/$BITS.$ENCODER.flactcl.wav" || exit 1
        ffmpeg -i "$TEMPDIR/$BITS.$ENCODER.flactcl.wav" -f $SFMT -ac 2 "$TEMPDIR/$BITS.$ENCODER.flactcl.wav.pcm"
    done
done

for BITS in 8 16 24; do
    for ENCODER in ffmpeg libflac; do
        if [ "$ENCODER" = "ffmpeg" ] && [ $BITS = 8 ]; then
            continue
        fi

        if ! cmp "$TEMPDIR/$BITS.wav.pcm" "$TEMPDIR/$BITS.$ENCODER.flactcl.wav.pcm"; then
            echo diff "$TEMPDIR/$BITS.wav.pcm" "$TEMPDIR/$BITS.$ENCODER.flactcl.wav.pcm"
            exit 1
        fi
    done
done

ls -l "$TEMPDIR"
