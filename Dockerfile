FROM alpine:3.11.6
RUN apk add tcl ffmpeg flac
COPY flac.tcl test.sh ./
RUN ./test.sh ./flac.tcl
ENTRYPOINT ["./flac.tcl"]
